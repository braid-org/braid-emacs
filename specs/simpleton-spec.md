# Simpleton Protocol Specification

Simpleton is a synchronization protocol for light clients.  It features:

- Implementable in just **~45 lines of client code**
- **Zero history** on the client
- **Any CRDT** on the server — simpleton only requires a diff function
- **Universal interoperable protocol:** interoperable versions, parents, and patches

Simpleton splits the work of collaborative editing between a **simpleton
client** and a **CRDT-capable server**:

- The **client** sends local edits as diffs and applies incoming patches. It
  maintains no history — just the current text, the current version, and a
  character counter for version IDs.
- The **server** runs a full CRDT (e.g. diamond-types). When it receives a
  client edit, it merges it into the CRDT, then rebases the server's current
  state onto the client's version and sends the diff back.

This design imparts one tradeoff:

- **Tradeoff**: the client is half-duplex. After sending a PUT, it will reject
  all incoming updates until the server responds with an update whose parents
  match the client's optimistic version.

# Protocol Messages

Simpleton can be easily expressed over Braid-HTTP, or any [CTM](/time-machines) protocol.  It uses `simpleton` Merge-Type, which can be expressed in HTTP with `Merge-Type: simpleton`.  When a client does a GET with `Merge-Type: simpleton`, it effectively means:

> "Hey server, I'm only a simpleton.  I cannot understand merges.  I only understand a line of time.  For any update you send me that exists in parallel to a version I already have, I will just ignore your update.  So it's on you to rebase that update on top of the versions that I have, which I am sending to you."

### Subscribe (GET)

```http
GET /resource HTTP/1.1
Host: example.com
Subscribe: true
Merge-Type: simpleton
Accept: text/plain
Peer: <peer>
```

If the client is reconecting a previous connection:

```http
GET /resource HTTP/1.1
Host: example.com
Subscribe: true
Merge-Type: simpleton
Accept: text/plain
Peer: <peer>
Parents: <client-version>
```

### Server Response (209 Multiresponse)

Initial snapshot (no parents — first connection):

```http
HTTP/1.1 209 Multiresponse
Subscribe: true

200 OK
Version: <version>
Merge-Type: simpleton
Content-Type: text/plain
Content-Length: <n>

<full text body>
```

Subsequent patches:

```http
200 OK
Version: <new-version>
Parents: <client-version>
Merge-Type: simpleton
Content-Type: text/plain
Content-Range: text [<start>:<end>]
Content-Length: <n>

<patch content>
```

### Client Edit (PUT)

```http
PUT /resource HTTP/1.1
Host: example.com
Version: "<peer>-<char_counter>"
Parents: <client-version>
Merge-Type: simpleton
Peer: <peer>
Content-Type: text/plain
Content-Range: text [<start>:<end>]
Content-Length: <n>

<patch content>
```

# Client Algorithm

The simpleton client uses a **decoupled update mechanism**.
1. Rather than sending each keystroke immediately, the application signals `changed()` 
2. The client decides *when* to actually diff and send based on network conditions and outstanding PUTs.
3. When ready, it diffs `client_state` against the current text and sends the result. 

This means throttling, batching, and offline editing become possible. Editing offline for hours, for instance, produces just one efficient diff upon reconnect.


The client has two modes:

 - **Online:** Each client edit is PUT to server immediately. The `client_state` is always equivalent to the current textarea value.
 - **Offline:** Edits are not sent to the server.  Instead, they are made directly to the textarea.  The `client_state` is frozen at the last-synced state.  Then, upon, reconnect, the client does a `diff(client_state, textarea)` and sends those offline edits to the server in one big patch, and then goes back online.

### State

A simpleton client maintains:

| Variable | Type | Description |
|----------|------|-------------|
| `client_version` | list of strings | The version the client is currently at (optimistically advanced on PUT) |
| `client_state` | string | The text content as of `client_version` |
| `peer` | string | Random unique identifier for this client |
| `char_counter` | integer | Cumulative count of inserted + deleted characters (unicode code points when using the `text` range unit); used to form version IDs. Starts at -1. |
| `outstanding_puts` | integer | Number of PUTs sent but not yet acknowledged |

### Receiving Updates

When an update arrives with `version` and `parents`:

1. **Parent check**: If `parents ≠ client_version`, silently drop the update.
   This is the core simpleton invariant — the client only accepts updates that
   are based on what it thinks is current.
2. **Apply the update**:
   - If the update has `patches`, apply them to the buffer/state.
     Each patch replaces `[start:end)` with the patch content.
     **Important: Patches use absolute positions.** Each patch's `start` and
     `end` refer to positions in the original state (before any patches in this
     update are applied), not to the state after previous patches. When applying
     patches sequentially, maintain a running offset to adjust positions:
     ```
     offset = 0
     for each patch:
         adjusted_start = patch.start + offset
         adjusted_end   = patch.end   + offset
         apply replacement at [adjusted_start:adjusted_end)
         offset += length(patch.content) - (patch.end - patch.start)
     ```
   - If the update has a full `body` (no patches), replace the entire state.
     This normally only happens on initial connection (when `client_version`
     is the empty array).
3. **Advance version**: Set `client_version = version` (from the update).
4. **Update client_state**: Set `client_state` to the current state (after
   applying patches).
5. **Integrity check** (optional): If the server sent a `Repr-Digest` header,
   compute the SHA-256 digest of the current state and compare. On mismatch, enter error state.

### Sending Edits

When the user makes a local edit:

1. **Throttle check**: If `outstanding_puts ≥ max_outstanding_puts` (default
   20), do not send. The edit will be captured on the next flush after an ACK
   arrives. See [Throttling](#throttling-optional-).
2. **Diff**: Compute the diff between `client_state` and the current state. A
   minimal single-edit diff (common prefix + common suffix) is sufficient.
3. **Compute version**: Increment `char_counter` by the number of characters
   inserted + deleted. Set the new version to `["<peer>-<char_counter>"]`.
4. **Send PUT**: Send the patch with `version` and `parents = client_version`.
5. **Advance version optimistically**: Set `client_version = version` (the
   one we just sent). This is key — the client now expects the server's next
   update to be parented at this version.
6. **Update client_state**: Set `client_state` to the current state.
7. **Increment outstanding_puts**.

### Reconnecting

When the network connection drops and the client reconnects:

1. **Open a new subscription** with `Parents: <client_version>`. This tells
   the server where the client last was.
2. **Retry unacknowledged PUTs.** The PUT connection is independent of the
   subscription. Queued PUTs still have valid version chains (since
   `client_version` is not reset) and must be retried on a new connection
   until the server acknowledges them. Without this, the server will never
   learn about the client's edits and cannot send correctly-parented updates.
3. **Reset `outstanding_puts` to 0** for throttling purposes — the old PUT
   connection is gone, so its ACKs will never arrive. The retried PUTs will
   increment the counter again as they are re-sent.

Note: Do *not* change `client_version`. The client's optimistic version is still valid — it represents the state the client has.  Also, do not modify the buffer/state.  The buffer may contain local edits made while offline.

What happens on the server side: <note>Uh, shouldn't the server stuff be described in the server section?  The audience is either implementing a server or a client, and should see all the server or client stuff in one place.</note>

- If the server recognizes the `Parents:`: it sends patches from that
  version to the current server state. These patches are applied incrementally
  to the client buffer, preserving any local edits in other regions.

- If the server does not recognize the `Parents:` (lost PUTs that advanced
  the version locally but never reached the server): the server registers the
  client and waits. When the client's PUT retry succeeds, the server processes
  it and then sends a rebased update with parents matching the client's
  version.

This is the critical insight: **patches applied incrementally preserve local
edits** because they only modify the regions that changed on the server.
Replacing the entire buffer with a snapshot would obliterate local edits — but
reconnecting with `client_version` as parents ensures the server sends patches
instead.

# Server Algorithm

### Per-Client State

For each simpleton client subscription, the server tracks:

| Variable | Type | Description |
|----------|------|-------------|
| `my_last_seen_version` | version | The last version the server received from (or sent to) this client |
| `my_last_sent_version` | version | The last version the server sent to this client |
| `timeout` | timer | Deferred rebase timer (for batching server-side updates) |

### On Subscribe

When a simpleton client subscribes:

- If **no Parents** header: send the current state as a full snapshot with the
  current server version. Set `my_last_sent_version` to the server's current
  version.
- If **Parents are known** (server has this version): send patches from the
  parent version to the server's current version, rebased onto the client's
  version. Set `my_last_sent_version` accordingly.
- If **Parents are unknown** (client's optimistic version from lost PUTs):
  register the client but do NOT send anything yet. Wait for the client's PUT
  to arrive, which will establish the version in the CRDT.

### On Receiving a PUT from a Simpleton

1. **Apply the edit** to the server's CRDT.
2. **Check if the server needs to send a rebase**: If `my_last_sent_version ≠
   PUT's parents`, the client has moved ahead of what the server last told it.
   The server should send a rebased update:
   - Compute the diff from `my_last_sent_version` to the server's current
     version, rebased onto the client's version (i.e., the PUT's version).
   - Send this as patches with `Parents: <client's PUT version>`.
   - Update `my_last_sent_version`.
3. **If the versions match** (`my_last_sent_version == PUT's parents`): the
   client is in sync. The server updates `my_last_seen_version` and will send
   the next update when other peers edit.

### Sending Updates to a Simpleton (from other peers' edits)

When the server receives an edit from another peer and needs to forward it to
a simpleton client:

1. If the client has **unseen versions** (edits the server sent but the client
   hasn't acknowledged), use a **timeout with exponential backoff** before
   sending:
   - The client may reject the update (parent mismatch from its own in-flight PUTs).
   - Wait and batch multiple updates into one.
   - On timeout: compute the rebase from `my_last_sent_version` to the
     server's current version, send as patches.
2. If the client is **caught up** (`my_last_seen_version == my_last_sent_version`):
   send the update immediately.

# Throttling (optional)

A unique ability of the simpleton algorithm is to *throttle* the frequency of updates, if a server, client, or network gets overloaded.  Since it is rebasing changes (both from the server to the client, and from the client to the server), it can decrease the frequency of updates, by effectively sending larger blobs of patches smashed together.

Both the server and the client can implement throttles.

<!--Throttling prevents the O(n²) problem described in [meeting-81](meeting-81.md) <note>Not true. We prevented that with a better server algorithm alrady.</note>
where rapid client edits cause the server to rebase repeatedly.-->

**Client-side throttling:**

- Track `outstanding_puts` (PUTs sent but not ACKed with a `200 OK`).
- When `outstanding_puts ≥ max_outstanding_puts` (default 10), stop sending.
- The client continues to accumulate local edits in its buffer.
- When a PUT ACK arrives (decrementing `outstanding_puts`), flush the
  accumulated edits as a single diff.

**Server-side backpressure (via response status 503):**

- If the server returns HTTP 503, the client enters a "mute" period (e.g. 3
  seconds) during which no PUTs are sent.
- Edits continue to accumulate locally.
- After the mute period expires, the client flushes accumulated edits as a
  single diff.

Why this works:

- When throttled, the client batches multiple small edits into one larger diff.
- This reduces the number of operations the server must rebase.
- The diff is computed against `client_state`, which was set after the last
  successful PUT — so the batched diff captures everything since then.

# PUT Response Status Codes

The server responds to each PUT with a status code. The client should handle:

| Status | Meaning | Client action |
|--------|---------|---------------|
| 2xx | Success | Decrement `outstanding_puts`. Flush accumulated edits. |
| 309 | **Version Unknown Here** — the server does not have the parents referenced by this PUT. This can happen when PUTs arrive out of order, or the server has not yet received an earlier PUT in the version chain. The server includes a `Retry-After` header. | Decrement `outstanding_puts`. Retry after the indicated delay (default 1s) by flushing — the flush re-diffs and re-sends, which will include the lost edit. |
| 503 | **Server overloaded** — backpressure signal. | Decrement `outstanding_puts`. Enter a mute period (e.g. 3 seconds) during which no PUTs are sent. Flush after the mute expires. |
| 550 | **Permanent rejection** — the server has determined the edit is invalid (e.g. repr-digest mismatch on the server side). | **Fatal error.** Stop syncing, close connections, alert the user. The edit is unrecoverable. |
| Other 4xx/5xx | Transient or unexpected error. | Decrement `outstanding_puts`. Retry after a delay (e.g. 1s) by flushing. |

# Protocol Details

## Version Format

<note>This section should move to /protocol/version-type/`<name>`</note>

Versions use this Version-Type:
```
Version-Type: peer-counter, rle
```

This Version-Type is defined in [draft-toomim-httpbis-versions](https://datatracker.ietf.org/doc/html/draft-toomim-httpbis-versions). To summarize here, each Event ID follows the format: `"<peer>-<char_counter>"`

- `peer`: A random alphanumeric string unique to this client session (e.g.
  `"4ye80t11max"`).
- `char_counter`: A monotonically increasing integer starting at -1. It
  increments by the number of characters (unicode codepoints, when using the `text` range unit) inserted plus the number of characters
  deleted in each edit. For example, deleting 3 characters and inserting 5
  characters increments the counter by 8.

Event IDs are transmitted as Structured Headers strings (double-quoted):
`"4ye80t11max-7"`.

Multiple Event IDs are comma-separated to compose a merge version: `"abc-123", "xyz-789"`.


## Patch Format

All patches are [Range Patches](https://raw.githubusercontent.com/braid-org/braid-spec/master/draft-toomim-httpbis-range-patch-01.txt). For text simpletons, the `Content-Range` header uses the range unit of `text`: <note>We need a spec for the text range unit, at [/protocol/range/text](/protocol/range/text)</note>

```
Content-Range: text [<start>:<end>]
```

- `start`: 0-indexed code-point offset where the replacement begins (inclusive)
- `end`: 0-indexed code-point offset where the replacement ends (exclusive)
- The body is the replacement text

Examples:
- Insert "xyz" at position 5: `Content-Range: text [5:5]` with body `xyz`
- Delete positions 3-7: `Content-Range: text [3:7]` with empty body
- Replace positions 0-3 with "hi": `Content-Range: text [0:3]` with body `hi`


## Protocol Examples

### Example 1: Initial Connection

Client subscribes and gets the full state:

```
→ GET /doc
  Subscribe: true
  Merge-Type: simpleton
  Peer: abc123

← HTTP/1.1 209 Multiresponse
  Subscribe: true

  200 OK
  Version: "server-42"
  Content-Type: text/plain
  Content-Length: 11

  hello world
```

Client state after: `client_version = ["server-42"]`, `client_state = "hello world"`

### Example 2: Client Edit

User types "!" at position 11:

```
→ PUT /doc
  Version: "abc123-0"
  Parents: "server-42"
  Merge-Type: simpleton
  Peer: abc123
  Content-Type: text/plain
  Content-Range: text [11:11]
  Content-Length: 1

  !
```

Client state after: `client_version = ["abc123-0"]`, `client_state = "hello world!"`

### Example 3: Concurrent Edit (Server Sends Rebase)

While the client's PUT is in flight, another peer inserts "dear " at position 6.
The server merges both, then sends the rebased result to our client:

```
← 200 OK
  Version: "server-43"
  Parents: "abc123-0"
  Content-Range: text [6:6]
  Content-Length: 5

  dear
```

The client checks: `parents ["abc123-0"] == client_version ["abc123-0"]`? Yes.
Applies the patch. Buffer becomes "hello dear world!".

Client state after: `client_version = ["server-43"]`, `client_state = "hello dear world!"`

### Example 3b: Multi-Patch Update (Absolute Positions)

If multiple concurrent edits are merged, the server may send multiple patches
in a single update. Patches use **absolute positions** — all relative to the
original state before any patches are applied.

Given `client_state = "abcdefghij"` (10 chars), the server sends:

```
← 200 OK
  Version: "server-44"
  Parents: "abc123-0"
  Patches: 2

  Content-Range: text [2:4]
  Content-Length: 3

  XYZ

  Content-Range: text [7:8]
  Content-Length: 2

  QR
```

Applying with offset tracking:
1. Patch 1: replace `[2:4]` with "XYZ" → `"abXYZefghij"`, offset = 3-2 = +1
2. Patch 2: replace `[7+1:8+1]` = `[8:9]` with "QR" → `"abXYZefgQRij"`

Without offset tracking, patch 2 would incorrectly replace `[7:8]` in the
already-modified string, producing `"abXYZefQRhij"` — **wrong**.

### Example 4: Reconnection

Client disconnects, makes local edits (buffer now has "hello dear world! bye"),
then reconnects:

```
→ GET /doc
  Subscribe: true
  Merge-Type: simpleton
  Peer: abc123
  Parents: "abc123-0"
```

Note: `Parents` is `client_version` — the client's optimistic version from its
last PUT. If the PUT reached the server, the server knows this version. If not,
the server will wait for the PUT retry.

Server responds with patches from the client's version to current:

```
← HTTP/1.1 209 Multiresponse
  Subscribe: true

  Version: "server-50"
  Parents: "abc123-0"
  Content-Range: text [0:5]
  Content-Length: 2

  hi
```

Client checks parents match, applies patch: "hello" → "hi" at [0:5].
Buffer becomes "hi dear world! bye" (local " bye" edit preserved because the
patch only touched positions 0-5).

### Example 5: Lost PUT (Unknown Parents on Reconnect)

Client sent a PUT that never reached the server, then reconnects with the
optimistic version as parents. The server doesn't recognize this version:

```
→ GET /doc
  Subscribe: true
  Merge-Type: simpleton
  Peer: abc123
  Parents: "abc123-5"
```

Server registers the client but sends nothing (unknown parents). The client's
PUT retry mechanism eventually re-sends the PUT. Once the server receives and
processes it, it sends the rebased update with the correct parents:

```
← 200 OK
  Version: "server-60"
  Parents: "abc123-5"
  ...
```

The client accepts this because `parents == client_version`.

<style>
td:nth-child(1) { min-width: 190px }
td:nth-child(2) { min-width: 80px }
</style>