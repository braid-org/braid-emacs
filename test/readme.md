# Test Harness

Integration and fuzz tests for braid-emacs against the braid-text server.
Verifies that the Emacs simpleton client (braid-text.el) correctly syncs
text and cursors with the JS reference implementation.

## Architecture

```
                          test-server.js
                        (node, port 8888)
                    ┌─────────┴──────────┐
                    │  /text/* braid-text │  CRDT (diamond-types)
                    │  /*      echo       │  stateless mirror
                    └─────────┬──────────┘
                              │
              ┌───────────────┼───────────────┐
              │               │               │
     ┌────────┴────────┐  ┌──┴───────┐  ┌────┴────────────┐
     │  Emacs agent    │  │ JS       │  │ JS cursor       │
     │  (--batch)      │  │ simpleton│  │ client          │
     │                 │  │ client   │  │ (braid_fetch)   │
     │  braid-text.el  │  │          │  │                 │
     │  braid-http.el  │  │ simpleton│  │ raw HTTP PUTs + │
     │  braid-cursors  │  │ -sync.js │  │ subscription    │
     └────────┬────────┘  └──┬───────┘  └────┬────────────┘
              │              │               │
         stdin/stdout     in-process      in-process
          JSON lines       JS calls        JS calls
              │              │               │
     ┌────────┴──────────────┴───────────────┴────────┐
     │                  Test Driver                    │
     │           (node: fuzz-test.js, etc.)            │
     │                                                 │
     │  1. Spawn clients                               │
     │  2. Generate random edits                       │
     │  3. Optionally kill connections                 │
     │  4. Wait for convergence                        │
     │  5. Compare states ← verification happens here  │
     └────────────────────────────────────────────────┘
```

## Running

Start the test server, then run any test:

```bash
cd test
node test-server.js &

# All tests:
node test-js-vs-js.js && node test-cursors.js && node fuzz-test.js 100

# Individual:
node fuzz-test.js [rounds] [seed]
node test-js-vs-js.js
node test-cursors.js
```

## Test Suites

### `test-js-vs-js.js` — JS-vs-JS convergence (20 rounds)

Two JS simpleton clients edit the same document concurrently. If they
diverge, the bug is in braid-text/simpleton-sync, not in the Emacs code.
This isolates server/protocol bugs from Elisp bugs.

```
  JS client A ──┐
                 ├──→ /text/test-N ──→ compare A.state === B.state
  JS client B ──┘
```

- Random inserts at random positions
- Waits for both clients to converge
- 20 rounds per run, seeded PRNG for reproducibility

### `test-cursors.js` — Cursor sync (6 assertions)

Tests cursor position sharing between Emacs and a JS cursor client.

| Test | What it verifies |
|------|-----------------|
| Emacs → JS | Emacs sets cursor position, JS subscription receives it |
| JS → Emacs | JS sends cursor PUT, Emacs `braid-cursors` receives it |
| Transform | After a text edit, server transforms stored cursor positions |

### `fuzz-test.js` — Emacs-vs-JS fuzz (default 100 rounds)

The main integration test. Each round:

1. Creates a fresh `/text/fuzz-<seed>-<round>` resource
2. Spawns an Emacs `--batch` agent + a JS simpleton client
3. Both connect and subscribe
4. Generates 1-5 random edits on each side (inserts and deletes)
5. 30% chance of network disruption:
   - Kill subscription connection only (10%)
   - Kill PUT connection only (10%)
   - Kill both connections (10%)
6. Waits for reconnection and convergence
7. **Verifies** `js_client.state === emacs_buffer_text`
8. Digest mismatch from the server triggers a failure too

```
  fuzz-test.js (orchestrator)
       │
       ├── spawn_emacs_agent()     ← emacs --batch -l fuzz-emacs-agent.el
       │      stdin:  {"cmd":"insert","pos":3,"text":"hi"}
       │      stdout: {"ok":true}
       │
       ├── create_js_client(url)   ← simpleton_client(url, {on_patches, ...})
       │
       └── for each round:
             random_edit(js)        ← js_client.insert(pos, text)
             random_edit(emacs)     ← emacs.send({cmd:"insert",...})
             maybe_kill_connection  ← emacs.send({cmd:"kill-sub"})
             wait_convergence       ← spin + sleep + retry
             assert(js === emacs)   ← PASS or FAIL
```

Features:
- **Seeded PRNG** (xorshift32) — failures are reproducible with `node fuzz-test.js <rounds> <seed>`
- **Network chaos** — randomly kills subscription and/or PUT connections
- **Convergence retry** — up to 5 extra spin+flush cycles before declaring failure
- **Debug output** — on failure, dumps both states + Emacs stderr

## File Reference

| File | Role |
|------|------|
| `test-server.js` | Combined Node server: `/text/*` braid-text CRDT, `/*` echo |
| `fuzz-test.js` | Fuzz driver: Emacs-vs-JS, random edits + network chaos |
| `fuzz-emacs-agent.el` | Emacs batch agent: JSON stdin/stdout command protocol |
| `test-js-vs-js.js` | JS-vs-JS convergence test (no Emacs) |
| `test-cursors.js` | Cursor sync tests: send, receive, transform |

### Emacs Agent Commands

The fuzz-emacs-agent accepts JSON commands on stdin and responds on stdout:

| Command | Description |
|---------|-------------|
| `connect` | Open braid-text connection to host:port/path |
| `open-cursors` | Start cursor sharing (braid-cursors) |
| `insert` | Insert text at 0-indexed code-point position |
| `delete` | Delete range [start:end) |
| `changed` | Trigger braid-text flush (send pending edits) |
| `wait-ack` | Block until all PUTs are acknowledged |
| `wait-connected` | Block until subscription is established |
| `state` | Return current buffer text |
| `client-state` | Return braid-text's internal client-state |
| `version` | Return current client-version |
| `set-cursor` | Move cursor and send position |
| `remote-cursors` | Return received remote cursor positions |
| `kill-sub` | Kill the subscription TCP connection |
| `kill-put` | Kill the PUT TCP connection |
| `spin` | Pump Emacs event loop for N seconds |
| `quit` | Close connections and exit |

## Verification Strategy

Tests verify correctness at multiple levels:

1. **State convergence**: After edits + network disruption, both clients must
   have identical text. This catches patch application bugs, version chain
   breaks, and reconnection failures.

2. **Digest verification**: The server sends `Repr-Digest: sha-256=:...:` with
   each update. Clients compute SHA-256 of their state and compare. A mismatch
   means the client applied patches incorrectly, even if both clients agree.

3. **Cursor round-trip**: Cursor positions set by one client must appear in the
   other client's received cursors. After text edits, cursor positions must be
   transformed by the edit offset.

## Known Limitations

- **Reconnection under load**: ~1% of fuzz rounds with `disconnect=put` fail
  because lost PUTs break the version chain. This is a known Elisp client bug
  (pending-puts not reset, PUTs not retried after put-proc death), tracked as
  plan items D5/D7.

- **Timing sensitivity**: Tests use `sleep()` and `spin()` for synchronization.
  On very slow machines, convergence timeouts may need to be increased.
