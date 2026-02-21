# Braid-Emacs

Collaborative editing from Emacs to anything and everything using the
[Braid-HTTP](https://braid.org) protocol.

## Installation

Add to your Emacs init:

```elisp
(add-to-list 'load-path "~/path/to/braid-emacs")
(require 'braid-mode)
```

## How it works

Syncs Emacs with HTTP resources.  
Uses the [simpleton](https://braid.org/simpleton) merge algorithm.  
Implements HTTP with a raw TLS over TCP connection and reads/writes Braid-HTTP in Emacs Lisp.

### Live Editing

Now you can open files at web resources via your `~/http/` directory.  When you open a file like:

```
~/http/example.com/my-document
~/http/localhost:8888/text/notes
```

braid-emacs automatically connects to the corresponding server and
starts syncing.  The modeline shows `●` when connected.  Edits you make
are pushed to the server immediately, and edits from other clients
appear in real time.

The path convention is:

```
~/http/<host>/<path>          → https://<host>/<path>
~/http/<host>:<port>/<path>   → https://<host>:<port>/<path>
```

You can toggle `live` mode with `M-x braid-live`:
1. **In live mode:** your file is always "saved" to the network.  Each keystroke live updates the web resource, and vice versa.  It never actually never saves to disk.  It syncs directly with the internet.
2. **Otherwise:** your file is edited normally, like to disk.  If you have braidfs running, then it will use the `Save == Sync` semantics.

Files opened within `~/http/*` will be `live` by default when opened. You can configure this with  `braid-mode-auto-live` (enabled by default).

### Manual connection

You can also connect any buffer manually:

```
M-x braid-connect
```

This prompts for host, port, and path.

## Libraries

braid-emacs includes general-purpose Emacs Lisp libraries that can be
used independently of the live-editing mode:

- **`braid-http.el`** — a Braid-HTTP client library.  Subscribe to any
  Braid resource and receive streaming updates (209 Multiresponse
  parsing, TLS support, automatic reconnection).  Send versioned PUTs
  with patches.

- **`braid-text.el`** — a simpleton sync client.  Connects an Emacs
  buffer to a braid-text resource and handles the full sync lifecycle:
  diffing, patching, version tracking, and digest verification.

- **`braid-mode.el`** — minor mode and auto-connect for `~/http/` files.

- **`test-server/`** — Node.js braid-text server for development and
  testing.
