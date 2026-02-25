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

### Opening a URL

Open any braid-text resource directly:

    C-x C-f http://localhost:8888/text/notes RET
    C-x C-f https://dt.braid.org/foo RET

braid-emacs automatically connects and starts syncing. The modeline
shows `●●` when connected and synced, `○○` when edits are pending,
and `**` when disconnected.

Edits you make are pushed to the server immediately, and edits from
other clients appear in real time. The buffer is never saved to disk —
`C-x C-s` is a no-op.


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

- **`braid-mode.el`** — minor mode, URL file handler, and auto-connect.

- **`test-server/`** — Node.js braid-text server for development and
  testing.
