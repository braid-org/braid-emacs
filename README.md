# braid-emacs

**Work in progress.** Collaborative editing for Emacs using the
[Braid-HTTP](https://braid.org) protocol.

## How it works

braid-emacs syncs Emacs buffers with HTTP resources using the
[simpleton](https://braid.org/simpleton) merge algorithm.  It connects
over raw TCP (or TLS) and speaks Braid-HTTP natively — no browser or
middleware required.

### Live mode with braidfs

The easiest way to use braid-emacs is with
[braidfs](https://github.com/braid-org/braidfs), which mounts HTTP
resources as files under `~/http/`.  When you open a file like:

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
~/http/<host>:<port>/<path>   → http://<host>:<port>/<path>
```

This auto-connect behavior is controlled by `braid-mode-auto-live`
(enabled by default).

### Manual connection

You can also connect any buffer manually:

```
M-x braid-connect
```

This prompts for host, port, and path.

## Setup

Add to your Emacs init:

```elisp
(add-to-list 'load-path "~/path/to/braid-emacs")
(require 'braid-mode)
```

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
