#!/usr/bin/env node
//
// Braid-HTTP test server for braid-http.el development.
//
//   /text/*  → served by braid-text (full simpleton CRDT, persistent state)
//   /*       → simple echo server (stateless, echoes PUTs to subscribers)
//
// Usage:
//   node test-server.js
//

var braid_http = require('braid-http')
var braid_text = require('braid-text')

// subscribers for the echo server: Map<path, Set<res>>
var subscribers = new Map()

function get_subscribers(path) {
    if (!subscribers.has(path)) subscribers.set(path, new Set())
    return subscribers.get(path)
}

require('http').createServer(
    braid_http.http_server(async (req, res) => {
        var path = req.url
        console.log(req.method, path)

        // ── braid-text routes ──────────────────────────────────────────────
        if (path.startsWith('/text/')) {
            try {
                await braid_text.serve(req, res)
            } catch (e) {
                console.error('braid-text error:', e.message)
                if (!res.headersSent) { res.writeHead(500); res.end(e.message) }
            }
            return
        }

        // ── echo routes ────────────────────────────────────────────────────
        if (req.subscribe) {
            var subs = get_subscribers(path)
            subs.add(res)
            res.braid_peer = req.peer || null   // remember peer for echo filtering

            res.startSubscription({ onClose: () => subs.delete(res) })

            // Send empty initial snapshot
            res.sendUpdate({ version: [], parents: [], body: '' })

        } else if (req.method === 'PUT') {
            var update = await req.parseUpdate()
            var version = req.version || []
            var parents = req.parents || []
            var put_peer = req.peer || null

            for (var sub of get_subscribers(path)) {
                // Don't echo back to the subscriber that sent this PUT
                if (put_peer && sub.braid_peer === put_peer) continue
                try {
                    sub.sendUpdate({ version, parents, patches: update.patches })
                } catch (e) {}
            }

            res.statusCode = 200
            res.end()

        } else {
            res.statusCode = 404
            res.end()
        }
    })
).listen(8888, '127.0.0.1', () => {
    console.log('Braid test server listening on http://127.0.0.1:8888')
    console.log('  /*       → echo server (for braid-http.el integration tests)')
    console.log('  /text/*  → braid-text server (simpleton CRDT)')
})
