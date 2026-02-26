#!/usr/bin/env node
//
// Cursor sync test: Emacs braid-cursors vs JS cursor client.
//
// Tests that cursor positions are correctly sent, received, and
// transformed through text edits.
//
// Usage:
//   cd test-server && node test-server.js &
//   node test-cursors.js
//
// Requires: test-server running on 127.0.0.1:8888

var { spawn } = require('child_process')
var readline = require('readline')
var http = require('http')
var path = require('path')
var braid_http = require('braid-http')
var braid_fetch = braid_http.fetch

// ── Config ──────────────────────────────────────────────────────────────

var HOST = '127.0.0.1'
var PORT = 8888
var PASSED = 0
var FAILED = 0

// ── Emacs agent ─────────────────────────────────────────────────────────

function spawn_emacs_agent() {
    var emacs_dir = path.join(__dirname, '..')
    var proc = spawn('emacs', [
        '--batch',
        '-L', emacs_dir,
        '-l', path.join(__dirname, 'fuzz-emacs-agent.el')
    ], {
        stdio: ['pipe', 'pipe', 'pipe'],
        cwd: emacs_dir
    })

    var rl = readline.createInterface({ input: proc.stdout })
    var pending_resolve = null
    var stderr_buf = ''

    proc.stderr.on('data', d => { stderr_buf += d.toString() })

    rl.on('line', line => {
        if (pending_resolve) {
            var resolve = pending_resolve
            pending_resolve = null
            try {
                resolve(JSON.parse(line))
            } catch (e) {
                resolve({ ok: false, error: 'bad JSON: ' + line })
            }
        }
    })

    async function send(cmd, timeout) {
        timeout = timeout || 10000
        return new Promise((resolve, reject) => {
            pending_resolve = resolve
            proc.stdin.write(JSON.stringify(cmd) + '\n')
            setTimeout(() => {
                if (pending_resolve === resolve) {
                    pending_resolve = null
                    reject(new Error(`Emacs timeout on ${cmd.cmd} (${timeout}ms)\nstderr: ${stderr_buf.slice(-500)}`))
                }
            }, timeout)
        })
    }

    return {
        send,
        kill: () => {
            try { proc.stdin.end() } catch(e) {}
            proc.kill('SIGTERM')
        },
        get stderr() { return stderr_buf },
        proc
    }
}

// ── JS cursor client ────────────────────────────────────────────────────

function create_js_cursor_client(url, peer) {
    var cursors = {}  // peer-id → [{from, to}]
    var connected = false
    var ac = new AbortController()

    // Subscribe using braid_fetch
    braid_fetch(url, {
        headers: {
            'Accept': 'application/text-cursors+json',
        },
        subscribe: true,
        retry: () => true,
        peer,
        signal: ac.signal
    }).then(res => {
        connected = true
        res.subscribe(update => {
            // Snapshot: body is a JSON object {peer-id: [{from,to}]}
            if (update.body_text) {
                try {
                    var json = JSON.parse(update.body_text)
                    if (typeof json === 'object' && !Array.isArray(json)) {
                        for (var [k, v] of Object.entries(json))
                            cursors[k] = v
                    }
                } catch(e) {}
            }
            // Patches: per-peer updates
            if (update.patches) {
                for (var p of update.patches) {
                    if (p.range) {
                        // range is like '["peer-id"]'
                        try {
                            var peer_id = JSON.parse(p.range)[0]
                            if (p.content_text) {
                                cursors[peer_id] = JSON.parse(p.content_text)
                            } else {
                                delete cursors[peer_id]
                            }
                        } catch(e) {}
                    }
                }
            }
        })
    }).catch(() => {})

    function send_cursor(from, to) {
        return new Promise((resolve, reject) => {
            var body = JSON.stringify([{from, to}])
            var put = http.request({
                hostname: HOST,
                port: PORT,
                path: new URL(url).pathname,
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/text-cursors+json',
                    'Content-Range': `json ["${peer}"]`,
                    'Peer': peer,
                    'Content-Length': Buffer.byteLength(body)
                }
            }, res => {
                res.resume()
                resolve(res.statusCode)
            })
            put.on('error', reject)
            put.end(body)
        })
    }

    return {
        get cursors() { return cursors },
        get connected() { return connected },
        send_cursor,
        close: () => { ac.abort() }
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────

function sleep(ms) { return new Promise(r => setTimeout(r, ms)) }

function assert_eq(actual, expected, label) {
    if (JSON.stringify(actual) === JSON.stringify(expected)) {
        PASSED++
        console.log(`  PASS: ${label}`)
    } else {
        FAILED++
        console.log(`  FAIL: ${label}`)
        console.log(`    expected: ${JSON.stringify(expected)}`)
        console.log(`    actual:   ${JSON.stringify(actual)}`)
    }
}

// Fetch cursor snapshot via simple GET (no subscription)
function get_cursor_snapshot(test_path) {
    return new Promise((resolve, reject) => {
        var req = http.request({
            hostname: HOST,
            port: PORT,
            path: test_path,
            method: 'GET',
            headers: { 'Accept': 'application/text-cursors+json' }
        }, res => {
            var body = ''
            res.on('data', d => body += d)
            res.on('end', () => {
                try { resolve(JSON.parse(body)) }
                catch(e) { resolve({}) }
            })
        })
        req.on('error', reject)
        req.end()
    })
}

// ── Tests ───────────────────────────────────────────────────────────────

async function test_emacs_sends_cursor() {
    console.log('\nTest: Emacs sends cursor → JS receives')
    var test_path = '/text/cursor-test-send-' + Date.now()
    var url = `http://${HOST}:${PORT}${test_path}`
    var emacs = spawn_emacs_agent()
    var js_cursor = null

    try {
        // Connect Emacs
        await emacs.send({ cmd: 'connect', host: HOST, port: PORT, path: test_path })
        await emacs.send({ cmd: 'wait-connected', timeout: 10 }, 15000)

        // Open cursors on Emacs
        var r = await emacs.send({ cmd: 'open-cursors' }, 15000)
        if (!r.ok) throw new Error('open-cursors failed: ' + r.error)
        await sleep(500)

        // Connect JS cursor client
        var js_peer = 'js-cursor-test-' + Math.random().toString(36).slice(2)
        js_cursor = create_js_cursor_client(url, js_peer)
        await sleep(1000)

        // Insert some text so cursor positions are meaningful
        await emacs.send({ cmd: 'insert', pos: 0, text: 'hello world' })
        await emacs.send({ cmd: 'changed' })
        await emacs.send({ cmd: 'wait-ack', timeout: 5 }, 10000)
        await sleep(300)

        // Set Emacs cursor at position 5
        await emacs.send({ cmd: 'set-cursor', pos: 5 })
        await sleep(500)
        await emacs.send({ cmd: 'spin', duration: 0.5 }, 5000)
        await sleep(500)

        // Check via GET snapshot (more reliable than streaming)
        var snapshot = await get_cursor_snapshot(test_path)
        var found = false
        for (var [peer_id, sels] of Object.entries(snapshot)) {
            if (peer_id !== js_peer && Array.isArray(sels) && sels.length > 0) {
                if (sels[0].from === 5 && sels[0].to === 5) found = true
            }
        }
        assert_eq(found, true, 'Server has Emacs cursor at position 5')

        // Also check JS streaming client received it
        var js_found = false
        for (var [peer_id, sels] of Object.entries(js_cursor.cursors)) {
            if (peer_id !== js_peer && Array.isArray(sels) && sels.length > 0) {
                if (sels[0].from === 5 && sels[0].to === 5) js_found = true
            }
        }
        assert_eq(js_found, true, 'JS streaming client sees Emacs cursor at position 5')

    } finally {
        if (js_cursor) js_cursor.close()
        try { await emacs.send({ cmd: 'quit' }) } catch(e) {}
        await sleep(100)
        emacs.kill()
    }
}

async function test_js_sends_cursor() {
    console.log('\nTest: JS sends cursor → Emacs receives')
    var test_path = '/text/cursor-test-recv-' + Date.now()
    var url = `http://${HOST}:${PORT}${test_path}`
    var emacs = spawn_emacs_agent()
    var js_cursor = null

    try {
        // Connect Emacs + open cursors
        await emacs.send({ cmd: 'connect', host: HOST, port: PORT, path: test_path })
        await emacs.send({ cmd: 'wait-connected', timeout: 10 }, 15000)
        var r = await emacs.send({ cmd: 'open-cursors' }, 15000)
        if (!r.ok) throw new Error('open-cursors failed: ' + r.error)
        await sleep(500)

        // Insert some text
        await emacs.send({ cmd: 'insert', pos: 0, text: 'hello world' })
        await emacs.send({ cmd: 'changed' })
        await emacs.send({ cmd: 'wait-ack', timeout: 5 }, 10000)
        await sleep(300)

        // Connect JS cursor client + subscribe
        var js_peer = 'js-cursor-recv-' + Math.random().toString(36).slice(2)
        js_cursor = create_js_cursor_client(url, js_peer)
        await sleep(1000)

        // JS sends cursor at position 3
        var status = await js_cursor.send_cursor(3, 3)
        assert_eq(status, 200, 'JS cursor PUT returns 200')

        // Wait for Emacs to receive it
        await sleep(500)
        await emacs.send({ cmd: 'spin', duration: 1.0 }, 5000)
        await sleep(300)

        // Check Emacs received the JS cursor
        var remote = await emacs.send({ cmd: 'remote-cursors' })
        var found_cursor = false
        if (remote.cursors) {
            // cursors is alist encoded as array of [key, value] or object
            var cursor_data = remote.cursors
            if (Array.isArray(cursor_data)) {
                for (var pair of cursor_data) {
                    // Each pair: [peer-id, [{from:3, to:3}]]
                    var sels = pair[1] || pair
                    if (Array.isArray(sels)) {
                        for (var sel of sels) {
                            if (sel && sel.from === 3 && sel.to === 3) found_cursor = true
                        }
                    }
                }
            } else if (typeof cursor_data === 'object') {
                for (var sels of Object.values(cursor_data)) {
                    if (Array.isArray(sels)) {
                        for (var sel of sels) {
                            if (sel && sel.from === 3 && sel.to === 3) found_cursor = true
                        }
                    }
                }
            }
        }
        assert_eq(found_cursor, true, 'Emacs sees JS cursor at position 3')

    } finally {
        if (js_cursor) js_cursor.close()
        try { await emacs.send({ cmd: 'quit' }) } catch(e) {}
        await sleep(100)
        emacs.kill()
    }
}

async function test_cursor_after_text_edit() {
    console.log('\nTest: Cursor positions transform through text edits')
    var test_path = '/text/cursor-test-xf-' + Date.now()
    var url = `http://${HOST}:${PORT}${test_path}`
    var emacs = spawn_emacs_agent()
    var js_cursor = null

    try {
        // Connect Emacs + open cursors
        await emacs.send({ cmd: 'connect', host: HOST, port: PORT, path: test_path })
        await emacs.send({ cmd: 'wait-connected', timeout: 10 }, 15000)
        var r = await emacs.send({ cmd: 'open-cursors' }, 15000)
        if (!r.ok) throw new Error('open-cursors failed: ' + r.error)
        await sleep(500)

        // Insert initial text: "abcdefghij"
        await emacs.send({ cmd: 'insert', pos: 0, text: 'abcdefghij' })
        await emacs.send({ cmd: 'changed' })
        await emacs.send({ cmd: 'wait-ack', timeout: 5 }, 10000)
        await sleep(300)

        // Connect JS cursor + subscribe
        var js_peer = 'js-cursor-xf-' + Math.random().toString(36).slice(2)
        js_cursor = create_js_cursor_client(url, js_peer)
        await sleep(1000)

        // JS sets cursor at position 5 (between 'e' and 'f')
        await js_cursor.send_cursor(5, 5)
        await sleep(500)

        // Verify server has cursor at 5
        var snapshot = await get_cursor_snapshot(test_path)
        var pre_edit_pos = snapshot[js_peer]?.[0]?.from
        assert_eq(pre_edit_pos, 5, 'JS cursor initially at position 5')

        // Emacs inserts "XX" at position 2 (before the cursor)
        // "abcdefghij" → "abXXcdefghij"
        // Cursor should shift from 5 → 7
        await emacs.send({ cmd: 'insert', pos: 2, text: 'XX' })
        await emacs.send({ cmd: 'changed' })
        await emacs.send({ cmd: 'wait-ack', timeout: 5 }, 10000)
        await sleep(500)

        // Check server-side cursor transform
        snapshot = await get_cursor_snapshot(test_path)
        var post_edit_pos = snapshot[js_peer]?.[0]?.from
        assert_eq(post_edit_pos, 7,
            'Server transformed JS cursor from 5 → 7 after insert at pos 2')

    } finally {
        if (js_cursor) js_cursor.close()
        try { await emacs.send({ cmd: 'quit' }) } catch(e) {}
        await sleep(100)
        emacs.kill()
    }
}

// ── Main ────────────────────────────────────────────────────────────────

async function main() {
    console.log('Cursor sync tests')
    console.log(`Server: http://${HOST}:${PORT}`)

    await test_emacs_sends_cursor()
    await test_js_sends_cursor()
    await test_cursor_after_text_edit()

    console.log(`\n========================================`)
    console.log(`Cursor results: ${PASSED} passed, ${FAILED} failed`)
    console.log(`========================================`)
    process.exit(FAILED > 0 ? 1 : 0)
}

main().catch(e => {
    console.error('Fatal:', e)
    process.exit(2)
})
