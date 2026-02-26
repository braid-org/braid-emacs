#!/usr/bin/env node
//
// Fuzz test: JS simpleton reference vs Emacs braid-text client.
//
// Drives both clients against the same /text/ resource on the test server.
// Each round: random edits on both sides, wait for convergence, verify
// state matches between JS and Emacs (and optionally server digest).
//
// Usage:
//   cd test && node test-server.js &
//   node fuzz-test.js [rounds] [seed]
//
// Requires: test server running on 127.0.0.1:8888

var { spawn } = require('child_process')
var readline = require('readline')
var http = require('http')
var path = require('path')

// Load braid-http — simpleton_client references braid_fetch as a global
var braid_http = require('braid-http')
var braid_fetch = braid_http.fetch
global.braid_fetch = braid_fetch

// Load simpleton_client — it's a browser script that defines a global function
var simpleton_path = path.join(__dirname, 'node_modules', 'braid-text', 'client', 'simpleton-sync.js')
var simpleton_src = require('fs').readFileSync(simpleton_path, 'utf8')
eval(simpleton_src)
// simpleton_client is now in scope

// ── Config ──────────────────────────────────────────────────────────────

var HOST = '127.0.0.1'
var PORT = 8888
var TOTAL_ROUNDS = parseInt(process.argv[2]) || 100
var SEED = parseInt(process.argv[3]) || (Date.now() % 100000)
var CONVERGE_TIMEOUT = 15000  // ms
var EMACS_CMD_TIMEOUT = 10000

// ── Seeded PRNG (xorshift32) ────────────────────────────────────────────

var rng_state = SEED
function rng() {
    rng_state ^= rng_state << 13
    rng_state ^= rng_state >>> 17
    rng_state ^= rng_state << 5
    return (rng_state >>> 0) / 4294967296
}
function rng_int(min, max) {
    return Math.floor(rng() * (max - min + 1)) + min
}
function rng_pick(arr) {
    return arr[Math.floor(rng() * arr.length)]
}

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
        timeout = timeout || EMACS_CMD_TIMEOUT
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

// ── JS simpleton client wrapper ─────────────────────────────────────────

function create_js_client(url) {
    var state = ''
    var connected = false
    var error_log = []

    var client = simpleton_client(url, {
        get_state: () => state,
        on_state: s => { state = s },
        on_patches: patches => {
            var offset = 0
            for (var p of patches) {
                state = state.slice(0, p.range[0] + offset) + p.content + state.slice(p.range[1] + offset)
                offset += p.content.length - (p.range[1] - p.range[0])
            }
        },
        on_error: e => { error_log.push(e.message || String(e)) },
        on_online: s => { connected = !!(s && s.online) },
        content_type: 'text/plain'
    })

    return {
        get state() { return state },
        set state(s) { state = s },
        changed: () => client.changed(),
        abort: () => client.abort(),
        get connected() { return connected },
        get errors() { return error_log },
        insert(pos, text) {
            state = state.slice(0, pos) + text + state.slice(pos)
            client.changed()
        },
        delete_(start, end) {
            state = state.slice(0, start) + state.slice(end)
            client.changed()
        }
    }
}

// ── Random edit generation ──────────────────────────────────────────────

var WORDS = ['hello', 'world', 'foo', 'bar', 'baz', 'qux', 'alpha', 'beta',
             'gamma', 'delta', 'x', 'yy', 'zzz', ' ', '  ', '\n', 'AB']

function random_insert_text() {
    var n = rng_int(1, 3)
    var parts = []
    for (var i = 0; i < n; i++) parts.push(rng_pick(WORDS))
    return parts.join('')
}

function random_edit(text_len) {
    // 70% insert, 30% delete
    if (text_len === 0 || rng() < 0.7) {
        var pos = rng_int(0, text_len)
        var text = random_insert_text()
        return { type: 'insert', pos, text }
    } else {
        var start = rng_int(0, text_len - 1)
        var max_del = Math.min(text_len - start, 10)
        var end = start + rng_int(1, max_del)
        return { type: 'delete', start, end }
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────

function sleep(ms) {
    return new Promise(r => setTimeout(r, ms))
}

async function wait_until(pred, timeout_ms, label) {
    var deadline = Date.now() + timeout_ms
    while (!pred()) {
        if (Date.now() > deadline)
            throw new Error(`Timeout: ${label} (${timeout_ms}ms)`)
        await sleep(50)
    }
}

// ── Main fuzz loop ──────────────────────────────────────────────────────

async function main() {
    console.log(`Fuzz test: ${TOTAL_ROUNDS} rounds, seed=${SEED}`)
    console.log(`Server: http://${HOST}:${PORT}`)

    var passed = 0
    var failed = 0

    for (var round = 1; round <= TOTAL_ROUNDS; round++) {
        var test_path = `/text/fuzz-${SEED}-${round}`
        var url = `http://${HOST}:${PORT}${test_path}`
        var emacs = null
        var js_client = null

        try {
            // ── Setup ───────────────────────────────────────────────
            emacs = spawn_emacs_agent()
            js_client = create_js_client(url)

            // Connect Emacs
            var r = await emacs.send({ cmd: 'connect', host: HOST, port: PORT, path: test_path })
            if (!r.ok) throw new Error('Emacs connect failed: ' + r.error)

            // Wait for both to connect
            await emacs.send({ cmd: 'wait-connected', timeout: 10 }, 15000)
            await wait_until(() => js_client.connected, 10000, 'JS connected')

            // Let initial state settle
            await sleep(200)

            // ── Generate random edits with network chaos ────────────
            var num_js_edits = rng_int(1, 5)
            var num_emacs_edits = rng_int(1, 5)
            var chaos_roll = rng()
            //   30% chance of some network disruption:
            //   - 10% kill sub only
            //   - 10% kill put-proc only
            //   - 10% kill both (full outage)
            var disconnect_mode = chaos_roll < 0.10 ? 'sub'
                                : chaos_roll < 0.20 ? 'put'
                                : chaos_roll < 0.30 ? 'both'
                                : null

            // Random delay before edits (simulates network latency)
            if (rng() < 0.3)
                await sleep(rng_int(50, 300))

            // JS edits (with random delays between them)
            for (var i = 0; i < num_js_edits; i++) {
                var edit = random_edit(js_client.state.length)
                if (edit.type === 'insert') {
                    js_client.insert(edit.pos, edit.text)
                } else {
                    js_client.delete_(edit.start, edit.end)
                }
                if (rng() < 0.2) await sleep(rng_int(10, 100))
            }

            // Random delay between sides (simulates asymmetric latency)
            await sleep(rng_int(0, 150))

            // Emacs edits (with random delays between them)
            for (var i = 0; i < num_emacs_edits; i++) {
                var emacs_state = await emacs.send({ cmd: 'state' })
                var text_len = emacs_state.length || 0
                var edit = random_edit(text_len)
                if (edit.type === 'insert') {
                    await emacs.send({ cmd: 'insert', pos: edit.pos, text: edit.text })
                } else {
                    await emacs.send({ cmd: 'delete', start: edit.start, end: edit.end })
                }
                await emacs.send({ cmd: 'changed' })
                if (rng() < 0.2) await sleep(rng_int(10, 100))
            }

            // Network disruption (before convergence — edits may be in-flight)
            if (disconnect_mode) {
                if (disconnect_mode === 'sub' || disconnect_mode === 'both')
                    await emacs.send({ cmd: 'kill-sub' })
                if (disconnect_mode === 'put' || disconnect_mode === 'both')
                    await emacs.send({ cmd: 'kill-put' })
                // Simulate outage duration
                await sleep(rng_int(200, 1000))
                await emacs.send({ cmd: 'wait-connected', timeout: 15 }, 20000)
            }

            // ── Wait for convergence ────────────────────────────────
            // After network disruption, give extra time for reconnection
            // and PUT retries before checking convergence.
            if (disconnect_mode) {
                await emacs.send({ cmd: 'spin', duration: 2.0 }, 5000)
            }
            // Flush Emacs, wait for acks
            var ack_timeout = disconnect_mode ? 25 : 15
            await emacs.send({ cmd: 'changed' })
            await emacs.send({ cmd: 'wait-ack', timeout: ack_timeout }, (ack_timeout + 5) * 1000)

            // Give server time to send updates to both clients
            await sleep(500)

            // Spin Emacs event loop to process incoming updates
            await emacs.send({ cmd: 'spin', duration: 1.0 }, 5000)

            // Get both states
            var js_state = js_client.state
            var emacs_resp = await emacs.send({ cmd: 'state' })
            var emacs_state = emacs_resp.state

            // If not converged yet, wait more
            if (js_state !== emacs_state) {
                // Try harder: more spinning + changed() calls
                for (var attempt = 0; attempt < 5; attempt++) {
                    await emacs.send({ cmd: 'changed' })
                    await emacs.send({ cmd: 'spin', duration: 1.0 }, 5000)
                    await sleep(500)
                    js_state = js_client.state
                    emacs_resp = await emacs.send({ cmd: 'state' })
                    emacs_state = emacs_resp.state
                    if (js_state === emacs_state) break
                }
            }

            // ── Verify ──────────────────────────────────────────────
            if (js_state !== emacs_state) {
                failed++
                console.log(`FAIL round ${round}/${TOTAL_ROUNDS} [disconnect=${disconnect_mode||'none'}, js_edits=${num_js_edits}, emacs_edits=${num_emacs_edits}]:`)
                console.log(`  JS state   (${js_state.length} chars): ${JSON.stringify(js_state.slice(0, 200))}`)
                console.log(`  Emacs state (${emacs_state.length} chars): ${JSON.stringify(emacs_state.slice(0, 200))}`)
                if (js_client.errors.length)
                    console.log(`  JS errors: ${js_client.errors.join('; ')}`)
                // Show Emacs internal state for debugging
                try {
                    var emacs_cs = await emacs.send({ cmd: 'client-state' }, 3000)
                    var emacs_v = await emacs.send({ cmd: 'version' }, 3000)
                    var emacs_p = await emacs.send({ cmd: 'pending' }, 3000)
                    console.log(`  Emacs client-state (${(emacs_cs.state||'').length} chars): ${JSON.stringify((emacs_cs.state||'').slice(0, 200))}`)
                    console.log(`  Emacs version: ${JSON.stringify(emacs_v.version)}, outstanding-changes: ${emacs_p.pending}`)
                } catch(e) {}
                // Dump Emacs debug stderr
                console.log(`  --- Emacs stderr (last 2000) ---`)
                console.log(emacs.stderr.slice(-2000))
                console.log(`  --- end stderr ---`)
            } else {
                passed++
                if (round % 10 === 0 || round === 1)
                    console.log(`  pass ${round}/${TOTAL_ROUNDS} (${js_state.length} chars)`)
            }

        } catch (e) {
            failed++
            console.log(`ERROR round ${round}/${TOTAL_ROUNDS}: ${e.message}`)
            if (emacs) console.log(`  Emacs stderr (last 300): ${emacs.stderr.slice(-300)}`)
        } finally {
            // Cleanup
            if (js_client) {
                try { await js_client.abort?.() } catch(e) {}
            }
            if (emacs) {
                try { await emacs.send({ cmd: 'quit' }) } catch(e) {}
                await sleep(100)
                emacs.kill()
            }
        }
    }

    console.log(`\n========================================`)
    console.log(`Fuzz results: ${passed} passed, ${failed} failed (${TOTAL_ROUNDS} rounds, seed=${SEED})`)
    console.log(`========================================`)
    process.exit(failed > 0 ? 1 : 0)
}

main().catch(e => {
    console.error('Fatal:', e)
    process.exit(2)
})
