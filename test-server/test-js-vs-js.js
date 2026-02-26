#!/usr/bin/env node
// Quick test: two JS simpleton clients editing the same doc.
// If they diverge, the bug is in braid-text/simpleton, not Emacs.

var braid_http = require('braid-http')
var braid_fetch = braid_http.fetch
global.braid_fetch = braid_fetch

var path = require('path')
var simpleton_path = path.join(__dirname, 'node_modules', 'braid-text', 'client', 'simpleton-sync.js')
eval(require('fs').readFileSync(simpleton_path, 'utf8'))

var HOST = '127.0.0.1', PORT = 8888

function create_client(url, name) {
    var state = ''
    var connected = false
    var errors = []

    var client = simpleton_client(url, {
        get_state: () => state,
        on_state: s => { state = s },
        on_patches: patches => {
            for (var p of patches)
                state = state.slice(0, p.range[0]) + p.content + state.slice(p.range[1])
        },
        on_error: e => { errors.push(e.message || String(e)) },
        on_online: s => { connected = !!(s && s.online) },
        content_type: 'text/plain'
    })

    return {
        name,
        get state() { return state },
        set state(s) { state = s },
        changed: () => client.changed(),
        abort: () => client.abort(),
        get connected() { return connected },
        get errors() { return errors },
        insert(pos, text) {
            state = state.slice(0, pos) + text + state.slice(pos)
            client.changed()
        }
    }
}

function sleep(ms) { return new Promise(r => setTimeout(r, ms)) }
async function wait_until(pred, ms) {
    var end = Date.now() + ms
    while (!pred() && Date.now() < end) await sleep(50)
}

async function main() {
    var ROUNDS = 20
    var SEED = parseInt(process.argv[2]) || 88888
    var state = SEED
    function rng() { state ^= state << 13; state ^= state >> 17; state ^= state << 5; return (state >>> 0) / 4294967296 }
    function rng_int(min, max) { return min + Math.floor(rng() * (max - min + 1)) }

    var WORDS = ['hello', 'world', 'foo', 'bar', 'baz', 'qux', 'alpha', 'beta', 'gamma', 'delta', 'x', 'yy', 'zzz', ' ', '  ', '\n', 'AB']

    console.log(`JS-vs-JS test: ${ROUNDS} rounds, seed=${SEED}`)
    var passed = 0, failed = 0

    for (var round = 1; round <= ROUNDS; round++) {
        var url = `http://${HOST}:${PORT}/text/jj-${SEED}-${round}`
        var a = create_client(url, 'A')
        var b = create_client(url, 'B')

        try {
            await wait_until(() => a.connected && b.connected, 10000)
            await sleep(200)

            // Random edits on both sides
            var na = rng_int(1, 5), nb = rng_int(1, 5)
            for (var i = 0; i < na; i++) {
                var pos = a.state.length === 0 ? 0 : rng_int(0, a.state.length)
                var n = rng_int(1, 3)
                var text = ''
                for (var j = 0; j < n; j++) text += WORDS[rng_int(0, WORDS.length - 1)]
                a.insert(pos, text)
                if (rng() < 0.2) await sleep(rng_int(10, 100))
            }
            await sleep(rng_int(0, 150))
            for (var i = 0; i < nb; i++) {
                var pos = b.state.length === 0 ? 0 : rng_int(0, b.state.length)
                var n = rng_int(1, 3)
                var text = ''
                for (var j = 0; j < n; j++) text += WORDS[rng_int(0, WORDS.length - 1)]
                b.insert(pos, text)
                if (rng() < 0.2) await sleep(rng_int(10, 100))
            }

            // Wait for convergence
            await sleep(2000)
            for (var attempt = 0; attempt < 5 && a.state !== b.state; attempt++)
                await sleep(1000)

            if (a.state === b.state) {
                passed++
                if (round % 5 === 0 || round === 1) console.log(`  pass ${round}/${ROUNDS} (${a.state.length} chars)`)
            } else {
                failed++
                console.log(`FAIL round ${round}: A(${a.state.length}) vs B(${b.state.length})`)
                console.log(`  A: ${JSON.stringify(a.state.slice(0, 100))}`)
                console.log(`  B: ${JSON.stringify(b.state.slice(0, 100))}`)
                if (a.errors.length) console.log(`  A errors: ${a.errors.join('; ')}`)
                if (b.errors.length) console.log(`  B errors: ${b.errors.join('; ')}`)
            }
        } finally {
            try { await a.abort() } catch(e) {}
            try { await b.abort() } catch(e) {}
        }
    }

    console.log(`\nResults: ${passed} passed, ${failed} failed`)
    process.exit(failed > 0 ? 1 : 0)
}

main().catch(e => { console.error('Fatal:', e); process.exit(2) })
