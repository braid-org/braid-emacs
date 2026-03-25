# Tests

## Running tests

```bash
# All tests (unit + braid-fuzz integration):
make test

# Just unit tests (Emacs buffer change → patch generation):
make test-unit

# Just braid-fuzz integration tests (39 tests across 6 suites):
make test-braid-fuzz

# Run a specific braid-fuzz suite:
braid-fuzz ./test/braid-fuzz-launcher.sh simpleton
braid-fuzz ./test/braid-fuzz-launcher.sh reliable-updates
braid-fuzz ./test/braid-fuzz-launcher.sh cursors
```

## Active test files

| File | What it tests |
|------|---------------|
| `hook-patch-test.el` | Emacs buffer operations → correct patches (35 operations) |
| `fuzz-controller.el` | braid-fuzz controller — implements the JSON-RPC protocol |
| `braid-fuzz-launcher.sh` | Launches Emacs as a braid-fuzz client subprocess |
| `test-server.js` | Local braid-text server for manual/interactive testing |

## braid-fuzz suites (39 tests)

| Suite | Tests | What it covers |
|-------|-------|----------------|
| basics | 1 | Connectivity |
| http | 2 | GET subscription, PUT |
| subscriptions | 7 | Snapshots, patches, Patches:N, Patches:0, Parents, unsubscribe |
| reliable-updates | 15 | Reconnection, TCP RST, 503/500 retry, heartbeat, PUT retry, network flapping |
| simpleton | 10 | Text sync, concurrent edits, digest verification, randomized fuzz |
| cursors | 4 | Send/receive cursors, transform through edits, reconnect |

## Archived tests

Previous tests in `test/archive/` are superseded by braid-fuzz. Kept for reference.
