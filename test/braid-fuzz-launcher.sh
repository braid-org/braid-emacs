#!/bin/bash
#
# Launcher for braid-fuzz: starts Emacs controller and forwards stdin/stdout
# via a TCP socket (since Emacs can't do non-blocking stdin reads in batch mode).
#
DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Create a temp file for Emacs stderr
STDERR_FILE=$(mktemp)

# Start Emacs in background, capture stderr to find the TCP port
emacs --batch -Q -L "$DIR" -l "$DIR/test/fuzz-controller.el" 2>"$STDERR_FILE" &
EMACS_PID=$!

# Wait for port to appear in stderr
PORT=""
for i in $(seq 1 50); do
    if [ -f "$STDERR_FILE" ]; then
        PORT=$(grep -o 'FC_PORT=[0-9]*' "$STDERR_FILE" | head -1 | cut -d= -f2)
        if [ -n "$PORT" ]; then
            break
        fi
    fi
    sleep 0.1
done

if [ -z "$PORT" ]; then
    echo '{"error":"Failed to get Emacs TCP port"}' >&2
    kill $EMACS_PID 2>/dev/null
    rm -f "$STDERR_FILE"
    exit 1
fi

rm -f "$STDERR_FILE"

# Forward stdin/stdout to Emacs's TCP socket
# nc will relay bidirectionally and exit when either side closes
exec nc 127.0.0.1 "$PORT"
