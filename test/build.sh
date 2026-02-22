#!/bin/sh
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}
echo "=== Android Build Test Suite ==="
echo "LINES=${LINES}:COLUMNS=${COLUMNS}"
echo ""

cat $(dirname "$0")/build.log

echo "=== Test Suite Complete ==="
echo ""
