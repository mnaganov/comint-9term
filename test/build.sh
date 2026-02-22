#!/bin/sh
echo "Diagnostics: reported size is LINES=${LINES}:COLUMNS=${COLUMNS}"
echo "=== Android Build Test Suite ==="
cat $(dirname "$0")/build.log
echo "=== Test Suite Complete ==="
