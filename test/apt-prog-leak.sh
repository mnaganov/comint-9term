#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# This script wraps `apt-prog.sh` with a restricted window height (`LINES=10`)
# to explicitly test viewport scrolling bugs. It verifies that progress bars
# pinned to the bottom of a very small terminal do not "leak" upward and overwrite
# historical log data when natural scrolling occurs.
# ==============================================================================
export LINES=10
export COLUMNS=80

echo "=== Apt Progress Bar Test Suite ==="
echo "SCREEN_DIMS: LINES=${LINES}:COLUMNS=${COLUMNS}"
printf "\033[8;${LINES};${COLUMNS}t"
echo ""

$(dirname "$0")/apt-prog-impl.sh

echo "=== Test Suite Complete ==="
echo ""
