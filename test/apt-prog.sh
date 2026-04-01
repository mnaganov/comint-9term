#!/bin/sh

# ==============================================================================
# Use Cases Covered:
# Tests the intermediate state of an `apt` progress bar by terminating it at 30%.
# It verifies that `comint-9term` accurately renders a mid-progress bar and
# exits cleanly without relying on the final cleanup sequences (`\e[J`) normally
# emitted at the end of an apt transaction.
#
# Then, allows the bar to run to completion and get erased from the screen.
# ==============================================================================
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}
export LINES
export COLUMNS

echo "=== Apt Progress Bar Test Suite ==="
echo "SCREEN_DIMS: LINES=${LINES}:COLUMNS=${COLUMNS}"
printf "\033[8;${LINES};${COLUMNS}t"
echo ""

$(dirname "$0")/apt-prog-impl.sh --stop-at 30
$(dirname "$0")/apt-prog-impl.sh

echo "=== Test Suite Complete ==="
# Ensure cursor is at the start of a new line
echo ""
