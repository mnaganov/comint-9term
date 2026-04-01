#!/bin/sh

# ==============================================================================
# Use Cases Covered:
# Tests the complete and intermediate states of an `apt` progress bar.
# It verifies that `comint-9term` accurately renders a full progress bar and
# a mid-progress bar (terminating at 30%).
# 
# It explicitly tests viewport scrolling bugs by calculating the remaining
# terminal height and generating enough messages in a final run to overflow 
# the terminal. This verifies that progress bars pinned to the bottom do not 
# "leak" upward and overwrite historical log data when natural scrolling occurs.
# ==============================================================================
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}
export LINES
export COLUMNS

echo "=== Apt Progress Bar Test Suite ==="
echo "SCREEN_DIMS: LINES=${LINES}:COLUMNS=${COLUMNS}"
printf "\033[8;${LINES};${COLUMNS}t"
echo ""

# Calculate messages to guarantee overflow and non-overflow states.
# We are running apt-prog-impl 3 times:
# 1. Non-overflowing: 1 message (~7 lines)
# 2. 30% bar: 4 messages, stops at 30% (~8 lines)
# Header overhead: 3 lines
# Total lines used before the 3rd run: 18 lines
# 3. Overflowing: Needs to exceed the remaining lines.
OVERFLOW_MSGS=$(( LINES - 18 + 5 ))
if [ "$OVERFLOW_MSGS" -lt 5 ]; then
    OVERFLOW_MSGS=5
fi

# Run 1: Non-overflowing
$(dirname "$0")/apt-prog-impl.sh --messages 1

# Run 2: 30% intermediate bar
$(dirname "$0")/apt-prog-impl.sh --stop-at 30 --messages 4

# Run 3: Overflowing (forces natural scrolling)
$(dirname "$0")/apt-prog-impl.sh --messages "$OVERFLOW_MSGS"

echo "=== Test Suite Complete ==="
# Ensure cursor is at the start of a new line
echo ""
