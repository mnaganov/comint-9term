#!/bin/bash
# Minimal reproduction for apt-style progress bar "leaks" during scrolling.
# We simulate a small terminal (10 lines) and a progress bar that stays at the bottom.

export LINES=10
export COLUMNS=80

echo "=== Apt Install Leak Test Suite ==="
echo "LINES=$LINES"

# 1. Fill the buffer so we are scrolling.
for i in {1..15}; do
    # Save cursor, move to bottom row, print progress, restore cursor.
    printf "\e7\e[10;1fProgress: [%-20s] %d%%\e8" "$(printf '#%.0s' $(seq 1 $((i % 5 + 1))))" "$((i * 5))"
    echo "Line $i: Scrolling message..."
    # Small sleep to simulate processing
    # sleep 0.1
done

echo "Done."
echo "=== Test Suite Complete ==="
