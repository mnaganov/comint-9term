#!/bin/bash
# Reproduction for apt-style progress bar "leaks" in the infinite scrollback.
#
# Real apt uses:
#   1. DECSTBM (\e[0;Nr) to restrict scrolling to a region (rows 1..N)
#   2. \e7 / \e[N+1;1f / <draw progress bar> / \e8  to draw at a fixed row
#      OUTSIDE the scroll region
#   3. Normal text scrolls within the restricted region only
#   4. \e[0;Mr / \e[J  at the end to reset and erase the progress bar
#
# The bug: in the infinite scrollback model, each scroll-offset advance moves
# the progress bar's CUP target to a new buffer line, leaving "ghost" progress
# bars scattered throughout the scrollback.  A real terminal never puts the
# progress bar row into the scrollback because DECSTBM keeps it outside.
#
# Note: apt actually sends \e[0;Nr where 0 means "default to 1" per VT100.

export LINES=10
export COLUMNS=80

echo "=== Apt Install Leak Test Suite ==="
echo "LINES=${LINES}:COLUMNS=${COLUMNS}"

# Set scroll region to rows 1-9, reserving row 10 for the progress bar.
# Use 0 as first param (like real apt) to also test the VT100 default handling.
printf "\e7\e[0;9r\e8"

for i in {1..15}; do
    # Save cursor, move to row 10 (outside scroll region), draw progress, restore.
    printf "\e7\e[10;1fProgress: [%-20s] %d%%\e8" \
        "$(printf '#%.0s' $(seq 1 $((i % 5 + 1))))" "$((i * 5))"
    echo "Line $i: Scrolling message..."
done

# Reset scroll region and erase the progress bar (apt cleanup sequence).
printf "\e7\e[0;10r\e8\e[J"

echo "Done."
echo "Diagnostics: reported size is LINES=${LINES}:COLUMNS=${COLUMNS}"
echo "=== Test Suite Complete ==="
