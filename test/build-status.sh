#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Simulates tools (like older ninja versions) that pin a 1-line status bar to the
# top of the viewport (`\e[1;1r`). It tests the "Monotonic Viewport" logic,
# verifying that jumping back to Row 1 (`\e[1;1H`) after a build finishes does
# not snap the viewport back to the top of the buffer, preserving history integrity.
# ==============================================================================

echo "=== Android Build Status Test Suite ==="

# Simulates the ninja build status bar behavior from big-build.log
echo "Line 1: Starting build"
echo "Line 2: Setting up environment"

# Set a 1-line scroll region to the top and move cursor there
printf '\033[1;1r\033[1;1H'
printf '\033[1m[1/3] Building component A\033[0m\033[K'

sleep 0.2

printf '\033[1;1r\033[1;1H'
printf '\033[1m[2/3] Building component B\033[0m\033[K'

sleep 0.2

printf '\033[1;1r\033[1;1H'
printf '\033[1mninja: Build Succeeded: 3 steps\033[0m\033[K'

sleep 0.2

# Reset scroll region and move to top
printf '\n\033[1;1r\033[1;1H\033[r\033[1;1H\033[?25h'
# Overwrite the status with final message
printf '\033[1;36mInfo: \033[0mBuild successful\n'

# Additional lines that should appear below the status
echo "Line 3: Packaging"
echo "Line 4: Done"

echo "=== Test Suite Complete ==="
