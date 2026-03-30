#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Verifies absolute positioning against dynamic window heights (`LINES` and `COLUMNS`).
# It intentionally writes content to specific offsets from the bottom (`H-2` and `H`)
# to ensure that `comint-9term`'s layout heuristic accurately captures the *actual*
# window dimensions calculated by Emacs, rather than frame-level defaults.
# ==============================================================================
echo "=== Window Height Test Suite ==="

# 1. Get dimensions first
unset LINES
unset COLUMNS
H=$(tput lines)
W=$(tput cols)

# 2. Output diagnostics (will scroll off, but grep finds it in file)
echo "Diagnostics: reported size is LINES=${H}:COLUMNS=${W}"

# 3. Output filler
for i in $(seq 1 50); do
    echo "Line $i - filler text to scroll the buffer"
done

# 4. Write STATUS BAR at H-2
TARGET=$((H - 2))
if [ $TARGET -lt 1 ]; then TARGET=1; fi
# Use CSI K (Erase Line) to ensure clean output
printf "\033[${TARGET};1H\033[KSTATUS BAR AT LINE ${TARGET}"

# 5. Write Footer at Bottom (Line H)
# Use CSI K to clear potential garbage from previous lines
printf "\033[${H};1H\033[K=== Test Suite Complete ===\n"
