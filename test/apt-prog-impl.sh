#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Simulates the complex ANSI sequences emitted by `apt` progress bars. This includes
# save/restore cursor (`\e7`/`\e8`), absolute bottom positioning, and restricted
# scroll regions (`\e[1;Nr`). It verifies that natural scrolling works correctly
# while a "protected" progress bar remains pinned to the bottom line of the screen.
# ==============================================================================

# 1. Setup Environment
# ------------------------------------------------------------------
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

# Parse arguments
STOP_AT=101
NUM_MESSAGES=8

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --stop-at) STOP_AT="$2"; shift ;;
        --messages) NUM_MESSAGES="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

if [ "$NUM_MESSAGES" -lt 1 ]; then
    NUM_MESSAGES=1
fi

# Calculate the scroll region boundary (Everything except the last line)
# If LINES=78, SCROLL_BOTTOM=77
SCROLL_BOTTOM=$((LINES - 1))

# Calculate Bar Width (Screen width - 22 chars for label/padding)
BAR_WIDTH=$(( $COLUMNS - 22 ))

# 2. Define Progress Function
# ------------------------------------------------------------------
update_progress() {
    local percent=$1
    local filled=$(($percent * $BAR_WIDTH / 100))
    local empty=$(($BAR_WIDTH - $filled))

    # Create visual bar strings
    local bar_filled=$(printf "%0.s#" $(seq 1 $filled))
    local bar_empty=$(printf "%0.s." $(seq 1 $empty))

    # Pad percentage number
    local pct_pad=""
    if [ "$percent" -lt 10 ]; then pct_pad="  "; elif [ "$percent" -lt 100 ]; then pct_pad=" "; fi

    # RENDER SEQUENCE (Matches dump)
    # \0337            : Save Cursor (Preserve where we are writing logs)
    # \033[${LINES};0f : Jump to absolute bottom line (The "Protected" Zone)
    # \033[42m\033[30m : Green Background, Black Text
    # Progress...      : The content
    # \033[49m\033[39m : Reset colors
    # \0338            : Restore Cursor (Go back to the log writing position)
    printf "\0337\033[${LINES};0f\033[42m\033[30mProgress: [%s%s%%]\033[49m\033[39m [%s%s] \0338" \
           "$pct_pad" "$percent" "$bar_filled" "$bar_empty"
}

# 3. Initialize The "APT" Mode
# ------------------------------------------------------------------
echo "Reading package lists... Done"
echo "Building dependency tree... Done"

# THE MAGIC SEQUENCE (From Dump offset 000011c6)
# 1. \n\n           : Output newlines. If at bottom, this scrolls text up.
# 2. \0337          : Save Cursor.
# 3. \033[0;...r    : Set Scroll Region (1 to N-1). Cursor usually jumps home.
# 4. \0338          : Restore Cursor.
# 5. \033[1A        : Move Up 1 Line.
#    CRITICAL: This pulls the cursor from the forbidden bottom line (Bar area)
#    back into the allowed scroll region.
printf "\n\n\0337\033[1;${SCROLL_BOTTOM}r\0338\033[1A"

# 4. Run Simulation
# ------------------------------------------------------------------
# The cursor is now safely inside the scroll region.
# Text will scroll nicely without touching the bar at the bottom.
WIPE_BAR=0

for (( i=1; i<=NUM_MESSAGES; i++ )); do
    printf "Status message %d...\n" "$i"

    if [ "$NUM_MESSAGES" -eq 1 ]; then
        percent=100
    else
        percent=$(( (i - 1) * 100 / (NUM_MESSAGES - 1) ))
    fi

    update_progress "$percent"

    if [ "$STOP_AT" -le "$percent" ]; then
        break
    fi
done

if [ "$STOP_AT" -gt "$percent" ]; then
    WIPE_BAR=1
fi

# 5. Cleanup Sequence (From Dump offset 0000201c)
# ------------------------------------------------------------------
# 1. \r\n\r\n       : Push text up / create spacing
# 2. \0337          : Save cursor
# 3. \033[0;${LINES}r : Reset Scroll Region to Full Screen
# 4. \0338          : Restore cursor
# 5. \033[1A        : Move Up 1 (To the line above the bar)
# 6. \033[J         : Clear to End of Screen (Wipes the bar)
printf "\r\n\r\n\0337\033[0;${LINES}r\0338"
if [ "$WIPE_BAR" -eq 1 ]; then
    printf "\033[1A\033[J"
else
    printf "\033[${LINES};0f\n\n"
fi

# We exit cleanly. The prompt will appear where the bar used to be (or just above it).
