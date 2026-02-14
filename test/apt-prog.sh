#!/bin/bash

echo "=== Apt Progress Bar Test Suite ==="
echo ""

# 1. Setup Environment
# ------------------------------------------------------------------
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

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
printf "\n\n\0337\033[0;${SCROLL_BOTTOM}r\0338\033[1A"


# 4. Run Simulation
# ------------------------------------------------------------------
# The cursor is now safely inside the scroll region.
# Text will scroll nicely without touching the bar at the bottom.

printf "Preparing to unpack .../mc-data_3%%3a4.8.29-2_all.deb ...\n"
update_progress 0
# sleep 0.5

printf "Unpacking mc-data (3:4.8.29-2) ...\n"
update_progress 10
# sleep 0.5

printf "Preparing to unpack .../mc_3%%3a4.8.29-2_arm64.deb ...\n"
update_progress 20
# sleep 0.5

printf "Unpacking mc (3:4.8.29-2) ...\n"
update_progress 40
# sleep 0.5

printf "Selecting previously unselected package unzip...\n"
update_progress 50
# sleep 0.5

printf "Setting up unzip (6.0-28) ...\n"
update_progress 70
# sleep 0.5

printf "Processing triggers for mailcap (3.70+nmu1) ...\n"
update_progress 90
# sleep 0.5

printf "Processing triggers for man-db (2.11.2-2) ...\n"
update_progress 100
# sleep 0.5


# 5. Cleanup Sequence (From Dump offset 0000201c)
# ------------------------------------------------------------------
# 1. \r\n\r\n       : Push text up / create spacing
# 2. \0337          : Save cursor
# 3. \033[0;${LINES}r : Reset Scroll Region to Full Screen
# 4. \0338          : Restore cursor
# 5. \033[1A        : Move Up 1 (To the line above the bar)
# 6. \033[J         : Clear to End of Screen (Wipes the bar)
printf "\r\n\r\n\0337\033[0;${LINES}r\0338\033[1A\033[J"

# We exit cleanly. The prompt will appear where the bar used to be (or just above it).
echo "=== Test Suite Complete ==="
# Ensure cursor is at the start of a new line
echo ""
