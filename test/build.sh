#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# This consolidated test simulates a complex split-screen build environment
# (like modern Android builds) while strictly verifying partial ANSI sequence
# buffering.
# 1. Viewport: Large scrolling region at top, fixed status area at bottom.
# 2. Sequences: DECSTBM (Scroll Region), CUP (Position), EL (Erase Line),
#    and partial sequence handling via split output chunks.
# ==============================================================================
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

echo "=== Consolidated Build & Compile Test Suite ==="

# Calculate scroll region (Top N-6 lines)
SCROLL_BOTTOM=$((LINES - 6))
STATUS_START=$((SCROLL_BOTTOM + 1))

# Initial output (from build.sh)
echo "including vendor/google/build/vendorsetup.sh"
echo "============================================"
echo "PLATFORM_VERSION_CODENAME=Baklava"
echo "TARGET_PRODUCT=aosp_cf_x86_64_only_phone"
echo "BUILD_ID=MAIN"
echo "============================================"

# Set Scroll Region
printf "\033[1;${SCROLL_BOTTOM}r"

# Loop
for i in $(seq 1 5); do
    # Update Status (Bottom 6 lines)
    printf "\033[${STATUS_START};1H\033[K[ %3d%% %d/5] Action $i: Compiling file_$i.cpp" "$((i*20))" "$i"
    printf "\033[$((STATUS_START+1));1H\033[K    0:0$i  clang++ -O2 file_$i.cpp -o file_$i.o"
    printf "\033[$((STATUS_START+2));1H\033[K    [Status: Building Dependencies]"
    printf "\033[$((STATUS_START+3));1H\033[K"
    printf "\033[$((STATUS_START+4));1H\033[K"
    printf "\033[$((STATUS_START+5));1H\033[K"

    # Move back to bottom of scroll region to print logs
    # CRITICAL: Split the escape sequence to test partial buffering (from compile.sh)
    printf "\033"
    sleep 0.1
    printf "[${SCROLL_BOTTOM};1H"
    
    # Print logs (mixed from both)
    echo "ninja: build step $i started"
    echo "file_$i.cpp:42: warning: unused variable 'x' [-Wunused-variable]"
    echo "ninja: build step $i finished"
done

# Reset Scroll Region
printf "\033[r"
# Move to bottom
printf "\033[${LINES};1H"
echo "SCREEN_DIMS: LINES=${LINES}:COLUMNS=${COLUMNS}"
printf "\033[8;${LINES};${COLUMNS}t"
echo "=== Test Suite Complete ==="
