#!/bin/bash
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

echo "=== Android Build Test Suite ==="
echo "Diagnostics: reported size is LINES=${LINES}:COLUMNS=${COLUMNS}"

# Calculate scroll region (Top N-6 lines)
SCROLL_BOTTOM=$((LINES - 6))
STATUS_START=$((SCROLL_BOTTOM + 1))

# Initial output
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
    # Move to STATUS_START, Clear Line, Print info
    # Repeat for a few lines
    printf "\033[${STATUS_START};1H\033[K[ %3d%% %d/5] Action $i..." "$((i*20))" "$i"
    printf "\033[$((STATUS_START+1));1H\033[K    0:0$i  details for $i"
    printf "\033[$((STATUS_START+2));1H\033[K"
    printf "\033[$((STATUS_START+3));1H\033[K"
    printf "\033[$((STATUS_START+4));1H\033[K"
    printf "\033[$((STATUS_START+5));1H\033[K"

    # Move back to bottom of scroll region to print logs
    printf "\033[${SCROLL_BOTTOM};1H"
    
    # Print logs
    echo "ninja: build step $i started"
    echo "ninja: build step $i finished"
    # sleep 0.1
done

# Reset Scroll Region
printf "\033[r"
# Move to bottom
printf "\033[${LINES};1H"
echo "=== Test Suite Complete ==="
