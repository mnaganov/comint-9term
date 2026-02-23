#!/bin/bash
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

echo "=== Compile Test Suite ==="
echo "Diagnostics: reported size is LINES=${LINES}:COLUMNS=${COLUMNS}"

SCROLL_BOTTOM=$((LINES - 6))
STATUS_START=$((SCROLL_BOTTOM + 1))

# Set Scroll Region
printf "\033[1;${SCROLL_BOTTOM}r"

for i in $(seq 1 3); do
    # Update Status
    printf "\033[${STATUS_START};1H\033[K[ %d/3] Compiling file_$i.cpp" "$i"
    printf "\033[$((STATUS_START+1));1H\033[K    0:0$i  clang++ file_$i.cpp"
    printf "\033[$((STATUS_START+2));1H\033[K"
    printf "\033[$((STATUS_START+3));1H\033[K"
    printf "\033[$((STATUS_START+4));1H\033[K"
    printf "\033[$((STATUS_START+5));1H\033[K"

    # Log
    printf "\033[${SCROLL_BOTTOM};1H"
    echo "file_$i.cpp: In function 'void test$i()':"
    echo "file_$i.cpp:42: warning: unused variable 'x' [-Wunused-variable]"
done

# Reset
printf "\033[r"
printf "\033[${LINES};1H"
echo "=== Test Suite Complete ==="
