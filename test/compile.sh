#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# This is a strict unit test for partial ANSI escape sequence buffering and chunk
# boundary handling. It deliberately splits an escape sequence across multiple
# output chunks (using `sleep`) to prove that `comint-9term` correctly buffers
# the incomplete sequence and recombines it without dropping data or printing
# literal escape characters.
# ==============================================================================
: ${LINES:=$(tput lines)}
: ${COLUMNS:=$(tput cols)}

echo "=== Compile Test Suite ==="

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
    # Trigger partial sequence parsing by splitting the escape sequence
    printf "\033"
    sleep 0.2
    printf "[${SCROLL_BOTTOM};1H"
    echo "file_$i.cpp: In function 'void test$i()':"
    echo "file_$i.cpp:42: warning: unused variable 'x' [-Wunused-variable]"
done

# Reset
printf "\033[r"
printf "\033[${LINES};1H"
echo "SCREEN_DIMS: LINES=${LINES}:COLUMNS=${COLUMNS}"
printf "\033[8;${LINES};${COLUMNS}t"
echo "=== Test Suite Complete ==="
