#!/bin/bash

# Configuration
ERRORS_FILE="out/elisp-errors.txt"
# Tests that simply compare output against a static file in test/
SIMPLE_TESTS=("ansi-seq")
# Tests that require screen execution to generate a golden master for comparison
# SCREEN_TESTS=("apt-prog" "apt-prog-0")
SCREEN_TESTS=("apt-prog-0")
# All tests combined
ALL_TESTS=("${SIMPLE_TESTS[@]}" "${SCREEN_TESTS[@]}")

# Cleanup
rm -f out/*

# Global failure tracker
EXIT_CODE=0

# ---------------------------------------------------------
# Function: run_emacs_test
# Purpose: Runs the emacs lisp test harness for a specific test name
# ---------------------------------------------------------
run_emacs_test() {
    local test_name=$1
    echo "Running Emacs test: $test_name"
    echo "$test_name" > out/current-script

    timeout 10s emacs -nw -q -l test/emacs.el
    local emacs_status=$?

    if [ -f "$ERRORS_FILE" ]; then
        echo "Elisp errors found during $test_name:"
        cat "$ERRORS_FILE"
        if [ $emacs_status -eq 124 ]; then
            echo "Process timed out! The Elisp code must cause an infinite loop"
        fi
        echo "FAILURE"
        exit 2
    fi
}

# ---------------------------------------------------------
# Function: verify_simple
# Purpose: Compares Emacs output against a static source file
# ---------------------------------------------------------
verify_simple() {
    local test_name=$1
    local source_file="test/${test_name}.txt"

    for mode in shell compile; do
        local out_file="out/${test_name}-out-${mode}.txt"
        git diff --no-index "$source_file" "$out_file"
        if [ $? -ne 0 ]; then
            echo "FAILURE: Diff mismatch for $test_name ($mode)"
            EXIT_CODE=1
        fi
    done
}

# ---------------------------------------------------------
# Function: verify_screen_gen
# Purpose: Parses dimensions, runs screen to generate golden files
# ---------------------------------------------------------
verify_screen_gen() {
    local test_name=$1
    local script_path="test/${test_name}.sh"

    # We need to run this for both shell and compile modes
    for mode in shell compile; do
        local out_file="out/${test_name}-out-${mode}.txt"
        local golden_file="out/${test_name}-out-${mode}-golden.txt"

        # 1. Extract dimensions from the Emacs output
        # Format in file: LINES=20:COLUMNS=80
        local stty_conf
        stty_conf=$(sed -nE 's/.*LINES=([0-9]+):COLUMNS=([0-9]+).*/rows \1 cols \2/p' "$out_file")

        if [ -z "$stty_conf" ]; then
            echo "FAILURE: Could not extract dimensions from $out_file"
            EXIT_CODE=1
            continue
        fi

        # 2. Run Screen to generate the golden master
        # We use a unique session name to prevent collisions
        local session_name="sess_${test_name}_${mode}"

        echo "Generating golden master for $test_name ($mode) using dims: $stty_conf"
        screen -c test/screenrc -d -m -S "$session_name" bash -c \
            "stty ${stty_conf}; \
             screen -c test/screenrc -X scrollback 10000; \
             ${script_path}; \
             sleep 3; \
             screen -X hardcopy -h ${golden_file}"
    done
}

# ---------------------------------------------------------
# Main Execution Flow
# ---------------------------------------------------------

export PAGER=cat

# 1. Run Emacs for all tests
for test in "${ALL_TESTS[@]}"; do
    run_emacs_test "$test"
done

# 2. Check that all expected output files exist
echo "Verifying file generation..."
for test in "${ALL_TESTS[@]}"; do
    for mode in shell compile; do
        if [ ! -f "out/${test}-out-${mode}.txt" ]; then
            echo "FAILURE: Missing output file out/${test}-out-${mode}.txt"
            exit 2
        fi
    done
done

# 3. Run Simple Verifications (Text Diff)
for test in "${SIMPLE_TESTS[@]}"; do
    verify_simple "$test"
done

# 4. Run Screen Verifications (Generate Golden)
# 4a. Launch Screen sessions
for test in "${SCREEN_TESTS[@]}"; do
    verify_screen_gen "$test"
done

# 4b. Wait for all golden files to appear
echo "Waiting for screen generation to complete..."
while true; do
    all_files_present=true

    # Check existence of all expected files
    for test in "${SCREEN_TESTS[@]}"; do
        for mode in shell compile; do
            if [ ! -f "out/${test}-out-${mode}-golden.txt" ]; then
                echo "out/${test}-out-${mode}-golden.txt is missing"
                all_files_present=false
                break 2  # Break out of both 'for' loops to sleep
            fi
        done
    done

    # If the flag wasn't flipped to false, everything is ready
    if [ "$all_files_present" = true ]; then
        break
    fi

    sleep 1
done

# 4c. Compare Emacs Output vs Screen Golden Output
echo "Comparing Emacs output with Screen output..."
for test in "${SCREEN_TESTS[@]}"; do
    for mode in shell compile; do
        local golden_file="out/${test}-out-${mode}-golden.txt"
        local emacs_file="out/${test}-out-${mode}.txt"

        # Trim the golden file in-place:
        # Quit processing immediately after printing "=== Test Suite Complete ==="
        sed -i '/=== Test Suite Complete ===/q' "$golden_file"

        echo "Comparing $test ($mode) Emacs output vs Screen golden..."
        git diff --no-index "$golden_file" "$emacs_file"

        if [ $? -ne 0 ]; then
            echo "FAILURE: Golden mismatch for $test ($mode)"
            EXIT_CODE=1
        fi
    done
done

# ---------------------------------------------------------
# Final Exit
# ---------------------------------------------------------
if [ $EXIT_CODE -eq 0 ]; then
    echo "SUCCESS"
    exit 0
else
    echo "FAILURE"
    exit 1
fi
