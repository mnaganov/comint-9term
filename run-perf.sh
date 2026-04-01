#!/bin/bash
set -e

# Configuration
APT_MESSAGES=500

# Global warm-up to populate caches and compile elisp quickly
echo "--------------------------------------------------"
echo "Running Global Warm-up..."
echo "--------------------------------------------------"
emacs -nw -q -l test/perf-warmup-base.el
emacs -nw -q -l test/perf-warmup-9term.el

run_test() {
    local name=$1
    local base_el=$2
    local test_el=$3

    echo "--------------------------------------------------"
    echo "Running Performance Test: $name"
    echo "--------------------------------------------------"

    # Baseline
    echo "Running baseline..."
    START=$(date +%s.%N)
    emacs -nw -q -l "$base_el"
    END=$(date +%s.%N)
    BASE_TIME=$(awk "BEGIN {print $END - $START}")
    echo "Baseline time: $BASE_TIME seconds"

    # Test
    echo "Running comint-9term..."
    START=$(date +%s.%N)
    emacs -nw -q -l "$test_el"
    END=$(date +%s.%N)
    TEST_TIME=$(awk "BEGIN {print $END - $START}")
    echo "Test time: $TEST_TIME seconds"

    # Ratio
    RATIO=$(awk "BEGIN {printf \"%.2f\", $TEST_TIME / $BASE_TIME}")
    echo "Ratio (Test/Baseline): $RATIO"

    # Check ratio (target <= 2.50)
    if awk "BEGIN {exit !($RATIO <= 2.50)}"; then
        echo "RESULT: PASSED"
        return 0
    else
        echo "RESULT: FAILED"
        return 1
    fi
}

# Run both tests
EXIT_CODE=0

# Original test (test/perf.py)
run_test "Standard (perf.py)" "test/perf-base.el" "test/perf-9term.el" || EXIT_CODE=1

echo ""

# APT test (apt-prog-impl.sh)
run_test "APT Simulation (apt-prog-impl.sh)" "test/perf-apt-base.el" "test/perf-apt-9term.el" || EXIT_CODE=1

if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo "ALL PERFORMANCE TESTS PASSED."
else
    echo ""
    echo "SOME PERFORMANCE TESTS FAILED."
fi

exit $EXIT_CODE
