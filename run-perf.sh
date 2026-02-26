#!/bin/bash
set -e

# Run baseline
echo "Running baseline (compile + ansi-color)..."
BASE_START=$(date +%s.%N)
emacs -nw -q -l test/perf-base.el
BASE_END=$(date +%s.%N)
BASE_TIME=$(awk "BEGIN {print $BASE_END - $BASE_START}")
echo "Baseline time: $BASE_TIME seconds"

# Run test
echo "Running comint-9term..."
TEST_START=$(date +%s.%N)
emacs -nw -q -l test/perf-9term.el
TEST_END=$(date +%s.%N)
TEST_TIME=$(awk "BEGIN {print $TEST_END - $TEST_START}")
echo "Test time: $TEST_TIME seconds"

# Calculate ratio
RATIO=$(awk "BEGIN {printf \"%.2f\", $TEST_TIME / $BASE_TIME}")
echo "Ratio (Test/Baseline): $RATIO"

# Check if within 20% (Ratio <= 1.20)
if awk "BEGIN {exit !($RATIO <= 1.20)}"; then
    echo "Performance test PASSED."
    exit 0
else
    echo "Performance test FAILED (comint-9term is more than 20% slower)."
    exit 1
fi
