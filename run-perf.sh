#!/bin/bash
set -e

# Warm-up runs to populate caches and compile elisp
echo "Running warm-up (baseline)..."
emacs -nw -q -l test/perf-base.el
echo "Running warm-up (comint-9term)..."
emacs -nw -q -l test/perf-9term.el

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

# Check if Ratio <= 2.50
if awk "BEGIN {exit !($RATIO <= 2.50)}"; then
    echo "Performance test PASSED."
    exit 0
else
    echo "Performance test FAILED (comint-9term is more than 2.5x slower)."
    exit 1
fi
