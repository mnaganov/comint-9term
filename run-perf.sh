#!/bin/bash
set -e

# Configuration
export APT_MESSAGES=3000

# Global warm-up to populate caches and compile elisp quickly
echo "--------------------------------------------------"
echo "Running Global Warm-up..."
echo "--------------------------------------------------"
emacs -batch -q -l test/perf-warmup-base.el > /dev/null 2>&1
emacs -batch -q -l test/perf-warmup-9term.el > /dev/null 2>&1

run_driver() {
    local driver_el=$1
    local start=$(date +%s.%N)
    # Use -L to ensure libraries are found, timeout to prevent hanging
    timeout 120 emacs -nw -q -L . -L test -L emacs-source/lisp/emacs-lisp -l "$driver_el" > /dev/null 2>&1
    local end=$(date +%s.%N)
    awk "BEGIN {print $end - $start}"
}

echo "--------------------------------------------------"
echo "Running Performance Benchmarks..."
echo "--------------------------------------------------"

# Standard (perf.py)
echo "Testing Standard (perf.py)..."
TIME_BASE_STD=$(run_driver "test/perf-base.el")
TIME_9TERM_STD=$(run_driver "test/perf-9term.el")
TIME_EAT_STD=$(run_driver "test/perf-eat.el")
TIME_XTERM_STD=$(run_driver "test/perf-xterm.el")

# APT Simulation (apt-prog-impl.sh)
echo "Testing APT Simulation (apt-prog-impl.sh)..."
TIME_BASE_APT=$(run_driver "test/perf-apt-base.el")
TIME_9TERM_APT=$(run_driver "test/perf-apt-9term.el")
TIME_EAT_APT=$(run_driver "test/perf-apt-eat.el")
TIME_XTERM_APT=$(run_driver "test/perf-apt-xterm.el")

# Format Time helper
fmt_time() {
    awk "BEGIN {printf \"%.2f s\", $1}"
}

# Table Header
printf "\n%-25s | %-12s | %-12s | %-12s | %-12s\n" "Test Type" "ansi-color" "comint-9term" "eat" "xterm-color"
printf "%-25s-|-%-12s-|-%-12s-|-%-12s-|-%-12s\n" "-------------------------" "------------" "------------" "------------" "------------"

# Table Rows
printf "%-25s | %12s | %12s | %12s | %12s\n" \
    "Standard (perf.py)" \
    "$(fmt_time $TIME_BASE_STD)" \
    "$(fmt_time $TIME_9TERM_STD)" \
    "$(fmt_time $TIME_EAT_STD)" \
    "$(fmt_time $TIME_XTERM_STD)"

printf "%-25s | %12s | %12s | %12s | %12s\n" \
    "APT Simulation" \
    "$(fmt_time $TIME_BASE_APT)" \
    "$(fmt_time $TIME_9TERM_APT)" \
    "$(fmt_time $TIME_EAT_APT)" \
    "$(fmt_time $TIME_XTERM_APT)"

# SUCCESS/FAILURE check (comint-9term vs ansi-color)
RATIO_STD=$(awk "BEGIN {print $TIME_9TERM_STD / $TIME_BASE_STD}")
RATIO_APT=$(awk "BEGIN {print $TIME_9TERM_APT / $TIME_BASE_APT}")

EXIT_CODE=0
echo ""
echo "Ratios (comint-9term / ansi-color):"
printf "Standard: %.2f (target <= 2.50) " $RATIO_STD
if awk "BEGIN {exit !($RATIO_STD <= 2.50)}"; then echo "[PASSED]"; else echo "[FAILED]"; EXIT_CODE=1; fi

printf "APT:      %.2f (target <= 2.50) " $RATIO_APT
if awk "BEGIN {exit !($RATIO_APT <= 2.50)}"; then echo "[PASSED]"; else echo "[FAILED]"; EXIT_CODE=1; fi

if [ $EXIT_CODE -eq 0 ]; then
    echo -e "\nOVERALL RESULT: PASSED"
else
    echo -e "\nOVERALL RESULT: FAILED"
fi

exit $EXIT_CODE
