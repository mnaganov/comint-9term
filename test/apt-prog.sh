#!/bin/sh

# ==============================================================================
# Use Cases Covered:
# Tests the intermediate state of an `apt` progress bar by terminating it at 30%.
# It verifies that `comint-9term` accurately renders a mid-progress bar and
# exits cleanly without relying on the final cleanup sequences (`\e[J`) normally
# emitted at the end of an apt transaction.
#
# Then, allows the bar to run to completion and get erased from the screen.
# ==============================================================================
$(dirname "$0")/apt-prog-impl.sh 30
$(dirname "$0")/apt-prog-impl.sh
