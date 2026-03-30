#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# This script wraps `apt-prog.sh` with a restricted window height (`LINES=10`)
# to explicitly test viewport scrolling bugs. It verifies that progress bars
# pinned to the bottom of a very small terminal do not "leak" upward and overwrite
# historical log data when natural scrolling occurs.
# ==============================================================================
export LINES=10
export COLUMNS=80
$(dirname "$0")/apt-prog.sh
