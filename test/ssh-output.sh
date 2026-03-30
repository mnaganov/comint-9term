#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Tests the parsing of Operating System Commands (OSC), specifically `OSC 8`
# hyperlinks, by reading a pre-recorded SSH login log (`ssh-output.log`). It
# verifies that standard output with embedded hyperlinks is properly decoded
# and rendered in Emacs.
# ==============================================================================
echo "=== SSH Login Test Suite ==="
cat $(dirname "$0")/ssh-output.log
echo "=== Test Suite Complete ==="
