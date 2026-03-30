#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Tests standard prompt behavior, particularly when prompts are emitted to stderr
# (unbuffered) via `read -p`. It ensures that `comint-9term` handles standard
# sudo-style password prompts accurately without corrupting the input line or
# duplicating text when interacting with standard Emacs comint prompt filters.
# ==============================================================================
echo "=== Password Test ==="
# Use read -p to print prompt to stderr (unbuffered)
read -p "[sudo] password for user: " password
echo ""
echo "Thank you."
echo "=== Test Suite Complete ==="
