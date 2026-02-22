#!/bin/bash
echo "=== Password Test ==="
# Use read -p to print prompt to stderr (unbuffered)
read -p "[sudo] password for user: " password
echo ""
echo "Thank you."
echo "=== Test Suite Complete ==="
