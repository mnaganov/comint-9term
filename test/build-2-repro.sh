#!/bin/bash
echo "12"
echo "ignoring X..."
echo "including..."

# Start ANSI: Hide cursor, Set Region 1-1, GoTo 1,1
printf "\033[?25l\033[1;1r\033[1;1H"

# Print large block (20 lines)
for i in {1..20}; do
  echo "Line $i of big block"
done

# Print [ 0% 0/1] at the bottom
printf "[  0%% 0/1] bootstrap blueprint"

# Reset to top
printf "\033[1;1r\033[1;1H"

# Print [ 50% 1/2] - This should be at the bottom if fix works, or top if bug exists
printf "[ 50%% 1/2] analyzing...\033[K"

echo ""
