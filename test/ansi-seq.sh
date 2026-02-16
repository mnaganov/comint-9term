#!/bin/bash
# Comprehensive test of ANSI escape sequences for Emacs comint rendering
# Each test is self-contained and leaves cursor at end of line

echo "=== ANSI Escape Sequence Test Suite ==="
echo ""

# Test 1: Basic control characters - Newline
echo "--- Test 1: Newline (\\n) ---"
echo -e "Line1\nLine2"
echo ""

# Test 2: Tab
echo "--- Test 2: Tab (\\t) ---"
echo -e "Col1\tCol2\tCol3"
echo ""

# Test 3: Backspace
echo "--- Test 3: Backspace (\\b) ---"
echo "Result: ABCXYZ (from ABCDEF with 3 backspaces then XYZ)"
echo ""

# Test 4: Carriage Return
echo "--- Test 4: Carriage Return (\\r) ---"
echo "Result: New (from 'OriginalText' then \\r then 'New')"
echo ""

# Test 5: CSI n A - Cursor Up
echo "--- Test 5: Cursor Up (CSI n A) ---"
echo "Before: Bottom, Middle, Top"
echo "After cursor up 2 and overwrite:"
echo "Bottom"
echo "REPLACED"
echo "Top"
echo ""

# Test 6: CSI n B - Cursor Down
echo "--- Test 6: Cursor Down (CSI n B) ---"
echo "Line1 then cursor down skips a line:"
echo "Line1"
echo ""
echo "Line3"
echo ""

# Test 7: CSI n C - Cursor Forward
echo "--- Test 7: Cursor Forward (CSI n C) ---"
echo "Start then 10 spaces forward then End:"
echo "Start          End"
echo ""

# Test 8: CSI n D - Cursor Backward
echo "--- Test 8: Cursor Backward (CSI n D) ---"
echo "0123456789 then back 5 then XXXXX:"
echo "01234XXXXX"
echo ""

# Test 9: CSI n F - Cursor Previous Line
echo "--- Test 9: Cursor Previous Line (CSI n F) ---"
echo "Three lines, then cursor to previous 2nd line:"
echo "REPLACED"
echo "LineB"
echo "LineC"
echo ""

# Test 10: CSI 0 K - Erase from cursor to end of line
echo "--- Test 10: Erase to End of Line (CSI 0 K) ---"
echo "Text with erase-to-end at position 14:"
echo "Keep this part"
echo ""

# Test 11: CSI 1 K - Erase from start of line to cursor
echo "--- Test 11: Erase from Start (CSI 1 K) ---"
echo "Text with erase-from-start, keeping end:"
echo "              Keep this"
echo ""

# Test 12: CSI 2 K - Erase entire line
echo "--- Test 12: Erase Entire Line (CSI 2 K) ---"
echo "Line fully erased then replaced:"
echo "Replacement"
echo ""

# Test 13: ESC 7 and ESC 8 - Save and Restore Cursor Position
echo "--- Test 13: Save/Restore Cursor (ESC 7, ESC 8) ---"
echo "Save after 'Start ', add 'Middle' and newline, restore to add 'End':"
echo "Start End"
echo "Middle"
echo ""

# Test 14: CSI n m - Foreground Colors
echo "--- Test 14: Foreground Colors (CSI n m) ---"
echo -e "\033[31mRed\033[0m \033[32mGreen\033[0m \033[33mYellow\033[0m \033[34mBlue\033[0m"
echo ""

# Test 15: CSI n m - Background Colors  
echo "--- Test 15: Background Colors (CSI n m) ---"
echo -e "\033[41mRed BG\033[0m \033[42mGreen BG\033[0m \033[43mYellow BG\033[0m \033[44mBlue BG\033[0m"
echo ""

# Test 16: CSI n m - Text Styles
echo "--- Test 16: Text Styles (CSI n m) ---"
echo -e "\033[1mBold\033[0m \033[4mUnderline\033[0m \033[7mReverse\033[0m"
echo ""

# Test 17: Combined Styles
echo "--- Test 17: Combined Styles ---"
echo -e "\033[1;31mBold Red\033[0m \033[4;32mUnderline Green\033[0m \033[1;44mBold Blue BG\033[0m"
echo ""

# Test 18: Progress Bar (Save/Restore with colors)
echo "--- Test 18: Progress Bar (Save/Restore) ---"
echo "Simple progress bar with colors:"
echo -e "Progress: [\033[32m##########\033[0m] 100%"
echo ""

# Test 19: Multiple Cursor Operations
echo "--- Test 19: Multiple Operations ---"
echo "ABC then back 3, write XXX, forward 3, write END:"
echo "XXXEND"
echo ""

echo "=== Test Suite Complete ==="
# Ensure cursor is at the start of a new line
echo ""
