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
echo -e "ABCDEF\b\b\bXYZ"
echo ""

# Test 4: Carriage Return
echo "--- Test 4: Carriage Return (\\r) ---"
echo -e "OriginalText\rNew\033[K"
echo ""

# Test 5: CSI n A - Cursor Up
echo "--- Test 5: Cursor Up (CSI n A) ---"
echo "Before: Bottom, Middle, Top"
echo "After cursor up 2 and overwrite:"
printf "Bottom\nMiddle\nTop\n\033[2A\rREPLACED\n\n"
echo ""

# Test 6: CSI n B - Cursor Down
echo "--- Test 6: Cursor Down (CSI n B) ---"
echo "Line1 then cursor down skips a line:"
# 1. \n\n     : Reserve 2 lines of space (forces scroll if at bottom)
# 2. \033[2A  : Move cursor UP 2 lines to the starting position
# 3. Line1\n  : Print Line1 and newline
# 4. \033[1B  : The actual test - Move cursor DOWN 1 (skipping a line)
# 5. Line3    : Print Line3
printf "\n\n\033[2ALine1\n\033[1BLine3\n"
echo ""

# Test 7: CSI n C - Cursor Forward
echo "--- Test 7: Cursor Forward (CSI n C) ---"
echo "Start then 10 spaces forward then End:"
echo -e "Start\033[10CEnd"
echo ""

# Test 8: CSI n D - Cursor Backward
echo "--- Test 8: Cursor Backward (CSI n D) ---"
echo "0123456789 then back 5 then XXXXX:"
echo -e "0123456789\033[5DXXXXX"
echo ""

# Test 9: CSI n F - Cursor Previous Line
echo "--- Test 9: Cursor Previous Line (CSI n F) ---"
echo "Three lines, then cursor to previous 2nd line:"
printf "LineA\nLineB\nLineC\033[2FREPLACED\033[2B\n"
echo ""

# Test 10: CSI 0 K - Erase from cursor to end of line
echo "--- Test 10: Erase to End of Line (CSI 0 K) ---"
echo "Text with erase-to-end at position 14:"
echo -e "Keep this part DELETE\033[15G\033[0K"
echo ""

# Test 11: CSI 1 K - Erase from start of line to cursor
echo "--- Test 11: Erase from Start (CSI 1 K) ---"
echo "Text with erase-from-start, keeping end:"
echo -e "DELETE THIS   Keep this\033[14G\033[1K"
echo ""

# Test 12: CSI 2 K - Erase entire line
echo "--- Test 12: Erase Entire Line (CSI 2 K) ---"
echo "Line fully erased then replaced:"
echo -e "Line to erase\033[2K\rReplacement"
echo ""

# Test 13: ESC 7 and ESC 8 - Save and Restore Cursor Position
echo "--- Test 13: Save/Restore Cursor (ESC 7, ESC 8) ---"
echo "Save after 'Start ', add 'Middle' and newline, restore to add 'End':"
# 1. \n      : Ensure strictly that a line exists below (scroll if at bottom)
# 2. \033[1A : Move back UP to the start line
# 3. Start   : Print first word
# 4. \0337   : Save Cursor (at the space after Start)
# 5. Middle  : Print Middle
# 6. \0338   : Restore Cursor (back to space after Start)
# 7. \033[B  : Move Down 1 line (keeping same column)
# 8. End     : Print End
printf "\n\033[1AStart \0337Middle\0338\033[BEnd\n"
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
printf "Progress: [\0337          ] 0%%\0338\033[32m##########\033[0m] 100%%\n"
echo ""

# Test 19: Multiple Cursor Operations
echo "--- Test 19: Multiple Operations ---"
echo "ABC then back 3, write XXX, forward 3, write END:"
echo -e "ABC\033[3DXXX\033[3CEND"
echo ""

echo "=== Test Suite Complete ==="
# Ensure cursor is at the start of a new line
echo ""
