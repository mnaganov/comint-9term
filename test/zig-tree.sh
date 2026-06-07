#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Reproduces the Zig build system's in-place progress tree. Zig wraps each frame
# in synchronized-output markers (`\e[?2026h` / `\e[?2026l`), erases the display
# (`\e[J`), redraws an N-line tree, and then issues a carriage return followed by
# N Reverse Index (`\eM`) sequences to move the cursor back to the top of the
# frame so the next frame overwrites it. The tree branches are drawn with the DEC
# Special Graphics charset (`\e(0` ... `\e(B`), where `t`->U+251C, `q`->U+2500 and
# `m`->U+2514.
#
# Before the fix, `\eM` (RI) and `\e(X` (SCS) were unhandled: `^[M` and `^[(0`
# leaked into the buffer literally and every frame was appended instead of
# overwriting, accumulating dozens of stale copies of the tree.
#
# This test draws several frames (which must collapse onto a single in-place
# rendering) and then commits a final frame WITHOUT the RI rewind, so the
# resulting tree persists and can be diffed against the golden text.
# ==============================================================================

echo "=== Zig Tree Test Suite ==="

# Draw one frame and rewind the cursor to its top (4 lines -> 4x RI).
draw_frame() {
    printf '\033[?2026h\033[J'
    printf '[%s] Compile Build Script\n' "$1"
    printf '\033(0tq\033(B [%s/100] Linking\n' "$1"
    printf '\033(0mq\033(B [%s] Semantic Analysis\n' "$1"
    printf '   \033(0mq\033(B Io.Writer.print\n'
    printf '\r\033M\033M\033M\033M'
    printf '\033[?2026l'
}

draw_frame 1
draw_frame 50
draw_frame 99

# Commit the final frame: redraw without rewinding, leaving the cursor on the
# line below so the tree is preserved in the buffer.
printf '\033[?2026h\033[J'
printf '[done] Compile Build Script\n'
printf '\033(0tq\033(B [100/100] Linking\n'
printf '\033(0mq\033(B [100] Semantic Analysis\n'
printf '   \033(0mq\033(B Io.Writer.print\n'
printf '\033[?2026l'

echo "=== Test Suite Complete ==="
