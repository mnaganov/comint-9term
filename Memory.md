# Project Plan: Extending Emacs `comint-mode` for Advanced ANSI Escape Sequences

**Project Goal:** Extend Emacs `comint-mode` to support advanced ANSI escape sequences without modifying Emacs C or elisp source code.

## ANSI Sequences to Support:

Based on `ansi-seq.sh` and `apt-prog.sh`:

| Sequence | Name | Description |
| :--- | :--- | :--- |
| `CSI n A` | CUU | Cursor Up |
| `CSI n B` | CUD | Cursor Down |
| `CSI n C` | CUF | Cursor Forward |
| `CSI n D` | CUB | Cursor Backward |
| `CSI n F` | CPL | Cursor Previous Line |
| `CSI n G` | CHA | Cursor Horizontal Absolute |
| `CSI n ; m H` | CUP | Cursor Position |
| `CSI n ; m f` | HVP | Horizontal Vertical Position |
| `CSI n J` | ED | Erase in Display (0: end, 1: start, 2: all) |
| `CSI n K` | EL | Erase in Line (0: end, 1: start, 2: all) |
| `CSI n ; m r` | DECSTBM | Set Scrolling Region |
| `ESC 7` | DECSC | Save Cursor |
| `ESC 8` | DECRC | Restore Cursor |
| `\b` | BS | Backspace (Handled by comint usually?) |
| `\r` | CR | Carriage Return (Handled by comint usually?) |

## Tasks:

- [x] **Task 1:** Run `verify.sh` to establish baseline and identify failures.
- [in_progress] **Task 2:** Implement basic cursor movement sequences (CUU, CUD, CUF, CUB).
- [ ] **Task 3:** Implement Erase Line (EL) sequences.
- [ ] **Task 4:** Implement Save/Restore Cursor sequences.
- [ ] **Task 5:** Implement advanced sequences (CHA, CUP, CPL).
- [ ] **Task 6:** Implement Scrolling Region (DECSTBM) and Erase Display (ED).
- [ ] **Task 7:** Final verification and cleanup.

## Implementation Strategy:

1.  Use `comint-preoutput-filter-functions` to process the string before it's inserted into the buffer.
2.  However, some operations (like cursor movement) might be easier to implement *after* insertion using `comint-output-filter-functions` because they need to manipulate the buffer.
3.  Wait, if I use `comint-preoutput-filter-functions`, I can return an empty string and do all the buffer manipulations myself.
4.  Actually, `term.el` has a lot of this logic. I can't use `term.el` directly as it's a different mode, but I can see how it handles things.

## Notes:

- `comint-preoutput-filter-functions` passes the string to be inserted.
- I can use a state machine to track the cursor position if needed, or just rely on `point` in the buffer.
- Emacs `comint-mode` already handles some sequences. I should only implement what's missing.
- `ansi-color` handles SGR (`CSI n m`).
