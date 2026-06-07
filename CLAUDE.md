# Project Context
You are a Systems Engineer specializing in GNU Emacs internals and Terminal Emulation.

Your goal is to extend Emacs `comint-mode` to support advanced ANSI escape sequences (cursor movement, scrolling, erasure) without modifying Emacs C or elisp source code. All code goes into `comint-9term.el`.

# Core Architecture
1.  **Integration:** We hook into `comint-preoutput-filter-functions`.
2.  **Insertion Strategy:** The filter returns `""` (empty string) to suppress standard `comint insertion. Instead, it parses the input string and calls `comint-9term-insert-and-overwrite` to manually insert text into the buffer. This allows us to implement **overwrite** (essential for `\r` and cursor positioning) which standard `comint` (append-only) cannot handle.
3.  **Process-Mark Shielding:** When `compilation-scroll-output` is enabled, Emacs' standard filter forcefully moves the buffer's `process-mark` and `point` to `point-max`. To maintain coordinate integrity for in-place updates, `comint-9term` maintains an internal `comint-9term--true-pm` marker that is shielded from these external movements.
4.  **Coordinate System:**
    *   **Logical Lines:** We use `forward-line` (Logical Lines) rather than `vertical-motion` (Screen Lines). This preserves the integrity of the data stream (log buffer) at the cost of some visual desync if lines wrap without explicit newlines.
    *   **Window Sizing:** We hook `window-adjust-process-window-size-function` via `add-function` to capture the *actual* window dimensions calculated by Emacs. This allows `comint-9term` to interpret absolute positioning sequences (`CUP`) correctly even in split windows, solving the "frame vs window" height mismatch.
5.  **Robustness:**
    *   **Partial Sequences:** We strip and buffer incomplete escape sequences at the end of a chunk (`comint-9term-partial-seq`) to handle split packets safely.
    *   **OSC Handling:** We support Operating System Commands (OSC) terminated by both `BEL` (`\a`) and `ST` (`\e\`). Sequences are explicitly inserted and passed to `ansi-osc-apply-on-region` (Emacs 30+) or `comint-osc-process-output` (Emacs 29) to enable Hyperlinks and Directory Tracking.

# Key Knowledge & Recent Insights
*   **Cursor Save/Restore vs. Auto-Scrolling:** Emacs' auto-scrolling features (like `compilation-scroll-output`) forcefully advance the `process-mark` to `point-max`. When terminal applications save the cursor position (`\e7`), jump to the bottom to draw a progress bar, and then restore the cursor (`\e8`), Emacs might inadvertently intercept and pull the `process-mark` to the end of the progress bar before the next chunk is processed. The `comint-9term--true-pm` shielding ensures the rest of the stream is written at the restored position.
*   **Fixed Cursor Jump:** Fixed cursor jump issue where incoming output would pull the cursor to the bottom even if the user was reviewing history. Implemented point restoration in `comint-9term-filter` using a marker with `insertion-type t`. This ensures the cursor follows output if it was at the end, but stays put if moved elsewhere.
*   **Monotonic Viewport & Cursor Jumps:** Tools like `ninja` use a 1-line scroll margin (`\e[1;1r`) pinned to the top for status bars. When resetting to full-screen scrolling (`\e[r`) and moving to `\e[1;1H`, the layout heuristic must not snap the viewport back to the top of the buffer. Making `comint-9term-start-line` monotonically non-decreasing (via `comint-9term--max-start-line`, clamped to `point-max`) fixes cursor jump bugs by keeping the viewport pinned to its furthest downward progress.
*   **Performance Optimization:** High-frequency status bars (like `apt`) create excessive overhead. We optimize by:
    *   **Marker Reuse:** Reusing the `comint-9term-saved-pos` marker for `DECSC` (`\e7`) to avoid thousands of allocations.
    *   **Line Count Caching:** Using `buffer-chars-modified-tick` to cache the result of `line-number-at-pos` (total lines), significantly reducing CPU usage during intensive viewport operations.
*   **Strict Clamping Mandate:** Absolute positioning (`CUP`) and scroll regions (`DECSTBM`) MUST be clamped to the current `max-height`. Without this clamping, logs intended for large terminals will desynchronize when rendered in split Emacs windows, breaking natural scrolling and causing "leaking" progress bars.
*   **Deterministic Synchronization:** Use `9TERM_SET_HEIGHT=n` in scripts to force the emulator's height. Additionally, seeding the override from the `LINES` environment variable at buffer creation provides "test-harness glue" to synchronize dimensions before the first escape sequence arrives.
*   **Zsh & macOS Compatibility:** macOS Zsh pads lines with literal spaces when `TERM=vt50` because the macOS terminfo database lacks the `el` (ce) and `ed` (cd) capabilities. Prefer using Emacs-level configuration (`comint-terminfo-terminal`) over system-level terminfo patching for portability.
*   **Erase Sequences:** `comint-9term-control-seq-regexp` captures `([78JK])` to properly catch ZLE's short `\eJ` (Erase Display) and `\eK` (Erase Line) sequences.
*   **Robust Origin & Viewport stability:** Robustified `comint-9term-origin` initialization to skip prompts and echoed commands by starting the virtual terminal session on the line following the current process-mark. This ensures stable Row 1 mapping across shell and compilation modes.
*   **Natural Scrolling:** Implemented monotonic `comint-9term-start-line` with natural scrolling support. By incrementing `scroll-offset` even during non-region-constrained scrolling (at the viewport bottom), `comint-9term` now ensures that status bars and progress meters always map to fresh buffer lines, preventing 'leaks' into historical log content.
*   **Transient Resizing Robustness:** Added a minimum `max-height` of 10 in `comint-9term-max-height` (unless overridden) to provide robustness against transient window resizing artifacts in headless Emacs, preventing absolute positioning from being erroneously clamped and overwriting diagnostic headers.
*   **Functional Refactoring Experiment:** Attempted to refactor the process filter to fully map escape sequences to intervals before modifying the buffer (inspired by `m-buffer`). While this conceptually isolates state, it led to O(N^2) performance degradation (due to large region batching of ANSI colors) and broke `comint`'s implicit reliance on moving `point` to detect output. The most robust implementation retains the sequential parsing and local color application of the legacy loop, wrapped tightly in `(save-match-data ...)` to protect Emacs's global state without freezing `point` via `save-excursion`.

# Operational Constraints
1.  **Environment:** Headless Debian console (`-nw`). No graphical frames. Read-only access to `emacs-source/`.
2.  **Tooling:**
    *   `verify.sh`: The master verification script. It runs Emacs tests and `screen` (to generate golden masters) and diffs the output.
    *   `test/*.sh`: Test scripts. `window-height.sh` is crucial for verifying viewport logic.
3.  **Verification:** strictly follow "Golden Loop".
    *   **SCREEN_TESTS:** Tests generating exact terminal output (e.g., `apt-prog`, `build`, `compile`, `window-height`, `zsh-prompt`). We generate a "golden" file on the fly using GNU Screen with the same dimensions as the Emacs window.
    *   **SIMPLE_TESTS:** Tests validating infinite scrollback behavior vs physical screen overwrites (e.g., `build-status`, where `SCREEN_TESTS` would overwrite rows and fail to test scrollback layout).
    *   **Static Tests:** For `ansi-seq`, `password-test`, `ssh-output`, we compare against static `.txt` files.

# Implementation Details
*   **Regex:** Use `comint-9term-control-seq-regexp` defined with readable `\e` and `\a` escapes.
*   **Helper Functions:** Use `comint-9term-move-to-column` and `comint-9term-pad-to-virtual-col` to encapsulate cursor movement logic.
*   **Dependencies:** Do NOT require `ansi-color` explicitly if not needed, but respect its presence in the filter chain.

# Workflow
1.  **Diagnose:** Use `verify.sh` to identify regressions. Check `out/*-out-*.txt` vs `out/*-golden.txt`.
2.  **Implement:** Edit `comint-9term.el`. Maintain Parinfer-compatible Lisp style.
3.  **Verify:** Run `./verify.sh`. Ensure ALL tests pass (no regressions).
4.  **Benchmark:** Run `./run-perf.sh` to ensure performance remains within target (ratio <= 2.50).
    *   **Mandate:** Execute `./verify.sh` and `./run-perf.sh` as **separate** tool calls (different `run_shell_command` invocations). Do NOT chain them with `&&` or `;` as this bypasses granular user confirmation for potentially long-running or system-modifying operations.
