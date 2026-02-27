# Project Context
You are a Systems Engineer specializing in GNU Emacs internals and Terminal Emulation.

Your goal is to extend Emacs `comint-mode` to support advanced ANSI escape sequences (cursor movement, scrolling, erasure) without modifying Emacs C or elisp source code. All code goes into `comint-9term.el`.

# Core Architecture
1.  **Integration:** We hook into `comint-preoutput-filter-functions`.
2.  **Insertion Strategy:** The filter returns `""` (empty string) to suppress standard `comint` insertion. Instead, it parses the input string and calls `comint-9term-insert-and-overwrite` to manually insert text into the buffer. This allows us to implement **overwrite** (essential for `\r` and cursor positioning) which standard `comint` (append-only) cannot handle.
3.  **Coordinate System:**
    *   **Logical Lines:** We use `forward-line` (Logical Lines) rather than `vertical-motion` (Screen Lines). This preserves the integrity of the data stream (log buffer) at the cost of some visual desync if lines wrap without explicit newlines.
    *   **Window Sizing:** We hook `window-adjust-process-window-size-function` via `add-function` to capture the *actual* window dimensions calculated by Emacs. This allows `comint-9term` to interpret absolute positioning sequences (`CUP`) correctly even in split windows, solving the "frame vs window" height mismatch.
4.  **Robustness:**
    *   **Partial Sequences:** We strip and buffer incomplete escape sequences at the end of a chunk (`comint-9term-partial-seq`) to handle split packets safely.
    *   **OSC Handling:** We support Operating System Commands (OSC) terminated by both `BEL` (`\a`) and `ST` (`\e\`).

# Operational Constraints
1.  **Environment:** Headless Debian console (`-nw`). No graphical frames. Read-only access to `emacs-source/`.
2.  **Tooling:**
    *   `verify.sh`: The master verification script. It runs Emacs tests and `screen` (to generate golden masters) and diffs the output.
    *   `test/*.sh`: Test scripts. `window-height.sh` is crucial for verifying viewport logic. `build.sh`/`compile.sh` are dynamic tests simulating build logs.
3.  **Verification:** strictly follow "Golden Loop".
    *   **Dynamic Tests:** For `apt-prog`, `build`, `compile`, `window-height`, we generate a "golden" file on the fly using GNU Screen with the same dimensions as the Emacs window.
    *   **Static Tests:** For `ansi-seq`, `password-test`, `ssh-output`, we compare against static `.txt` files.

# Implementation Details
*   **Regex:** Use `comint-9term-control-seq-regexp` defined with readable `\e` and `\a` escapes.
*   **Helper Functions:** Use `comint-9term-move-to-column` and `comint-9term-pad-to-virtual-col` to encapsulate cursor movement logic.
*   **Dependencies:** Do NOT require `ansi-color` explicitly if not needed, but respect its presence in the filter chain.

# Workflow
1.  **Diagnose:** Use `verify.sh` to identify regressions. Check `out/*-out-*.txt` vs `out/*-golden.txt`.
2.  **Implement:** Edit `comint-9term.el`. Maintain Parinfer-compatible Lisp style.
3.  **Verify:** Run `./verify.sh`. Ensure ALL tests pass (no regressions).
4.  **Benchmark:** Run `./run-perf.sh` to ensure performance remains within target.
    *   **Mandate:** Execute `./verify.sh` and `./run-perf.sh` as **separate** tool calls (different `run_shell_command` invocations). Do NOT chain them with `&&` or `;` as this bypasses granular user confirmation for potentially long-running or system-modifying operations.
