# Project Plan: Extending Emacs `comint-mode` for Advanced ANSI Escape Sequences

**Project Goal:** Extend Emacs `comint-mode` to support advanced ANSI escape sequences without modifying Emacs C or elisp source code. All integration will be performed by utilizing hooks provided by `comint` mode, with all code residing in `comint-9term.el`. The primary hook for ANSI parsing will be `comint-preoutput-filter-functions`.

## Workflow (Golden Loop Methodology):

1.  **Initial Verification & Task Identification:**
    *   Execute `verify.sh` to establish a baseline.
    *   Analyze the differences between `out/*.txt` (actual Emacs output) and `test/*.txt` (golden output) to pinpoint specific ANSI control sequences that `comint-mode` currently misinterprets or ignores.
    *   Examine `test/ansi-seq.sh` and `test/apt-prog.sh` to understand the types of ANSI sequences being tested (e.g., color, cursor movement, erase commands).
    *   Create a detailed list of individual "tasks," each targeting the implementation of support for a specific unsupported ANSI feature.

2.  **Implementation (Iterative & Incremental):**
    *   Address each task from the identified list sequentially.
    *   Develop and integrate Elisp code within `comint-9term.el`. This code will be added to `comint-preoutput-filter-functions` to preprocess the output and handle ANSI escape sequences.
    *   Utilize `grep` and inspect Emacs Lisp source files under `emacs-source/lisp/` (e.g., `ansi-color.el`, `term.el`, `comint.el`) to understand existing implementations, identify relevant functions, and ensure consistency with Emacs's internal workings.

3.  **Rigorous Verification:**
    *   After each implementation step (or a small set of related steps), execute `verify.sh`.
    *   **Critical Step:** Immediately check `out/elisp-errors.txt` for any Emacs Lisp errors. If errors are present, diagnose and fix them before proceeding to further implementation.
    *   Compare `out/*.txt` against `test/*.txt` to confirm that the newly implemented ANSI support functions correctly.
    *   **No Regressions:** Strictly ensure that changes made for one ANSI feature do not negatively impact previously working features or introduce new discrepancies in the `ansi-seq.sh` or `apt-prog.sh` tests. Avoid a vicious cycle of fixing and re-fixing.

4.  **Commit & Task Closure:**
    *   Once a specific ANSI feature (or a logical group of features) is fully implemented and passes all verification steps (no errors, no regressions, correct output), commit the changes to version control.
    *   Craft a clear, concise commit message describing the "why" and "what" of the changes.
    *   Mark the corresponding task as `completed`.

## Initial Tasks:

*   **Task 1 (Pending):** Run `verify.sh` to get the initial output and identify the specific ANSI escape sequences that are currently unsupported or incorrectly rendered.
*   **Task 2 (Pending):** Based on the analysis from Task 1, break down the problem into smaller, manageable sub-tasks, each focusing on supporting a particular ANSI feature (e.g., specific SGR parameters, cursor movements).