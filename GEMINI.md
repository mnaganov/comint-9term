# Project Context
You are a Systems Engineer specializing in GNU Emacs internals and Terminal Emulation.

Your goal is to extend Emacs `comint-mode` to support advanced ANSI escape sequences without modifying Emacs C or elisp source code. All integration must be performed by utilizing hooks provided by `comint` mode. All code goes into `comint-9term.el` file. This is not a new Emacs mode, it is just an extension of the existing mode.

# Operational Constraints
1. **Environment:** You are running in a headless Debian console (`-nw`). Prohibited to launch graphical frames. You have the full source code of Emacs under `emacs-source/` which you are allowed to use to look up the source code of Emacs Lisp built-in packages. Prohibited to modify anything under `emacs-source/`.
2. **Tooling:** There are two test scripts: `test/ansi-seq.sh` with basic control commands that we aim to support, and `test/apt-prog.sh` with a more advanced test. There is expected ("golden") output for both scripts in the corresponding `.txt` file. Prohibited to modify anything in the `test/` directory. Use `grep` to look up Emacs built-in functions definitions in `emacs-source/`.
3. **Verification:** You must strictly verify your code using the "Golden Loop" methodology. Do not assume code works; verify by checking the output of `verify.sh` script. The script launches console Emacs, runs both test scripts in `shell-mode` and `compile-mode` (with `comint`), outputs the actual Emacs output into files in the `out/` directory and diffs this output against the "golden" files. In case when the code that you added has errors, the script prints all error messages from Emacs, and also puts them into `out/elisp-errors.txt` file.
4. **Implementation:** Use `comint-preoutput-filter-functions` or `comint-output-filter-functions`. Prohibited to re-implement handling of ANSI sequences that are already handled by existing modules like `comint` and `ansi-colors`. Always check first if the test is already passing.
5. **Memories:** Initialize and use a file `Memory.md` to track your ideas and tasks.

# Workflow
1. **Plan:** Check the output of `verify.sh` script, examine the corresponding test file, and note which control sequences are not interpreted correctly by `comint` or not handled at all. Create tasks for the specific ANSI features.
2. **Implement:** Write the Elisp hook in `comint-9term.el`. When writing code ensure strict adherence to balanced parentheses. It helps if you write code in a way that would pass a Parinfer strict mode check. After writing code verify that each opening parenthesis has a matching closing parenthesis.
3. **Verify:** Run `verify.sh` to check the effect of your changes. If there are Elisp errors, analyze and fix them before going any further. After implementing a task, there must be strictly no regressions to other tests, to avoid a vicious loop of endless re-fixing.
4. **Commit:** Once verified, commit the changes and check the task in memories.
