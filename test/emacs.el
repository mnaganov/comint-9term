(setq inhibit-splash-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(require 'ansi-color)

(defun my-run-test-in-shell (test-file output-file)
  "Runs test.sh in a shell, waits for completion, saves output, and cleans up."
  (interactive)
  (let* ((buf-name "*my-test-shell*")
         (buf (get-buffer-create buf-name)))

    ;; Opens a shell buffer
    (pop-to-buffer buf)
    (shell buf)

    (let ((proc (get-buffer-process buf)))
      ;; Prevent Emacs from asking for confirmation when killing the buffer later
      (set-process-query-on-exit-flag proc nil)

      ;; Wait briefly for the shell prompt to appear before typing
      (accept-process-output proc 0.5)

      ;; Executes script `test.sh`
      (goto-char (point-max))
      (insert test-file)
      (comint-send-input)

      ;; Waits until the script displays `=== Test Suite Complete ===`
      ;; accept-process-output ensures Emacs doesn't freeze while waiting
      (let ((start-time (float-time))
            (found nil))
        ;; Wait while string not found AND less than 5 seconds have passed
        (while (and (not (setq found (save-excursion
                                       (goto-char (point-min))
                                       (search-forward "=== Test Suite Complete ===" nil t))))
                    (< (- (float-time) start-time) 5.0))
          (accept-process-output proc 0.5))

        (if found
            (progn
              ;; Jumps to the beginning of the buffer
              (goto-char (point-min))

              ;; Finds `===` and starts region selection from the beginning of this substring
              (search-forward "===")
              (goto-char (match-beginning 0))
              (push-mark (point) nil t) ; Sets the mark and activates the region

              ;; Jumps to the end of the buffer
              (goto-char (point-max))

              ;; Finds `===` and sets the cursor on the line after it
              (search-backward "===")
              (forward-line 1)

              ;; Saves the selected region into a file
              (write-region (mark) (point) output-file))
          ;; If the script did not finish, just save the entire buffer contents
          (write-region (point-min) (point-max) output-file)))

      ;; Closes the shell buffer
      (kill-buffer buf))))

(defun my-run-test-compile (test-file output-file)
  "Runs test.sh via compile, waits for completion, saves output, and cleans up."
  (interactive)

  ;; Executes script `test.sh` via compile with comint argument set to t
  (compile test-file t)

  ;; Get the compilation buffer (Emacs defaults to "*compilation*")
  (let* ((buf (get-buffer "*compilation*"))
         (proc (get-buffer-process buf)))

    ;; Execute the following commands specifically inside the compilation buffer
    (with-current-buffer buf

      ;; Prevent Emacs from asking for confirmation when killing the buffer later
      (when proc
        (set-process-query-on-exit-flag proc nil))

      ;; Waits until the script displays `=== Test Suite Complete ===`
      (let ((start-time (float-time))
            (found nil))

        (while (and (not (setq found (save-excursion
                                       (goto-char (point-min))
                                       (search-forward "=== Test Suite Complete ===" nil t))))
                    (< (- (float-time) start-time) 5.0))
          ;; Use accept-process-output to wait if process is running,
          ;; otherwise sleep briefly to avoid a tight CPU loop if the process ends instantly
          (if (and proc (process-live-p proc))
              (accept-process-output proc 0.5)
            (sleep-for 0.5)))

        (if found
            (progn
              ;; Jumps to the beginning of the buffer
              (goto-char (point-min))

              ;; Finds `===` and starts region selection from the beginning of this substring
              (search-forward "===")
              (goto-char (match-beginning 0))
              (push-mark (point) nil t) ; Sets the mark and activates the region

              ;; Jumps to the end of the buffer
              (goto-char (point-max))

              ;; Finds `===` and sets the cursor on the line after it
              (search-backward "===")
              (forward-line 1)

              ;; Saves the selected region into a file
              (write-region (mark) (point) output-file))
          ;; If the script did not finish, just save the entire buffer contents
          (write-region (point-min) (point-max) output-file)))

      ;; Closes the compilation buffer
      (kill-buffer buf))))

(defun load-script-and-log-errors (script-file log-file)
  "Load SCRIPT-FILE and write any errors to LOG-FILE."
  (interactive "fScript to load: \nFLog file: ")
  (condition-case err
      (progn
        ;; 'load' arguments: file, noerror (nil means throw error), nomessage, nosuffix
        (load script-file nil nil t)
        (message "Successfully loaded: %s" script-file)
        t)

    ;; Catch all errors inheriting from the 'error' symbol
    (error
     (let* ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]"))
            ;; Extract and format the human-readable error message
            (err-string (error-message-string err))
            (log-message (format "%s Error loading '%s': %s\n"
                                 timestamp script-file err-string)))

       ;; Write the formatted message to the log file (appending it)
       (write-region log-message nil log-file 'append)
       (message "Failed to load '%s'. Error written to '%s'." script-file log-file)
       nil))))

(defun log-command-errors-to-file (data context caller)
  "Log interactive command errors to a file, then perform default behavior.
DATA is the error object, CONTEXT is context info, CALLER is the command."
  (let* ((log-file "out/elisp-errors.txt")
         (timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]"))
         (err-string (error-message-string data))
         (caller-name (if caller (symbol-name caller) "unknown command"))
         (log-message (format "%s Error in '%s': %s\n"
                              timestamp caller-name err-string)))

    ;; Write to the log file
    (write-region log-message nil log-file 'append)

    ;; Fall back to the default Emacs behavior (prints to *Messages*)
    (command-error-default-function data context caller)))

;; Activate the global error logger
(setq command-error-function #'log-command-errors-to-file)

(make-directory "out" t)

(unwind-protect
    (when (load-script-and-log-errors (concat default-directory "comint-9term.el") "out/elisp-errors.txt")
      (let ((script-file (expand-file-name "out/current-script")))
        (when (file-exists-p script-file)
          (with-temp-buffer
            (insert-file-contents script-file)
            (let ((script (string-trim (buffer-string))))
              (my-run-test-in-shell (concat "test/" script ".sh") (concat "out/" script "-out-shell.txt"))
              (my-run-test-compile (concat "test/" script ".sh") (concat "out/" script "-out-compile.txt")))))))
  (kill-emacs))
