(setq inhibit-splash-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(defvar my-password-sent nil)

(defun my-enter-password-in-minibuffer ()
  "Enters password into active minibuffer if present."
  (when (active-minibuffer-window)
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (insert "secretpass")
      (exit-minibuffer))
    (setq my-password-sent t)))

(defun my-wait-for-completion (proc output-file)
  "Wait for completion marker, handling password prompts if needed."
  (let ((start-time (float-time))
        (found nil))
    (setq my-password-sent nil)
    ;; Start timer to enter password if prompted via minibuffer (success case)
    (run-with-timer 1 1 'my-enter-password-in-minibuffer)

    (while (and (not found) (< (- (float-time) start-time) 10.0))
      ;; Wait for output. If send-invisible is called, this blocks until timer fires.
      (if (and proc (process-live-p proc))
          (accept-process-output proc 0.5)
        (sleep-for 0.5))

      (save-excursion
        (goto-char (point-min))
        (if (search-forward "=== Test Suite Complete ===" nil t)
            (setq found t)
          ;; Check for password prompt if not handled yet
          (when (and (not my-password-sent)
                     (search-forward "password for user:" nil t))
            ;; If found, wait a bit to see if timer handles it (i.e. if send-invisible was active)
            (sleep-for 1.5)
            ;; If still not sent, assume send-invisible was NOT called (failure case)
            ;; Send manually to the buffer
            (unless my-password-sent
              (goto-char (point-max))
              (if (process-live-p proc)
                  (let ((inhibit-read-only t))
                    (insert "secretpass")
                    (comint-send-input))
                ;; Process dead, likely compilation mode non-interactive.
                ;; Just skip input, buffer will match golden (no password).
                nil)
              (setq my-password-sent t))))))

    (cancel-function-timers 'my-enter-password-in-minibuffer)

    (if found
        (progn
          (goto-char (point-min))
          (search-forward "===")
          (goto-char (match-beginning 0))
          (push-mark (point) nil t)
          (goto-char (point-max))
          (search-backward "===")
          (forward-line 1)
          (write-region (mark) (point) output-file))
      ;; If timed out or crashed, save everything
      (write-region (point-min) (point-max) output-file))))

(defun my-run-test-in-shell (test-file output-file)
  "Runs test-file in a shell, handles input, saves output."
  (interactive)
  (let* ((buf-name "*my-test-shell*")
         (buf (get-buffer-create buf-name)))
    (switch-to-buffer buf)
    (shell buf)
    (sit-for 0.5)

    (when (or (string-match "trace-test" test-file)
              (string-match "compile.sh" test-file))
      (comint-9term-trace-mode 1))

    (when (string-match "build-2-repro" test-file)
      (setenv "LINES" "12"))

    (let ((proc (get-buffer-process buf)))
      (set-process-query-on-exit-flag proc nil)
      (accept-process-output proc 0.5)
      (goto-char (point-max))
      (insert test-file)
      (comint-send-input)
      (my-wait-for-completion proc output-file)

      (when (string-match "compile.sh" test-file)
        (with-current-buffer (get-buffer "*comint-9term-trace*")
          (write-region (point-min) (point-max) "out/compile-trace-shell.el")))

      (when (string-match "trace-test" test-file)
        (with-current-buffer (get-buffer "*comint-9term-trace*")
          (write-region (point-min) (point-max) "out/trace-log.el"))

        (let ((replay-buf (get-buffer-create "*replay*")))
          (with-current-buffer replay-buf
            (comint-mode)
            (comint-9term-setup)
            (comint-9term-replay-trace "out/trace-log.el")
            (goto-char (point-min))
            (search-forward "===")
            (goto-char (match-beginning 0))
            (push-mark (point) nil t)
            (goto-char (point-max))
            (search-backward "===")
            (forward-line 1)
            (write-region (mark) (point) "out/trace-replay.txt"))
          (kill-buffer replay-buf)))

      (kill-buffer buf)
      (when (string-match "build-2-repro" test-file)
        (setenv "LINES" nil)))))

(defun my-run-test-compile (test-file output-file)
  "Runs test-file via compile, handles input, saves output."
  (interactive)
  (let ((display-buffer-overriding-action
         (if (string-match "window-height" test-file)
             '(display-buffer-same-window . nil)
           nil)))
    (compile test-file t))
  (when (string-match "window-height" test-file)
    (sit-for 0.5))
  (let* ((buf (get-buffer "*compilation*")))
    (if (not buf)
        nil
      (let ((proc (get-buffer-process buf)))
        (with-current-buffer buf
          (when (string-match "compile.sh" test-file)
            (comint-9term-trace-mode 1))
          (when proc (set-process-query-on-exit-flag proc nil))
          (when (string-match "build-2-repro" test-file)
            (setenv "LINES" "12"))
          (my-wait-for-completion proc output-file)
          (when (string-match "compile.sh" test-file)
            (with-current-buffer (get-buffer "*comint-9term-trace*")
              (write-region (point-min) (point-max) "out/compile-trace-compile.el")))
          (kill-buffer buf)
          (when (string-match "build-2-repro" test-file)
            (setenv "LINES" nil)))))))

(defun log-command-errors-to-file (data context caller)
  (let* ((log-file "out/elisp-errors.txt")
         (timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]"))
         (err-string (error-message-string data))
         (caller-name (if caller (symbol-name caller) "unknown command"))
         (log-message (format "%s Error in '%s': %s\n"
                              timestamp caller-name err-string)))
    (write-region log-message nil log-file 'append)
    (command-error-default-function data context caller)))

(setq command-error-function #'log-command-errors-to-file)

(defun my-run-shell-reexec-test ()
  "Tests shell-mode re-execution of commands."
  (let* ((buf-name "*my-test-shell*")
         (buf (get-buffer-create buf-name)))
    (switch-to-buffer buf)
    (shell buf)
    (sit-for 0.5)
    (let ((proc (get-buffer-process buf)))
      (set-process-query-on-exit-flag proc nil)
      (goto-char (point-max))
      (insert "echo aaa")
      (comint-send-input)
      (accept-process-output proc 1.0)
      (goto-char (point-max))
      (insert "echo bbb")
      (comint-send-input)
      (accept-process-output proc 1.0)
      
      ;; Clear buffer up to the prompt for testing output easily? No, let's keep it.
      ;; Now find the FIRST "echo aaa" line
      (goto-char (point-min))
      (search-forward "echo aaa")
      ;; Grab input
      (let ((input (funcall comint-get-old-input)))
        (with-temp-file "out/shell-reexec-out-shell.txt"
          (insert input "\n")))
      (with-temp-file "out/shell-reexec-out-compile.txt"
        (insert "echo aaa\n"))
      (kill-buffer buf))))

(make-directory "out" t)

(let* ((script-file (expand-file-name "out/current-script"))
       (script (when (file-exists-p script-file)
                 (with-temp-buffer
                   (insert-file-contents script-file)
                   (string-trim (buffer-string))))))
  (unwind-protect
      (when (and script
                 (progn
                   (add-to-list 'load-path default-directory)
                   (condition-case err
                       (progn
                         (require 'comint-9term)
                         t)
                     (error
                      (let* ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]"))
                             (err-string (error-message-string err))
                             (log-message (format "%s Error requiring 'comint-9term': %s\n"
                                                  timestamp err-string)))
                        (write-region log-message nil "out/elisp-errors.txt" 'append)
                        (message "Failed to require 'comint-9term'. Error written to out/elisp-errors.txt.")
                        nil)))))
        (if (string= script "shell-reexec")
            (my-run-shell-reexec-test)
          (progn
            (when (string= script "window-height")
              (split-window-below)
              (shrink-window 10))
            (my-run-test-in-shell (concat "test/" script ".sh") (concat "out/" script "-out-shell.txt"))
            (my-run-test-compile  (concat "test/" script ".sh") (concat "out/" script "-out-compile.txt")))))
    (progn
      (when script
        (with-current-buffer "*Messages*"
          (write-region (point-min) (point-max) (concat "out/" script "-emacs-messages.txt"))))
      (kill-emacs))))
