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
      (while (not (save-excursion
                    (goto-char (point-min))
                    (search-forward "=== Test Suite Complete ===" nil t)))
        (accept-process-output proc 0.5))
      
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
      (write-region (mark) (point) output-file)
      
      ;; Closes the shell buffer
      (kill-buffer buf)
      
      )))

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
      (while (not (save-excursion
                    (goto-char (point-min))
                    (search-forward "=== Test Suite Complete ===" nil t)))
        ;; Use accept-process-output to wait if process is running,
        ;; otherwise sleep briefly to avoid a tight CPU loop if the process ends instantly
        (if (and proc (process-live-p proc))
            (accept-process-output proc 0.5)
          (sleep-for 0.5)))
      
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
      (write-region (mark) (point) output-file)
      
      ;; Closes the compilation buffer
      (kill-buffer buf)
      
      )))

(make-directory "out" t)
(load (concat default-directory "comint-9term.el"))
(my-run-test-in-shell "test/ansi-seq.sh" "out/ansi-seq-\out-shell.txt")
(my-run-test-in-shell "test/apt-prog.sh" "out/apt-prog-out-shell.txt")
(my-run-test-compile "test/ansi-seq.sh" "out/ansi-seq-out-compile.txt")
(my-run-test-compile "test/apt-prog.sh" "out/apt-prog-out-compile.txt")
(kill-emacs)
