;;; comint-9term.el --- Advanced ANSI escape sequences for comint-mode

;; Restriction: do not add `(require 'ansi-color)`
;; Restriction: do not add `(provide 'comint-9term)`

(defvar-local comint-9term-saved-pos nil)

(defun comint-9term-parse-params (params-str &optional default)
  (setq default (or default 1))
  (if (or (not params-str) (string= params-str ""))
      (list default)
    (let ((params (mapcar (lambda (s) (if (string= s "") default (string-to-number s)))
                          (split-string params-str ";"))))
      params)))

(defun comint-9term-handle-csi (char params)
  (let ((n (or (nth 0 params) 1))
        (m (or (nth 1 params) 1)))
    (cond
     ((eq char ?A) ; CUU - Cursor Up
      (let ((col (current-column)))
        (forward-line (- n))
        (move-to-column col t)))
     ((eq char ?B) ; CUD - Cursor Down
      (let ((col (current-column)))
        (forward-line n)
        (move-to-column col t)))
     ((eq char ?C) ; CUF - Cursor Forward
      (let ((end (line-end-position)))
        (if (> (+ (point) n) end)
            (progn
              (goto-char end)
              (insert (make-string (- (+ (point) n) end) ?\s)))
          (forward-char n))))
     ((eq char ?D) ; CUB - Cursor Backward
      (backward-char (min n (- (point) (line-beginning-position)))))
     ((eq char ?F) ; CPL - Cursor Previous Line
      (forward-line (- n))
      (beginning-of-line))
     ((eq char ?G) ; CHA - Cursor Horizontal Absolute
      (move-to-column (1- n) t))
     ((or (eq char ?H) (eq char ?f)) ; CUP / HVP - Cursor Position
      (goto-char (point-min))
      (let ((remaining-lines (forward-line (1- n))))
        (when (> remaining-lines 0)
          (goto-char (point-max))
          (insert (make-string remaining-lines ?\n))
          (goto-char (point-max))))
      (move-to-column (1- m) t))
     ((eq char ?J) ; ED - Erase in Display
      (cond
       ((= n 0) (delete-region (point) (point-max)))
       ((= n 1) (delete-region (point-min) (point)))
       ((= n 2) (delete-region (point-min) (point-max)))))
     ((eq char ?K) ; EL - Erase in Line
      (let ((beg (line-beginning-position))
            (end (line-end-position))
            (cur (point)))
        (cond
         ((= n 0) (delete-region cur end))
         ((= n 1)
          (delete-region beg cur)
          (insert (make-string (- cur beg) ?\s))
          (goto-char cur))
         ((= n 2)
          (delete-region beg end)
          (insert (make-string (- end beg) ?\s))
          (goto-char cur)))))
     ((eq char ?r) ; DECSTBM - Set Scrolling Region
      nil))))

(defun comint-9term-insert-and-overwrite (text)
  "Insert TEXT at process-mark, overwriting existing text if not at end of line."
  (when (> (length text) 0)
    (let* ((proc (get-buffer-process (current-buffer)))
           (pm (process-mark proc)))
      (goto-char pm)
      (dolist (c (append text nil))
        (cond
         ((eq c ?\n)
          (if (eobp)
              (insert "\n")
            (forward-line 1)))
         ((eq c ?\r) (beginning-of-line))
         ((eq c ?\b) (if (> (current-column) 0) (backward-char 1)))
         (t
          (if (and (not (eobp)) (not (eq (following-char) ?\n)))
              (delete-char 1))
          (insert (char-to-string c))))
        (set-marker pm (point))))))

(defun comint-9term-filter (string)
  (condition-case nil
      (let ((proc (get-buffer-process (current-buffer))))
        (if (not proc)
            string
          (with-current-buffer (process-buffer proc)
            (let ((inhibit-read-only t)
                  (start 0)
                  (min-p (process-mark proc)))
              (while (string-match "\033\\(\\[\\([0-9;]*\\)\\([A-Za-z]\\)\\|\\([78]\\)\\)" string start)
                (let* ((pre-text (substring string start (match-beginning 0)))
                       (is-csi (match-beginning 2))
                       (char (and is-csi (aref (match-string 3 string) 0)))
                       (seq-end (match-end 0)))
                  (comint-9term-insert-and-overwrite pre-text)
                  (setq min-p (min min-p (point)))
                  (if is-csi
                      (comint-9term-handle-csi char (comint-9term-parse-params (match-string 2 string) (if (memq char '(?J ?K)) 0 1)))
                    (let ((esc-char (aref (match-string 4 string) 0)))
                      (cond
                       ((eq esc-char ?7) (setq comint-9term-saved-pos (point-marker)))
                       ((eq esc-char ?8) (when comint-9term-saved-pos (goto-char comint-9term-saved-pos))))))
                  (setq min-p (min min-p (point)))
                  (set-marker (process-mark proc) (point))
                  (setq start seq-end)))
              (comint-9term-insert-and-overwrite (substring string start))
              (setq min-p (min min-p (point)))
              (set-marker (process-mark proc) (point))
              (when (fboundp 'ansi-color-apply-on-region)
                (ansi-color-apply-on-region (min min-p (process-mark proc)) (point-max)))))))
    (error nil))
  "")

(defun comint-9term-setup ()
  (make-local-variable 'comint-9term-saved-pos)
  (add-hook 'comint-preoutput-filter-functions 'comint-9term-filter nil t))

(add-hook 'comint-mode-hook 'comint-9term-setup)

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (derived-mode-p 'comint-mode)
      (comint-9term-setup))))
