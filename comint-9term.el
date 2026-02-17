;;; comint-9term.el --- Advanced ANSI escape sequences for comint-mode

;; Restriction: do not add `(require 'ansi-color)`
;; Restriction: do not add `(provide 'comint-9term)`

(defvar-local comint-9term-saved-pos nil)
(defvar-local comint-9term-scroll-bottom nil)
(defvar-local comint-9term-lines-below-scroll 0)
(defvar-local comint-9term-virtual-col nil)

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
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line (- n))
        (move-to-column col nil)
        (if (< (current-column) col)
            (setq comint-9term-virtual-col col)
          (setq comint-9term-virtual-col nil))))
     ((eq char ?B) ; CUD - Cursor Down
      (let ((col (or comint-9term-virtual-col (current-column))))
        (forward-line n)
        (move-to-column col nil)
        (if (< (current-column) col)
            (setq comint-9term-virtual-col col)
          (setq comint-9term-virtual-col nil))))
     ((eq char ?C) ; CUF - Cursor Forward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (let ((target (+ curr n)))
          (move-to-column target nil)
          (if (< (current-column) target)
              (setq comint-9term-virtual-col target)
            (setq comint-9term-virtual-col nil)))))
     ((eq char ?D) ; CUB - Cursor Backward
      (let ((curr (or comint-9term-virtual-col (current-column))))
        (let ((target (max 0 (- curr n))))
          (setq comint-9term-virtual-col nil)
          (move-to-column target nil))))
     ((eq char ?F) ; CPL - Cursor Previous Line
      (let ((inhibit-field-text-motion t))
        (forward-line (- n))
        (beginning-of-line)
        (setq comint-9term-virtual-col nil)))
     ((eq char ?G) ; CHA - Cursor Horizontal Absolute
      (let ((target (1- n)))
        (move-to-column target nil)
        (if (< (current-column) target)
            (setq comint-9term-virtual-col target)
          (setq comint-9term-virtual-col nil))))
     ((or (eq char ?H) (eq char ?f)) ; CUP / HVP - Cursor Position
      (if comint-9term-scroll-bottom
          (let* ((height (1+ comint-9term-scroll-bottom))
                 (total-lines (line-number-at-pos (point-max)))
                 (start-line (if (> total-lines height) (- total-lines height) 0))
                 (target-line (+ start-line n)))
            (goto-char (point-min))
            (forward-line (1- (max 1 target-line))))
        (goto-char (point-min))
        (forward-line (1- n)))
      (let ((target (1- m)))
        (move-to-column target nil)
        (if (< (current-column) target)
            (setq comint-9term-virtual-col target)
          (setq comint-9term-virtual-col nil))))
     ((eq char ?J) ; ED - Erase in Display
      (setq comint-9term-virtual-col nil)
      (cond
       ((= n 0) (delete-region (point) (point-max)))
       ((= n 1) (delete-region (point-min) (point)))
       ((= n 2) (delete-region (point-min) (point-max)))))
     ((eq char ?K) ; EL - Erase in Line
      (let ((beg (line-beginning-position))
            (end (line-end-position))
            (cur (point)))
        (when comint-9term-virtual-col
           (let ((gap (- comint-9term-virtual-col (current-column))))
             (when (> gap 0) (insert (make-string gap ?\s))))
           (setq comint-9term-virtual-col nil)
           (setq cur (point))
           (setq end (line-end-position)))
        (cond
         ((= n 0) (delete-region cur end))
         ((= n 1)
          (let ((target (min (1+ cur) end)))
            (delete-region beg target)
            (insert (make-string (- target beg) ?\s))))
         ((= n 2) (delete-region beg end)))))
     ((eq char ?r) ; DECSTBM - Set Scrolling Region
      (let ((bottom (nth 1 params)))
        (when bottom
          (setq comint-9term-scroll-bottom bottom)
          (setq comint-9term-lines-below-scroll 1)))))))

(defun comint-9term-insert-and-overwrite (text)
  "Insert TEXT at process-mark."
  (when (> (length text) 0)
    (let* ((proc (get-buffer-process (current-buffer)))
           (pm (process-mark proc)))
      (goto-char pm)
      (dolist (c (append text nil))
        (cond
         ((eq c ?\n)
          (setq comint-9term-virtual-col nil)
          (let ((at-bottom-check
                 (and (> comint-9term-lines-below-scroll 0)
                      (> (save-excursion (forward-line (+ 2 comint-9term-lines-below-scroll))) 0))))
            (if at-bottom-check
                (insert "\n")
              (forward-line 1)
              ;; If we just moved to EOB, do NOT insert a newline if the command was just navigation.
              ;; But here we ARE processing \n.
              (if (eobp) (insert "\n")))))
         ((eq c ?\r) 
          (setq comint-9term-virtual-col nil)
          (beginning-of-line))
         ((eq c ?\b) 
          (if comint-9term-virtual-col
              (let ((new-col (max 0 (1- comint-9term-virtual-col))))
                (if (<= new-col (current-column))
                    (progn (setq comint-9term-virtual-col nil) (move-to-column new-col))
                  (setq comint-9term-virtual-col new-col)))
            (if (> (current-column) 0) (backward-char 1))))
         (t
          (when comint-9term-virtual-col
             (let ((gap (- comint-9term-virtual-col (current-column))))
               (when (> gap 0) (insert (make-string gap ?\s))))
             (setq comint-9term-virtual-col nil))
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
                  (inhibit-field-text-motion t)
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
  (make-local-variable 'comint-9term-scroll-bottom)
  (make-local-variable 'comint-9term-lines-below-scroll)
  (make-local-variable 'comint-9term-virtual-col)
  (add-hook 'comint-preoutput-filter-functions 'comint-9term-filter nil t))

(add-hook 'comint-mode-hook 'comint-9term-setup)

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (derived-mode-p 'comint-mode)
      (comint-9term-setup))))
