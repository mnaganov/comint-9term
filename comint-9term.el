(require 'ansi-color)

(defun comint-9term-filter (string)
  "Filter function to handle 9term-style terminal emulation for comint."
  (ansi-color-process-string string))

(defun comint-9term-mode ()
  "Set up comint-9term filter."
  (add-to-list 'comint-output-filter-functions 'comint-9term-filter))

(add-hook 'comint-mode-hook 'comint-9term-mode)

(provide 'comint-9term)
