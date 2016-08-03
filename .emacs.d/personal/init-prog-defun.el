;; defuns that are common to most programming modes

(defun my-backward-char (&optional n)
  "defun that simply ignores errors.

Made to use with `less-evil-mode'"
  (unless n (setq n 1))
  (ignore-errors
    (backward-char n)))

(provide 'init-prog-defun)
