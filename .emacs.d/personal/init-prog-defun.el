;; defuns that are common to most programming modes

(defun my-backward-char (&optional n)
  "defun that simply ignores errors.

Made to use with `less-evil-mode'"
  (unless n (setq n 1))
  (ignore-errors
    (backward-char n)))

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun last-char-space-p ()
  (member (preceding-char) '(?\  ?\t ?\n)))

(defun insert-period ()
  (interactive)
  (insert "."))

(defun insert-comma ()
  (interactive)
  (insert ","))

(defun insert-semi-colon ()
  (interactive)
  (insert ";"))

(defun insert-brace ()
  (interactive)
  (insert "{"))

(defun insert-space ()
  (interactive)
  (insert " "))

(provide 'init-prog-defun)
