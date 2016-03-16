;; Some personal functions

(defun set-mode-comment ()
  "Generate mode comment. Eg: in C, /* mode: c; indent-tabs-mode ... */"
  (let ((mode-comment "")
	(mode-now ""))
    (setq mode-now (replace-regexp-in-string
		    "-mode" "" (symbol-name major-mode)))
    (setq mode-comment (concat mode-comment
			       "-*- mode: " mode-now
			       "; indent-tabs-mode: "
			       (symbol-name indent-tabs-mode)
			       mode-comment
			       "; tab-width: "
			       (number-to-string tab-width)))
    (if (string= mode-now "c")
	(setq mode-comment (concat mode-comment
				   "; c-basic-offset: "
				   (number-to-string c-basic-offset))))
    (setq mode-comment (concat mode-comment " -*-"))
    (comment-region (point)
		    (progn (insert mode-comment)
			   (point)))))

(defun str-to-snake-style (str)
  (let ((out str)
        (case-fold-search nil))
    (cond ((string-match "[- ]" str)
           (setq out
                 (replace-regexp-in-string "[- ]" "_" str)))
          ((string-match "[A-Z]" str)
           (setq out
                 (replace-regexp-in-string "\\(.\\)\\([A-Z]\\)" "\\1_\\2" str))
           (setq out (downcase out))))
    out))

(defun str-to-style (style str)
  (let* ((str (str-to-snake-style str))
         (out str))
    (cond ((string= style "upcamel")
           (setq str (upcase-initials str))
           (setq out (replace-regexp-in-string "_" "" str)))
          ((string= style "lisp")
           (setq out (replace-regexp-in-string "_" "-" str))))
    out))

(provide 'init-defun)
