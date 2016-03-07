;; Some personal functions

;; Generate mode comment. Eg: in C, /* mode: c; indent-tabs-mode ... */
(defun set-mode-comment ()
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
    (insert mode-comment)
    (comment-region (line-beginning-position) (line-end-position))))


(provide 'init-defun)
