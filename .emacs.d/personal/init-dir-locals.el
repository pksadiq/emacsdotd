;; Configure some `dir-locals' hacks


(defvar my-project-title nil)
(make-variable-buffer-local 'my-project-title)

;; Shall be used to set emacs frame title
(defvar my-project-frame-title nil)
(make-variable-buffer-local 'my-project-frame-title)


(provide 'init-dir-locals)
