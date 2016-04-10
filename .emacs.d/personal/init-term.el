;; Configures `ansi-term'

(add-hook 'term-exec-hook 'kill-process-buffer-on-exit)

(provide 'init-term)
