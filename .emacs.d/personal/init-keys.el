;; Some keyboard configurations/shortcut definitions.

;; Let Control-h kill a char back, as in shell.
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Create a newline after current line, and jump to that
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(global-set-key (kbd "<f5>") 'my-term)

(provide 'init-keys)
