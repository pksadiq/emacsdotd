;; Some keyboard configurations/shortcut definitions.

;; Let Control-h kill a char back, as in shell.
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Create a newline after current line, and jump to that
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(global-set-key (kbd "<f5>") 'my-term)

(global-set-key (kbd "M-g M-g") 'avy-goto-line)

(require 'key-chord)
(key-chord-define-global "; " 'avy-goto-char)
(key-chord-define-global ";u" 'undo)
(key-chord-define-global ";a" 'beginning-of-line)
(key-chord-define-global ";s" 'save-buffer)
(key-chord-define-global ";d" 'read-only-mode)
(key-chord-define-global ";f" 'ido-find-file)
(key-chord-define-global ";g" 'avy-goto-line)

(key-chord-mode 1)

(provide 'init-keys)
