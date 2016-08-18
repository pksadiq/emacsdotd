;; Some keyboard configurations/shortcut definitions.

;; Let Control-h kill a char back, as in shell.
(global-set-key (kbd "C-h") 'delete-backward-char)

(defun new-line-may-be ()
  (interactive)
  (cond ((and (eq (preceding-char) ?\{)
              (eq (following-char) ?\}))
         nil)
        ((or (minibufferp)
             (not (derived-mode-p 'prog-mode)))
         nil)
        (t
         (call-interactively 'move-end-of-line)))
  (call-interactively (key-binding "\C-m")))

;; Create a newline after current line, and jump to that
(global-set-key (kbd "<return>") 'new-line-may-be)
(global-set-key (kbd "<S-return>") 'newline)

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-x C-s") 'force-save-buffer)

(global-set-key (kbd "<f5>") 'my-term)
(global-set-key (kbd "<f6>") 'my-make-compile)
(global-set-key (kbd "<f7>") 'my-make-run)
(global-set-key (kbd "<f8>") 'devhelp-word-at-point)

(global-set-key (kbd "M-g M-g") 'avy-goto-line)

(require 'key-chord)
(key-seq-define-global "; " 'avy-goto-char)
(key-seq-define-global ";u" 'undo)
(key-seq-define-global ";a" 'beginning-of-line)
(key-seq-define-global ";s" 'save-buffer)
(key-seq-define-global ";d" 'read-only-mode)
(key-seq-define-global ";f" 'ido-find-file)
(key-seq-define-global ";g" 'avy-goto-line)

(key-chord-mode 1)

(provide 'init-keys)
