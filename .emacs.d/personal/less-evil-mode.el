;; This mode binds a few keys to read-only-mode as if in evil.

(require 'avy)
(require 'ido)

(defun le/insert-here ()
  (interactive)
  (read-only-mode -1)
  (if (looking-at "[[:space:]]*$")
      (indent-according-to-mode)))

(defun le/return ()
  (interactive)
  (if (eq major-mode 'compilation-mode)
      (compile-goto-error)
    (progn
      (read-only-mode -1)
      (end-of-line)
      (insert "\n")
      (indent-according-to-mode))))

(defun le/navigate ()
  (interactive)
  (if (eq major-mode 'web-mode)
      (web-mode-navigate)))

(defun le/s-return ()
  (interactive)
  (read-only-mode -1)
  (insert "\n")
  (indent-according-to-mode))

(defun le/kill-region ()
  (interactive)
  (read-only-mode -1)
  (kill-region-or-backward-word)
  (read-only-mode 1))

(defun le/delete-next-char ()
  (interactive)
  (read-only-mode -1)
  (delete-char 1)
  (read-only-mode 1))

(defun le/delete-previous-char ()
  (interactive)
  (read-only-mode -1)
  (delete-char -1)
  (read-only-mode 1))

(defun le/insert-after ()
  (interactive)
  (right-char)
  (le/insert-here))

(defun le/flycheck-next-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-next-error)))

(defun le/flycheck-prev-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-previous-error)))


(defun le/fold-unfold ()
  (interactive)
  (if (eq major-mode 'web-mode)
      (web-mode-fold-or-unfold)
    (hs-toggle-hiding)))

(defun le/undo ()
  (interactive)
  (le/insert-here)
  (with-demoted-errors
      (undo))
  (read-only-mode 1))

(defun le/kill-line ()
  (interactive)
  (le/insert-here)
  (if (and mark-active
           (not (eq (mark) (point))))
      (kill-region-or-backward-word)
    (with-demoted-errors
        (progn
          (kill-line)
          (indent-according-to-mode))))
  (read-only-mode 1))

(defun le/mark ()
  (interactive)
  (set-mark (point)))

(defun my-quit ()
  (interactive)
  (if (memq major-mode '(help-mode apropos-mode compilation-mode))
      (quit-window)
    (keyboard-quit)))

(defun switch-my-buffer ()
  (interactive)
  (if (one-window-p)
      (ido-switch-buffer)
    (other-window 1)))


;;;###autoload
(define-minor-mode less-evil-mode
  "A lesser version of evil mode"
  :lighter " le"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "0") 'delete-window)
            (define-key map (kbd "1") 'delete-other-windows)
            (define-key map (kbd "2") 'split-window-below)
            (define-key map (kbd "3") 'split-window-right)
            (define-key map (kbd "SPC") 'scroll-up)
            (define-key map (kbd "S-SPC") 'scroll-down)
            (define-key map (kbd ";") 'avy-goto-line)
            (define-key map (kbd ".") 'le/insert-after)
            (define-key map (kbd "/") 'isearch-forward-regexp)
            (define-key map (kbd "q") 'my-quit)
            (define-key map (kbd "w") 'avy-goto-char)
            (define-key map (kbd "e") 'end-of-line)
            (define-key map (kbd "t") 'le/fold-unfold)
            (define-key map (kbd "u") 'le/undo)
            (define-key map (kbd "i") 'le/insert-here)
            (define-key map (kbd "o") 'switch-my-buffer)
            (define-key map (kbd "p") 'avy-pop-mark)
            (define-key map (kbd "a") 'beginning-of-line)
            (define-key map (kbd "s") 'save-buffer)
            (define-key map (kbd "d") 'le/kill-line)
            (define-key map (kbd "f") 'ido-find-file)
            (define-key map (kbd "g") 'beginning-of-buffer)
            (define-key map (kbd "\S-g") 'end-of-buffer)
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "C-h") 'le/delete-previous-char)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'right-char)
            (define-key map (kbd "z") 'execute-extended-command)
            (define-key map (kbd "x") 'le/delete-next-char)
            (define-key map (kbd "c") 'le/flycheck-next-error)
            (define-key map (kbd "v") 'le/flycheck-prev-error)
            (define-key map (kbd "n") 'le/navigate)
            (define-key map (kbd "m") 'le/mark)
            (define-key map (kbd "C-k") 'le/kill-line)
            (define-key map (kbd "C-w") 'le/kill-region)
            (define-key map (kbd "<backspace>") 'le/delete-previous-char)
            (define-key map (kbd "<return>") 'le/return)
            (define-key map (kbd "<S-return>") 'le/s-return)
            map))

;;;###autoload
(add-hook 'read-only-mode-hook
          (lambda ()
            (if buffer-read-only
                (less-evil-mode 1)
              (less-evil-mode -1))))

(defun evil-in-erc-mode-hook ()
  (let ((oldmap (cdr (assoc 'less-evil-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "<return>") nil)
    (define-key newmap (kbd "i") nil)
    (define-key newmap (kbd "x") nil)
    (define-key newmap (kbd "s") nil)
    (define-key newmap (kbd "d") nil)
    (define-key newmap (kbd "f") nil)
    (define-key newmap (kbd "u") nil)
    (define-key newmap (kbd ".") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(less-evil-mode . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'erc-mode-hook 'evil-in-erc-mode-hook)

(provide 'less-evil-mode)
