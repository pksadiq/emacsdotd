;; This mode binds a few keys to read-only-mode as if in evil.

(require 'avy)
(require 'ido)
(require 'init-speedbar)
(require 'paredit)

(defun le/insert-here ()
  (interactive)
  (read-only-mode -1)
  (if (looking-at "[[:space:]]*$")
      (indent-according-to-mode)))

(defun le/return ()
  (interactive)
  (cond ((eq major-mode 'compilation-mode)
         (compile-goto-error))
        ((eq major-mode 'speedbar-mode)
         (speedbar-edit-line))
        (t
         (read-only-mode -1)
         (end-of-line)
         (insert "\n")
         (indent-according-to-mode))))

(defun le/navigate ()
  (interactive)
  (cond ((and mark-active
              (not (eq (mark) (point))))
         (narrow-to-region (mark) (point))
         (deactivate-mark))
        ((eq major-mode 'web-mode)
         (web-mode-navigate))))

(defun le/widen ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen)))

(defun le/diff-hl-revert-hunk ()
  (interactive)
  (read-only-mode -1)
  (diff-hl-revert-hunk)
  (read-only-mode 1))

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

(defun le/delete-previous-char-or-mark ()
  (interactive)
  (read-only-mode -1)
  (if (and mark-active
           (not (eq (mark) (point))))
      (delete-region (mark) (point))
    (paredit-backward-delete))
  (read-only-mode 1))

(defun le/save-buffer ()
  (interactive)
  (read-only-mode -1)
  (save-buffer)
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
  (cond ((eq major-mode 'web-mode)
         (web-mode-fold-or-unfold))
        ((eq major-mode 'org-mode)
         (org-cycle))
        (t
         (hs-toggle-hiding))))

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

;; TODO: merge this with `le/kill-line'
(defun le/par-kill-line ()
  (interactive)
  (le/insert-here)
  (if (and mark-active
           (not (eq (mark) (point))))
      (kill-region-or-backward-word)
    (with-demoted-errors
        (progn
          (if (bound-and-true-p paredit-mode)
              (paredit-kill)
            (kill-line))
          (indent-according-to-mode))))
  (read-only-mode 1))

(defun le/mark ()
  (interactive)
  (set-mark (point)))

(defun my-quit ()
  (interactive)
  (cond ((memq major-mode '(help-mode apropos-mode compilation-mode))
         (quit-window))
        ((eq major-mode 'speedbar-mode)
         (sr-speedbar-toggle))
        (t
         (keyboard-quit))))

(defun switch-my-buffer ()
  (interactive)
  (if (one-window-p)
      (ido-switch-buffer)
    (other-window 1)))

(defun le/scroll-down ()
  (interactive)
  (let ((my-point (point)))
    (scroll-down)
    (unless (eq my-point (point))
      (push-mark my-point))))

(defun le/avy-goto-char ()
  (interactive)
  (cond ((and mark-active
              (not (eq (mark) (point))))
         (kill-ring-save (point) (mark)))
        (t
         (call-interactively 'avy-goto-char))))

(defun le/yank ()
  (interactive)
  (read-only-mode -1)
  (yank)
  (if (derived-mode-p 'prog-mode)
      (indent-region (mark) (point)))
  (read-only-mode 1))

(defun le/end-of-line ()
  (interactive)
  (let ((last-point (point)))
    (end-of-line)
    (if (and (> (abs (-
                 (point)
                 last-point))
                6)
             (not mark-active))
        (push-mark last-point))))

(defun le/beginning-of-line ()
  (interactive)
  (let ((last-point (point)))
    (beginning-of-line)
    (if (and (> (abs (-
                 (point)
                 last-point))
                6)
             (not mark-active))
        (push-mark last-point))))

(defun le/scroll-up ()
  (interactive)
  (let ((my-point (point)))
    (cond ((eq major-mode 'speedbar-mode)
           (if (string-match-p " *>" (buffer-substring-no-properties
                                      (point)
                                      (save-excursion
                                        (beginning-of-line)
                                        (point))))
               (progn
                 (speedbar-edit-line)
                 (other-window 1)
                 (read-only-mode 1))
             (speedbar-toggle-line-expansion)
             ))
          (t
           (scroll-up)
           (unless (eq my-point (point))
             (push-mark my-point))))))

(defun le/beginning-of-buffer ()
  (interactive)
  (if (eq (point) 1)
      (end-of-buffer)
    (beginning-of-buffer)))

;;;###autoload
(define-minor-mode less-evil-mode
  "A lesser version of evil mode"
  :lighter " le"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "0") 'delete-window)
            (define-key map (kbd "1") 'delete-other-windows)
            (define-key map (kbd "2") 'split-window-below)
            (define-key map (kbd "3") 'split-window-right)
            (define-key map (kbd "SPC") 'le/scroll-up)
            (define-key map (kbd "S-SPC") 'le/scroll-down)
            (define-key map (kbd ";") 'le/end-of-line)
            (define-key map (kbd "<") 'diff-hl-previous-hunk)
            (define-key map (kbd ".") 'le/insert-after)
            (define-key map (kbd ">") 'diff-hl-next-hunk)
            (define-key map (kbd "/") 'isearch-forward-regexp)
            (define-key map (kbd "?") 'shell-command)
            (define-key map (kbd "q") 'my-quit)
            (define-key map (kbd "w") 'le/avy-goto-char)
            (define-key map (kbd "\S-w") 'avy-goto-char-2)
            (define-key map (kbd "e") 'avy-goto-line)
            (define-key map (kbd "r") 'flycheck-buffer)
            (define-key map (kbd "y") 'le/yank)
            (define-key map (kbd "t") 'le/fold-unfold)
            (define-key map (kbd "u") 'le/undo)
            (define-key map (kbd "i") 'le/insert-here)
            (define-key map (kbd "o") 'switch-my-buffer)
            (define-key map (kbd "\S-o") 'ido-switch-buffer)
            (define-key map (kbd "p") 'avy-pop-mark)
            (define-key map (kbd "a") 'le/beginning-of-line)
            (define-key map (kbd "s") 'le/save-buffer)
            (define-key map (kbd "\S-s") 'sr-speedbar-toggle)
            (define-key map (kbd "d") 'le/par-kill-line)
            (define-key map (kbd "\S-d") 'le/kill-line)
            (define-key map (kbd "f") 'ido-find-file)
            (define-key map (kbd "g") 'le/beginning-of-buffer)
            (define-key map (kbd "\S-g") 'end-of-buffer)
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "C-h") 'le/delete-previous-char-or-mark)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'right-char)
            (define-key map (kbd "z") 'execute-extended-command)
            (define-key map (kbd "x") 'le/delete-next-char)
            (define-key map (kbd "c") 'le/flycheck-next-error)
            (define-key map (kbd "\S-c") 'vc-next-action)
            (define-key map (kbd "v") 'le/flycheck-prev-error)
            (define-key map (kbd "n") 'le/navigate)
            (define-key map (kbd "\S-n") 'le/widen)
            (define-key map (kbd "m") 'le/mark)
            (define-key map (kbd "\S-m") 'le/diff-hl-revert-hunk)
            (define-key map (kbd "C-k") 'le/kill-line)
            (define-key map (kbd "C-w") 'le/kill-region)
            (define-key map (kbd "<backspace>") 'le/delete-previous-char-or-mark)
            (define-key map (kbd "RET") 'le/return)
            (define-key map (kbd "<S-return>") 'le/s-return)
            map))

;;;###autoload
(add-hook 'read-only-mode-hook
          (lambda ()
            (if buffer-read-only
                (less-evil-mode 1)
              (less-evil-mode -1))))

(defun evil-in-speedbar-mode-hook ()
  (let ((oldmap (cdr (assoc 'less-evil-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "e") nil)
    (define-key newmap (kbd "f") nil)
    (define-key newmap (kbd "i") nil)
    (define-key newmap (kbd "x") nil)
    (define-key newmap (kbd "s") nil)
    (define-key newmap (kbd "d") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(less-evil-mode . ,newmap) minor-mode-overriding-map-alist)))

(defun evil-in-erc-mode-hook ()
  (let ((oldmap (cdr (assoc 'less-evil-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "RET") nil)
    (define-key newmap (kbd "<backspace>") nil)
    (define-key newmap (kbd "C-h") nil)
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
(add-hook 'speedbar-mode-hook 'evil-in-speedbar-mode-hook)
(provide 'less-evil-mode)
