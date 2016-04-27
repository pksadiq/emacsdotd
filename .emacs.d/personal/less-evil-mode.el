;; This mode binds a few keys to read-only-mode as if in evil.

(require 'avy)
(require 'ido)

(defun le/insert-here ()
  (interactive)
  (read-only-mode -1))

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

(defun le/undo ()
  (interactive)
  (le/insert-here)
  (with-demoted-errors
      (undo))
  (read-only-mode 1))

(defun le/kill-line ()
  (interactive)
  (le/insert-here)
  (with-demoted-errors
      (kill-line))
  (read-only-mode 1))

(defun my-quit ()
  (interactive)
  (if (eq major-mode 'help-mode)
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
            (define-key map (kbd "SPC") 'avy-goto-char)
            (define-key map (kbd ";") 'avy-goto-line)
            (define-key map (kbd ".") 'le/insert-after)
            (define-key map (kbd "/") 'isearch-forward-regexp)
            (define-key map (kbd "q") 'my-quit)
            (define-key map (kbd "e") 'end-of-line)
            (define-key map (kbd "t") 'hs-toggle-hiding)
            (define-key map (kbd "u") 'le/undo)
            (define-key map (kbd "i") 'le/insert-here)
            (define-key map (kbd "o") 'switch-my-buffer)
            (define-key map (kbd "p") 'avy-pop-mark)
            (define-key map (kbd "a") 'beginning-of-line)
            (define-key map (kbd "s") 'save-buffer)
            (define-key map (kbd "d") 'le/kill-line)
            (define-key map (kbd "f") 'ido-find-file)
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'right-char)
            (define-key map (kbd "x") 'execute-extended-command)
            (define-key map (kbd "c") 'le/flycheck-next-error)
            (define-key map (kbd "v") 'le/flycheck-prev-error)
            map))

;;;###autoload
(add-hook 'read-only-mode-hook
          (lambda ()
            (if buffer-read-only
                (less-evil-mode 1)
              (less-evil-mode -1))))

(provide 'less-evil-mode)
