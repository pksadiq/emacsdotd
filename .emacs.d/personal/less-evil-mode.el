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
  (undo)
  (read-only-mode 1))

;;;###autoload
(define-minor-mode less-evil-mode
  "A lesser version of evil mode"
  :lighter " le"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'right-char)
            (define-key map (kbd "SPC") 'avy-goto-char)
            (define-key map (kbd ";") 'avy-goto-line)
            (define-key map (kbd "/") 'isearch-forward-regexp)
            (define-key map (kbd "u") 'le/undo)
            (define-key map (kbd "i") 'le/insert-here)
            (define-key map (kbd "o") 'ido-switch-buffer)
            (define-key map (kbd "p") 'pop-global-mark)
            (define-key map (kbd "a") 'le/insert-after)
            (define-key map (kbd "d") 'kill-line)
            (define-key map (kbd "f") 'le/flycheck-next-error)
            (define-key map (kbd "x") 'execute-extended-command)
            (define-key map (kbd "v") 'le/flycheck-prev-error)
            map))

;;;###autoload
(add-hook 'read-only-mode-hook
          (lambda ()
            (if buffer-read-only
                (less-evil-mode 1)
              (less-evil-mode -1))))

(provide 'less-evil-mode)
