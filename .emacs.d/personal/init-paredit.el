;; Hacks around `paredit'

(require 'paredit)

(add-hook 'prog-mode-hook 'paredit-mode)

(defun my-paredit-less-key ()
  (unless (eq major-mode 'emacs-lisp-mode)
    (define-key paredit-mode-map ";" nil)
    (define-key paredit-mode-map "'" nil)
    (define-key paredit-mode-map "(" nil)
    (define-key paredit-mode-map "[" nil)
    (define-key paredit-mode-map ">" nil)
    (define-key paredit-mode-map "<" nil)
    (define-key paredit-mode-map "DEL" nil)
    (define-key paredit-mode-map "\S-d" nil)
    (define-key paredit-mode-map "<backspace>" nil)
    ))

(add-hook 'cc-mode 'my-paredit-less-key)
(add-hook 'js2-mode 'my-paredit-less-key)
(add-hook 'web-mode 'my-paredit-less-key)

(provide 'init-paredit)
