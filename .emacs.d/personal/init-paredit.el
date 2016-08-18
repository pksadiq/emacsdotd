;; Hacks around `paredit'

(require 'paredit)

(add-hook 'prog-mode-hook 'paredit-mode)
(add-hook 'paredit-mode-hook
          (lambda ()
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
              )))

(provide 'init-paredit)
