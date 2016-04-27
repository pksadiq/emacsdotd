;; Check syntax errors on the fly using `flycheck'

(require 'flycheck)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-gcc-warnings '("all")))


;; Enable flycheck globally
(global-flycheck-mode)

;; Include path for gtk3+
(setq
 flycheck-gcc-include-path
 (list
  (expand-file-name "/usr/include/gtk-3.0")
  (expand-file-name "/usr/include/at-spi2-atk/2.0")
  (expand-file-name "/usr/include/at-spi-2.0")
  (expand-file-name "/usr/include/dbus-1.0")
  (expand-file-name "/usr/lib64/dbus-1.0/include")
  (expand-file-name "/usr/include/gtk-3.0")
  (expand-file-name "/usr/include/gio-unix-2.0/")
  (expand-file-name "/usr/include/cairo")
  (expand-file-name "/usr/include/pango-1.0")
  (expand-file-name "/usr/include/harfbuzz")
  (expand-file-name "/usr/include/pango-1.0")
  (expand-file-name "/usr/include/atk-1.0")
  (expand-file-name "/usr/include/cairo")
  (expand-file-name "/usr/include/pixman-1")
  (expand-file-name "/usr/include/freetype2")
  (expand-file-name "/usr/include/libpng16")
  (expand-file-name "/usr/include/freetype2")
  (expand-file-name "/usr/include/libdrm")
  (expand-file-name "/usr/include/libpng16")
  (expand-file-name "/usr/include/gdk-pixbuf-2.0")
  (expand-file-name "/usr/include/libpng16")
  (expand-file-name "/usr/include/glib-2.0")
  (expand-file-name "/usr/lib64/glib-2.0/include")
  ))


(provide 'init-flycheck)
