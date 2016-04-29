;; Complete anything in GNU Emacs

(require 'company)

;; Enable globally
(add-hook 'after-init-hook 'global-company-mode)

;; Begin completion after 1 char
(setq company-minimum-prefix-length 1)

;; Show completion faster
(setq company-idle-delay 0.3)

;; Show enumerated completion
(setq company-show-numbers t)

;; use clang for autocompletion untill AST is present in GCC
(setq company-clang-arguments '(
                                "-I/usr/include/gtk-3.0"
                                "-I/usr/include/at-spi2-atk/2.0"
                                "-I/usr/include/at-spi-2.0"
                                "-I/usr/include/dbus-1.0"
                                "-I/usr/lib64/dbus-1.0/include"
                                "-I/usr/include/gtk-3.0"
                                "-I/usr/include/gio-unix-2.0/"
                                "-I/usr/include/cairo"
                                "-I/usr/include/pango-1.0"
                                "-I/usr/include/harfbuzz"
                                "-I/usr/include/pango-1.0"
                                "-I/usr/include/atk-1.0"
                                "-I/usr/include/cairo"
                                "-I/usr/include/pixman-1"
                                "-I/usr/include/freetype2"
                                "-I/usr/include/libpng16"
                                "-I/usr/include/freetype2"
                                "-I/usr/include/libdrm"
                                "-I/usr/include/libpng16"
                                "-I/usr/include/gdk-pixbuf-2.0"
                                "-I/usr/include/libpng16"
                                "-I/usr/include/glib-2.0"
                                "-I/usr/lib64/glib-2.0/include"))

(provide 'init-company)
