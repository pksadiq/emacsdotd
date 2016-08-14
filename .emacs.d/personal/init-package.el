;; Emacs supports packages that can be installed on the go.

(require 'package)

;; set ui before installing package (if required)
(require 'init-ui)

;; The package list to be installed
(setq package-list
      '(yasnippet
        gnome-c-style
        web-mode
        company
        avy
        flycheck
        markdown-mode
        js2-mode
        key-chord
        electric-spacing
        tern
        company-tern
        ggtags
        key-seq))

;; Repositories to be used to search and install packages.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 30)
        ("melpa-stable" . 20)
        ("melpa" . 10)))

;; Install packages that are not installed
(let ((refreshed nil))
  (dolist (pkg package-list)
    (when (not (package-installed-p pkg))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

(provide 'init-package)
