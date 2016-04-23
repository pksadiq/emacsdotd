;; Emacs supports packages that can be installed on the go.

(require 'package)

;; The package list to be installed
(setq package-list
      '(yasnippet
        gnome-c-style
        web-mode
        company
        avy
        flycheck))

;; Repositories to be used to search and install packages.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 30)
        ("melpa-stable" . 20)
        ("marmalade" . 10)))

(package-initialize)

;; Install packages that are not installed
(let ((refreshed nil))
  (dolist (pkg package-list)
    (when (not (package-installed-p pkg))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

(provide 'init-package)
