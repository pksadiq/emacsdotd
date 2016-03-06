;; Configurations that relates to Emacs UI elements in general.
;; Like menubar, toolbar, fringes, fonts, theme, etc.

;; Fonts should already be installed before you can use it.
(set-default-font "Inconsolata-16")

;; Set Dark theme
(load-theme 'manoj-dark 1)

;; Hide space consuming things from screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable Fringes, the gray bar of about 0.5 cm (default)
;; on left and right of Emacs X11
(set-fringe-mode '(0 . 0))

;; Let this file provide a feature named 'init-ui'.
;; Use `require' function to load to current Emacs session.
(provide 'init-ui)
