;; Configurations that relates to Emacs UI elements in general.
;; Like menubar, toolbar, fringes, fonts, theme, etc.

;; Don't show startup screen
(setq inhibit-startup-message t)

;; Fonts should already be installed before you can use it.
(set-default-font "Inconsolata-16")

;; Set Dark theme
(load-theme 'manoj-dark 1)

;; Disable bold face for function names
(set-face-attribute 'font-lock-function-name-face nil :weight 'normal)

;; Open Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Hide space consuming things from screen
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable Fringes, the gray bar of about 0.5 cm (default)
;; on left and right of Emacs X11
(when (fboundp 'set-fringe-mode) (set-fringe-mode '(0 . 0)))

;; Let this file provide a feature named 'init-ui'.
;; Use `require' function to load to current Emacs session.
(provide 'init-ui)
