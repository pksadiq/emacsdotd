;; Configurations that relates to Emacs UI elements in general.
;; Like menubar, toolbar, fringes, fonts, theme, etc.

(require 'init-dir-locals)

;; Don't show startup screen
(setq inhibit-startup-message t)

;; Show Buffer name as title
(setq frame-title-format "%b - Emacs %&")

;; Fonts should already be installed before you can use it.
(set-default-font "Inconsolata-13")
(setq default-frame-alist '((font . "Inconsolata-13")))

;; Set Dark theme
(load-theme 'manoj-dark 1)

;; Disable bold face for function names
(set-face-attribute 'font-lock-function-name-face nil :weight 'normal)

;; prettify symbols like 'lambda'
(global-prettify-symbols-mode 1)

;; Open Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Hide space consuming things from screen
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable Fringes, the gray bar of about 0.5 cm (default)
;; on left and right of Emacs X11
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode '(4 . 0))
  (set-face-attribute 'fringe nil :background "black"))

;; Let this file provide a feature named 'init-ui'.
;; Use `require' function to load to current Emacs session.
(provide 'init-ui)
