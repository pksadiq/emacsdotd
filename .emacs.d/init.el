;; `init.el': Helper script to load other files

;; This script just loads other files. Nothing else. KISS?

;; Assign one more path to be searched for elisp files.
(add-to-list 'load-path "~/.emacs.d/personal/")

;; Configures common Emacs settings.
;; Defined in: ~/.emacs.d/personal/init-common.el
(load-library "init-common")
(require 'init-common)

;; Search for the library 'init-ui' in 'load-path'
;; and load it.
;; Configures Emacs UI related elements.
;; Defined in: ~/.emacs.d/personal/init-ui.el
(load-library "init-ui")
(require 'init-ui)
