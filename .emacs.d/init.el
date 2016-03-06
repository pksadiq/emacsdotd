;; `init.el': Helper script to load other files
;; This script just loads other files. Nothing else. KISS?

;; Assign one more path to be searched for elisp files.
(add-to-list 'load-path "~/.emacs.d/personal/")

;; Search for the library 'init-common' in 'load-path' and load it.
;; Configures common Emacs settings.
;; Defined in: ~/.emacs.d/personal/init-common.el
(load-library "init-common")
(require 'init-common)

;; Configures Emacs UI related elements.
(load-library "init-ui")
(require 'init-ui)

;; Configures Shell script mode
(load-library "init-sh")
(require 'init-sh)
