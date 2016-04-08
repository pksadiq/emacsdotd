;; `init.el': Helper script to load other files
;; This script just loads other files. Nothing else. KISS?

;; Assign one more path to be searched for elisp files.
(add-to-list 'load-path "~/.emacs.d/personal/")

;; Search for the library 'init-common' in 'load-path' and load it.
;; Configures common Emacs settings.
;; Defined in: ~/.emacs.d/personal/init-common.el
(load-library "init-common")
(require 'init-common)

;; Configure repositories and install required packages
(load-library "init-package")
(require 'init-package)

;; Load user defined functions
(load-library "init-defun")
(require 'init-defun)

;; Configures Common keyboard shortcuts
(load-library "init-keys")
(require 'init-keys)

;; Configures Emacs UI related elements.
(load-library "init-ui")
(require 'init-ui)

;; Configure yasnippet snippets.
(load-library "init-yas")
(require 'init-yas)

;; Configure interactive do
(load-library "init-ido")
(require 'init-ido)

;; Configure avy-mode
(load-library "init-avy")
(require 'init-avy)

;; Configures `company-mode' 
(load-library "init-company")
(require 'init-company)

;; Configures cc-mode
(load-library "init-cc")
(require 'init-cc)

;; Configures c-mode
(load-library "init-c")
(require 'init-c)

;; Configures Shell script mode
(load-library "init-sh")
(require 'init-sh)

;; Configures `erc' IRC client
(load-library "init-erc")
(require 'init-erc)

;; Configures `web-mode' 
(load-library "init-web")
(require 'init-web)
