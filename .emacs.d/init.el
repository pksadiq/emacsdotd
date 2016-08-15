;; `init.el': Helper script to load other files
;; This script just loads other files. Nothing else. KISS?

;; Required without fail :). Else, GNU Emacs automatically adds this.
(package-initialize)

;; Assign one more path to be searched for elisp files.
(add-to-list 'load-path "~/.emacs.d/personal/")

;; Configures common Emacs settings.
;; Defined in: ~/.emacs.d/personal/init-common.el
(require 'init-common)

;; Configure repositories and install required packages
(require 'init-package)

;; Load C prgramming related user defined functions
(require 'init-c-defun)

;; Load javascript prgramming related user defined functions
(require 'init-js-defun)

;; Load user defined functions
(require 'init-defun)

;; Configures Common keyboard shortcuts
(require 'init-keys)

;; A minor evil mode
(require 'less-evil-mode)

;; Configures Emacs UI related elements.
(require 'init-ui)

;; Configure yasnippet snippets.
(require 'init-yas)

;; Configure interactive do
(require 'init-ido)

;; Configure avy-mode
(require 'init-avy)

;; Configures `company-mode' 
(require 'init-company)

;; Configures cc-mode
(require 'init-cc)

;; Configures c-mode
(require 'init-c)

;; Configures flycheck
(require 'init-flycheck)

;; Configures Shell script mode
(require 'init-sh)

;; Configures `erc' IRC client
(require 'init-erc)

;; Configures `web-mode'
(require 'init-web)

;; Configures `js-mode' and `js2-mode'
(require 'init-js)

;; Configures `org-mode'
(require 'init-org)

;; Configures `org-mode' web export
(require 'init-org-web)

;; Configures `ansi-term' Terminal Emulator
(require 'init-term)

;; Configure specific files to be opened with specific modes.
(require 'init-files)

;; Configure embedded sidebar speedbar
(require 'init-speedbar)

;; Configure diff modes
(require 'init-diff)
