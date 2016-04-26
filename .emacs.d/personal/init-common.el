;; Configurations that are common to more than one Emacs mode.
;; Or configurations that doesn't fit anywhere else.

;; Let's type y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto close quotes and brackets: ", [, (, {, etc.
(electric-pair-mode 1)

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Show column number -- The number of characters from the beginning
;; of the current line up to the cursor
(column-number-mode 1)

;; No TABs, please. Let everything be space of 2
(setq-default tab-width 2
      indent-tabs-mode nil)

;; Disabled, this is CPU hungry.
;; Highlight trailing whitespace.
;; (setq-default show-trailing-whitespace t)

;; hide-show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'hs-hide-initial-comment-block t)

(add-hook 'prog-mode-hook 'my-read-only-mode)
(add-hook 'help-mode-hook 'my-read-only-mode)
(add-hook 'erc-mode-hook 'my-read-only-mode)
(add-hook 'gud-mode-hook 'my-read-only-mode)


;; Let this file provide a feature named 'init-common'.
;; Use `require' function to load to current Emacs session.
(provide 'init-common)
