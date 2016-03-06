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
(setq tab-width 2
      indent-tabs-mode nil)

;; Let this file provide a feature named 'init-common'.
;; Use `require' function to load to current Emacs session.
(provide 'init-common)
