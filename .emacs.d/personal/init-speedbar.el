;; Configure `speedbar' and `sr-speedbar'

;; Don't show images
(setq
 speedbar-use-images nil
 sr-speedbar-width 23
 speedbar-initial-expansion-list-name "quick buffers")

;; reduce font size so that we require even less space
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Inconsolata-11")
(setq speedbar-mode-hook
          '(lambda ()
             (buffer-face-set 'speedbar-face)
             (less-evil-mode 1)))

(provide 'init-speedbar)
