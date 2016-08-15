;; Configure `diff-hl-mode' and other `diff' modes

;; mark diff even if not saved
(diff-hl-flydiff-mode)

(set-face-attribute 'diff-hl-change nil :background "black" :foreground "#8888ff")
(set-face-attribute 'diff-hl-delete nil :background "black" :foreground "#ff2222")
(set-face-attribute 'diff-hl-insert nil :background "black" :foreground "#22aa22")

(global-diff-hl-mode)

(provide 'init-diff)
