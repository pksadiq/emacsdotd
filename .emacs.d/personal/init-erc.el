;; erc IRC client configuration

(eval-after-load "erc"
  '(add-to-list 'erc-modules 'replace))

(eval-after-load "erc"
  '(add-to-list 'erc-modules 'notifications))

(eval-after-load "erc-replace"
  '(add-hook 'erc-insert-modify-hook 'erc-replace-insert))

(setq erc-replace-alist
      '(("fuck" . " f..k")))

;; Hide distracting info from `erc' IRC client
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; See https://tools.ietf.org/html/rfc2812.html#section-5
;; Hide Topic, user list, etc when joining a channel
(setq  erc-hide-list '("324"
                       "332"
                       "353"))

;; Interpret MIRC colors
(setq erc-interpret-mirc-color t)

;; Consider nicks concealed for 10 hours to be lurkers
(setq erc-lurker-threshold-time (* 10 60 60))

(defvar nick-n 0)
(make-variable-buffer-local 'nick-n)

(add-hook 'erc-mode-hook
    '(lambda ()
       (local-set-key (kbd "<tab>") 'erc-get-last-nick)))

(provide 'init-erc)
