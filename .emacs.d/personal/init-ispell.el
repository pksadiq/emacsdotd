;; Configure spell checking

(setq ispell-program-name "aspell")

;; no autocorrection. I just need to see where the mistakes are
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(setq-default ispell-word-extra-args '("--sug-mode=ultra" "--lang=en_US"))
(setq-default ispell-extra-args
              (append ispell-word-extra-args
                      '("--run-together"
                      "--run-together-limit=6" "--run-together-min=2")))

(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args ispell-word-extra-args)
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "C-,") nil)))

(provide 'init-ispell)
