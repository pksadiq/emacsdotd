;; Packages required for the functions below
(require 'subr-x)
(require 'init-prog-defun)

;; Several features may be a dump of `init-c-defun'.
;; Too lazy to do things ordered :)

(defun js-my-end-statement ()
  (let ((last-point (point)))
    (unless (save-excursion
              (end-of-line)
              (point-in-comment-p))
      (end-of-line)
      ;; `push-mark' if the end of line is at least 4 char far than
      ;; our last `point'
      (if (> (abs (-
                   (point)
                   last-point))
             4)
          (push-mark last-point))
      (delete-trailing-whitespace
       (line-beginning-position) (line-end-position))
      (js2-backward-sws))
    (unless (eq (char-before) ?\;)
      (delete-trailing-whitespace
       (line-beginning-position) (line-end-position))
      (js2-indent-line)
      (unless (memq (preceding-char) '(?\, ?\| ?\\ ?\& ?\* ?\< ?\> ?\=))
        (insert ";"))
      (js2-indent-line))
    (my-read-only-mode)))

(defun js-end-statement ()
  (interactive)
  (cond ((point-in-comment-p)
         (insert ";"))
        (t
         (js-my-end-statement))))

(defun dwim-more-js-mode ()
  "Do What I Mean where ever possible

This function is mostly hooked with `self-insert-command'"
  (interactive)
  (let ((char-at-point (preceding-char))
        (char-before-point (char-before (1- (point)))))
    (cond ;; ((eq last-command-event ?\n)
          ;;  (delete-backward-char 1)
          ;;  (dwim-with-return))
          ;; ((eq last-command-event ?\<)
          ;;  (dwim-with-<))
          ;; ((eq last-command-event ?\>)
          ;;  (dwim-with->))
          ;; ((eq char-at-point ?\{)
          ;;  (dwim-with-brace))
          ;; ((eq char-at-point ?\*)
          ;;  (dwim-with-asterisk))
          ((eq last-command-event ?\,)
           (dwim-with-comma))
          ;; ((eq char-at-point ?\ )
          ;;  (delete-char -1)
          ;;  (dwim-with-space))
          ;; ((eq char-at-point ?\))
          ;;  (dwim-with-paren-close))
          ;; ((eq char-at-point ?\()
          ;;  (dwim-with-paren-open))
          ;; ((eq char-at-point ?\')
          ;;  (dwim-with-quote))
          ;; ((eq char-at-point ?\.)
          ;;  (dwim-with-dot))
          ;; ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
          ;;  (under-score-to-space 1)
          ;;  (dwim-with-context))
     (t
      nil))
    (setq second-last-point last-command-event)))

(provide 'init-js-defun)
