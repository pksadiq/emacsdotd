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

(defun js-insert-block (&optional is-var-defun is-defun)
  (save-excursion
    (my-backward-char)
    (unless (eq (preceding-char) ?\ )
      (insert " ")
      (my-backward-char -1)))
  (save-excursion
    (if is-defun
        (insert ")")))
  (when (save-excursion
          (my-backward-char -1)
          (js2-forward-sws)
          (not (eq (following-char) ?\{)))
    (save-excursion
      (my-backward-char -1)
      (js2-backward-sws)
      (insert " {\n}")
      (if (or is-var-defun
              (and (eq (following-char) ?\))
                   (progn
                     (my-backward-char -1)
                     t)))
          (insert ";"))
      (indent-according-to-mode))))

(defun js-dwim-with-brace ()
  (let ((is-var-defun nil))
    (cond ((save-excursion
             (my-backward-char 2)
             (and (member 'font-lock-keyword-face (text-properties-at (point)))
                  (not (equal (c-token-at-point) "function"))))
           (js-insert-block))
          ((and (js--inside-param-list-p)
                (not (point-at-first-token-p)))
           (save-excursion
             (if (string-match-p
                  "= ?function" (buffer-substring-no-properties
                                 (point)
                                 (progn
                                   (my-backward-char)
                                   (re-search-backward "[=,();{}]")
                                   (point))))
                 (setq is-var-defun t)))
           (js-insert-block is-var-defun t))
          )))

(defun dwim-more-js-mode ()
  "Do What I Mean where ever possible

This function is mostly hooked with `self-insert-command'"
  (interactive)
  (let ((char-at-point (preceding-char))
        (char-before-point (char-before (1- (point)))))
    (cond ((eq last-command-event ?\!)
           (dwim-with-!))
          ((eq last-command-event ?\,)
           (dwim-with-comma))
          ((eq last-command-event ?\()
           (js-dwim-with-brace))
          (t
           nil))
    (setq second-last-point last-command-event)))

(provide 'init-js-defun)
