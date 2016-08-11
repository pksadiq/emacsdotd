;; defuns that are common to most programming modes

(defun my-backward-char (&optional n)
  "defun that simply ignores errors.

Made to use with `less-evil-mode'"
  (unless n (setq n 1))
  (ignore-errors
    (backward-char n)))

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun last-char-space-p ()
  (member (preceding-char) '(?\  ?\t ?\n)))

(defun may-not-be-char ()
  (if (and (point-in-string-p)
           (save-excursion
             (while (and (not (bobp))
                         (point-in-string-p))
               (my-backward-char 1))
             (and (eq (following-char) ?\')
                  (string-match-p "[a-zA-Z0-9_]" (char-to-string (preceding-char))))))
      t
    nil))

(defun insert-period ()
  (interactive)
  (insert "."))

(defun insert-comma ()
  (interactive)
  (insert ","))

(defun insert-semi-colon ()
  (interactive)
  (insert ";"))

(defun insert-brace ()
  (interactive)
  (insert "{"))

(defun insert-space ()
  (interactive)
  (insert " "))

(defun dwim-with-comma ()
  (let ((last-char nil)
        (inside-enum nil))
    (unless (and (point-in-string-p)
                 (not (eq major-mode 'c-mode)))
      (under-score-to-space 1)
      (c-do-common-defun))
    (cond ((and (eq major-mode 'c-mode)
                (point-in-string-p)
                (may-not-be-char))
           ;; replace token with upcase like "my_str" to "MY_STR"
           (save-excursion
             (my-backward-char 1)
             (delete-char -1)
             (my-backward-char 2)
             (replace-token-at-point "upsnake")))
          ((point-in-string-p)
           (delete-char -1)
           (while (and (point-in-string-p)
                       (not (eobp)))
             (my-backward-char -1))
           (if (eq (following-char) ?\,)
               (my-backward-char -1)
             (insert ",")))
          ;; if ".," is typed before a closing brace ')', insert ',' after ')'
          ;; like ".,)" will be replaced with "), "
          ((and (save-excursion
                  (if (eq (preceding-char) ?\ )
                      (my-backward-char 1))
                  (and (eq (preceding-char) ?\,)
                       (eq (char-before (1- (point))) ?\.)))
                (eq (following-char) ?\)))
           (zap-to-char -1 ?\.)
           (my-backward-char -1)
           (if (eq (following-char) ?\,)
               (my-backward-char -1)
             (electric-spacing-insert "," 'after)))
          ;; on occurrences of ",," or ", ," replace it with " ="
          ((save-excursion
             (if (eq (preceding-char) ?\ )
                 (my-backward-char 1))
             (and (eq (char-before) ?\,)
                  (eq (char-before (1- (point))) ?\,)))
           (zap-to-char -2 ?\,)
           (electric-spacing-insert "="))
          ((save-excursion
             (if (eq (preceding-char) ?\ )
                 (backward-char 1))
             (my-backward-char 1)
             (setq last-char (char-before))
             (memq last-char '(?\= ?\> ?\< ?\! ?\/ ?\* ?\% ?\[)))
           (zap-to-char -1 last-char)
           (insert-char last-char)
           (when (and (eq last-char ?\[)
                      (eq (following-char) ?\]))
             (my-backward-char -1)
             (if (eq (following-char) ?\ )
                 (my-backward-char -1)
               (insert " ")))
           (if (eq last-char ?\!)
               (electric-spacing-insert "=" 'after)
             (electric-spacing-insert "=")))
          ;; enum constants are usually in upcase.
          ;; So upcase the constants in enums on ','
          ((and (eq major-mode 'c-mode)
                (c-inside-enum-p))
           (setq inside-enum t)
           (save-excursion
             (if (eq (preceding-char) ?\ )
                 (my-backward-char 1))
             (my-backward-char 1)
             (c-backward-token-2)
             (my-backward-char -1)
             (replace-token-at-point "upsnake")))
          ;; if the point is at/after the first token of the statement,
          ;; an insertion if ',' will be replaced with ' ='
          ((and (eq major-mode 'c-mode)
                (eq (save-excursion
                      (c-true-beginning-of-statement)
                      (point))
                    (save-excursion
                      (if (eq (preceding-char) ?\ )
                          (my-backward-char 1))
                      (my-backward-char)
                      (c-backward-token-2)
                      (when (eq (preceding-char) ?\.)
                        (my-backward-char)
                        (c-backward-token-2))
                      (point)))
                (not (c-in-function-arg-p))
                )
           (zap-to-char -1 ?\,)
           (electric-spacing-insert "="))
          )))


(provide 'init-prog-defun)
