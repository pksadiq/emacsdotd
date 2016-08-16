;; Packages required for the functions below
(require 'subr-x)
(require 'init-prog-defun)
(require 'js2-mode)

;; This value is hardcored, so just redefine the defun
(defun js--multi-line-declaration-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js--declaration-keyword-re))
        (when (looking-at js--indent-operator-re)
          (goto-char (match-end 0)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at js--indent-operator-re)
                                   (js--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (js--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js--declaration-keyword-re)
          (goto-char (match-end 0))
          2)))))

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
      (unless (memq (preceding-char) '(?\, ?\| ?\\ ?\& ?\* ?\< ?\> ?\= ?\{))
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
          (unless (eq (following-char) ?\;)
            (insert ";")))
      (indent-according-to-mode))))

(defun js-inside-defun-arg-p ()
  (and (js--inside-param-list-p)
       (save-excursion
         (my-backward-char 1)
         (js2-backward-sws)
         (my-backward-char 1)
         (or (member 'font-lock-keyword-face (text-properties-at (point)))
             (member 'font-lock-function-name-face (text-properties-at (point)))
             (member 'js2-function-param (text-properties-at (point)))))
       (save-excursion
         (my-backward-char 1)
         (js2-backward-sws)
         (my-backward-char 1)
         (not (or (equal (c-token-at-point) "if")
                  (equal (c-token-at-point) "return")
                  (equal (c-token-at-point) "var")
                  (equal (c-token-at-point) "while")
                  (equal (c-token-at-point) "new"))))
       ))

(defun js-dwim-with-brace ()
  (let ((is-var-defun nil))
    (cond ((save-excursion
             (my-backward-char 2)
             (and (member 'font-lock-keyword-face (text-properties-at (point)))
                  (not (or (equal (c-token-at-point) "function")
                           (equal (c-token-at-point) "return")
                           (equal (c-token-at-point) "var")))))
           (js-insert-block))
          ((and (js-inside-defun-arg-p)
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
          ((save-excursion
            (my-backward-char)
            (point-at-first-token-p))
           (save-excursion
             (if (point-to-end-empty-p)
                 (insert ";"))))
          )))

(defun js-dwim-with-{ ()
  (cond ((save-excursion
            (my-backward-char)
            (js2-backward-sws)
            (eq (preceding-char) ?\=))
         (if (point-to-end-empty-p)
             (save-excursion
               (insert ";"))))
        ((save-excursion
           (my-backward-char)
           (js2-backward-sws)
           (eq (preceding-char) ?\:))
         (if (point-to-end-empty-p)
             (save-excursion
               (insert ",")))
         )))

(defun js-dwim-with-. ()
  (zap-to-char -1 ?\.)
  (cond ((and (eq (preceding-char) ?\()
              (eq (following-char) ?\)))
         (my-backward-char -1)
         (insert "."))
        ((and (eq (preceding-char) ?\.)
              (eq (following-char) ?\)))
         (delete-char -1)
         (my-backward-char -1)
         (insert "."))
        (t
         (insert "."))))

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
          ((eq last-command-event ?\{)
           (js-dwim-with-{))
          ((eq last-command-event ?\.)
           (js-dwim-with-.))
          (t
           nil))
    (setq second-last-point last-command-event)))

(provide 'init-js-defun)
