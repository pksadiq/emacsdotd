;; Packages required for the functions below
(require 'subr-x)

(defun electric-pair-inhibit-me (char)
  (or
   (eq char (char-after))
   (and (eq char (char-before))
        (eq char (char-before (1- (point)))))
   (eq (char-syntax (following-char)) ?w)
   (and (eq (nth 0 (syntax-ppss)) 1)
        (if (equal major-mode 'c-mode)
            (save-excursion
              (c-backward-sws)
              (c-backward-token-2)
              (c-backward-sws)
              (c-in-function-header-p))
          nil)
        (eq
         (save-excursion
           (c-backward-sws)
           (preceding-char)) ?\{))
   (and (equal major-mode 'c-mode)
        (eq
         (save-excursion
           (c-backward-sws)
           (preceding-char)) ?\{)
        (save-excursion
          (c-backward-sws)
          (c-backward-token-2)
          (c-backward-sws)
          (c-in-struct-or-enum-p)))
   ))

(setq-default electric-pair-inhibit-predicate 'electric-pair-inhibit-me)

(defun c-next-line-empty-p ()
  (let ((my-point (point)))
    (save-excursion
      (forward-line 1)
      (if (eq my-point (point))
          nil
        (string-match-p "^[ \t]*$"
                        (buffer-substring-no-properties
                         (save-excursion
                           (beginning-of-line)
                           (point))
                         (save-excursion
                           (end-of-line)
                           (point)))))
      )))

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun last-char-space-p ()
  (member (preceding-char) '(?\  ?\t ?\n)))

(defun c-re-search-backward-p (regexp)
  (save-excursion
    (re-search-backward regexp nil t)))

(defun c-in-function-name-p ()
  (save-excursion
    (if (eq (face-at-point) 'font-lock-function-name-face)
        t
      nil)))

(defun c-inside-enum-p ()
  (cond ((not (eq (nth 0 (syntax-ppss)) 1))
         nil)
        ((save-excursion
           (when (search-backward "{" nil t)
             (c-backward-sws)
             (backward-char 1)
             (if (and (c-token-at-point)
                      (string= (c-token-at-point) "enum"))
                 t
               (progn
                 (c-beginning-of-current-token)
                 (c-backward-token-2)
                 (forward-char 1)
                 (and (c-token-at-point)
                      (string= (c-token-at-point) "enum"))))))
         )))

(defun c-in-if-else-while-case-p ()
  (save-excursion
    (c-true-beginning-of-statement)
    (unless (eobp)
      (forward-char 1))
    (if (string-match-p "if\\|else\\|while\\|case" (c-token-at-point))
        t
      nil)))

(defun c-in-struct-or-enum-p ()
  (save-excursion
    (unless (eobp)
      (forward-char 1))
    (if (last-char-space-p)
        (backward-char 1))
    (if (last-char-space-p)
        (backward-char 1))
    (when (string= (c-token-at-point) "typedef")
      (c-forward-token-2)
      (forward-char 2))
    (backward-char 1)
    (when (eq (face-at-point) 'font-lock-type-face)
      (c-beginning-of-current-token)
      (c-backward-token-2)
      (forward-char 1))
    (if (string-match-p "struct\\|enum" (c-token-at-point))
        t
      nil)))

(defun c-in-function-header-p (&optional is-not)
  "(c-where-wrt-brace-construct) is not reporting the 'in-header to be right.
So, a hack to fix it."
  (if (and (not (save-excursion (c-beginning-of-macro)))
           (eq (c-where-wrt-brace-construct) 'in-header)
           (save-excursion
             (forward-char 1)
             (if (eq (preceding-char) ?\))
                 (backward-char 1))
             (when (last-char-space-p)
               (c-backward-sws)
               (backward-char 1))
             (if (and (not (> (nth 0 (syntax-ppss)) 0))
                      (not (c-in-struct-or-enum-p)))
                 (search-forward "(" nil t))
             (while (and (not (bobp))
                         (> (nth 0 (syntax-ppss)) 0))
               (forward-char 1))
             (c-forward-sws)
             (if is-not
                 (not (eq (following-char) ?\{))
               (eq (following-char) ?\{))))
      t
    nil))

(defun c-in-incomplete-function-arg-p ()
  (if (c-in-function-arg-p)
      (save-excursion
        (search-forward-regexp "[;)]" nil t)
        (if (eq (preceding-char) ?\;)
            nil
          (progn
            (c-forward-sws)
            (not (eq (following-char) ?\{)))))
    nil))

(defun c-after-incomplete-function-arg-p ()
  (save-excursion
    (c-backward-sws)
    (backward-char 1)
    (c-in-function-arg-p)))

(defun c-in-header-fname-p ()
  (save-excursion
    (unless (bobp)
      (backward-char 1))
    (when (and (eq (face-at-point) 'font-lock-string-face)
               (c-beginning-of-macro))
      (forward-char 2)
      (if (string= (c-token-at-point) "include")
          t
        nil))))

(defun c-in-function-arg-p ()
  (let ((my-point nil)
        (paren-count nil))
    (setq paren-count (nth 0 (syntax-ppss)))
    (if (and (> paren-count 0)
             (< paren-count 3))
        (setq my-point (c-re-search-backward-p "(")))
    (if (eq my-point nil)
        nil
      (save-excursion (while (and (not (bobp))
                                  (> (nth 0 (syntax-ppss (point))) 0))
                        (backward-char 1))
                      (c-backward-token-2)
                      (forward-char 1)
                      (c-in-function-name-p)))))
(defun c-in-array-p ()
  (let ((depth (nth 0 (syntax-ppss)))
        (is-sure-array nil)
        (is-array t))
    (save-excursion
      (while (and (not is-sure-array)
                  (not (bobp))
                  (> (nth 0 (syntax-ppss)) 0)
                  is-array)
        (while (and (not (bobp))
                    (= depth (nth 0 (syntax-ppss))))
          (backward-char 1))
        (if (eq (following-char) ?\{)
            (if (save-excursion
                  (c-backward-sws)
                  (eq (preceding-char) ?\=))
                (setq is-sure-array t))
          (setq is-array nil))
        (setq depth (nth 0 (syntax-ppss))))
      (c-backward-sws)
      (unless (eq (preceding-char) ?=)
        (setq is-array nil)))
    is-sure-array))

(defun c-point-in-token-p (&optional point-before use-space)
  "Check if the (point) is inside a C token
return t if true, else nil.

if POINT-AFTER is non nil, check if the (1- point) is inside token.

if USE-SPACE is t replace '_' with ' ' (only if last keyboard key 
was SPC)"
  (interactive)
  (let ((deleted nil)
        (status nil)
        (buffer-undo-list t))
    (when (and use-space
               (eq (preceding-char) ?\_)
               (eq last-command-event ?\ ))
      (setq deleted t)
      (delete-char -1)
      (insert " "))
    (setq status (save-excursion
                   (if point-before (backward-char))
                   (c-beginning-of-current-token)))
    (when deleted
      (delete-char -1)
      (insert "_"))
    status))

(defun c-single-char-token-at-point ()
  (interactive)
  (let ((token nil))
    (cond ((eq (preceding-char) ?\0)
           nil)
          ((not (or (last-char-space-p)
                    (c-point-in-token-p)))
           (setq token (buffer-substring-no-properties
                        (save-excursion (c-backward-token-2) (point))
                        (point)))))
    (if (= (length token) 1)
        token
      nil)))

(defun c-token-at-point (&optional return-points?)
  (interactive)
  (let ((token "")
        (points (cons (point) (point))))
    (cond ((c-point-in-token-p nil t)
           (setq token (buffer-substring-no-properties
                        (setcar points
                                (save-excursion (c-beginning-of-current-token) (point)))
                        (setcdr points
                                (save-excursion (c-end-of-current-token) (point))))))
          (t
           (setq token (c-single-char-token-at-point))))
    (if (= (length token) 1)
        (setcar points (1- (point))))
    (unless token
      (setq token ""))
    (if return-points?
        points
      token)))

(defun replace-token-at-point (style)
  (let ((token "")
        (token-stripped ""))
    (setq token (save-excursion
                  (forward-char)
                  (c-token-at-point)))
    (setq token-stripped token)
    (if (string-match-p "_t$" token-stripped)
        (setq token-stripped (substring token-stripped 0 -2)))
    (when (or (string-match-p "._." token-stripped)
              (string-match-p "snake\\|lisp" style))
      (setq token (str-to-style token style))
      (c-beginning-of-current-token)
      (delete-region (point) (save-excursion
                               (forward-char)
                               (c-end-of-current-token)
                               (point)))
      (insert token))))

;; experimental
(defun c-in-for-loop-p ()
  (let ((depth nil)
        (in-for-loop nil))
    (save-excursion
      (c-beginning-of-statement-1)
      (setq depth (nth 0 (syntax-ppss)))
      (while (and (not in-for-loop)
                  (not (bobp))
                  (<= depth (nth 0 (syntax-ppss))))
        (backward-char 1))
      (c-backward-sws)
      (unless (bobp)
        (backward-char 1))
      (if (string= (c-token-at-point) "for")
          (setq in-for-loop t))
      in-for-loop)))

(defun c-true-beginning-of-statement ()
  (c-beginning-of-statement-1)
  (c-backward-sws)
  (while (c-in-array-p)
    (backward-char 1))
  (while (not (memq (preceding-char) '(?\) ?\; ?\{ ?\} ?\0)))
    (backward-char 1)
    (c-beginning-of-statement-1)
    (c-backward-sws))
  (c-forward-sws))

(defun str-to-snake-case (str)
  "Convert STR like 'snake style', SnakeStyle', 'snake-style'
'SNAKE_STYLE', etc. to snake_style"
  (let ((out str)
        (case-fold-search nil))
    (if (string-match "\\([a-z]\\)\\([A-Z]\\)" str)
        (setq str
              (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1_\\2" str)))
    (if (string-match "\\b[A-Z _-]\\{4,\\}\\b" str)
        (setq out (downcase str)))
    (if (string-match "[-_ ]" out)
        (setq out
              (replace-regexp-in-string "[-_ ]" "_" out)))
    (if (string-match "[A-Z]" out)
        (setq out
              (replace-regexp-in-string "\\([^_]\\)\\([A-Z]\\)" "\\1_\\2" out)))
    (setq out (downcase out))
    out))

(defun str-to-snake-style (str)
  (if (< (length str) 4)
      str
    (str-to-snake-case str)))

(defun str-to-style (str style)
  "Convert STR to corresponding STYLE.

STR can be of the form CamelCase, snake_case, space text, or lisp-case

STYLE can be 'upcamel', 'lisp', 'upsnake'. any other STYLE defaults to 'snake'"
  (let* ((str (str-to-snake-style str))
         (out str))
    (cond ((string= style "upcamel")
           (setq str (upcase-initials str))
           (setq out (replace-regexp-in-string "\\(.\\)_\\(.\\)" "\\1\\2" str)))
          ((string= style "lisp")
           (setq out (replace-regexp-in-string "_" "-" str)))
          ((string= style "upsnake")
           (setq out (upcase str))))
    out))

(defun get-first (str)
  "Return the substring up to first '_' char from STR"
  (let ((out str))
    (setq out (replace-regexp-in-string "_.*" "" str))
    (setq out (upcase out))
    out))

(defun get-next (str)
  "Return the substring after the first '_' char from STR"
  (let ((out str))
    (setq out (replace-regexp-in-string "^[a-zA-Z0-9]*_" "" str))
    (setq out (upcase out))
    out))

(defun get-dot-h (str)
  (let ((file str))
    (setq file (replace-regexp-in-string "\.c" "\.h" str))
    (if (file-exists-p file)
        t
      nil)))

(defun my-end-statement ()
  (end-of-line)
  (delete-trailing-whitespace
   (line-beginning-position) (line-end-position))
  (unless (eq (char-before) ?\;)
    (c-indent-line)
    (insert ";")))

(defun end-statement ()
  (interactive)
  (cond ((point-in-comment-p)
         (insert ";"))
        ((do-common-defun)
         nil)
        ((c-in-function-arg-p)
         (align-current)
         (my-end-statement))
        ((c-inside-enum-p)
         (while (and (not (eobp))
                     (c-inside-enum-p))
           (forward-char 1))
         (my-end-statement))
        (t
         (my-end-statement))))

(defun c-beginning-of-expression ()
  (while (and (not (bobp))
              (not (c-at-expression-start-p)))
    (backward-char 1)))

(defun insert-semi-colon ()
  (interactive)
  (insert ";"))

(defun insert-brace ()
  (interactive)
  (insert "{"))

(defun insert-space ()
  (interactive)
  (insert " "))

(defun dwim-with-space ()
  (interactive)
  (cond ((eq (preceding-char) ?\0)
         (insert " "))
        ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
         (insert " "))
        ((c-in-header-fname-p)
         (insert "-"))
        ((or (point-in-comment-p)
             (point-in-string-p))
         (insert " "))
        ((save-excursion
           (backward-char)
           (c-beginning-of-current-token)
           (looking-at-p c-keywords-regexp))
         (insert " "))
        (t
         (insert "_"))
        ))

(defun under-score-to-space (value)
  (save-excursion
    (goto-char (- (point) value))
    (when (eq (char-before (point)) ?\_)
      (delete-char -1)
      (unless (string-match-p
               "[,;)]" (char-to-string (char-before (1+ (point)))))
        (insert " ")))))

(defun c-do-brace ()
  (unless mark-active
    (delete-trailing-whitespace
     (line-beginning-position) (line-end-position))
    (insert "\n{")
    (c-indent-line)
    (insert "\n\n}")
    (c-indent-line)
    (unless (c-next-line-empty-p)
      (insert "\n")
      (forward-line -1))
    (forward-line -1)
    (c-indent-line)))

(defun dwim-with-brace ()
  (unless mark-active
    (do-dwim-with-brace)))

(defun do-dwim-with-brace ()
  (let ((put-brace nil))
    (delete-backward-char 1)
    (cond ((c-in-struct-or-enum-p)
           (c-do-brace))
          ((save-excursion
             (if (and (> (point-max) (point))
                      (not (eq (following-char) ?\{)))
                 (forward-char))
             (c-backward-sws)
             (c-in-function-header-p))
           (if (c-in-function-arg-p)
               (align-current))
           (search-forward "{")
           (c-forward-sws)
           (if (eq (following-char) ?\})
               (backward-char 1)))
          ((or (c-in-incomplete-function-arg-p)
               (c-after-incomplete-function-arg-p))
           (when (c-after-incomplete-function-arg-p)
             (c-backward-sws)
             (backward-char 1))
           (align-current)
           (search-forward ")" nil t)
           (c-do-brace))
          ((c-in-function-arg-p)
           (align-current)
           (unless (eq (save-excursion
                         (c-end-of-statement)
                         (preceding-char)) ?\;)
             (c-end-of-statement)
             (c-do-brace)))
          (t
           (insert "{")
           (under-score-to-space 1)))))

(defun dwim-with-return ()
  (cond ((c-in-header-fname-p)
         (end-of-line)
         (if (not (c-next-line-empty-p))
             (insert "\n")
           (forward-line)))
        ((save-excursion
           (c-backward-sws)
           (c-backward-token-2)
           (unless (eobp)
             (forward-char 1))
           (eq (face-at-point) 'font-lock-type-face))
         (insert "\n")
         (save-excursion
           (c-backward-sws)
           (c-backward-token-2)
           (unless (eobp)
             (forward-char 1))
           (replace-token-at-point "upcamel")))
        (t
         (insert "\n"))))

(defun dwim-with-asterisk ()
  (under-score-to-space 1)
  (cond ((and (eq (save-excursion
                    (c-beginning-of-expression)
                    (point))
                  (save-excursion
                    (backward-char)
                    (c-backward-token-2)
                    (point)))
              (string-match-p "._." (save-excursion
                                      (backward-char)
                                      (c-backward-token-2)
                                      (forward-char)
                                      (c-token-at-point))))
         (save-excursion
           (backward-char)
           (c-backward-token-2)
           (replace-token-at-point "upcamel")))
        ((save-excursion
           (backward-char 1)
           (c-backward-token-2)
           (forward-char 1)
           (if (eq (face-at-point)
                   'font-lock-type-face)
               (replace-token-at-point "upcamel"))))
        nil))

(defun do-common-defun ()
  (if change-not-last
      nil
    (do-common-defun-defuns)))

(defun do-common-defun-defuns ()
  (save-excursion
    (backward-char 3)
    (if (and (c-token-at-point)
             (string= (c-token-at-point) "null"))
        (replace-token-at-point "upsnake")))
  (cond ((string-match-p "^g_application_flags_.*"
                         (save-excursion
                           (backward-char 1)
                           (c-backward-token-2)
                           (forward-char 1)
                           (if (not (c-token-at-point))
                               ""
                             (c-token-at-point))))
         (save-excursion
           (backward-char 3)
           (replace-token-at-point "upsnake"))))
  nil)

(defun dwim-with-comma ()
  (under-score-to-space 1)
  (do-common-defun)
  (cond ((c-inside-enum-p)
         (save-excursion
           (backward-char 1)
           (c-backward-token-2)
           (forward-char 1)
           (replace-token-at-point "upsnake")))
        ((and (eq (save-excursion
                    (c-true-beginning-of-statement)
                    (point))
                  (save-excursion
                    (backward-char)
                    (c-backward-token-2)
                    (point)))
              (not (c-in-function-arg-p))
              )
         (backward-delete-char 1)
         (insert " =")))
  (if (eq (following-char) ?\ )
      (forward-char 1)
    (insert " ")))


(defun dwim-with-paren-close ()
  (under-score-to-space 1)
  (do-common-defun)
  )

(defun dwim-with-paren-open ()
  (under-score-to-space 1)
  (do-common-defun)
  (cond ((eq (save-excursion
               (backward-char 3)
               (face-at-point))
             'font-lock-variable-name-face)
         (save-excursion
           (backward-char 1)
           (c-backward-token-2)
           (c-backward-sws)
           (while (string= (c-token-at-point) "*")
             (backward-char 1))
           (c-backward-token-2)
           (forward-char 1)
           (replace-token-at-point "upcamel")))))

(defun dwim-with-dot ()
  (let ((changed nil))
    (save-excursion
      (backward-char 1)
      (when (and (eq (preceding-char) ?\.)
                 (not (point-in-comment-p))
                 (not (point-in-string-p)))
        (delete-backward-char 1)
        (delete-forward-char 1)
        (insert "->")
        (setq changed t)))
    (if changed
        (forward-char 2))))

(defun dwim-with-> ()
  (let ((buffer-undo-list t))
    (cond ((and (c-in-header-fname-p)
                (eq (following-char) ?\>))
           (backward-delete-char -1)
           ))))

(defun dwim-with-< ()
  (under-score-to-space 1)
  (let ((in-include nil))
    (save-excursion
      (when (c-beginning-of-macro)
        (forward-char 2)
        (if (string= (c-token-at-point) "include")
            (setq in-include t))))
    (cond (in-include
           (insert ">")
           (backward-char 1))
          (t
           nil)
          )))

(defun dwim-with-context ())

(defun dwim-more ()
  (interactive)
  (let ((char-at-point (preceding-char)))
    (cond ((eq last-command-event ?\n)
           (delete-backward-char 1)
           (dwim-with-return))
          ((eq char-at-point ?\ )
           (delete-char -1)
           (dwim-with-space))
          ((eq char-at-point ?\<)
           (dwim-with-<))
          ((eq char-at-point ?\>)
           (dwim-with->))
          ((eq char-at-point ?\{)
           (dwim-with-brace))
          ((eq char-at-point ?\*)
           (dwim-with-asterisk))
          ((eq char-at-point ?\,)
           (dwim-with-comma))
          ((eq char-at-point ?\))
           (dwim-with-paren-close))
          ((eq char-at-point ?\()
           (dwim-with-paren-open))
          ((eq char-at-point ?\.)
           (dwim-with-dot))
          ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
           (under-score-to-space 1)
           (dwim-with-context))
          )
    (setq second-last-point last-command-event)))

(provide 'init-c-defun)
