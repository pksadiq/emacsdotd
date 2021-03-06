;; Packages required for the functions below
(require 'subr-x)
(require 'init-prog-defun)

;; Track non-electric insertion of pairs
(defvar my-pair-inserted nil)
(make-variable-buffer-local 'my-pair-inserted)

(defun electric-pair-inhibit-me (char)
  ;; Cases where `electric-pair-mode' should not insert pairs
  (or
   (and my-pair-inserted
        (progn
          (setq my-pair-inserted nil)
          t))
   (eq char (char-after))
   (and (eq char (char-before))
        (eq char (char-before (1- (point)))))
   (eq (char-syntax (following-char)) ?w)
   (and (equal major-mode 'c-mode)
        (eq (preceding-char) ?\')
        (not (point-in-string-p)))
   (and (equal major-mode 'c-mode)
        (point-in-string-p)
        (may-not-be-char))
   (and (eq major-mode 'js2-mode)
        (js-inside-defun-arg-p))
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

(defun c-re-search-backward-p (regexp)
  (save-excursion
    (re-search-backward regexp nil t)))

(defun c-in-function-name-p ()
  "get if the `point' is above a function name"
  (save-excursion
    ;; FIXME: check based on font-lock is not the right way to do this.
    ;; Do something better
    (if (member 'font-lock-function-name-face (text-properties-at (point)))
        t
      nil)))

(defun c-inside-enum-p ()
  (let ((brace-count (nth 0 (syntax-ppss)))
        (inside-enum t))
    (cond ((> brace-count 3)
           nil)
          ((save-excursion
             (when (search-backward-regexp "[{;]" nil t)
               (if (eq brace-count (nth 0 (syntax-ppss)))
                   nil
                 (progn
                   (c-backward-sws)
                   (my-backward-char 1)
                   (if (and (c-token-at-point)
                            (string= (c-token-at-point) "enum"))
                       t
                     (progn
                       (c-beginning-of-current-token)
                       (c-backward-token-2)
                       (my-backward-char -1)
                       (and (c-token-at-point)
                            (string= (c-token-at-point) "enum")))))))
           )))))

(defun c-in-if-else-while-case-p ()
  (save-excursion
    (c-true-beginning-of-statement)
    (my-backward-char -1)
    (if (string-match-p "if\\|else\\|while\\|case" (c-token-at-point))
        t
      nil)))

(defun c-incomplete-if-else-while-case-p ()
  (cond ((not (c-in-if-else-while-case-p))
         nil)
        (t
         (save-excursion
           (c-beginning-of-statement-1)
           (my-backward-char -1)
           (if (string= (c-token-at-point) "else")
               (c-end-of-current-token)
             (search-forward ")" nil t))
           (c-forward-sws)
           (if (eq (following-char) ?\{)
               nil
             t)))))

(defun c-in-struct-or-enum-p ()
  (save-excursion
    (my-backward-char -1)
    (if (last-char-space-p)
        (my-backward-char 1))
    (if (last-char-space-p)
        (my-backward-char 1))
    (when (string= (c-token-at-point) "typedef")
      (c-forward-token-2)
      (my-backward-char -2))
    (my-backward-char 1)
    (when (member 'font-lock-type-face (text-properties-at (point)))
      (c-beginning-of-current-token)
      (c-backward-token-2)
      (my-backward-char -1))
    (if (string-match-p "struct\\|enum" (c-token-at-point))
        (c-token-at-point)
      nil)))

(defun c-in-function-header-p (&optional is-not)
  "(c-where-wrt-brace-construct) is not reporting the 'in-header to be right.
So, a hack to fix it."
  (if (and (not (save-excursion (c-beginning-of-macro)))
           (eq (c-where-wrt-brace-construct) 'in-header)
           (save-excursion
             (my-backward-char -1)
             (if (eq (preceding-char) ?\))
                 (my-backward-char 1))
             (when (last-char-space-p)
               (c-backward-sws)
               (my-backward-char 1))
             (if (and (not (> (nth 0 (syntax-ppss)) 0))
                      (not (c-in-struct-or-enum-p)))
                 (search-forward "(" nil t))
             (while (and (not (bobp))
                         (> (nth 0 (syntax-ppss)) 0))
               (my-backward-char -1))
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
    (my-backward-char 1)
    (c-in-function-arg-p)))

(defun c-in-header-fname-p ()
  (save-excursion
    (my-backward-char 1)
    (when (and (member 'font-lock-string-face (text-properties-at (point)))
               (c-beginning-of-macro))
      (my-backward-char -2)
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
                        (my-backward-char 1))
                      (c-backward-token-2)
                      (my-backward-char -1)
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
          (my-backward-char 1))
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
                  (my-backward-char -1)
                  (c-token-at-point)))
    (setq token-stripped token)
    (if (string-match-p "_t$" token-stripped)
        (setq token-stripped (substring token-stripped 0 -2)))
    (when (or (string-match-p "._." token-stripped)
              (string-match-p "snake\\|lisp" style))
      (setq token (str-to-style token style))
      (c-beginning-of-current-token)
      (unless (string= token "")
        (delete-region (point) (save-excursion
                                 (my-backward-char -1)
                                 (c-end-of-current-token)
                                 (point)))
        (insert token)))))

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
    (my-backward-char 1))
  (while (not (memq (preceding-char) '(?\) ?\; ?\{ ?\} ?\0)))
    (my-backward-char 1)
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
    (cond ((string-match-p "[a-z]*_t$" str)
           (setq out str))
          ((string= style "upcamel")
           (setq str (upcase-initials str))
           ;; FIXME: testcase that doesn't work: "m_y_str"
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

(defun c-my-end-statement ()
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
      (c-backward-sws))
    (while (and (c-in-function-arg-p)
                (not (eobp)))
      (my-backward-char -1))
    (unless (eq (char-before) ?\;)
      (delete-trailing-whitespace
       (line-beginning-position) (line-end-position))
      (c-indent-line)
      (unless (memq (preceding-char) '(?\, ?\| ?\\ ?\& ?\* ?\< ?\> ?\=))
        (insert ";"))
      (c-indent-line))
    (my-read-only-mode)))

(defun c-end-statement ()
  (interactive)
  (cond ((point-in-comment-p)
         (insert ";"))
        ((c-do-common-defun)
         nil)
        ((c-in-function-arg-p)
         (c-align-fn-arg-p)
         (c-my-end-statement))
        ((c-inside-enum-p)
         (while (and (not (eobp))
                     (c-inside-enum-p))
           (my-backward-char -1))
         (c-my-end-statement))
        ((and (point-in-string-p)
              (may-not-be-char))
         (save-excursion
           (delete-char -1)
           (my-backward-char 2)
           (replace-token-at-point "upsnake"))
         (c-my-end-statement))
        (t
         (c-my-end-statement))))

(defun c-beginning-of-expression ()
  (while (and (not (bobp))
              (not (c-at-expression-start-p)))
    (my-backward-char 1)))

(defun c-dwim-with-space ()
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
           (my-backward-char)
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
    (if (or (string= (c-in-struct-or-enum-p) "enum")
            (string= (c-token-at-point) "="))
        (insert " ")
      (if (and (not (eq (preceding-char) ?\n))
               (c-next-line-empty-p))
          (forward-line)
        (insert "\n")))
    (insert "{")
    (c-indent-line)
    (insert "\n\n}")
    (c-indent-line)
    (setq my-pair-inserted t)
    (when (and (eq (nth 0 (syntax-ppss)) 0)
               (not (c-next-line-empty-p)))
      (insert "\n")
      (forward-line -1))
    (forward-line -1)
    (c-indent-line)))

(defun c-align-fn-arg-p ()
  (let ((beg nil)
        (end nil))
    (save-excursion
      (while (c-in-function-arg-p)
        (my-backward-char 1))
      (setq beg (point)))
    (save-excursion
      (while (c-in-function-arg-p)
        (my-backward-char -1))
      (setq end (point)))
    (indent-region beg end)
    (align beg end)))

(defun c-dwim-with-brace ()
  (unless mark-active
    (c-do-dwim-with-brace)))

(defun c-do-dwim-with-brace ()
  (let ((put-brace nil)
        (in-array (c-in-array-p)))
    (delete-backward-char 1)
    (cond ((or (c-in-struct-or-enum-p)
               (and in-array
                    (not (c-in-array-p))))
           (c-do-brace))
          ((save-excursion
             (if (and (> (point-max) (point))
                      (not (eq (following-char) ?\{)))
                 (forward-char))
             (c-backward-sws)
             (c-in-function-header-p))
           (when (c-in-function-arg-p)
             (delete-trailing-whitespace
              (line-beginning-position) (line-end-position))
             (c-align-fn-arg-p))
           (search-forward "{")
           (setq my-pair-inserted t)
           (c-forward-sws)
           (if (eq (following-char) ?\})
               (unless (bobp)
                 (my-backward-char 1))))
          ((or (c-in-incomplete-function-arg-p)
               (c-after-incomplete-function-arg-p))
           (delete-trailing-whitespace
            (line-beginning-position) (line-end-position))
           (when (c-after-incomplete-function-arg-p)
             (c-backward-sws)
             (unless (bobp)
               (my-backward-char 1)))
           (c-align-fn-arg-p)
           (search-forward ")" nil t)
           (c-do-brace))
          ((c-in-function-arg-p)
           (delete-trailing-whitespace
            (line-beginning-position) (line-end-position))
           (c-align-fn-arg-p)
           (unless (eq (save-excursion
                         (c-end-of-statement)
                         (preceding-char)) ?\;)
             (c-end-of-statement)
             (c-do-brace)))
          (t
           (insert "{")
           (under-score-to-space 1)))))

(defun c-dwim-with-return ()
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
           (member 'font-lock-type-face (text-properties-at (point))))
         (insert "\n")
         (save-excursion
           (c-backward-sws)
           (c-backward-token-2)
           (unless (eobp)
             (forward-char 1))
           (replace-token-at-point "upcamel")))
        (t
         (insert "\n"))))

(defun c-dwim-with-asterisk ()
  (under-score-to-space 1)
  (cond ((and (eq (save-excursion
                    (c-beginning-of-expression)
                    (point))
                  (save-excursion
                    (c-backward-sws)
                    (my-backward-char)
                    (c-backward-token-2)
                    (point)))
              (string-match-p "._." (save-excursion
                                      (c-backward-sws)
                                      (backward-char)
                                      (c-backward-token-2)
                                      (forward-char)
                                      (c-token-at-point))))
         (save-excursion
           (c-backward-sws)
           (my-backward-char)
           (c-backward-token-2)
           (replace-token-at-point "upcamel"))
         (if (eq (preceding-char) ?\ )
             (delete-char -1)))
        ((save-excursion
           (unless (bobp)
             (my-backward-char 1))
           (c-backward-token-2)
           (unless (eobp)
             (forward-char 1))
           (if (member 'font-lock-type-face (text-properties-at (point)))
               (replace-token-at-point "upcamel"))))
        nil))

(defun c-do-common-defun ()
  (if change-not-last
      nil
    (c-do-common-defun-defuns)))

(defun c-do-common-defun-defuns ()
  (save-excursion
    (my-backward-char 3)
    (if (and (c-token-at-point)
             (member (c-token-at-point) '("null" "True" "False")))
        (replace-token-at-point "upsnake")))
  (cond ((string-match-p "^g_application_flags_.*"
                         (save-excursion
                           (my-backward-char 1)
                           (c-backward-token-2)
                           (my-backward-char -1)
                           (if (not (c-token-at-point))
                               ""
                             (c-token-at-point))))
         (save-excursion
           (my-backward-char 3)
           (replace-token-at-point "upsnake"))))
  nil)

(defun c-dwim-with-paren-close ()
  (under-score-to-space 1)
  (c-do-common-defun)
  )

(defun c-dwim-with-paren-open ()
  (under-score-to-space 1)
  (c-do-common-defun)
  (cond ((member 'font-lock-variable-name-face
                 (save-excursion
                   (my-backward-char 3)
                   (text-properties-at (point))))
         (save-excursion
           (my-backward-char 1)
           (c-backward-token-2)
           (c-backward-sws)
           (while (string= (c-token-at-point) "*")
             (backward-char 1))
           (c-backward-token-2)
           (my-backward-char -1)
           (replace-token-at-point "upcamel")))
        (t
         (save-excursion
           (my-backward-char 1)
           (unless (eq (preceding-char) ?\ )
             (insert-char ?\ ))))))

(defun c-dwim-with-dot ()
  (let ((changed nil))
    (cond ((save-excursion
             (c-backward-sws)
             (my-backward-char 1)
             (and (eq (preceding-char) ?\.)
                  (not (point-in-comment-p))
                  (not (point-in-string-p))
                  (progn (my-backward-char 1)
                         (not (eq (preceding-char) ?\.)))))
           (my-zap-to-char -2 ?\.)
           (insert "->")
           (setq changed t))
          ((save-excursion
             (my-backward-char 1)
             (c-backward-sws)
             (memq (preceding-char) '(?\, ?\( ?\=)))
           (delete-char -1)
           (insert "&"))
          ((save-excursion
             (my-backward-char)
             (eq (preceding-char) ?\&))
           (delete-char -2)
           (insert "..")))
    ))

(defun c-dwim-with-> ()
  (under-score-to-space 1)
  (let ((buffer-undo-list t))
    (cond ((and (c-in-header-fname-p)
                (eq (following-char) ?\>))
           (backward-delete-char -1))
          ((and (not (c-in-header-fname-p))
                (not (point-in-string-p))
                (string-match-p "[a-zA-Z0-9]" (char-to-string (char-before (1- (point))))))
           (save-excursion
             (my-backward-char 1)
             (insert-space))
           (if (eq (following-char) ?\ )
               (my-backward-char -1)
             (insert-space)))
          )))

(defun c-dwim-with-quote ()
  "dwim with \'"
  (cond ((and (point-in-string-p)
              (may-not-be-char))
         (save-excursion
           ;; replace the previous token with upcase
           ;; FIXME: token is assumed to be built with atleast 3 chars
           (my-backward-char 3)
           (replace-token-at-point "upsnake"))
         ;; Delete the quote inserted with `self-insert-command'
         (delete-char -1))))

(defun c-dwim-with-context ())

(defun dwim-more-c-mode ()
  "Do What I Mean where ever possible

This function is mostly hooked with `self-insert-command'"
  (interactive)
  (let ((char-at-point (preceding-char))
        (char-before-point (char-before (1- (point)))))
    (cond ((eq last-command-event ?\n)
           (delete-backward-char 1)
           (c-dwim-with-return))
          ((eq last-command-event ?\!)
           (dwim-with-!))
          ((eq last-command-event ?\>)
           (c-dwim-with->))
          ((eq last-command-event ?\.)
           (c-dwim-with-dot))
          ((eq char-at-point ?\{)
           (c-dwim-with-brace))
          ((eq last-command-event ?\*)
           (c-dwim-with-asterisk))
          ((eq last-command-event ?\,)
           (dwim-with-comma))
          ((eq char-at-point ?\ )
           (delete-char -1)
           (c-dwim-with-space))
          ((eq char-at-point ?\))
           (c-dwim-with-paren-close))
          ((eq char-at-point ?\()
           (c-dwim-with-paren-open))
          ((eq char-at-point ?\')
           (c-dwim-with-quote))
          ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
           (under-score-to-space 1)
           (c-dwim-with-context))
          )
    (setq second-last-point last-command-event)))

(provide 'init-c-defun)
