;; Packages required for the functions below
(require 'subr-x)

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun last-char-space-p ()
  (member (preceding-char) '(?\  ?\t ?\n)))

(defun c-re-search-backward-p (regexp)
  (save-excursion
    (re-search-backward regexp nil t)))

(defun c-is-function-before-p ()
  (save-excursion
    (c-backward-token-2)
    (forward-char 1)
    (if (eq (face-at-point) 'font-lock-function-name-face)
        t
      nil)))

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
                        (backward-char))
                      (c-is-function-before-p)))))

;; (defun c-end-of-defun-arg ()
;;   (let ((brace-count (nth 0 (syntax-ppss)))
;;         (my-point (point)))
;;     (while (and (not (eobp)) (nth 0 (syntax-ppss (point))))
;;           (forward-char)))))

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
    (if return-points?
        points
      token)))

(defun replace-token-at-point (style)
  (let ((token ""))
    (setq token (save-excursion
                  (forward-char)
                  (c-token-at-point)))
    (setq token (str-to-style token "upcamel"))
    (c-beginning-of-current-token)
    (delete-region (point) (save-excursion
                             (forward-char)
                             (c-end-of-current-token)
                             (point)))
    (insert token)
    (forward-char 2)))


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
           (setq out (replace-regexp-in-string "_\\(.\\)" "\\1" str)))
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
  (if (point-in-comment-p)
      (insert ";")
    (my-end-statement)))

(defun insert-semi-colon ()
  (interactive)
  (insert ";"))

(defun insert-space ()
  (interactive)
  (insert " "))

(defun check-or-insert ()
  (interactive)
  (cond ((eq (preceding-char) ?\0)
         (insert " "))
        ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
         (insert " "))
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

(defun dwim-with-brace ()
  (delete-backward-char 1)
  (cond ((eq (save-excursion
               (if (> (point-max) (point)) (forward-char))
               (c-where-wrt-brace-construct)) 'in-header)
         (search-forward "{"))
        ((c-in-function-arg-p)
         (search-forward ")")
         (unless (eq (following-char) ?\;)
           (insert "\n")
           (insert "{")
           ))
        (t
         (insert "{"))))

(defun dwim-with-context ()
  (cond ((and (eq (preceding-char) ?\*)
              (eq (save-excursion (c-beginning-of-statement-1) (point))
                  (save-excursion (backward-char)(c-backward-token-2) (point)))
              (string-match-p "._." (save-excursion
                                      (backward-char)
                                      (c-backward-token-2)
                                      (forward-char)
                                      (c-token-at-point))))
         (backward-char)
         (c-backward-token-2)
         (replace-token-at-point "upcamel"))))

(defun dwim-more ()
  ;; (interactive)
  ;; (if (< second-last-point 0)
  ;;     (setq last-last-point nil))
  ;; (let ((char-at-point (preceding-char)))
  ;;   (cond ((eq char-at-point ?\ )
  ;;          (delete-char -1)
  ;;          (check-or-insert))
  ;;         ((eq char-at-point ?\{)
  ;;          (under-score-to-space 1)
  ;;          (dwim-with-brace))
  ;;         ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
  ;;          (under-score-to-space 1)
  ;;          (dwim-with-context)))
          )

(provide 'init-c-defun)
