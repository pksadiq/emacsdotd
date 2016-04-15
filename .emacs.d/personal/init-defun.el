;; Some personal functions

;; Packages required for the functions below
(require 'subr-x)

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun last-char-space-p ()
  (member (preceding-char) '(?\  ?\t ?\n)))

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
  (let ((token nil)
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

(defun set-mode-comment ()
  "Generate mode comment. Eg: in C, /* mode: c; indent-tabs-mode ... */"
  (let ((mode-comment "")
	(mode-now ""))
    (setq mode-now (replace-regexp-in-string
		    "-mode" "" (symbol-name major-mode)))
    (setq mode-comment (concat mode-comment
			       "-*- mode: " mode-now
			       "; indent-tabs-mode: "
			       (symbol-name indent-tabs-mode)
			       mode-comment
			       "; tab-width: "
			       (number-to-string tab-width)))
    (if (string= mode-now "c")
	(setq mode-comment (concat mode-comment
				   "; c-basic-offset: "
				   (number-to-string c-basic-offset))))
    (setq mode-comment (concat mode-comment " -*-"))
    (comment-region (point)
		    (progn (insert mode-comment)
			   (point)))))

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

(defun str-to-style (style str)
  "Convert STR to corresponding STYLE.

STR can be of the form CamelCase, snake_case, space text, or lisp-case

STYLE can be 'upcamel', 'lisp'. any other STYLE defaults to 'snake'"
  (let* ((str (str-to-snake-style str))
         (out str))
    (cond ((string= style "upcamel")
           (setq str (upcase-initials str))
           (setq out (replace-regexp-in-string "_\\(.\\)" "\\1" str)))
          ((string= style "lisp")
           (setq out (replace-regexp-in-string "_" "-" str))))
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

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p)
       'kill-region
     'backward-kill-word)))

(defun my-term ()
  (interactive)
  (ansi-term (getenv "SHELL")))

(defun kill-buffer-on-exit (process event)
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))

(defun kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-buffer-on-exit))

(defun erc-get-last-nick ()
  (interactive)
  (let ((last-nick "")
        (users (erc-sort-channel-users-by-activity
                (erc-get-channel-user-list))))
    (cond ((= (length users) 0)
           (setq nick-n -1))
          ((and (eq (point) (point-max))
                (get-text-property (1- (point)) 'read-only))
           (setq nick-n 0))
          ((>= (1+ nick-n) (length users))
           (setq nick-n 0))
          ((and (not (get-text-property (point) 'read-only))
                (or (eq (char-before) ?:)
                    (eq (char-before (1- (point))) ?:))
                (save-excursion
                  (goto-char (1- (point)))
                  (search-backward " ")
                  (get-text-property (point) 'read-only)))
           (setq nick-n (1+ nick-n)))
          (t
           (setq nick-n -1)))
    (when (>= nick-n 0)
      (erc-kill-input)
      (setq last-nick (erc-server-user-nickname
                       (car (nth nick-n users))))
      (insert last-nick ": "))))

(defun check-or-insert ()
  (interactive)
  (cond ((eq (preceding-char) ?\0)
         (insert " "))
        ((string-match-p "[^a-zA-Z0-9_]" (char-to-string (preceding-char)))
         (insert " "))
        ((or (point-in-comment-p)
             (point-in-string-p))
         (insert " "))
        ((and (save-excursion
                (re-search-backward "[ ,;()]\\|^")
                (forward-char)
                (looking-at-p c-keywords-regexp)))
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

(defun dwim-more ()
  (interactive)
  (let ((char-at-point (preceding-char)))
    (cond ((eq char-at-point ?\ )
           (delete-char -1)
           (check-or-insert)
           (message "bad"))
  ;;         ((string-match-p "[^a-zA-Z0-9_]"(char-to-string (preceding-char)))
  ;;          (under-score-to-space 1))
          )))

(provide 'init-defun)
