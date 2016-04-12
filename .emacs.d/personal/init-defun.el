;; Some personal functions

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

(defun end-statement ()
  (interactive)
  (end-of-line)
  (delete-trailing-whitespace
   (line-beginning-position) (line-end-position))
  (unless (eq (char-before) ?\;)
    (c-indent-line)
    (insert ";")))

(defun insert-semi-colon ()
  (interactive)
  (insert ";"))

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
  (when (and (= (save-excursion
                  (goto-char (1- (point)))
                  (c-beginning-of-current-token) (point))
                (save-excursion
                  (c-beginning-of-statement-1) (point)))))
  (insert " "))


(defun dwim-more ()
  (interactive)
  (let ((char-at-point (char-before (point))))
    (cond ((eq char-at-point ? )
           (delete-char -1)
           (check-or-insert)))))


(provide 'init-defun)
