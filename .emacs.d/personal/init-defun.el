;; Some personal functions

(defun my-make-compile ()
  (interactive)
  (my-read-only-mode)
  (compile "make"))

(defun my-make-run ()
  (interactive)
  (my-read-only-mode)
  (compile "make run"))

(defun my-read-only-mode ()
  (if (and (> (buffer-size) 0)
           (not (eq major-mode 'emacs-lisp-mode)))
      (read-only-mode 1))
  (if buffer-read-only
      (less-evil-mode 1)
    (less-evil-mode -1)))

(defun force-save-buffer ()
  (interactive)
  (let ((read-only-enabled buffer-read-only))
    (read-only-mode -1)
    (save-buffer)
    (if read-only-enabled
        (read-only-mode 1))))

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


(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p)
       'kill-region
     'backward-kill-word)))

(defun my-zap-to-char (arg char)
  "zap-to-char version for non-interactive use,
won't modify kill ring"
  (delete-region
   (point)
   (progn
     (search-forward (char-to-string char) nil nil arg)
     (point))))

(defun my-git-add ()
  (interactive)
  (call-process "git" nil nil nil "add" (buffer-file-name))
  ;; FIXME: There isn't any easy way to ensure if file is really added
  (message "File Added"))

(defun my-term ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
         (previous-buffer))
        ((get-buffer "*ansi-term*")
         (switch-to-buffer "*ansi-term*"))
        (t
         (ansi-term (getenv "SHELL")))))

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

(defun no-more-yawn ()
  (if (and (eq (preceding-char) ?\()
           (map (char-before (1- (point))) '(?\; ?\:))
           (eq (following-char) ?\)))
      (delete-char 1)))

(provide 'init-defun)
