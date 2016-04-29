;; Configurations for the `ido' interactive do

(require 'ido)

;; Enable ido where ever possible
(setq ido-everywhere t)
(ido-mode 1)

;; Guess file at point on `ido-find-file'
(setq ido-use-filename-at-point 'guess)

;; Don't prompt if buffer doesn't exist
(setq ido-create-new-buffer 'always)
;; The above one seems not working, This was introduced in 23.1
(setq confirm-nonexistent-file-or-buffer nil)

;; Ignore some buffers
(setq
 ido-ignore-buffers (append ido-ignore-buffers
                            '( "^\*Mess" "^\*scratch" ".*Completion"
                               "\*Buffer")))

;; Ignore uninteresting dirs
(setq
 ido-ignore-directories (append ido-ignore-directories
                                '("m4" "build-aux" "autom4te\.cache"
                                  "\.git")))
;; Ignore some files
(setq
 ido-ignore-files (append ido-ignore-files
                          '("[.]m4" "[.]in" "[.]am" "^Makefile" "^config"
                            "COPYING" "AUTHORS" "INSTALL" "README"
                            "NEWS" "autogen[.]sh" "ChangeLog" "libtool"
                            "THANKS" "TODO" "BUGS" "[.]git" "GTAGS"
                            "GRTAGS" "GPATH"
                            "build-aux/" "^m4/$" "^po/$" "[.]out$"
                            "[.]mp4$" "[.]jpg$" "[.]png$" "[.]pdf$"
                            "[.]ps$" "[.]webm$" "[.]svg$" "^#")))

(provide 'init-ido)
