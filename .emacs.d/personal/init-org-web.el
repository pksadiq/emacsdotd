;; My org-mode based web export

;; Motivations from:
;; http://nicolas-petton.fr/blog/blogging-with-org-mode.html
;; https://ogbe.net/blog/blogging_with_org.html

(require 'org)

(defvar my-org-base-dir "~/org/web/")
(defvar my-org-pub-dir "~/org/web/export/")

(setq org-publish-project-alist
      `(("org"
         :base-directory ,my-org-base-dir
         :base-extension "org"
         :publishing-directory ,my-org-pub-dir
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         )

        ("website" :components ("org"))))

(provide 'init-org-web)
