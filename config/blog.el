;;; -*- lexical-binding: t -*-

(on-linux
 (use-package htmlize :defer t)
 ;; default one would pick source colours from my current theme...
 (setq org-html-htmlize-output-type 'css)

 (setq org-publish-project-alist
       '(("org-blog"
	      :base-directory "~/private-sync/blog/"
	      :base-extension "org"
	      :publishing-directory "~/public_html/"
	      :recursive t
	      :publishing-function org-html-publish-to-html
	      :headline-levels 4             ; Just the default for this project.
	      :auto-preamble t )
	     ("org-static"
	      :base-directory "~/private-sync/blog/"
	      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	      :publishing-directory "~/public_html/"
	      :recursive t
	      :publishing-function org-publish-attachment)
         ))

 (defun g/org-publish-all-force ()
     (interactive)
     (org-publish-all t)))
