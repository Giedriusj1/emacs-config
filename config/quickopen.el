;;; -*- lexical-binding: t -*-

(on-linux
 (defun g/helm-projectile-grep-notes (dir)
   ;; In case we use this function before helm-projectile was loaded:
   (require 'helm-projectile)
   (require 'helm-mode)

   (let* ((default-directory dir)
	      (helm-ff-default-directory default-directory)
	      (helm-grep-in-recurse t)
	      (helm-grep-ignored-files (cl-union (cl-union (projectile-ignored-files-rel)  grep-find-ignored-files)
                                             '("*.doc" "*.ovpn" "*.pcap" "*.pcapng" "*.png" "*.pem" )))
	      (helm-grep-ignored-directories
	       (cl-union (mapcar 'directory-file-name (projectile-ignored-directories-rel))
		             grep-find-ignored-directories))
	      (helm-grep-default-command "grep -a -r %e -n%cH -e %p %f .")
	      (helm-grep-default-recurse-command helm-grep-default-command))
     (setq helm-source-grep
	       (helm-build-async-source
	           (capitalize (helm-grep-command t))
	         :header-name  (lambda (_name) "grep" )
	         :candidates-process 'helm-grep-collect-candidates
	         :filter-one-by-one 'helm-grep-filter-one-by-one
	         :candidate-number-limit 9999
	         :nohighlight t
	         ;; We need to specify keymap here and as :keymap arg [1]
	         ;; to make it available in further resuming.
	         :keymap helm-grep-map
	         :history 'helm-grep-history
	         :action (apply #'helm-make-actions helm-projectile-grep-or-ack-actions)
	         :persistent-action 'helm-grep-persistent-action
	         :persistent-help "Jump to line (`C-u' Record in mark ring)"
	         :requires-pattern 2))
     (helm
      :sources '(helm-source-grep
		         helm-source-projectile-buffers-list
		         helm-source-projectile-files-list)
      :input (when helm-projectile-set-input-automatically
	           (if (region-active-p)
		           (buffer-substring-no-properties (region-beginning) (region-end))
		         (thing-at-point 'symbol)))
      :default-directory default-directory
      :keymap helm-grep-map
      :history 'helm-grep-history
      :truncate-lines helm-grep-truncate-lines))))

(setq helm-projectile-fuzzy-match nil)


(pretty-hydra-define hydra-quickopen (:color blue)
  ("quickopen"
   (("t" (lambda ()
	   (interactive)
	   (find-file "~/private-sync/temp.org")) "~/private-sync/temp.org")
    ("c" (lambda ()
           (interactive)
           (find-file "~/.emacs.d/init.el")) "~/.emacs.d/init.el")
    ("l" (lambda ()
           (interactive)
           (progn (zygospore-toggle-delete-other-windows)
	          (dired "~/private-sync")
	          (helm-find-files-1 default-directory))) "dired ~/private-sync/")
    (";" (lambda ()
           (interactive)
           (g/helm-projectile-grep-notes "~/private-sync")) "grep notes"))))

(define-key tab-map (kbd ";") 'hydra-quickopen/body)
