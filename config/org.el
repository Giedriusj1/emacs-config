(use-package org :defer 5 :ensure nil
  :mode (("\\.org$" . org-mode)
	 ("\\.org.gpg$" . org-mode)
	 ("\\.notes$" . org-mode)
	 ("\\.ref$" . org-mode)
	 ("\\.ref.gpg$" . org-mode))
  :diminish org-indent-mode
  :config
  (pretty-hydra-define hydra-org (:color blue)
    ("org move"
     (( "o" org-metaright "org-metaright")
      ( "u" org-metaleft "org-metaleft")
      ( "p" org-metaup "org-metaup")
      ( "n" org-metadown "org-metadown"))
     "org shift"
     (( "C-o" org-shiftright "org-shiftright")
      ( "C-u" org-shiftleft "org-shiftleft")
      ( "t" org-todo "toggle todo"))
     ""
     (( "c" helm-yas-complete "complate")
      ( "e" org-edit-src-code "source"))))

  (setq org-directory "~/private-sync/notes")
  (setq org-default-notes-file "~/private-sync/notes/notes.org")
  (setq org-src-preserve-indentation t)
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-export-with-toc nil)
  (setq org-hierarchical-todo-statistics nil)
  (setq org-imenu-depth 5)
  (customize-set-variable 'helm-split-window-default-side 'right)

  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-initialize-agent))))
