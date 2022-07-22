(use-package org :defer t :pin gnu
  :mode (("\\.org$" . org-mode))
  :bind
  (:map org-mode-map (("C-j" . helm-mini) ("<C-tab>" . tab-bar-switch-to-next-tab) ))
  :config

  (defhydra hydra-org (:color blue)
    "
    [_o_]   metaright   [_u_]   metaleft  [_n_]   metaup  [_p_]   metadown
    [_C-o_] shiftright  [_C-u_] shiftleft [_C-n_] shiftup [_C-p_] shiftdown
    [_e_]   edit source [_s_] exit source edit buffer [_E_]   babel execute
    [_c_]   yas helm expand
      "
    ( "o" org-metaright nil)
    ( "u" org-metaleft nil)
    ( "p" org-metaup nil)
    ( "n" org-metadown nil)
    ( "C-o" org-shiftright nil)
    ( "C-u" org-shiftleft nil)
    ( "C-p" org-shiftup nil)
    ( "C-n" org-shiftdown nil)
    ( "e" org-edit-src-code nil)
    ( "E" org-babel-execute-src-block nil)
    ( "s" org-edit-src-exit nil)
    ( "c" helm-yas-complete nil))

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
