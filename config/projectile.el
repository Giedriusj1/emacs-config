(defhydra hydra-projectile (:pre
                            (projectile-global-mode t)
                            :color blue)
  "
[_q_] invalidate cache [_p_] projects
[_j_] helm projectile  [_d_] dired projectile root
[_g_]rep [_a_]ck [_r_] projectile-ripgrep [_R_] helm-projectile-ripgrep
" ("p" helm-projectile-projects nil)
  ("q" projectile-invalidate-cache nil)
  ("j" helm-projectile nil)
  ("d" projectile-dired nil)
  ("g" helm-projectile-grep nil)
  ("a" helm-projectile-ack nil)
  ("r" projectile-ripgrep nil)
  ("R" helm-projectile-rg nil))

(use-package helm-projectile :defer 5
  :bind (:map tab-map
              ("p" . hydra-projectile/body))
  :diminish projectile-mode

  :config
  (projectile-global-mode t)

  (remove-hook 'find-file-hook #'projectile-find-file-hook-function)

  ;; Make projectile use external tools for file indexing.
  ;; If this breaks revert to 'native for more reliability.
  (setq projectile-indexing-method 'alien)

  (defcustom g/helm-source-projectile-projects-actions
    (helm-make-actions "Open Dired in project's directory `C-d'" #'dired "Switch to project"
                       (lambda (project)
                         (let ((projectile-completion-system 'helm))
                           (projectile-switch-project-by-name
                            project)))
                       "Open project root in vc-dir or magit `M-g'" #'helm-projectile-vc
                       "Switch to Eshell `M-e'" #'helm-projectile-switch-to-eshell
                       "Grep in projects `C-s'" #'helm-projectile-grep
                       "Compile project `M-c'. With C-u, new compile command"
                       #'helm-projectile-compile-project "Remove project(s) from project list `M-D'"
                       #'helm-projectile-remove-known-project)
    "Actions for `helm-source-projectile-projects'."
    :group 'helm-projectile
    :type '(alist :key-type string
                  :value-type function))

  (defvar g/helm-source-projectile-projects
    (helm-build-sync-source "Projectile projects"
      :candidates (lambda ()
                    (with-helm-current-buffer projectile-known-projects))
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action 'g/helm-source-projectile-projects-actions)
    "Helm source for known projectile projects.")

  (defun helm-projectile-projects ()
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(g/helm-source-projectile-projects)
            :buffer "*helm projectile projects*"
            :truncate-lines helm-projectile-truncate-lines)))

  (customize-set-variable 'helm-projectile-sources-list '(helm-source-projectile-buffers-list
                                                          helm-source-projectile-files-list)))

(use-package projectile-git-autofetch :defer t
  :config
  (projectile-git-autofetch-mode))

(use-package projectile-ripgrep :defer t)
