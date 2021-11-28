(use-package diminish)

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode t))

(cua-mode 1)

(setq cua-prefix-override-inhibit-delay 0.01)

(bind-key "C-f" 'cua-exchange-point-and-mark)

(bind-key* "C-v" 'yank)

(use-package recentf :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250))

(use-package shell-here :defer t
  :bind* (( "C-`" . shell-here)))

(setq windmove-wrap-around t)

(use-package zygospore :ensure nil
  :bind* (("C-1" . 'window-swap-states)
          ("C-2" . 'windmove-up)
          ("C-3" . 'windmove-right)
          :map control-semi-map
          ("C-1" . zygospore-toggle-delete-other-windows)))

(use-package posframe)



(define-key tab-map (kbd "o") 'hydra-search-helper/body)

(defhydra hydra-search-helper
  (:color blue)
  "
[_q_] update tags        [_o_] find gtag
[_c_] create gtag        [_p_] hydra-lsp
"
  ("q" ggtags-update-tags nil)
  ("c" ggtags-create-tags nil)
  ("o" ggtags-find-tag-dwim nil)
  ("p" hydra-lsp/body nil))

(define-prefix-command 'mc-map)
(use-package multiple-cursors :defer t
  :bind
  (:map tab-map(("l" . mc-map)))
  (:map mc-map (("l" . mc/edit-lines))))

(use-package helm-projectile :defer t
  :bind (:map tab-map
              ("p" . hydra-projectile/body))
  :diminish projectile-mode
  :init
  (projectile-global-mode t)

  :config
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
                                                          helm-source-projectile-files-list))

  (defhydra hydra-projectile
    (:color blue)
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
    ("R" helm-projectile-rg nil)))

(use-package projectile-git-autofetch :defer t
  :config
  (projectile-git-autofetch-mode))

(use-package dired-x :ensure nil :defer t)
(use-package dired-subtree :defer t)
(use-package dired-extension :ensure nil)
(use-package dired-toggle-sudo :ensure nil)

(setq dired-dwim-target t)

(add-hook 'dired-mode-hook
	      (lambda ()
	        (dired-hide-details-mode 1)
            (dired-omit-mode)))

(define-key dired-mode-map (kbd "l") 'dired-up-directory)
(define-key dired-mode-map (kbd "r") 'dired-do-redisplay)
(define-key dired-mode-map (kbd "C-i") (lambda ()
                                         (interactive)
                                         (dired-subtree-cycle)
                                         (dired-omit-mode)))

(setq dired-listing-switches "-alFh")

(when (memq system-type '(gnu gnu/linux))
  (setq dired-listing-switches
        (concat dired-listing-switches " --group-directories-first -v")))

(defun open-in-external-app ()
  (interactive)
  (let ((fileList (cond ((string-equal major-mode "dired-mode")
                         (dired-get-marked-files))
                        (t (list (buffer-file-name))))))
    (cond ((string-equal system-type "windows-nt")
           (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t))) fileList))
          ((string-equal system-type "darwin")
           (mapc (lambda (path) (shell-command (format "open \"%s\"" path))) fileList))
          ((string-equal system-type "gnu/linux")
           (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path))) fileList)))))

(use-package auto-sudoedit
  :diminish auto-sudoedit-mode
  :config (auto-sudoedit-mode 1))

(use-package org :pin gnu
  :defer t
  :mode (("\\.org$" . org-mode))
  :bind
  (:map org-mode-map (("C-j" . helm-mini) ("<C-tab>" . eyebrowse-next) ))
  :config
  (eldoc-mode -1)

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
  (customize-set-variable 'helm-split-window-default-side 'right))

(defun private-synch-fn ()
  (interactive)
  (let* ((default-directory "~/private-sync"))
	(message "pulling private-sync repo")
	(start-process "proc-git-pull" "notes-sync-output" "git" "pull")))

;; Run the above every 2 mins (if we are idle)
(run-with-idle-timer (* 60 2) t 'private-synch-fn)

(use-package git-auto-commit-mode :defer t)

(defun turn-on-auto-commit-hook ()
  (cond ((string-match (concat "^" (expand-file-name "~/private-sync")) buffer-file-name)
         (progn
           (git-auto-commit-mode 1)
           (setq gac-automatically-push-p t)))))

(add-hook 'find-file-hook 'turn-on-auto-commit-hook)

;; todo (Sat Jul 18 16:34:44 2020) eldoc was throwing non stop errors in org-mode.
;; Maybe we want to enable it back at some point *shrug*
(global-eldoc-mode -1)

;; set to t to investigate crashes
(setq debug-on-error t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq column-number-mode t)
(setq history-length 25)
(setq select-enable-clipboard t) ;; Merge OS and Emacs' clipboards

(setq auto-window-vscroll nil)   ;; Gives us better line scrolling performance

;; We'll ask emacs to put all customizations made via it's customize package in a
;; separate file... so we can ignore it later :)
(setq custom-file (concat user-emacs-directory "/custom--ignored.el"))

(blink-cursor-mode -1)

(use-package auto-highlight-symbol
  :init (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(delete-selection-mode 1)
(show-paren-mode t)

;; Make the interface a bit more snappy
(setq idle-update-delay 0.05)

(customize-set-variable 'electric-pair-mode t)
(customize-set-variable 'bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks" )

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(use-package google-this :defer t)

(setq ring-bell-function 'ignore)

(use-package mvn :defer t
  :config
  (setq compilation-scroll-output t)
  (defun mvn-integration-test ()
    (interactive)
    (mvn "integration-test")))

(use-package ggtags :defer t
  :config
  ;; This should prevent Emacs from asking "Keep current list of tags tables also?"
  (setq tags-add-tables nil)

  ;; Prevent ggtags mode from displaying project name in mode line.
  ;; Projectile already displays this information.
  (setq ggtags-mode-line-project-name nil))

(use-package projectile-ripgrep :defer t)
(use-package helm-rg :defer t)

(use-package dumb-jump :defer t
  :config
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-force-searcher 'rg))

(use-package lsp-ui :defer t)
(use-package helm-lsp :defer t)

(pretty-hydra-define hydra-lsp (:foreign-keys warn :title "LSP" :quit-key "q" :color blue)
  ("Buffer"
   (("f" lsp-format-buffer "format")
    ("menu" lsp-ui-imenu "imenu")
    ("c" ace-delete-other-windows "maximize")
    ("u" helm-lsp-code-actions "execute action"))
   "Server"
   (("M-r" move-border-left "restart")
    ("S" lsp-shutdown-workspace "restart")
    ("M-s" lsp-describe-session "shutdown"))
   "Symbol"
   (("d" lsp-find-declaration "declaration")
    ("D" lsp-ui-peek-find-definitions "definition")
    ("R" lsp-ui-peek-find-references "references")
    ("i" lsp-ui-peek-find-implementation "implementation")
    ("o" lsp-describe-thing-at-point "documentation")
    ("t" lsp-find-type-definition "type")
    ("r" lsp-rename "rename")
    ("s" lsp-signature-help "signature"))))

(use-package yasnippet
  ;; :defer t
  :ensure yasnippet-snippets
  :ensure yasnippet-classic-snippets
  :ensure helm-c-yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

(use-package  company-box :defer t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors
        '((company-lsp .
                       ( :selected (:background "orange"  :foreground "black")))
          (company-capf .
			            ( :selected (:background "orange" :foreground "black"))))))

(use-package company :defer t
  :bind ( :map company-active-map
          (("C-n" . company-select-next)
           ("C-p" . company-select-previous))
          :map control-semi-map
          (("n" . company-complete)
           ("C-n" . dabbrev-expand)))
  :diminish company-mode
  :config
  (require 'color)

  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

  (global-company-mode t)

  (setq company-tooltip-limit 25))

(define-key tab-map (kbd "k") 'hydra-gdb-helper/body)

(defhydra hydra-gdb-helper (:color blue)

  "
_h_  restore-windows  |  _j_  next       _b_  set break     _p_ print
_m_  many-windows     |  _k_  step       _r_  remove break
                    |  _l_  up
                    |  _c_  cont
"
  ( "h" gdb-restore-windows nil)
  ( "m" gdb-many-windows nil)
  ( "j" gud-next nil)
  ( "k" gud-step nil)
  ( "l" gud-up nil)
  ( "b" gud-break nil)
  ( "r" gud-remove nil)
  ( "c" gud-cont nil)
  ( "p" gud-print nil))

(use-package dap-mode :defer t)

(semantic-mode 1) ;; global mode

;; This effectively disables idle reparsing for all files
(setq semantic-idle-scheduler-max-buffer-size 1)

;; We don't care about saving db when exiting emacs
(remove-hook 'kill-emacs-hook #'semanticdb-kill-emacs-hook)

(defun ds () t)
(add-hook 'semantic-inhibit-functions  #'ds)

(use-package clang-format :defer t
  :config
  ;; The following somewhat resembles Resilient's coding style
  (setq clang-format-style "{BasedOnStyle: google, ColumnLimit: 100, IndentWidth: 3, BreakBeforeBraces: Stroustrup}"))

(use-package elisp-format :defer t)

(define-key tab-map (kbd "i")
	        (lambda ()
	          (interactive)
	          (cond ((or ( string= "c++-mode" major-mode)
			             ( string= "c-mode" major-mode))
		             (if (use-region-p)
			             (clang-format-region (region-beginning)
					                          (region-end))
		               (clang-format-region (point)
					                        (point))))
		            (( string= "emacs-lisp-mode" major-mode)
		             (elisp-format-region))
		            (( string= "rustic-mode" major-mode)
		             (rustic-format-buffer))
		            (( string= "json-mode" major-mode)
		             (json-reformat-region))
		            (t (message "Argh...don't know how to format in this mode :(")))))

(defun get-prefered-indentation-size ()
  (cond
   ;; EAS expects 3
   ((string-match  "^c:/workspace/src" buffer-file-name) 3)
   ;; smartblock-paren uses 2
   ((string-match "^c:/workspace/resilient/smartblock-parent" buffer-file-name) 2)
   ;; Everything else gets a sane default of 4
   (t 4)))

(defun set-sane-indentation ()
  (let ((identation-size (get-prefered-indentation-size)))
    (progn
      (setq c-basic-offset identation-size c-default-style "linux")
      (setq tab-width identation-size indent-tabs-mode nil))))

(add-hook 'find-file-hook 'set-sane-indentation)

(defhydra hydra-c (:color blue)
  ( "c" helm-yas-complete "helm yas complete"))

(add-hook 'c-mode-common-hook
          (lambda()
	        ;; Only use lsp on non Win machines, since thing can be quite slow there...
	        (cond ((not (string-equal system-type "windows-nt"))
		           (lsp)))
            ;; Use C++ style comments
            (setq comment-start "//" comment-end  "")))

(use-package ob-rust :defer t)

(use-package toml-mode :defer t)

(use-package rustic :defer t
  :config
  (defhydra hydra-rust (:color blue)
    ("c" helm-yas-complete "yas complete")
    ("C" rustic-cargo-clean "cargo clean")
    ("r" rustic-cargo-run "cargo run")
    ("b" rustic-cargo-build "cargo build")
    ("SPC" rustic-cargo-check "cargo check")))

(use-package go-mode :defer t
  :hook (go-mode . lsp-deferred)
  :config
  (defhydra hydra-go (:color blue)
    ( "c" helm-yas-complete "yas complete"))
  (setq lsp-gopls-codelens nil)
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)
            (setq tab-width 4)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(defhydra hydra-python (:color blue)
  ( "c" helm-yas-complete "helm yas complete"))

(add-hook 'scheme-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

(defhydra hydra-emacs-lisp (:color blue)
  ( "j" eval-buffer "eval buffer")
  ( "k" eval-last-sexp "eval-last-sexp")
  ( "c" helm-yas-complete "yas complete"))

;; lsp-java pulls the whole treemacs for itself...
;; (use-package lsp-java :defer t)

(use-package typescript-mode :defer t)

(use-package powershell :defer t)

(use-package g-adl-mode :ensure nil)

(use-package graphql-mode :defer t)

(use-package yaml-mode :defer t)

(setq nxml-child-indent 4 nxml-attribute-indent 4)

(defun reformat-xml ()
  (interactive)
  ;;todo: this only works in xml-mode, we should spit out an error if we are not

  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(use-package ldap-mode :ensure nil :defer t)

(use-package json-mode :defer t)

(use-package dockerfile-mode :defer t)





(setq auto-mode-alist
      '(("[Mm]ake[Ff]ile\\'" . makefile-mode)
        ("\\.mak\\'" . makefile-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.notes$" . org-mode)
        ("\\.org$" . org-mode)
        ("\\.org.gpg$" . org-mode)
        ("\\.pdf\\'" . doc-view-mode)
        ("\\.ref$" . org-mode)
        ("\\.ref.gpg$" . org-mode)
        ("\\.xml\\'" . xml-mode)
        ("\\.pom\\'" . xml-mode)
        ("\\.ldif\\'" . ldif-mode)
        ("\\.toml\\'" . toml-mode)
        ("\\.json\\'" . json-mode)
        ("\\.sql\\'" . sql-mode)
        ("[Dd]ockerfile\\'" . dockerfile-mode)

        ;;programming modes
        ("\\.ps1\\'" . powershell-mode)
        ("\\.bat\\'" . bat-mode)
        ("\\.c\\'" . c-mode)
        ("\\.cc\\'" . c-mode)
        ("\\.cmd\\'" . bat-mode)
        ("\\.cpp\\'" . c++-mode)
        ("\\.el\\'" . emacs-lisp-mode)
        ("\\.el.gz\\'" . emacs-lisp-mode)
        ("\\.elc\\'" . elisp-byte-code-mode)
        ("\\.h\\'" . c++-mode)
        ("\\.hh\\'" . c++-mode)
        ("\\.hpp\\'" . c++-mode)
        ("\\.rs\\'" . rustic-mode)
        ("\\.go\\'" . go-mode)
        ("\\.ts\\'" . typescript-mode)
        ("\\.zig\\'" . zig-mode)
        ("\\.java\\'" . java-mode)
        ("\\.js\\'" . javascript-mode)
        ("\\.mc\\'" . c++-mode)
        ("\\.pm\\'" . perl-mode)
        ("\\.py\\'" . python-mode)
        ("\\.scm\\'" . scheme-mode)
        ("\\.sh\\'" . sh-mode)
        ("bashrc\\'" . sh-mode)
        ("\\.yml\\'" . yaml-mode)
        ("\\.graphql\\'" . graphql-mode)
        ("\\.s\\'" . asm-mode)
        ("\\.S\\'" . asm-mode)
        ("\\.adl\\'" . adl-mode)))

(use-package asm-mode :defer t :ensure nil
  :bind (:map asm-mode-map
              ("C-j" . helm-mini)))

(use-package view :defer t :pin manual
  :bind (:map view-mode-map
              ("C-j" . helm-mini)))

(defun g/helm-semantic-or-imenu (arg)
  (interactive "P")
  (remove-hook 'semantic-inhibit-functions #'ds)
  (semantic-new-buffer-fcn)
  (helm-semantic-or-imenu arg)
  (add-hook 'semantic-inhibit-functions  #'ds))

(use-package helm :defer t
  :bind
  (("C-j" . helm-mini))
  (:map control-semi-map
        (( "C-s" . g/helm-semantic-or-imenu)
         ( "l" . helm-M-x)
         ( "r" . helm-mark-ring)
         ( "C-r" . helm-global-mark-ring)
         ( "b" . helm-resume)
         ( "C-b" . helm-resume)))

  (:map lisp-interaction-mode-map (("C-j" . helm-mini)))
  :config
  (setq helm-candidate-number-limit 500)
  (setq helm-buffer-max-length 60)

  (custom-set-faces '(helm-rg-file-match-face ((t (:foreground "purple" :background "black" :weight bold))))))

(use-package swiper :defer t)
(use-package swiper-helm :defer t
  :bind (:map control-semi-map (())
              ("o" . swiper-helm)
              ("C-;" . swiper-helm)))

(use-package helm-swoop :defer t
  :bind (:map control-semi-map
              (("C-m" . helm-swoop)
               ("m" . helm-multi-swoop-all))))

(defun swift-up(&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-down-line)
    (previous-line)))

(defun swift-down(&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-up-line)
    (next-line)))

(define-key control-semi-map (kbd "C-f") 'toggle-swift-mode)

(defvar swift-command-map
  (let ((map (make-sparse-keymap)))
    ;; movement
    (define-key map (kbd "i") (lambda ()
                                (interactive)
                                (swift-up 2)))

    (define-key map (kbd "k") (lambda ()
                                (interactive)
                                (swift-down 2)))

    (define-key map (kbd "o") 'swift-up)
    (define-key map (kbd "l") 'swift-down)

    (define-key map (kbd "p") 'beginning-of-defun)
    (define-key map (kbd "n") 'end-of-defun)

    (define-key map (kbd "u") 'cua-scroll-down)
    (define-key map (kbd "j") 'cua-scroll-up)

    ;; cua mode
    (define-key map (kbd "C-z") 'toggle-swift-mode)
    (define-key map (kbd "C-x") 'kill-region)
    (define-key map (kbd "C-c") 'kill-ring-save)
    (define-key map (kbd "C-v") 'yank)
    map))

(define-minor-mode swift-mode
  "Toggle SWIFT buffer mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " SWIFT"
  ;; The minor mode bindings.
  :keymap swift-command-map)

(define-globalized-minor-mode global-swift-mode swift-mode
  swift-mode
  :init-value nil)

(defun toggle-swift-mode()
  (interactive)
  (if (eq global-swift-mode t)
      (progn ;; turning mode off
        (custom-set-faces '(cursor ((t (:background "OrangeRed")))))
	    (set-face-attribute 'mode-line nil
                            :foreground "Black"
                            :background "DarkOrange3"
                            :box nil)
        (global-swift-mode -1))

    (progn ;; turning mode off
      (custom-set-faces '(cursor ((t (:background "blue")))))
      (custom-set-faces '(mode-line ((t (:background "#333377")))))
      (global-swift-mode))))

(use-package eyebrowse :ensure t
  :config
  (setq eyebrowse-mode-line-separator " " eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode t)
  (customize-set-variable 'eyebrowse-mode-line-style 'smart))

(defhydra hydra-frame-helper
  (:color blue)
  "
eyebrowse               frame management
------------------------------------------
[_M-r_]ename              [_M-m_]ake frame
[_M-c_]reate              [_M-o_]ther frame
[_M-k_]lose current       [_M-SPC_]other frame
                        [_M-d_]elete frame
%s(eyebrowse-mode-line-indicator)^^
"
  ("M-m" make-frame nil)
  ("M-o" other-frame nil)
  ("M-SPC" other-frame nil)
  ("M-d" delete-frame nil)
  ("M-r" eyebrowse-rename-window-config nil)
  ("M-c"  (lambda ()
	        (interactive)
	        (progn
	          (eyebrowse-create-window-config)
	          (show-eyebrowse-posframe)) nil))
  ("M-k" (lambda ()
	       (interactive)
	       (progn
	         (eyebrowse-close-window-config)
	         (show-eyebrowse-posframe)) nil)))

(global-set-key (kbd "M-SPC") 'hydra-frame-helper/body)
(global-set-key (kbd "<C-tab>") 'eyebrowse-next)
(global-set-key (kbd "<C-iso-lefttab>") 'eyebrowse-prev)
(global-set-key (kbd "<C-S-tab>") 'eyebrowse-prev)

(use-package posframe :defer t)
(defun show-eyebrowse-posframe ()
  (interactive)
  (progn
    (when (posframe-workable-p)
      (posframe-show " *eyebrowse-posframe*"
                     :string (eyebrowse-mode-line-indicator)
                     :position (cons -40 10)
                     :font "Monospace-16"
                     :timeout 3
                     :internal-border-width 4
                     :internal-border-color "orange"))))

(custom-set-faces '(eyebrowse-mode-line-active ((t (:foreground "DarkMagenta" :weight bold :height 1.2)))))

(defun eyebrowse-next (args)
  (interactive "P")
  (progn (eyebrowse-next-window-config args)
         (show-eyebrowse-posframe)))

(defun eyebrowse-prev (args)
  (interactive "P")
  (progn (eyebrowse-prev-window-config args)
         (show-eyebrowse-posframe)))

(use-package password-generator :defer t)

(defun recompile-custom-packages ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/custom-packages" 0))

(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun org-babel-reload-emacs-org()
  (interactive)
  (org-babel-load-file "~/.emacs.d/init.el"))

(defun emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (interactive)
  (let ((str (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str) str)))



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
	     :publishing-function org-publish-attachment)))

