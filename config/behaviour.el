;;; -*- lexical-binding: t -*-

(use-package diminish :demand)

(on-linux
 (use-package shell-here :bind* ( "C-`" . shell-here)))

(use-package vertico
  :init
  (vertico-mode)

  (setq vertico-count 25
	vertico-resize nil))

(setq enable-recursive-minibuffers t)

(bind-keys* :map minibuffer-local-map ("C-g" . exit-recursive-edit))

(use-package consult :defer 1
  :init
  (defun consult-line-empty (&optional initial start)
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
          (top (not (eq start consult-line-start-from-top))))
      (consult--line
       (or
	(consult--line-candidates top curr-line)
	(user-error "No lines"))
       :curr-line curr-line
       :prompt "Go to line: ")))

  :config
  (setq consult-async-input-throttle 0.1
	consult-async-refresh-delay 0.1
	consult-async-input-debounce 0.05
	consult-async-min-input 0)

  (advice-add #'consult-line
              :around
              #'consult-line-advice
              '((name . "wrapper")))

  (i-defun consult-line-advice (consult-line-function &rest rest)
    (if (use-region-p)
	(apply consult-line-function
               (buffer-substring (region-beginning) (region-end)) rest)
      (apply consult-line-function
             (thing-at-point 'symbol) rest))))

(use-package marginalia
  :bind (:map minibuffer-local-map ("C-l" . marginalia-cycle))
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eldoc-mode :ensure nil
  :init
  (setq max-mini-window-height 3) 	; Make sure the minibuffer docs are sensible in size
  (setq eldoc-idle-delay 0.1)
  (add-hook 'find-file-hook
	    (lambda()
	      (diminish 'eldoc-mode)
	      (diminish 'auto-revert-mode))))

(use-package recentf :ensure nil :demand
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250))

(use-package zygospore
  :bind* (("C-1" . 'window-swap-states)
          ("C-2" . 'windmove-up)
          ("C-3" . 'windmove-right)
          :map control-semi-map
          ("C-1" . zygospore-toggle-delete-other-windows)))

(use-package ripgrep)

(use-package multiple-cursors
  :init
  (define-prefix-command 'mc-map)
  :bind
  (:map tab-map(("l" . mc-map)))
  (:map mc-map (("l" . mc/edit-lines))))

(cua-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(column-number-mode t)

(setq windmove-wrap-around t
      debug-on-error nil
      cua-prefix-override-inhibit-delay 0.01
      inhibit-splash-screen t
      initial-scratch-message ""
      history-length 25
      select-enable-clipboard t ;; Merge OS and Emacs' clipboards
      auto-window-vscroll nil   ;; Gives us better line scrolling performance

      ;; We'll ask emacs to put all customizations made via it's customize package in a
      ;; separate file... so we can ignore it later :)
      custom-file (concat user-emacs-directory "/custom--ignored.el")

      ;; Make the interface a bit more snappy
      idle-update-delay 0.05
      bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
      ring-bell-function 'ignore
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t ; use versioned backups
      uniquify-buffer-name-style 'forward
      isearch-lazy-count t
      use-short-answers t
      ;; Native comp
      native-comp-speed 3
      package-native-compile t)

(setq native-comp-compiler-options '("-O2" "-mtune=native"))
(setq native-comp-driver-options '("-O2" "-mtune=native"))

(define-key tab-map (kbd "j")
	    (i-lambda () (cond ((eq 'org-mode major-mode)
				(g/org-transient))
			       ((eq 'emacs-lisp-mode major-mode)
				(g/emacs-lisp-transient))
			       ((eq 'rust-mode major-mode)
				(g/rust-transient))
			       (t (g/default-transient)))))

(define-key tab-map (kbd ";") 'g/quickopen-transient)

(use-package transient :demand)

(use-package helm-projectile)

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
     :truncate-lines helm-grep-truncate-lines)))

(transient-define-prefix g/quickopen-transient ()
  ["quickopen"
   ("t"  "~/private-sync/temp.org" (lambda ()
				     (interactive)
				     (find-file "~/private-sync/temp.org")))


   ("c"  "~/.emacs.d/init.el" (lambda ()
				(interactive)
				(find-file "~/.emacs.d/init.el")))
   ("l"  "dired ~/private-sync/" (lambda ()
				   (interactive)
				   (progn (zygospore-toggle-delete-other-windows)
					  (dired "~/private-sync")
					  (find-file default-directory))))
   (";"  "grep notes"
        (lambda ()
           (interactive)
           (g/helm-projectile-grep-notes "~/private-sync")))


    ;; (lambda ()
    ;; 			(interactive)
    ;; 			;; ;TODO: ignore
    ;; 			;; '("*.doc" "*.ovpn" "*.pcap" "*.pcapng" "*.png" "*.pem" )
    ;; 			(consult-ripgrep "~/private-sync"))
    ])

(use-package project :diminish :ensure nil
  :bind (:map tab-map ("p" . g/project-transient))
  :config
  (transient-define-prefix g/project-transient ()
    "Project"
    ["Project"
     ("p"  "projects" g/project-switch-project)
     ("j" "find file" project-find-file)
     ("d" "dired" project-dired)]
    ["Search"
     ("r" "find regexp" project-find-regexp)
     ("R" "consult ripgrep" consult-ripgrep)])
  (defun g/project-switch-project (dir)
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir))
      (project-dired))))

(defalias 'lp 'list-packages)
(defalias 'msf 'menu-set-font)
