;;; -*- lexical-binding: t -*-

;; Make sure the windows are always balanced
(add-hook 'window-configuration-change-hook
	  (lambda ()
	    ;; check if we are in ediff mode
	    (if (not (eq major-mode 'ediff-mode))
		(balance-windows))))

(g/up vertico
  :init
  (vertico-mode)

  (vertico-buffer-mode)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (define-key control-semi-map (kbd "C-r") 'vertico-repeat)

  (setq vertico-count 25
	vertico-resize nil
	vertico-cycle t))

(bind-keys* :map minibuffer-local-map ("C-g" . exit-recursive-edit))

(g/up consult :defer 1
  :init
  (defun consult-line-empty (&optional initial start)
    (interactive (list nil (not (not current-prefix-arg))))
    (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
           (top (not (eq start consult-line-start-from-top)))
           (candidates (or (consult--line-candidates top curr-line)
                           (user-error "No lines"))))
      (consult--read
       candidates
       :annotate (consult--line-prefix curr-line)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last isearch string to future history
       :add-history (list (thing-at-point 'symbol) isearch-string)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       :default (car candidates)
       ;; Add isearch-string as initial input if starting from isearch
       :initial (or initial
                    (and isearch-mode
			 (prog1 isearch-string (isearch-done))))
       :state (consult--location-state candidates))))

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

(g/up marginalia
  :bind (:map minibuffer-local-map ("C-l" . marginalia-cycle))
  :init (marginalia-mode))

(g/up orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(g/up eldoc-mode :ensure nil
  :init
  (setq max-mini-window-height 3) 	; Make sure the minibuffer docs are sensible in size
  (setq eldoc-idle-delay 0.1)
  ;; (add-hook 'find-file-hook
  ;; 	    (lambda()
  ;; 	      (diminish 'eldoc-mode)
  ;; 	      (diminish 'auto-revert-mode)))
  )

(g/up recentf :ensure nil :demand
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250))

(g/up ripgrep)

(g/up multiple-cursors
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
			       ((eq 'rust-ts-mode major-mode)
				(g/rust-transient))
			       ((eq 'rust-mode major-mode)
				(g/rust-transient))
			       (t (g/default-transient)))))

(define-key tab-map (kbd ";") 'g/quickopen-transient)

(g/up transient :demand)

(transient-define-prefix g/quickopen-transient ()
  ["quickopen"
   ("t"  "~/temp.org" (lambda ()
			(interactive)
			(find-file "~/temp.org")))


   ("c"  "~/.emacs.d/init.el" (lambda ()
				(interactive)
				(find-file "~/.emacs.d/init.el")))
   ("l"  "dired ~/private-sync/" (lambda ()
				   (interactive)
				   (progn (zygospore-toggle-delete-other-windows)
					  (dired "~/private-sync")
					  (find-file default-directory))))
   (":"  "grep notes"
    ;; (lambda ()
    ;;    (interactive)
    ;;    (g/helm-projectile-grep-notes "~/private-sync"))

    (lambda ()
      (interactive)
      ;; ;TODO: ignore
      ;; '("*.doc" "*.ovpn" "*.pcap" "*.pcapng" "*.png" "*.pem" )
      (consult-ripgrep "~/private-sync"))
    )

   (";"  "find notes"
    (lambda ()
      (interactive)
      (let* ((root "~/private-sync")
	     (pr (project-current nil root)))
	(project-find-file-in nil (list root) pr nil))))
   ])

(g/up project :diminish :ensure nil
  :bind (:map tab-map ("p" . g/project-transient))
  :config
  (transient-define-prefix g/project-transient ()
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

(transient-define-prefix g/frame-helper-transient ()
  ["tab-bar"
   ("M-c" "new" tab-bar-new-tab)
   ("M-k" "close" tab-bar-close-tab)]
  ;; ["frame"
  ;;  ("M-m" "new" make-frame)
  ;;  ("M-o" "other" other-frame)
  ;;  ("M-SPC" "other" other-frame)
  ;;  ("M-d" "delete" delete-frame)]
  )


(defvar g-toggle-delete-other-windows-reg "")

(i-defun g-toggle-delete-other-windows ()
  (if (and (equal (selected-window) (next-window)))
      (jump-to-register g-toggle-delete-other-windows-reg)
    (progn
      (window-configuration-to-register g-toggle-delete-other-windows-reg)
      (delete-other-windows))))

(defalias 'lp 'list-packages)
(defalias 'msf 'menu-set-font)
(defalias 'dtw 'delete-trailing-whitespace)
