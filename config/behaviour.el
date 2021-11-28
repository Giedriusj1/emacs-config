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

(define-prefix-command 'mc-map)
(use-package multiple-cursors :defer t
  :bind
  (:map tab-map(("l" . mc-map)))
  (:map mc-map (("l" . mc/edit-lines))))

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

(use-package auto-sudoedit
  :diminish auto-sudoedit-mode
  :config (auto-sudoedit-mode 1))

;; todo (Sat Jul 18 16:34:44 2020) eldoc was throwing non stop errors in org-mode.
;; Maybe we want to enable it back at some point *shrug*
(global-eldoc-mode -1)

;; set to t to investigate crashes
(setq debug-on-error nil)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq column-number-mode t)
(setq history-length 25)
(setq select-enable-clipboard t) ;; Merge OS and Emacs' clipboards

(setq auto-window-vscroll nil)   ;; Gives us better line scrolling performance

;; We'll ask emacs to put all customizations made via it's customize package in a
;; separate file... so we can ignore it later :)
(setq custom-file (concat user-emacs-directory "/custom--ignored.el"))

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

(use-package posframe :defer t)
