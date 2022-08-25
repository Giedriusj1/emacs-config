;;; -*- lexical-binding: t -*-

(on-linux
 (use-package shell-here :bind* (( "C-`" . shell-here)))

 (use-package auto-sudoedit :diminish auto-sudoedit-mode
   :config (auto-sudoedit-mode t)))

(cond-linux-win
 (use-package paradox
   :init (i-defun lp ()
	   (paradox-enable)
	   (list-packages)))

 (defalias 'lp 'list-packages))

(use-package pretty-hydra)

(use-package diminish :demand)

(use-package eldoc-mode :ensure nil :diminish)

(use-package recentf :ensure nil
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

(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :init (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))


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
      isearch-lazy-count t)

(use-package pixel-scroll :ensure nil :demand
  :init
  (if (bound-and-true-p pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode)
    (pixel-scroll-mode)))


(define-key tab-map (kbd "j")
            (i-lambda () (cond ((eq 'org-mode major-mode)
	                            (hydra-org/body))
	                           ((eq 'emacs-lisp-mode major-mode)
	                            (hydra-emacs-lisp/body))
	                           ((eq 'rust-mode major-mode)
	                            (hydra-rust/body))
	                           ((eq 'rustic-mode major-mode)
	                            (hydra-rust/body))
	                           (t (hydra-default/body)))))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'describe-bindings 'helm-descbinds)
(defalias 'rel 'g/reload-emacs-config)
(defalias 'msf 'menu-set-font)
