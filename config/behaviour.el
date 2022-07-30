;;; -*- lexical-binding: t -*-

(on-linux
 (use-package shell-here :defer t
  :bind* (( "C-`" . shell-here)))

 (use-package auto-sudoedit :defer t
   :diminish auto-sudoedit-mode
   :config (auto-sudoedit-mode 1))

 (use-package paradox :defer t
   :config (paradox-enable)))

(use-package hydra :ensure t :defer t)

(use-package diminish)

(diminish 'eldoc-mode)

(use-package google-this :defer t)

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode t))

(use-package recentf :ensure nil :defer t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250))

(use-package zygospore :ensure nil
  :bind* (("C-1" . 'window-swap-states)
          ("C-2" . 'windmove-up)
          ("C-3" . 'windmove-right)
          :map control-semi-map
          ("C-1" . zygospore-toggle-delete-other-windows)))

(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :init (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(define-prefix-command 'mc-map)
(use-package multiple-cursors :defer t
  :bind
  (:map tab-map(("l" . mc-map)))
  (:map mc-map (("l" . mc/edit-lines))))

(cua-mode 1)
(delete-selection-mode 1)
(show-paren-mode t)
(electric-pair-mode t)
(tooltip-mode -1)

(setq windmove-wrap-around t
      debug-on-error nil
      cua-prefix-override-inhibit-delay 0.01
      inhibit-splash-screen t
      initial-scratch-message ""
      column-number-mode t
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
      )


(require 'pixel-scroll)

(if (bound-and-true-p pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)
  (pixel-scroll-mode))

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(define-key tab-map (kbd "j")
            (i-lambda () (cond (( string= "org-mode" major-mode)
	                            (hydra-org/body))
	                           (( string= "emacs-lisp-mode" major-mode)
	                            (hydra-emacs-lisp/body))
	                           (( string= "rust-mode" major-mode)
	                            (hydra-rust/body))
	                           (( string= "rustic-mode" major-mode)
	                            (hydra-rust/body))
	                           (t (hydra-default/body)))))

(setq auto-mode-alist
      '(("[Mm]ake[Ff]ile\\'" . makefile-mode)
        ("\\Makefile.conf\\'" . makefile-mode)
        ("\\.mak\\'" . makefile-mode)
        ("\\CMakeLists.txt$" . cmake-mode)
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
        ("\\.hs\\'" . haskell-mode)
        ("\\.js\\'" . javascript-mode)
        ("\\.mc\\'" . c++-mode)
        ("\\.pm\\'" . perl-mode)
        ("\\.py\\'" . python-mode)
        ("\\.scm\\'" . scheme-mode)
        ("\\.sh\\'" . sh-mode)
        ("bashrc\\'" . sh-mode)
        ("\\.yml\\'" . yaml-mode)
        ("\\.yaml\\'" . yaml-mode)
        ("\\.graphql\\'" . graphql-mode)
        ("\\.s\\'" . asm-mode)
        ("\\.S\\'" . asm-mode)
        ("\\.adl\\'" . adl-mode)
        ("\\opensips.*.cfg\\'" . opensips-cfg-mode)))
