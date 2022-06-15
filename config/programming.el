;;; -*- lexical-binding: t -*-

(use-package mvn :defer t
  :config
  (setq compilation-scroll-output t)
  (defun mvn-integration-test ()
    (interactive)
    (mvn "integration-test")))

;; (use-package ggtags :defer t
;;   :config
;;   ;; This should prevent Emacs from asking "Keep current list of tags tables also?"
;;   (setq tags-add-tables nil)

;;   ;; Prevent ggtags mode from displaying project name in mode line.
;;   ;; Projectile already displays this information.
;;   (setq ggtags-mode-line-project-name nil))

;; (use-package dumb-jump :defer t
;;   :config
;;   (setq dumb-jump-selector 'helm)
;;   (setq dumb-jump-force-searcher 'rg))

(use-package yasnippet :defer t
  :ensure yasnippet-snippets
  :ensure yasnippet-classic-snippets
  :ensure helm-c-yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))


(when (display-graphic-p)
  (use-package  company-box :defer t
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors
          '((company-lsp .
                         ( :selected (:background "orange"  :foreground "black")))
            (company-capf .
			              ( :selected (:background "orange" :foreground "black")))))
    (setq company-box-doc-delay 1)))

(use-package company :defer t
  :bind ( :map company-active-map
          (("C-n" . company-select-next)
           ("C-p" . company-select-previous))
          :map control-semi-map
          (("n" . company-complete)
           ("C-n" . dabbrev-expand)))
  :diminish company-mode
  :config
  (when (display-graphic-p)
    (require 'color)

    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

  (global-company-mode t)

  (setq company-tooltip-limit 25))

(defhydra hydra-c (:color blue)
  ( "c" helm-yas-complete "helm yas complete"))

(add-hook 'c-mode-common-hook
          (lambda()
            (setq comment-start "//" comment-end  "")))

;; (use-package ob-rust :defer t)

(use-package toml-mode :defer t)

(use-package rustic :defer t
  :config
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
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
            (setq tab-width 4)
            (tree-sitter-hl-mode)))

(use-package lsp-pyright :defer t :ensure t)

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

(cond ((string-equal system-type "gnu/linux")
       (use-package haskell-mode :defer t)
       (use-package lsp-haskell :defer t)))

(use-package typescript-mode :defer t)

(use-package powershell :defer t)

(use-package g-adl-mode :ensure nil)    ; We can't defer this one for some reason

(use-package graphql-mode :defer t)

(use-package yaml-mode :defer t)

(use-package ldap-mode :ensure nil :defer t)

(use-package json-mode :defer t)

(use-package dockerfile-mode :defer t)

(use-package cmake-mode :defer t)

(use-package asm-mode :defer t :ensure nil
  :bind (:map asm-mode-map
              ("C-j" . helm-mini)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Formatting stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; Try to set an appropriate identation size
(add-hook 'find-file-hook
          (lambda ()
            (let ((identation-size
                   (cond
                    ;; EAS expects 3
                    ((string-match  "^c:/workspace/src" buffer-file-name) 3)
                    ;; smartblock-paren uses 2
                    ((string-match "^c:/workspace/resilient/smartblock-parent" buffer-file-name) 2)
                    ;; Everything else gets a sane default of 4
                    (t 4))))
              (progn
                (setq c-basic-offset identation-size c-default-style "linux")
                (setq tab-width identation-size indent-tabs-mode nil)))))
