;;; -*- lexical-binding: t -*-

(on-linux
 (use-package haskell-mode :defer t)
 (use-package lsp-haskell :defer t)
 (use-package lsp-pyright :defer t :ensure t))

(on-windows
 (use-package powershell :defer t))

(use-package mvn :defer t
  :config
  (setq compilation-scroll-output t)
  (defun mvn-integration-test ()
    (interactive)
    (mvn "integration-test")))

(add-hook 'c-mode-common-hook
          (lambda()
            (setq comment-start "//" comment-end  "")))

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

(defhydra hydra-default (:color blue)
    ("c" helm-yas-complete "yas complete"))

(use-package go-mode :defer t
  :config
  (setq lsp-gopls-codelens nil)

  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save))))

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)
            (setq tab-width 4)
            (tree-sitter-hl-mode)))

(add-hook 'scheme-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

(defhydra hydra-emacs-lisp (:color blue)
  ( "j" eval-buffer "eval buffer")
  ( "k" eval-last-sexp "eval-last-sexp")
  ( "c" helm-yas-complete "yas complete"))

(use-package typescript-mode :defer t
  :config
  (setq-default typescript-indent-level 2))

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
;; Formatting stuff
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clang-format :defer t
  :config
  ;; The following somewhat resemble Resilient's coding style
  (setq clang-format-style "{BasedOnStyle: google, ColumnLimit: 100, IndentWidth: 3, BreakBeforeBraces: Stroustrup}"))

(use-package elisp-format :defer t)

(define-key tab-map (kbd "i")
	        (i-lambda ()
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
