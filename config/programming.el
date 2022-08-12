;;; -*- lexical-binding: t -*-

(on-linux
 (use-package haskell-mode :defer t)

 (use-package lsp-pyright :defer t :ensure t)

 (use-package toml-mode :defer t)

 (use-package typescript-mode :defer t
   :config
   (setq-default typescript-indent-level 2))

 (use-package rustic :defer t
   :config
   (defhydra hydra-rust (:color blue)
     ("c" helm-yas-complete "yas complete")
     ("C" rustic-cargo-clean "cargo clean")
     ("r" rustic-cargo-run "cargo run")
     ("b" rustic-cargo-build "cargo build")
     ("SPC" rustic-cargo-check "cargo check"))))

(on-windows
 (use-package powershell :defer t)
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
                 (setq tab-width identation-size indent-tabs-mode nil))))))

;; (use-package go-mode :defer t
;;   :config
;;   (setq lsp-gopls-codelens nil)

;;   (add-hook 'go-mode-hook
;;             (lambda ()
;;               (add-hook 'before-save-hook 'gofmt-before-save))))


;; (use-package graphql-mode :defer t)

(use-package yaml-mode :defer t)

(use-package ldap-mode :ensure nil :defer t)

(use-package json-mode :defer t)

(use-package dockerfile-mode :defer t)

(use-package cmake-mode :defer t)

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)
            (setq tab-width 4)))

(add-hook 'scheme-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

(add-hook 'c-mode-common-hook
          (lambda()
            (setq comment-start "//" comment-end  "")))


(defhydra hydra-default (:color blue)
    ("c" helm-yas-complete "yas complete"))

(defhydra hydra-emacs-lisp (:color blue)
  ( "j" eval-buffer "eval buffer")
  ( "k" eval-last-sexp "eval-last-sexp")
  ( "c" helm-yas-complete "yas complete"))

(define-key tab-map (kbd "i")
	    (i-lambda ()
	          (cond ((eq 'rustic-mode major-mode)
		         (rustic-format-buffer))
		        ((eq 'json-mode major-mode)
		         (json-reformat-region))
		        (t (message "Argh...don't know how to format in this mode :(")))))
