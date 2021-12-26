(add-to-list 'load-path "~/.emacs.d/config/helm-tree-sitter")

(use-package tree-sitter :defer t)

(use-package tree-sitter-langs :defer t
  :config
  (require 'helm-tree-sitter))

(global-tree-sitter-mode)

(define-key lisp-playground-map (kbd "SPC") 'load-listp-playground)
(define-key lisp-playground-map (kbd "g") 'g-tree)
(define-key lisp-playground-map (kbd "s") 'helm-tree-sitter-or-imenu)

;TODO: We want to use-package with sensible autoloads, etc...
;; (require 'helm-tree-sitter)
