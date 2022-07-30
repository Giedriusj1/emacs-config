(add-to-list 'load-path "~/.emacs.d/config/helm-tree-sitter")
(use-package tree-sitter :defer t
  :diminish tree-sitter-mode
  :diminish tree-sitter-debug-mode)

(use-package tree-sitter-langs :defer t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
