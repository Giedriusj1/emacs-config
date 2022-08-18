(add-to-list 'load-path "~/.emacs.d/config/helm-tree-sitter")

(use-package tree-sitter :defer 4
  :hook (agda-mode       . (lambda () (tree-sitter-mode)))
  :hook (sh-mode         . (lambda () (tree-sitter-mode)))
  :hook (c-mode          . (lambda () (tree-sitter-mode)))
  :hook (caml-mode       . (lambda () (tree-sitter-mode)))
  :hook (csharp-mode     . (lambda () (tree-sitter-mode)))
  :hook (c++-mode        . (lambda () (tree-sitter-mode)))
  :hook (d-mode          . (lambda () (tree-sitter-mode)))
  :hook (css-mode        . (lambda () (tree-sitter-mode)))
  :hook (elm-mode        . (lambda () (tree-sitter-mode)))
  :hook (elixir-mode     . (lambda () (tree-sitter-mode)))
  :hook (go-mode         . (lambda () (tree-sitter-mode)))
  :hook (hcl-mode        . (lambda () (tree-sitter-mode)))
  :hook (terraform-mode  . (lambda () (tree-sitter-mode)))
  :hook (html-mode       . (lambda () (tree-sitter-mode)))
  :hook (mhtml-mode      . (lambda () (tree-sitter-mode)))
  :hook (nix-mode        . (lambda () (tree-sitter-mode)))
  :hook (java-mode       . (lambda () (tree-sitter-mode)))
  :hook (javascript-mode . (lambda () (tree-sitter-mode)))
  :hook (js-mode         . (lambda () (tree-sitter-mode)))
  :hook (js2-mode        . (lambda () (tree-sitter-mode)))
  :hook (js3-mode        . (lambda () (tree-sitter-mode)))
  :hook (json-mode       . (lambda () (tree-sitter-mode)))
  :hook (jsonc-mode      . (lambda () (tree-sitter-mode)))
  :hook (julia-mode      . (lambda () (tree-sitter-mode)))
  :hook (ocaml-mode      . (lambda () (tree-sitter-mode)))
  :hook (php-mode        . (lambda () (tree-sitter-mode)))
  :hook (prisma-mode     . (lambda () (tree-sitter-mode)))
  :hook (python-mode     . (lambda () (tree-sitter-mode)))
  :hook (pygn-mode       . (lambda () (tree-sitter-mode)))
  :hook (rjsx-mode       . (lambda () (tree-sitter-mode)))
  :hook (ruby-mode       . (lambda () (tree-sitter-mode)))
  :hook (rust-mode       . (lambda () (tree-sitter-mode)))
  :hook (rustic-mode     . (lambda () (tree-sitter-mode)))
  :hook (scala-mode      . (lambda () (tree-sitter-mode)))
  :hook (swift-mode      . (lambda () (tree-sitter-mode)))
  :hook (tuareg-mode     . (lambda () (tree-sitter-mode)))
  :hook (typescript-mode . (lambda () (tree-sitter-mode)))
  :hook (zig-mode        . (lambda () (tree-sitter-mode)))

  :diminish tree-sitter-mode
  :diminish tree-sitter-debug-mode)

(use-package tree-sitter-langs)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
