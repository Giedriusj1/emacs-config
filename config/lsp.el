;;; -*- lexical-binding: t -*-

(on-linux
 (use-package lsp-ui :defer t)
 (use-package helm-lsp :defer t)

 (use-package pretty-hydra :ensure t :defer t)
 (pretty-hydra-define hydra-lsp
   (:pre
    (if (and
         ;; If lsp-mode is not enabled... and we are not on Windows
         (not (bound-and-true-p lsp-mode)) (not (is-windows)))

        ;; Enable lsp
        (progn (message "enabling lsp mode...") (lsp)))

    :foreign-keys warn :title "LSP" :quit-key "q" :color blue)
   ("Buffer"
    (("f" lsp-format-buffer "format")
     ("menu" lsp-ui-imenu "imenu")
     ("c" ace-delete-other-windows "maximize")
     ("u" helm-lsp-code-actions "execute action"))
    "Server"
    (("M-r" move-border-left "restart")
     ("S" lsp-shutdown-workspace "restart")
     ("M-s" lsp-describe-session "shutdown"))
    "Symbol"
    (("d" lsp-find-declaration "declaration")
     ("D" lsp-ui-peek-find-definitions "definition")
     ("R" lsp-ui-peek-find-references "references")
     ("i" lsp-ui-peek-find-implementation "implementation")
     ("o" lsp-describe-thing-at-point "documentation")
     ("t" lsp-find-type-definition "type")
     ("r" lsp-rename "rename")
     ("s" lsp-signature-help "signature")))))
