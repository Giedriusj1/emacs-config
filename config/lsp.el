;;; -*- lexical-binding: t -*-

(on-linux
 (use-package helm-lsp :defer t)

 (use-package pretty-hydra :ensure t :defer t)
 (pretty-hydra-define hydra-lsp
   (:pre
    (if
        ;; If major mode is understood by LSP
        (and (->> lsp-language-id-configuration
                  (-first (-lambda ((mode-or-pattern . language))
                            (eq mode-or-pattern major-mode)))
                  cl-rest)
             (and
              ;; and lsp-mode is not enabled
              (not (bound-and-true-p lsp-mode))
              ;; and we are not on Windows
              (not (is-windows))))

        ;; Enable LSP
        (progn (message "enabling lsp mode...") (lsp)))

    :foreign-keys warn :title "LSP" :quit-key "q" :color blue)
   ("Buffer"
    (("f" lsp-format-buffer "format")
     ("u" helm-lsp-code-actions "execute action")
     ("h" helm-lsp-diagnostics "diagnostics")
     )
    "Server"
    (("S" lsp-restart-workspace "restart"))
    "Symbol"
    (("d" lsp-find-declaration "declaration")
     ("D" lsp-find-definition "definition")
     ("R" lsp-find-references "references")
     ("i" lsp-find-implementation "implementation")
     ("o" lsp-describe-thing-at-point "documentation")
     ("t" lsp-find-type-definition "type")
     ("r" lsp-rename "rename")
     ("s" lsp-signature-help "signature")))))
