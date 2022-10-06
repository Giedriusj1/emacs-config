;;; -*- lexical-binding: t -*-

(on-linux
 (use-package eglot
   :bind ( :map tab-map ("o" . g/eglot-transient))
   :config
   (define-transient-command g/eglot-transient ()
     ["Buffer"
      ("f" "format" eglot-format-buffer)
      ("u" "execute action" eglot-code-actions)
      ("h" "diagnostics" consult-flymake)]
     ["Server"
      ("s" "shutdown" eglot-shutdown)
      ("S" "shutdown all" eglot-shutdown-all)]
     ["Symbol"
      ;; ("d" lsp-find-declaration "declaration")
      ("D" "definition" xref-find-definitions)
      ;; ("R" lsp-find-references "references")
      ;; ("i" lsp-find-implementation "implementation")
      ("o" "documentation" eldoc)
      ;; ("t" lsp-find-type-definition "type")
      ("r" "rename" eglot-rename)
      ;; ("s" lsp-signature-help "signature")
      ])

   ;; (i-defun describe-thing-in-popup ()
   ;;   (require 'popup)
   ;;   (require 'xref)

   ;;   (let* ((thing (symbol-at-point))
   ;;          (help-xref-following t)
   ;;          (description (with-temp-buffer
   ;;                         (help-mode)
   ;;                         (help-xref-interned thing)
   ;;                         (buffer-string))))
   ;;     (popup-tip description
   ;; 		  :point (point)
   ;; 		  :around t
   ;; 		  :height 300
   ;; 		  :scroll-bar t
   ;; 		  :margin t)))

   ;; (i-defun g-lsp/invoke ()
   ;;   ;; (require 'lsp-mode)

   ;;   (eglot)

   ;;   (if ;; major mode is understood by LSP
   ;; 	 (->> lsp-language-id-configuration
   ;;            (-first (-lambda ((mode-or-pattern . language))
   ;; 			(eq mode-or-pattern major-mode)))
   ;;            cl-rest)

   ;; 	 ;; Use lsp hydra
   ;; 	 (lsp-mode-hydra/body)
   ;;     ;; default to describe hydra
   ;;     (hydra-describe/body))

   ;;   )

   ;; (pretty-hydra-define hydra-describe (:foreign-keys warn  :quit-key "q" :color blue)
   ;;   ("Symbol"
   ;;    (("d" describe-thing-in-popup "declaration")
   ;;     ("D" xref-find-definitions "find function"))
   ;;    "Eval"
   ;;    (("b" eval-buffer "buffer")
   ;;     ("e" eval-last-sexp "last sexp"))))
   ))
