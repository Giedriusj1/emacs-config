;;; -*- lexical-binding: t -*-

(on-linux
 (use-package eglot
   :bind ( :map tab-map ("o" . eglot-hydra/body))
   :pretty-hydra (
		  (;; :pre
		   ;; (if (and ;; lsp-mode is not enabled
		   ;; 	(not (bound-and-true-p lsp-mode))
		   ;; 	;; and we are not on Windows
		   ;; 	(not (on-windows t)))

		   ;;     ;; Enable LSP
		   ;;     (progn (message "enabling lsp mode...") (lsp)))

		   :foreign-keys warn :title "LSP" :quit-key "q" :color blue)
		  ("Buffer"
		   (("f" eglot-format-buffer "format")
		    ("u" eglot-code-actions "execute action")
		    ;; ("h" consult-lsp-diagnostics "diagnostics")
		    )
		   "Server"
		   (("S" eglot-shutdown "shutdown"))
		   "Symbol"
		   (;; ("d" lsp-find-declaration "declaration")
		    ("D" xref-find-definitions "definition")
		    ;; ("R" lsp-find-references "references")
		    ;; ("i" lsp-find-implementation "implementation")
		    ("o" eldoc "documentation")
		    ;; ("t" lsp-find-type-definition "type")
		    ;; ("r" lsp-rename "rename")
		    ;; ("s" lsp-signature-help "signature")
		    )))

   :config
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
