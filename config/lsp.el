;;; -*- lexical-binding: t -*-

(on-linux
 (define-key tab-map (kbd "o") 'g-lsp/invoke)

 (use-package helm-lsp :defer t)

 (use-package pretty-hydra :ensure t :defer t)


 (defun describe-foo-at-point ()
   "Show the documentation of the Elisp function and variable near point.
	This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call
	"
   (interactive)
   (let (sym)
	 ;; sigh, function-at-point is too clever.  we want only the first half.
	 (cond ((setq sym (ignore-errors
                        (with-syntax-table emacs-lisp-mode-syntax-table
                          (save-excursion
                            (or (not (zerop (skip-syntax-backward "_w")))
                                (eq (char-syntax (char-after (point))) ?w)
                                (eq (char-syntax (char-after (point))) ?_)
                                (forward-sexp -1))
                            (skip-chars-forward "`'")
        	                (let ((obj (read (current-buffer))))
                              (and (symbolp obj) (fboundp obj) obj))))))
            (describe-function sym))
           ((setq sym (variable-at-point)) (describe-variable sym))
           ;; now let it operate fully -- i.e. also check the
           ;; surrounding sexp for a function call.
           ((setq sym (function-at-point)) (describe-function sym)))))


 (defun g-lsp/invoke ()
   (interactive)

   (require 'lsp-mode)

   (if ;; major mode is understood by LSP
       (->> lsp-language-id-configuration
            (-first (-lambda ((mode-or-pattern . language))
                      (eq mode-or-pattern major-mode)))
            cl-rest)

       ;; Use lsp hydra
       (hydra-lsp/body)
     ;; default to describe hydra
     (hydra-describe/body)))

 (pretty-hydra-define hydra-describe
   (:foreign-keys warn  :quit-key "q" :color blue)
   ("Symbol"
    (("d" describe-foo-at-point "declaration")
     ("D" find-function-at-point "find function")
     )))


 (pretty-hydra-define hydra-lsp
   (:pre
    (if (and
         ;; lsp-mode is not enabled
         (not (bound-and-true-p lsp-mode))
         ;; and we are not on Windows
         (not (is-windows)))

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
