;;; -*- lexical-binding: t -*-

(on-linux
 (g/up eglot :ensure nil
   :bind ( :map tab-map ("o" . g/eglot-transient))
   :init
   ;; enable by default on high memory machines
   (when (> (string-to-number (shell-command-to-string "free -m | awk '/^Mem/ {print $2}'")) 32000)
     (add-hook 'python-mode-hook 'eglot-ensure)
     (add-hook 'python-ts-mode-hook 'eglot-ensure)
     (add-hook 'c++-mode-hook 'eglot-ensure)
     (add-hook 'c++-ts-mode 'eglot-ensure)
     (add-hook 'c-mode-hook 'eglot-ensure)
     (add-hook 'c-ts-mode-hook 'eglot-ensure)

     ;; js and ts
     (add-hook 'typescript-mode-hook 'eglot-ensure)
     (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
     (add-hook 'js-mode-hook 'eglot-ensure)
     (add-hook 'js-ts-mode-hook 'eglot-ensure)
     (add-hook 'js-jsx-mode-hook 'eglot-ensure)
     (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
     (add-hook 'typescript-mode 'eglot-ensure)
     (add-hook 'typescript-ts-mode 'eglot-ensure)

     (add-hook 'rust-ts-mode-hook 'eglot-ensure))
   :config
   (setq eglot-events-buffer-size 0)

   ;; eglot format is hard to get to work well, so we use prettier for js and ts
   (defun prettify-or-eglot-format-buffer ()
     (interactive)
     (if (derived-mode-p 'typescript-mode 'js-mode 'js-jsx-mode 'tsx-mode)
	 (prettier-prettify)
       (eglot-format-buffer)))

   (transient-define-prefix g/eglot-transient ()
     ["Buffer"
      ("f" "format" prettify-or-eglot-format-buffer)
      ("u" "execute action" eglot-code-actions)
      ("h" "diagnostics" consult-flymake)]
     ["Server"
      ("s" "shutdown" eglot-shutdown)
      ("S" "shutdown all" eglot-shutdown-all)]
     ["Symbol"
      ("D" "definition" xref-find-definitions)
      ("R" "references" xref-find-references)
      ("o" "documentation" eldoc)
      ("r" "rename" eglot-rename)
      ])))
