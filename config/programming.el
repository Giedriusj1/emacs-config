;;; -*- lexical-binding: t -*-

(on-linux
 (g/up corfu
   :bind (:map control-semi-map
               (("n" . completion-at-point)
		("C-n" . dabbrev-expand)))
   :init
   (global-corfu-mode)
   (setq corfu-popupinfo-delay 0.1)
   (corfu-popupinfo-mode))

 (g/up conf-toml-mode :mode (("\\.toml\\'" . conf-toml-mode)
			     ("Cargo.lock" . conf-toml-mode))
   :ensure nil)

 (g/up typescript-ts-mode :mode ("\\.ts\\'" . typescript-ts-mode) :ensure nil)

 (g/up rust-ts-mode :ensure nil
   :mode ("\\.rs\\'" . rust-ts-mode)
   :config
   (transient-define-prefix g/rust-transient ()
     ["cargo"
      ("C" "clean" cargo-process-clean)
      ("r" "run" cargo-process-run)
      ("b" "build" cargo-process-build)
      ("SPC" "check" cargo-process-check)
      ("t" "test all" cargo-process-test)
      ("T" "test current test" cargo-process-current-test)]
     ["yas"
      ("c" "complete" consult-yasnippet)]))

 (g/up cargo))

(on-windows
 (g/up powershell :mode ("\\.ps1\\'" . powershell-mode))
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

(g/up yasnippet
  :ensure consult-yasnippet
  :ensure yasnippet-snippets
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(g/up yaml-ts-mode :ensure nil  :mode ("\\.yml\\'" . yaml-ts-mode) ("\\.yaml\\'" . yaml-ts-mode))

(g/up json-js-mode :ensure nil :mode ("\\.json\\'" . json-ts-mode))

(on-windows
 (g/up dockerfile-mode
   :mode ("[Dd]ockerfile\\'" . dockerfile-mode)))

(on-linux
 (g/up dockerfile-ts-mode
   :mode ("[Dd]ockerfile\\'" . dockerfile-ts-mode)))


(g/up markdown-mode :ensure nil
  :mode ("\\.md\\'" . markdown-mode))

(g/up doc-view :ensure nil
  :mode ("\\.pdf\\'" . doc-view-mode))

(g/up python :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda()
              (setq indent-tabs-mode nil)
              (setq python-indent 4)
              (setq tab-width 4))))

(g/up sh-script :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
	 ("bashrc\\'" . sh-mode)))

;; (g/up cmake-mode
;;   :mode ("\\CMakeLists.txt$" . cmake-mode))

(g/up makefile-mode :ensure nil
  :mode (("[Mm]ake[Ff]ile\\'" . makefile-mode)
         ("\\Makefile.conf\\'" . makefile-mode)
         ("\\.mak\\'" . makefile-mode)))

(g/up bat-mode :ensure nil
  :mode (("\\.bat\\'" . bat-mode)
         ("\\.cmd\\'" . bat-mode)))

(g/up asm-mode :ensure nil
  :mode (("\\.s\\'" . asm-mode)
	 ("\\.S\\'" . asm-mode)))

(on-windows
 (g/up cc-mode :ensure nil
   :mode (("\\.c\\'" . c-mode)
	  ("\\.cc\\'" . c-mode)
	  ("\\.cpp\\'" . c++-mode)
	  ("\\.h\\'" . c++-mode)
	  ("\\.hh\\'" . c++-mode)
	  ("\\.hpp\\'" . c++-mode)
	  ("\\.mc\\'" . c++-mode))
   :config
   (add-hook 'c-mode-common-hook
             (lambda()
               (setq comment-start "//" comment-end  "")))  ) )

(on-linux
 (g/up c-ts-mode :ensure nil
   :mode (("\\.c\\'" . c-ts-mode)
	  ("\\.cc\\'" . c-ts-mode)
	  ("\\.cpp\\'" . c++-ts-mode)
	  ("\\.h\\'" . c++-ts-mode)
	  ("\\.hh\\'" . c++-ts-mode)
	  ("\\.hpp\\'" . c++-ts-mode)
	  ("\\.mc\\'" . c++-ts-mode))
   :config
   (add-hook 'c-ts-mode-hook
             (lambda()
               (setq comment-start "//" comment-end  "")))))


(g/up elisp-mode :ensure nil
  :mode
  ("\\.g1\\'" . emacs-lisp-mode)
  ("\\.el\\'" . emacs-lisp-mode)
  ("\\.el.gz\\'" . emacs-lisp-mode)
  ("\\.elc\\'" . elisp-byte-code-mode))

(g/up xml-mode :ensure nil
  :mode (("\\.xml\\'" . xml-mode)
	 ("\\.pom\\'" . xml-mode)
	 ("\\.sql\\'" . sql-mode)))

(add-hook 'scheme-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

(transient-define-prefix g/default-transient ()
  ["yas"
   ("c" "complete" consult-yasnippet)])

(transient-define-prefix g/emacs-lisp-transient ()
  ["eval"
   ( "j" "eval buffer" eval-buffer)
   ( "k" "eval-last-sexp" eval-last-sexp)]
  ["yas"
   ("c" "complete" consult-yasnippet)])

(define-key tab-map (kbd "i")
	    (i-lambda ()
	      (cond ((eq 'rust-mode major-mode)
		     (rustic-format-buffer)) ;TODO:
		    ((eq 'json-mode major-mode)
		     (json-reformat-region))
		    (t (message "Argh...don't know how to format in this mode :(")))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
