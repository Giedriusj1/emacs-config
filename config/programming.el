;;; -*- lexical-binding: t -*-

(on-linux
 (use-package toml-mode :mode ("\\.toml\\'" . toml-mode))

 (use-package typescript-mode :mode ("\\.ts\\'" . typescript-mode)
   :config
   (setq-default typescript-indent-level 2))

 (use-package rustic :mode ("\\.rs\\'" . rustic-mode)
   :pretty-hydra ((:color blue)
		  ("cargo"
		   (("C" rustic-cargo-clean "clean")
		    ("r" rustic-cargo-run "run")
		    ("b" rustic-cargo-build "build")
		    ("SPC" rustic-cargo-check "check"))
		   "yas"
		   (("c" helm-yas-complete "complete"))))))

(on-windows
 (use-package powershell :mode ("\\.ps1\\'" . powershell-mode))
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

(use-package yaml-mode  :mode ("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode))

(use-package ldap-mode :ensure nil :mode ("\\.ldif\\'" . ldif-mode))

(use-package json-mode :mode ("\\.json\\'" . json-mode))

(use-package dockerfile-mode
  :mode ("[Dd]ockerfile\\'" . dockerfile-mode))

(use-package markdown-mode :ensure nil
  :mode ("\\.md\\'" . markdown-mode))

(use-package doc-view :ensure nil
  :mode ("\\.pdf\\'" . doc-view-mode))

(use-package python :ensure nil
  :mode ("\\.py\\'" . python-mode))

(use-package sh-script :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
	 ("bashrc\\'" . sh-mode)))

(use-package cmake-mode
  :mode ("\\CMakeLists.txt$" . cmake-mode))

(use-package makefile-mode :ensure nil
  :mode (("[Mm]ake[Ff]ile\\'" . makefile-mode)
         ("\\Makefile.conf\\'" . makefile-mode)
         ("\\.mak\\'" . makefile-mode)))

(use-package bat-mode :ensure nil
  :mode (("\\.bat\\'" . bat-mode)
         ("\\.cmd\\'" . bat-mode)))

(use-package asm-mode :ensure nil
  :mode (("\\.s\\'" . asm-mode)
	 ("\\.S\\'" . asm-mode)))

(use-package cc-mode :ensure nil
  :mode (("\\.c\\'" . c-mode)
	 ("\\.cc\\'" . c-mode)
	 ("\\.cpp\\'" . c++-mode)
	 ("\\.h\\'" . c++-mode)
	 ("\\.hh\\'" . c++-mode)
	 ("\\.hpp\\'" . c++-mode)
	 ("\\.mc\\'" . c++-mode)))

(use-package elisp-mode :ensure nil
  :mode
  ("\\.el\\'" . emacs-lisp-mode)
  ("\\.el.gz\\'" . emacs-lisp-mode)
  ("\\.elc\\'" . elisp-byte-code-mode))

(use-package xml-mode :ensure nil
  :mode (("\\.xml\\'" . xml-mode)
	 ("\\.pom\\'" . xml-mode)
	 ("\\.sql\\'" . sql-mode)))

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)
            (setq tab-width 4)))

(add-hook 'scheme-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

(add-hook 'c-mode-common-hook
          (lambda()
            (setq comment-start "//" comment-end  "")))

(pretty-hydra-define hydra-default (:color blue)
  ("yas"
   (("c" helm-yas-complete "complete"))))

(pretty-hydra-define hydra-emacs-lisp (:color blue)
  ("eval"
   (( "j" eval-buffer "eval buffer")
    ( "k" eval-last-sexp "eval-last-sexp"))
   "yas"
   (( "c" helm-yas-complete "complete"))))

(define-key tab-map (kbd "i")
  (i-lambda ()
    (cond ((eq 'rustic-mode major-mode)
	   (rustic-format-buffer))
	  ((eq 'json-mode major-mode)
	   (json-reformat-region))
	  (t (message "Argh...don't know how to format in this mode :(")))))
