;;; -*- lexical-binding: t -*-

(on-linux
 (use-package corfu
   :bind (:map control-semi-map
               (("n" . completion-at-point)
		("C-n" . dabbrev-expand)))
   :init
   (global-corfu-mode)
   (setq corfu-popupinfo-delay 0.2)
   (corfu-popupinfo-mode))

 (up conf-toml-mode :mode (("\\.toml\\'" . conf-toml-mode)
			   ("Cargo.lock" . conf-toml-mode))
   :ensure nil)

 (use-package tsx-ts-mode :mode ("\\.ts\\'" . tsx-ts-mode) :ensure nil)

 (use-package rust-mode
   :mode ("\\.rs\\'" . rust-mode)
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

 (use-package cargo))

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

(use-package yasnippet
  :ensure consult-yasnippet
  :ensure yasnippet-snippets
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(use-package yaml-mode  :mode ("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode))

(use-package js-mode :ensure nil :mode ("\\.json\\'" . js-mode))

(use-package dockerfile-mode
  :mode ("[Dd]ockerfile\\'" . dockerfile-mode))

(use-package markdown-mode :ensure nil
  :mode ("\\.md\\'" . markdown-mode))

(use-package doc-view :ensure nil
  :mode ("\\.pdf\\'" . doc-view-mode))

(use-package python :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda()
              (setq indent-tabs-mode nil)
              (setq python-indent 4)
              (setq tab-width 4))))

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

(on-windows
 (use-package cc-mode :ensure nil
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
 (use-package cc-mode :ensure nil
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


(use-package elisp-mode :ensure nil
  :mode
  ("\\.g1\\'" . emacs-lisp-mode)
  ("\\.el\\'" . emacs-lisp-mode)
  ("\\.el.gz\\'" . emacs-lisp-mode)
  ("\\.elc\\'" . elisp-byte-code-mode))

(use-package xml-mode :ensure nil
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
