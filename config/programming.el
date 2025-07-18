;;; -*- lexical-binding: t -*-

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(on-linux-or-mac
 (g/up corfu
   :bind (:map control-semi-map
               (("n" . completion-at-point)
                ("C-n" . dabbrev-expand)))
   :init
   (global-corfu-mode)
   (corfu-popupinfo-mode)
   (setq corfu-popupinfo-max-height 40)
   (setq corfu-count 20))

 (g/up conf-toml-mode :mode (("\\.toml\\'" . conf-toml-mode)
                             ("Cargo.lock" . conf-toml-mode))
   :ensure nil)

 (g/up cargo)

 ;; Only want to load it for rust-compile.el
 (g/up rust-mode)

 (on-linux
  (g/up rust-ts-mode :ensure nil
    :mode ("\\.rs\\'" . rust-ts-mode)
    :config
    (require 'rust-compile)        ; Give cargo-process links to source files
    ))

 (on-mac
  (g/up rust-mode :ensure nil
    :mode ("\\.rs\\'" . rust-mode)
    :config
    (require 'rust-compile)        ; Give cargo-process links to source files
    ))

 (defun cargo-process-clippy-tests ()
   (interactive)
   (cargo-process--start "Clippy"
                         "clippy --tests"
                         nil
                         nil
                         cargo-process--command-clippy--additional-args))

 (transient-define-prefix g/rust-transient-tests ()
   ["cargo test"
    ("t" "all file" cargo-process-current-file-tests)
    ("T" "current test" cargo-process-current-test)
    ("a" "all" cargo-process-test)
    ("c" "clippy tests" cargo-process-clippy-tests)])

 (transient-define-prefix g/rust-transient ()
   ["cargo"
    ("C" "clean" cargo-process-clean)
    ("r" "run" cargo-process-run)
    ("b" "build" cargo-process-build)
    ("SPC" "check" cargo-process-check)
    ("t" "test" g/rust-transient-tests)]
   ["yas"
    ("c" "complete" consult-yasnippet)])
 )


(on-linux
 (g/up go-ts-mode :ensure nil
   :mode ("\\.go\\'" . go-ts-mode))

 (g/up protobuf-ts-mode
   :mode ("\\.proto\\'" . protobuf-ts-mode))
 )

(on-mac
 (g/up protobuf-mode
   :mode ("\\.proto\\'" . protobuf-mode)))

(on-linux-or-mac
 (g/up eglot :ensure nil
   :bind (:map tab-map ("o" . g/eglot-transient))
   :init

   (defalias 'eg 'eglot)

   ;; enable by default on high memory machines
   (when (> (string-to-number (shell-command-to-string "free -m | awk '/^Mem/ {print $2}'")) 32000)
     (add-hook 'python-mode-hook 'eglot-ensure)
     (add-hook 'python-ts-mode-hook 'eglot-ensure)
     (add-hook 'c++-mode-hook 'eglot-ensure)
     (add-hook 'c++-ts-mode 'eglot-ensure)
     (add-hook 'c-mode-hook 'eglot-ensure)
     (add-hook 'c-ts-mode-hook 'eglot-ensure)

     (add-hook 'go-ts-mode-hook 'eglot-ensure)

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

   (defun prettify-or-eglot-format-buffer ()
     (interactive)
     (cond
      ((derived-mode-p 'typescript-mode 'typescript-ts-mode 'js-mode 'js-jsx-mode 'tsx-mode)
       (prettier-prettify))
      ((derived-mode-p 'emacs-lisp-mode)
       (indent-region (point-min) (point-max)))
      (t
       (eglot-format-buffer))))


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

(on-windows
 (g/up powershell :mode ("\\.ps1\\'" . powershell-mode))
 (setq c-basic-offset identation-size c-default-style "linux")
 (setq tab-width identation-size indent-tabs-mode nil))

(g/up yasnippet
  :ensure consult-yasnippet
  :ensure yasnippet-snippets
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(g/up yaml-mode :mode ("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode))

(g/up json-js-mode :ensure nil :mode ("\\.json\\'" . json-ts-mode))

(on-linux
 (g/up prettier)

 (g/up js-ts-mode :ensure nil
   :mode (("\\.js\\'" . js-ts-mode)
          ("\\.tsx\\'" . tsx-ts-mode)
          ("\\.ts\\'" . typescript-ts-mode))
   :config
   (dolist (hook '(js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook))
     (add-hook hook
               (lambda()
                 (progn
                   (setq indent-tabs-mode nil)
                   (setq js-indent-level 2)
                   (setq tab-width 2)
                   )
                 )))))

(on-linux-or-mac (g/up npm))

(on-windows
 (g/up dockerfile-mode
   :mode ("[Dd]ockerfile\\'" . dockerfile-mode)))

(on-mac
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

(g/up cmake-ts-mode
  :mode ("\\CMakeLists.txt$" . cmake-ts-mode))

(g/up makefile-mode :ensure nil
  :mode (("[Mm]ake[Ff]ile\\'" . makefile-mode)
         ("\\Makefile.conf\\'" . makefile-mode)
         ("\\.mak\\'" . makefile-mode)))

(on-windows
 (g/up bat-mode :ensure nil
   :mode (("\\.bat\\'" . bat-mode)
          ("\\.cmd\\'" . bat-mode))))

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
          ("\\.cc\\'" . c-ts-mode))
   :config
   (add-hook 'c-ts-mode-hook
             (lambda()
               (setq comment-start "//" comment-end  ""))))


 (g/up c++-ts-mode :ensure nil
   :mode (("\\.cpp\\'" . c++-ts-mode)
          ("\\.h\\'" . c++-ts-mode)
          ("\\.hh\\'" . c++-ts-mode)
          ("\\.hpp\\'" . c++-ts-mode)
          ("\\.mc\\'" . c++-ts-mode))
   :config


   (setq read-process-output-max (* 4 1024 1024))
   (setq process-adaptive-read-buffering nil)

   (transient-define-prefix g/c++-transient ()
     ["compile"
      ("r" "compile and run"
       (lambda ()
         (interactive)
         (let ((command (concat "clang++ -lgtest -lgtest_main -pthread -g -O0 -std=c++20 -o "
                                (file-name-sans-extension (buffer-file-name))
                                " "
                                (buffer-file-name)
                                " && "
                                (file-name-sans-extension (buffer-file-name))
                                " 2>&1")))
           (compile command)))
       )

      ("A" "compile and run + ASAN"
       (lambda () (interactive) (compile (concat "clang++ -lgtest -lgtest_main -pthread -fsanitize=address -std=c++23 -o " (file-name-sans-extension (buffer-file-name)) " " (buffer-file-name) " && " (file-name-sans-extension (buffer-file-name))))
         )

       )

      ("R" "compile and run all checks"
       (lambda () (interactive) (compile (concat "clang++ -std=c++20 -Wall -Wextra -Werror -o " (file-name-sans-extension (buffer-file-name)) " " (buffer-file-name) " && " (file-name-sans-extension (buffer-file-name))))
         )

       )
      ]

     ["yas"
      ("c" "complete" consult-yasnippet)])

   )

 )

(use-package project
  :init
  (setq project-vc-extra-root-markers '("Cargo.toml")))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; create major mode for editing .g1 files that extends emacs-lisp-mode
(define-derived-mode g1-mode emacs-lisp-mode "g1"
  "Major mode for editing .g1 files"
  ;; (setq-local comment-start "#")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+\\s-*")
  )


(transient-define-prefix g/g1-transient ()
  ["eval"
   ("j" "run file"
    (lambda () (interactive) (g/g1-run-file t)))
   ("J" "run file release"
    (lambda () (interactive) (g/g1-run-file nil)))
   ("t" "run g1 tests"
    (lambda () (interactive) (shell-command (concat "cd /home/giedrius/prog/g1 && ./run-tests.sh "))))
   ]
  ["yas"
   ("c" "complete" consult-yasnippet)])

(defun g/g1-run-file (&optional debug)
  (interactive)
  (let ((file (buffer-file-name)))
    (if debug
        (async-shell-command (concat "cd /home/giedrius/prog/GITHUB_GiedriusJ1/g1 && cargo run " file))
      (async-shell-command (concat "cd /home/giedrius/prog/GITHUB_GiedriusJ1/g1 && cargo run --release " file)))
    ;; (switch-to-buffer-other-window "*Async Shell Command Output*")
    ))

;; make sure .g1 files are opened in g1-mode
(add-to-list 'auto-mode-alist '("\\.g1\\'" . g1-mode))

(g/up elisp-mode :ensure nil
  :mode
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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(defun g/comp-mode()
  (interactive)
  ;; enable eglot
  (eglot-ensure)

  ;; disable copilot-mode
  (copilot-mode -1))
