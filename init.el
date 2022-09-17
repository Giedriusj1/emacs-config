;;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/custom-packages")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; Make sure we have use-package installed.
;; All other packages will be installed by it.
(unless (package-installed-p 'use-package)
  (progn (package-refresh-contents)
         (package-install 'use-package)))

;; Make sure we install any packages that aren't on the system.
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(setq warning-suppress-types '(((package reinitialization)) (comp) (lsp-mode) (emacs)))

(require 'use-package)

(use-package gcmh :demand :diminish
  :config
  (gcmh-mode t)

  ;;Let's garbage collect when focusing out of the window..
  (add-hook 'focus-out-hook #'garbage-collect)
  ;;  and saving files.
  (add-hook 'after-save-hook #'garbage-collect))

;; This defines in which order we want to load our config.
(setq basic-load-sequence '("looks.el"        ; We want looks ASAP, to reduce any flickering
                            "key-bindings.el" ; Key bindings are also needed early, for prefixes
                            "behaviour.el"    ; We need hydra macros
                            "dired.el"
                            "magit.el"
                            "org.el"
                            "projectile.el"
                            "private-sync.el"
                            "quickopen.el"
                            "swift.el"))

;; Let's load the files specified in the basic load sequence:
(dolist (element basic-load-sequence)
  (measure-time (load (concat "~/.emacs.d/config/" element))))

;; Now let's load the remaining stuff:
(dolist (element (seq-filter (lambda (item)
                               (and (string-match-p ".el$" item)
                                    (not (member item basic-load-sequence))))
                             (directory-files "~/.emacs.d/config")))
  (measure-time
   (load (concat "~/.emacs.d/config/" element))))

(fset 'display-startup-echo-area-message 'ignore)

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (message (concat "emacs uptime: " (emacs-uptime)))))

(message (concat "Emacs took " (emacs-init-time) " seconds to start."
		 (if (fboundp 'native-compile-async) " With native compiler!")))
