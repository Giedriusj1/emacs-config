;;; -*- lexical-binding: t -*-

(if (member "Iosevka" (font-family-list))
    (if (and (eq (display-pixel-width) 1920) (eq (display-pixel-height) 1080))
	;; larger font for 1080p
	(custom-set-faces '(default ((t (:background "#131818"  :font "Iosevka-12")))))
      (custom-set-faces '(default ((t (:background "#131818"  :font "Iosevka-8"))))))
  (custom-set-faces '(default ((t (:background "#131818"))))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; On Windows system we'll just ignore signatures altogether...
;; too much hassle.
(on-windows (setq package-check-signature nil)
	    (unless (package-installed-p 'use-package)
	      (progn (package-refresh-contents)
		     (package-install 'use-package))))

(require 'use-package) (defalias 'g/up 'use-package)

;; Make sure we install any packages that aren't on the system.
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Let's load our config
(dolist (element '("key-bindings.el" ; Needed early for prefixes
                   "behaviour.el"
                   "dired.el"
                   "magit.el"
                   "org.el"
                   "private-sync.el"
		   "lsp.el"
		   "programming.el"
		   "sn-modes.el"
		   "utility.el"
		   "copilot.el"))

  (measure-time (load (concat "~/.emacs.d/config/" element))))

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (message (concat "emacs uptime: " (emacs-uptime)))))

;; load ~/.emacs.d/config/private.el
(if (file-exists-p "~/private-sync/private.el")
    (load "~/private-sync/private.el"))

(message (concat (format "Emacs took %.2f seconds to start" (float-time (time-subtract after-init-time before-init-time)))
		 (if (fboundp 'native-compile-async) " With native compiler!")))
