;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; On Windows system we'll just ignore signatures altogether...
;; too much hassle.
(on-windows (setq package-check-signature nil)
	    (unless (package-installed-p 'use-package)
	      (progn (package-refresh-contents)
		     (package-install 'use-package))))

(require 'use-package)

;; Make sure we install any packages that aren't on the system.
(setq use-package-always-ensure t
      use-package-always-defer t)

;; This defines in which order we want to load our config.
(setq basic-load-sequence '("key-bindings.el" ; Key bindings are also needed early, for prefixes
                            "behaviour.el"
                            "dired.el"
                            "magit.el"
                            "org.el"
                            "private-sync.el"
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

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (message (concat "emacs uptime: " (emacs-uptime)))))

(message (concat "Emacs took " (emacs-init-time) " seconds to start."
		 (if (fboundp 'native-compile-async) " With native compiler!")))
