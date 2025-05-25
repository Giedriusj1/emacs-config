;;; -*- lexical-binding: t -*-

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
                   "magit.el"
                   "org.el"
                   "private-sync.el"
                   "programming.el"
                   "utility.el"
                   "ai.el"))

  (measure-time (load (concat "~/.emacs.d/config/" element))))

(if (file-exists-p "~/private-sync/private.el")
    (load "~/private-sync/private.el"))

(message (concat (format "Emacs took %.2f seconds to start" (float-time (time-subtract after-init-time before-init-time)))
                 (if (fboundp 'native-compile-async) " With native compiler!")))
