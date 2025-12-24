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

(defmacro g/load (file) `(measure-time (load ,file)))

(g/load "~/.emacs.d/extra-packages/feline") ; Needed early for the modeline

(dolist (element '("key-bindings.el" ; Needed early for prefixes
                   "behaviour.el"
                   "magit.el"
                   "org.el"
                   "private-sync.el"
                   "programming.el"
                   "utility.el"
                   "ai.el"))

  (g/load (concat "~/.emacs.d/config/" element)))

(on-mac
 (g/load "~/.emacs.d/extra-packages/exec-path-from-shell")
 (setq exec-path-from-shell-arguments '("-l"))
 (exec-path-from-shell-initialize))

(g/load-if-exists "~/.emacs.d/config/sql.el")

(message (concat (format "Emacs took %.2f seconds to start" (float-time (time-subtract after-init-time before-init-time)))
                 (if (fboundp 'native-compile-async) " With native compiler!")))
