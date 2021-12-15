;;; -*- lexical-binding: t -*-
(message "init.el : starting init stage")

(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/custom-packages")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; On Windows system we'll just ignore signatures altogether...
;; too much hassle.
(cond ((string-equal system-type "windows-nt")
       (setq package-check-signature nil)))

;; Make sure we have use-package installed.
;; All other packages will be installed by it.
(unless (package-installed-p 'use-package)
  (progn (package-refresh-contents)
         (package-install 'use-package)))

;; Make sure we install any packages that aren't on the system.
(setq use-package-always-ensure t)

(setq warning-suppress-types '(((package reinitialization)) (comp)))

(use-package gcmh
  :diminish gcmh-mode
  :config
  (gcmh-mode 1)

  ;; (setq gcmh-verbose t)
  (setq gcmh-time-constant 10)

  ;;Let's garbage collect when focusing out of the window..
  (add-hook 'focus-out-hook #'garbage-collect)
  ;;  and saving files.
  (add-hook 'after-save-hook #'garbage-collect))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "took %.06f seconds" (float-time (time-since time)))))

;; This defines in which order we want to load our config.
(setq basic-load-sequence '("looks.el"        ; We want looks ASAP, to reduce any flickering
                            "key-bindings.el" ; Key bindings are also needed early, for prefixes
                            "behaviour.el"))

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
(message (concat "Emacs took " (emacs-init-time) " seconds to start."
		         (if (fboundp 'native-compile-async) " With native compiler!")))
