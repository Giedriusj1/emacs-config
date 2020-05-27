(message "early-init.el: starting early init stage")

;; Increase garbage collector threshold for better performance
(setq gc-cons-threshold most-positive-fixnum)

(setq package-quickstart t)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(modify-all-frames-parameters '((vertical-scroll-bars . nil)))

(advice-add 'x-apply-session-resources :override 'ignore)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))
