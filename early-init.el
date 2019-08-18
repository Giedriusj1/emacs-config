(message "early-init.el: starting early init stage")

;; Increase garbage collector threshold for better performance
(setq gc-cons-threshold most-positive-fixnum)

(setq package-quickstart t)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
