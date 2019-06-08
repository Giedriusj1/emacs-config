;; Increase garbage collector threshold for better performance
(setq gc-cons-threshold (* 5 gc-cons-threshold))

(setq package-quickstart t)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(message "from very early")
