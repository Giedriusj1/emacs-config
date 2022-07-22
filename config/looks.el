(message "looks.el : starting looks stage")

(setq current-font "normal")

(defun set-windows-font (mode)
  (cond ((string= mode "normal") (set-face-attribute 'default nil :font "Consolas" :height 100))
	    ((string= mode "medium") (set-face-attribute 'default nil :font "Consolas" :height 110))
	    ((string= mode "4k") (set-face-attribute 'default nil :font "Consolas" :height 120))))

(defun set-linux-font (mode)
  (cond
   ((string= mode "normal") (set-face-attribute 'default nil :font "Monospace" :height 100))
   ((string= mode "medium") (set-face-attribute 'default nil :font "Monospace" :height 120))
   ((string= mode "4k") (set-face-attribute 'default nil :font "Monospace" :height 140))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5))))

(defun set-font (mode)
  (progn
    (setq current-font mode)
    (cond-linux-win-mac
     (set-linux-font mode)
     (set-windows-font mode)
     (message "noo wayy"))))

(defun toggle-font-size()
  (interactive)
  (cond ((string= current-font "normal")
	     (set-font "medium"))
	    ((string= current-font "medium")
	     (set-font "4k"))
	    ((string= current-font "4k")
	     (set-font "normal"))))

;; Default font size
(defun set-font-normal ()
  (interactive)
  (set-font "normal"))

;; Larger than normal, but not as big as 4k
(defun set-font-medium ()
  (interactive)
  (set-font "medium"))

;; Sets font size to something that's usable under 4k monitor
(defun set-font-4k ()
  (interactive)
  (set-font "4k"))

(defun is-4k-monitor ()
  (and (<= 2840 (x-display-pixel-width)) (<= 2160 (x-display-pixel-height))))

(defun set-font-for-current-resolution ()
  (interactive)
  (cond ((is-4k-monitor) (set-font-normal))
	    (t (set-font-normal))))

(defun load-graphic-settings (&optional _a)

  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)
  
  (customize-set-variable 'custom-enabled-themes '(wombat))

  (customize-set-variable
   'custom-safe-themes '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight demibold :foreground "DarkOrange3" :height 1.2))))
   '(org-level-2 ((t (:inherit outline-2 :weight demibold :foreground "darkOliveGreen3" :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :weight demibold :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :weight demibold :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :weight demibold :height 1.0))))

   '(default ((t (:background "#131818"))))
   '(cursor ((t (:background "OrangeRed"))))
   '(mode-line-inactive ((t (:background "#101010"))))
   '(mode-line ((t (:background "#404040"))))
   '(hl-line ((t (:inherit nil :background "#222222"))))
   '(minibuffer-prompt ((t (:foreground "#ff584d"))))

   ;; Make some default wombat colours a bit more lively
   '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#ff685d"))))
   '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#ff685d"))))

   ;; Some default helm faces are quite ugly... let's fix em up.
   '(helm-selection ((t (:background "grey24" :distant-foreground "black"))))
   '(helm-buffer-directory ((t (:weight bold :foreground "LightSlateBlue" :distant-foreground "black"))))
   '(helm-ff-directory ((t :inherit helm-buffer-directory )))
   '(helm-source-header ((t (:background "#450a6b" :foreground "#dddddd" :weight bold :height 1.3 :family "Sans Serif")))))

  (set-font-for-current-resolution)

  (global-hl-line-mode t)

  (use-package powerline :ensure t
    :config
    (powerline-default-theme)
    (set-face-attribute 'mode-line nil
    		            :foreground "Black"
    		            :background "DarkOrange3"
    		            :box nil)))

(add-hook 'after-make-frame-functions 'load-graphic-settings)

(when (display-graphic-p)
  (load-graphic-settings))

(use-package uniquify :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(defun generate-frame-title ()
  (if (>= 1 (length (tab-bar-tabs)))

      (format "%s %s" (buffer-name)
              (cond (buffer-file-truename (concat "(" buffer-file-truename ")"))
                    (dired-directory (concat "{" dired-directory "}"))
                    (t "[no file]")))

    (mapcar (lambda (tab)
              (interactive)
              (let ((tab-name (alist-get 'name tab)))
                (if (eq (car tab) 'current-tab)
                    (format " [ %s ] " tab-name)
                  (format  " %s "tab-name ))  ))
            (tab-bar-tabs))))

(setq-default frame-title-format '(:eval (generate-frame-title)))

(blink-cursor-mode -1)
