;; Increase garbage collector threshold for better performance
(setq gc-cons-threshold most-positive-fixnum)

;; Some basic macros that will be used throughout the config
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (let ((elapsed (float-time (time-since time))))
       (message "took %.06f seconds or %.03f milliseconds" elapsed (* 1000.0 elapsed)))))

(defmacro on-linux (&rest body)
  (if (memq system-type '(gnu gnu/linux))
      `(progn ,@body )))

(defmacro on-windows (&rest body)
  (if (memq system-type '(windows-nt ms-dos))
      `(progn ,@body )))

(defmacro cond-linux-win (linux windows)
  (cond ((memq system-type '(windows-nt ms-dos))
         `(progn ,windows))
        ((memq system-type '(gnu gnu/linux))
         `(progn ,linux))))

(defmacro cond-linux-win-mac (linux windows darwin)
  (cond ((memq system-type '(windows-nt ms-dos))
         `(progn ,windows))
        ((memq system-type '(gnu gnu/linux))
         `(progn ,linux))
        ((memq system-type '(darwin))
         `(progn ,darwin))))

(defmacro i-defun (name arglist &rest body)
  (declare (indent defun))
  `(defun ,name ,arglist
     (interactive)
     ,@body))

(defmacro i-lambda (arglist &rest body)
  (declare (indent defun))
  `(lambda ,arglist
     (interactive)
     ,@body))

;; Disable unneeded UI elements
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(pixel-scroll-precision-mode)

;; General UI settings
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

(customize-set-variable 'custom-enabled-themes '(wombat))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :weight demibold :height 1.2))))
 '(default ((t (:background "#131818"))))
 '(cursor-orig ((t (:inherit cursor))))
 '(mode-line-inactive ((t (:background "#101010"))))
 '(mode-line ((t (:background "#404040"))))
 '(hl-line ((t (:inherit nil :background "#222222"))))
 '(minibuffer-prompt ((t (:foreground "#ff584d"))))

 ;; Make some default wombat colours a bit more lively
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#ff685d"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#ff685d")))))

(global-hl-line-mode t)
(fringe-mode 0)
(tooltip-mode -1)
(blink-cursor-mode -1)
(setq tab-bar-show -1)
(tab-bar-mode)

(setq-default frame-title-format
              '(:eval
                (if (>= 1 (length (tab-bar-tabs)))
                    (format "%s %s" (buffer-name)
                            (cond (buffer-file-truename (concat "(" buffer-file-truename ")"))
                                  (dired-directory (concat "{" dired-directory "}"))
                                  (t "[no file]")))
                  (mapcar (i-lambda (tab)
			    (let ((tab-name (alist-get 'name tab)))
			      (if (eq (car tab) 'current-tab)
				  (format " [ %s ☀️ ] " tab-name)
				(format  " %s "tab-name ))  ))
			  (tab-bar-tabs)))))

(modify-all-frames-parameters '((vertical-scroll-bars . nil)))

(setq g/message-buff (generate-new-buffer "G/Messages"))

(defun g/message (msg)
  (with-current-buffer "G/Messages" ; replace with the name of the buffer you want to append
    (goto-char (point-max))
    (insert (format "%s %s\n"
                    (format-time-string "%d/%m/%Y - %H:%M:%S")
                    msg) )))

(g/message "Emacs booting up")
(g/message (emacs-version))

(setq package-quickstart t)

(advice-add 'x-apply-session-resources :override 'ignore)

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

(fset 'display-startup-echo-area-message 'ignore)

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))

(setq warning-suppress-types '(((package reinitialization)) (comp) (emacs)))

;;Let's garbage collect when focusing out of the window..
(add-hook 'focus-out-hook #'garbage-collect)
;;  and saving files.
(add-hook 'after-save-hook #'garbage-collect)

(run-with-idle-timer 5 t (lambda () (garbage-collect)))


;; add hook to increase frame size on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
