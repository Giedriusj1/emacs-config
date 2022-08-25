;; Increase garbage collector threshold for better performance
(setq gc-cons-threshold most-positive-fixnum)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(tooltip-mode -1)
(blink-cursor-mode -1)

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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))


;; Some basic macros that will be used throughout the config
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "took %.06f seconds" (float-time (time-since time)))))  

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
