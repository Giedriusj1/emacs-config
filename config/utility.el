(use-package password-generator :defer t)

(defun recompile-custom-packages ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/custom-packages" 0))

(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (interactive)
  (let ((str (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str) str)))


(setq nxml-child-indent 4 nxml-attribute-indent 4)
(defun reformat-xml ()
  (interactive)
  ;;todo: this only works in xml-mode, we should spit out an error if we are not

  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))
