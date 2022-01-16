;;; -*- lexical-binding: t -*-

(use-package password-generator :defer t)

(defun g/recompile-custom-packages ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/custom-packages" 0))

(defun g/recompile-config ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/config" 0))

(defun g/recompile-helm-tree-sitter ()
  (interactive)
  (byte-force-recompile "~/.emacs.d/config/helm-tree-sitter"))


(defun g/reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (interactive)
  (let ((str (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str) str)))

(defun g/open-in-external-app ()
  (interactive)
  (let ((fileList (cond ((string-equal major-mode "dired-mode")
                         (dired-get-marked-files))
                        (t (list (buffer-file-name))))))
    (cond ((string-equal system-type "windows-nt")
           (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t))) fileList))
          ((string-equal system-type "darwin")
           (mapc (lambda (path) (shell-command (format "open \"%s\"" path))) fileList))
          ((string-equal system-type "gnu/linux")
           (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path))) fileList)))))


(setq nxml-child-indent 4 nxml-attribute-indent 4)
(defun reformat-xml ()
  (interactive)
  ;;todo: this only works in xml-mode, we should spit out an error if we are not

  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

;; to check if the function is natively compiled
;; (subr-native-comp-unit (symbol-function 'org-mode))
