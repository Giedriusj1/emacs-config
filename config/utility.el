;;; -*- lexical-binding: t -*-

;; lisp playground
(define-key lisp-playground-map (kbd "SPC") 'load-listp-playground)

(i-defun g/recompile-custom-packages ()
  (byte-recompile-directory "~/.emacs.d/custom-packages" 0))

(i-defun g/recompile-config ()
  (byte-recompile-directory "~/.emacs.d/config" 0))

(i-defun g/rel () (load-file "~/.emacs.d/init.el"))

(i-defun g/compile-elpa-and-custom-to-native ()
  (progn
    (g/recompile-custom-packages)
    (g/recompile-config)
    (native-compile-async '("~/.emacs.d/custom-packages"
                            "~/.emacs.d/elpa"
                            "~/.emacs.d/config"
                            )'recursively)))

(i-defun emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (let ((str (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str) str)))

(i-defun g/open-in-external-app ()
  (let ((fileList (cond ((string-equal major-mode "dired-mode")
                         (dired-get-marked-files))
                        (t (list (buffer-file-name))))))
    (cond-linux-win-mac
     (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path))) fileList)
     (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t))) fileList)
     (mapc (lambda (path) (shell-command (format "open \"%s\"" path))) fileList))))


(setq nxml-child-indent 4 nxml-attribute-indent 4)
(i-defun reformat-xml ()
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(write-region "something something" nil "~/ttt")


(i-defun g/set-github-key-personal()
  (write-region "# personal key
Host github.com
	HostName github.com
	User git
	IdentityFile ~/.ssh/id_rsa_github_personal" nil "~/.ssh/config"))


(i-defun g/set-github-key-normal()
  (write-region "# default (work) key
Host github.com
	HostName github.com
	User git
	IdentityFile ~/.ssh/id_rsa" nil "~/.ssh/config"))

(i-defun generate-password ()
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$&_+[]:.?/")
        (password ""))
    (dotimes (i 32 password)
      (setq password (concat password (char-to-string (elt chars (random (length chars)))))))

    (message password)))

(i-defun google-selected-text ()
  (let ((selected-text (if (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (current-word))))
    (let ((modified-text (read-from-minibuffer "Modify text (or press C-g to cancel): " selected-text)))
      (if (equal modified-text "")
          (message "Google search cancelled.")
        (browse-url (concat "https://www.google.com/search?q=" (url-hexify-string modified-text)))))))
