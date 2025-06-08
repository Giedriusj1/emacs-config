;;; -*- lexical-binding: t -*-

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
    (dotimes (i 32)
      (setq password (concat password (char-to-string (elt chars (random (length chars)))))))
    (insert password)))

(i-defun generate-password ()
  (let* ((letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (digits "0123456789")
         (specials "!@#$&_+[]:.?/")
         (password-len 20)
         (num-digits 3)
         (num-specials 3)
         (num-letters (- password-len num-digits num-specials))
         (chars '()))

    ;; Add mostly letters
    (dotimes (_ num-letters)
      (push (elt letters (random (length letters))) chars))

    ;; Add some digits
    (dotimes (_ num-digits)
      (push (elt digits (random (length digits))) chars))

    ;; Add some special characters
    (dotimes (_ num-specials)
      (push (elt specials (random (length specials))) chars))

    ;; Fisher–Yates shuffle
    (let ((n (length chars)))
      (dotimes (i n)
        (let* ((j (+ i (random (- n i))))
               (tmp (nth i chars)))
          (setcar (nthcdr i chars) (nth j chars))
          (setcar (nthcdr j chars) tmp))))

    ;; Insert password
    (insert (concat chars))))


(i-defun google-selected-text ()
  (let ((selected-text (if (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (current-word))))
    (let ((modified-text (read-from-minibuffer "Modify text (or press C-g to cancel): " selected-text)))
      (if (equal modified-text "")
          (message "Google search cancelled.")
        (browse-url (concat "https://www.google.com/search?q=" (url-hexify-string modified-text)))))))

(defun create-shell-here ()
  (interactive)
  (let* ((dir default-directory)
         (shell-name (format "*shell* <%s>" dir))
         (shell-buffer (get-buffer shell-name)))
    (if shell-buffer
        (switch-to-buffer shell-buffer)
      (shell (generate-new-buffer-name shell-name)))))

(bind-keys* ( "C-`" . create-shell-here))

(on-linux
 (i-defun g/cargo-new-without-git ()
   (unless (derived-mode-p 'dired-mode)
     (error "This command must be run from a Dired buffer"))
   (let ((dir (dired-current-directory)))
     ;; Run cargo init --vcs none in the directory.
     (shell-command (format "cd %s && cargo init --vcs none"
                            (shell-quote-argument dir)))
     ;; Create .gitignore file with the content "target/".
     (with-temp-file (expand-file-name ".gitignore" dir)
       (insert "target/\n"))
     ;; Refresh the Dired buffer.
     (revert-buffer)
     (message "Initialized Cargo project without Git in %s" dir)))


 (i-defun g/translate-rus-to-en()
   (progn (g/up google-translate)

          (setq google-translate-default-target-language "en")
          (setq google-translate-default-source-language "ru")

          (google-translate-at-point)

          (setq google-translate-default-target-language nil)
          (setq google-translate-default-source-language nil)))

 (i-defun g/org-publish-all-force ()
   (progn (g/up htmlize)
          (require 'htmlize)

          (require 'ox-publish)
          ;; default one would pick source colours from my current theme...
          (setq org-html-htmlize-output-type 'css)

          (setq org-publish-project-alist
                '(("org-blog"
                   :base-directory "~/private-sync/blog/"
                   :base-extension "org"
                   :publishing-directory "~/prog/giedriusj1.github.io"
                   :recursive t
                   :publishing-function org-html-publish-to-html
                   :headline-levels 4             ; Just the default for this project.
                   :auto-preamble nil )
                  ("org-static"
                   :base-directory "~/private-sync/blog/"
                   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                   :publishing-directory "~/prog/giedriusj1.github.io"
                   :recursive t
                   :publishing-function org-publish-attachment)))

          (org-publish-all t))))
