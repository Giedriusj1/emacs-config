;;; -*- lexical-binding: t -*-

(use-package dired-subtree :defer t)

(diminish 'dired-omit-mode)

(use-package dired :ensure nil :defer 5
  :config
  (require 'dired-x)

  (define-key dired-mode-map (kbd "(") (lambda ()
                                         (interactive)
                                         (if dired-hide-details-mode
                                             (dired-hide-details-mode -1)
                                           (dired-hide-details-mode ))

                                         (if dired-omit-mode
                                               (dired-omit-mode -1)
                                             (dired-omit-mode))))

  (define-key dired-mode-map (kbd "l") 'dired-up-directory)
  (define-key dired-mode-map (kbd "r") 'dired-do-redisplay)
  (define-key dired-mode-map (kbd "SPC") (lambda ()
                                           (interactive)
                                           (if dired-omit-mode
                                               (dired-omit-mode -1)
                                             (dired-omit-mode))))

  (define-key dired-mode-map (kbd "C-i") (lambda ()
                                           (interactive)
                                           (dired-subtree-cycle)
                                           (dired-omit-mode)))

  (setq dired-listing-switches "-alFh")

  (on-linux
   (use-package dired-toggle-sudo :ensure nil)
   (setq dired-listing-switches
         (concat dired-listing-switches " --group-directories-first -v")))

  (setq dired-dwim-target t)

  (add-hook 'dired-mode-hook
	        (lambda ()
	          (dired-hide-details-mode 1)
              (dired-omit-mode)))

  (set-face-foreground 'dired-directory "LightSlateBlue" )

  ;; Most of the above were taken from Thierry Volpiato's dired-extension.el
  (setq dired-font-lock-keywords
        (list
         ;; Marked files.
         ;; Allow copy/rename/sym/hard files to be marked also.
         (list ;(concat "^[" (char-to-string dired-marker-char) "]")
          (concat "^\\([^ " (char-to-string dired-del-marker) "]\\)")
          '(".+" nil nil (0 dired-marked-face))) ; Don't jump to filename to mark whole line.

         ;; Flagged files.
         (list (concat "^[" (char-to-string dired-del-marker) "]")
               '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))

         ;; Symbolic links.
         (list dired-re-sym ;"\\([^ ]+\\) -> [^ ]+$"
	           '(".+" (dired-move-to-filename) nil (0 dired-symlink-face)))

         ;; Flagged files or not yet saved (.# or #.#)
         (list "\\(^..*-\\).*\\( [0-9:]* \\)\\(\.?#.*#?\\)$" '(3 dired-symlink-face))

         ;; Directory headers.
         (list dired-subdir-regexp '(1 dired-header-face))

         ;; Dired marks. (C,D, etc... at beginning of line)
         (list dired-re-mark '(0 dired-mark-face))


         ;; Subdirectories.
         (list dired-re-dir
	           '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))

         ;; Files suffixed with `completion-ignored-extensions'.
         '(eval .
                ;; It is quicker to first find just an extension, then go back to the
                ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
                (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
	                  '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
         ;; plus a character put in by -F.
         '(eval .
                (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		                      "\\|#\\)[*=|]$")
	                  '(".+" (progn
		                       (end-of-line)
		                       ;; If the last character is not part of the filename,
		                       ;; move back to the start of the filename
		                       ;; so it can be fontified.
		                       ;; Otherwise, leave point at the end of the line;
		                       ;; that way, nothing is fontified.
		                       (unless (get-text-property (1- (point)) 'mouse-face)
		                         (dired-move-to-filename)))
	                    nil (0 dired-ignored-face))))

         ;; Regular file names.
         (list "\\(^..*-\\).*\\( [0-9:]* \\)\\(.*\\)$"
               '(".+" (dired-move-to-filename) nil (0 '((:foreground "yellow")))))
         ;; '(".+" (dired-move-to-filename) nil (0 '((:foreground "Dodgerblue3")))))

         ;; Filenames extensions.
         (list "[^ .]\\.\\([a-zA-Z]*\\)$" '(1 '((:foreground "green2")) t))

         ;; Executable flags (Use C-u s)
         (list "[^ .]\\([*]?$\\)" '(1 '((:foreground "red")) t))

         ;; Compressed filenames extensions.
         (list "[^ .]\\.\\([tz7]?[bgi]?[pzZ]2?\\)[*]?$" '(1 '((:foreground "yellow")) t))


         ;; Files that are group or world writable.
         (list (concat dired-re-maybe-mark dired-re-inode-size
        	           "\\([-d]\\(....w....\\|.......w.\\)\\)")
               '(1 dired-warning-face)
               '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
         )))
