;;; magit-log-author-colors.el --- Color Magit log authors -*- lexical-binding: t -*-

(require 'subr-x)

(declare-function magit--propertize-face "magit-section" (string face))

(defgroup magit-log-author-colors nil
  "Color author names in Magit log margins."
  :group 'magit-log)

(defcustom magit-log-author-colors nil
  "Alist of author substrings and foreground colors.
Each entry has the form (AUTHOR-SUBSTRING . COLOR).  When an author
name in `magit-log-mode' contains AUTHOR-SUBSTRING, that matching part
is displayed using COLOR."
  :type '(alist :key-type string :value-type color)
  :group 'magit-log-author-colors)

(defun magit-log-author-colors--propertize-author (author)
  "Return AUTHOR with `magit-log-author' and configured color properties."
  (let ((author (propertize (or author "")
                            'face 'magit-log-author
                            'font-lock-face 'magit-log-author)))
    (pcase-dolist (`(,substring . ,color) magit-log-author-colors)
      (let ((start 0)
            (case-fold-search nil))
        (while (and (stringp substring)
                    (not (string-empty-p substring))
                    (string-match (regexp-quote substring) author start))
          (add-face-text-property (match-beginning 0)
                                  (match-end 0)
                                  `(:foreground ,color :weight bold)
                                  nil
                                  author)
          (setq start (match-end 0)))))
    author))

(defun magit-log-author-colors--propertize-face (orig-fn string face)
  "Apply configured author colors around ORIG-FN for Magit log margins."
  (if (and (derived-mode-p 'magit-log-mode)
           (eq face 'magit-log-author))
      (magit-log-author-colors--propertize-author string)
    (funcall orig-fn string face)))

;;;###autoload
(define-minor-mode magit-log-author-colors-mode
  "Color configured author substrings in `magit-log-mode' margins."
  :global t
  :group 'magit-log-author-colors
  (if magit-log-author-colors-mode
      (progn
        (advice-remove #'magit--propertize-face
                       #'magit-log-author-colors--propertize-face)
        (advice-add #'magit--propertize-face
                    :around
                    #'magit-log-author-colors--propertize-face
                    '((name . magit-log-author-colors-mode))))
    (advice-remove #'magit--propertize-face
                   #'magit-log-author-colors--propertize-face)))

(provide 'magit-log-author-colors)

;;; magit-log-author-colors.el ends here
