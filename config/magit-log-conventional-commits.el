;;; magit-log-conventional-commits.el --- Highlight conventional commit titles -*- lexical-binding: t -*-

(require 'subr-x)

(declare-function magit--add-face-text-property "magit-section"
                  (beg end face &optional append object adopt-face))

(defgroup magit-log-conventional-commits nil
  "Highlight conventional commit prefixes in Magit log summaries."
  :group 'magit-log)

(defface magit-log-conventional-commit-type
  '((((class color) (background light)) :foreground "#6f42c1" :weight bold)
    (((class color) (background dark)) :foreground "#c792ea" :weight bold))
  "Face used for conventional commit types."
  :group 'magit-log-conventional-commits)

(defface magit-log-conventional-commit-scope
  '((((class color) (background light)) :foreground "#a3a3a3")
    (((class color) (background dark)) :foreground "#a3a3a3"))
  "Face used for conventional commit scopes."
  :group 'magit-log-conventional-commits)

(defface magit-log-conventional-commit-breaking
  '((((class color) (background light)) :foreground "#cf222e" :weight bold)
    (((class color) (background dark)) :foreground "#ff6b6b" :weight bold))
  "Face used for conventional commit breaking-change markers."
  :group 'magit-log-conventional-commits)

(defface magit-log-conventional-commit-separator
  '((t :inherit shadow))
  "Face used for conventional commit separators."
  :group 'magit-log-conventional-commits)

(defcustom magit-log-conventional-commit-type-colors
  '(("feat" . "#b9f27c")
    ("fix" . "#ff685d")
    ("docs" . "#82aaff")
    ("style" . "#c792ea")
    ("refactor" . "#ffd166")
    ("perf" . "#ffb86c")
    ("test" . "#7ee7d7")
    ("build" . "#89ddff")
    ("ci" . "#60d5ff")
    ("chore" . "#a6accd")
    ("revert" . "#f07178"))
  "Alist of conventional commit types and foreground colors."
  :type '(alist :key-type string :value-type color)
  :group 'magit-log-conventional-commits)

(defconst magit-log-conventional-commits--regexp
  "\\`\\([[:alpha:]][[:alnum:]-]*\\)\\(([^)[:cntrl:]]+)\\)?\\(!\\)?\\(:\\) "
  "Regexp matching a conventional commit prefix.")

(defun magit-log-conventional-commits--type-face (type)
  "Return face for conventional commit TYPE."
  (if-let ((color (cdr (assoc type magit-log-conventional-commit-type-colors))))
      `(:inherit magit-log-conventional-commit-type :foreground ,color)
    'magit-log-conventional-commit-type))

(defun magit-log-conventional-commits--highlight-summary ()
  "Highlight the conventional commit prefix in the current summary buffer."
  (goto-char (point-min))
  (when (looking-at magit-log-conventional-commits--regexp)
    (magit--add-face-text-property (match-beginning 1)
                                   (match-end 1)
                                   (magit-log-conventional-commits--type-face
                                    (match-string 1))
                                   'append)
    (when (match-beginning 2)
      (magit--add-face-text-property (match-beginning 2)
                                     (match-end 2)
                                     'magit-log-conventional-commit-scope
                                     'append))
    (when (match-beginning 3)
      (magit--add-face-text-property (match-beginning 3)
                                     (match-end 3)
                                     'magit-log-conventional-commit-breaking
                                     'append))
    (magit--add-face-text-property (match-beginning 4)
                                   (match-end 4)
                                   'magit-log-conventional-commit-separator
                                   'append)))

;;;###autoload
(define-minor-mode magit-log-conventional-commits-mode
  "Highlight conventional commit prefixes in `magit-log-mode' titles."
  :global t
  :group 'magit-log-conventional-commits
  (if magit-log-conventional-commits-mode
      (add-hook 'magit-log-wash-summary-hook
                #'magit-log-conventional-commits--highlight-summary)
    (remove-hook 'magit-log-wash-summary-hook
                 #'magit-log-conventional-commits--highlight-summary)))

(provide 'magit-log-conventional-commits)

;;; magit-log-conventional-commits.el ends here
