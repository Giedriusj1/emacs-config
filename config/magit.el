;;; -*- lexical-binding: t -*-

(g/up magit
  :init (define-key tab-map (kbd "m") 'g/magit-transient)
  (transient-define-prefix g/magit-transient ()
    [" magit 🐈‍⬛"
     [("m" "status" magit-status)
      ("d" "dispatch" magit-dispatch)
      ("D" "dispatch file" magit-file-dispatch)
      ("b" "blame" magit-blame-addition)]
     [("p" "pull" magit-pull)
      ("P" "push" magit-push)]
     [("l" "log" magit-log)]
     [("r" "refs" magit-show-refs)
      ("o" "diff vs origin/main" g/magit-diff-origin-main)]])

  :commands (magit-diff-range)

  :custom
  ;; (magit-refs-show-commit-count nil)
  (magit-refs-pad-commit-counts t)

  :config
  (defface g/magit-log-own-commit
    '((((class color) (background light)) :foreground "#4f6bb3")
      (((class color) (background dark)) :foreground "#9fbdff"))
    "Face used for own commits in `magit-log-mode'.")

  (defvar-local g/magit-log-own-authors nil)

  (defun g/magit-diff-origin-main ()
    "Show diff between current branch and origin/main."
    (interactive)
    (magit-diff-range "origin/main...HEAD"))

  (defun g/magit-log-refresh-own-authors ()
    (setq-local g/magit-log-own-authors
                (delete-dups
                 (delq nil
                       (mapcar (lambda (name)
                                 (when (and (stringp name)
                                            (not (string= name "")))
                                   name))
                               (list (magit-get "author.name")
                                     (magit-get "user.name")
                                     user-full-name))))))

  (defun g/magit-log-own-author-p (author)
    (and (stringp author)
         (member author g/magit-log-own-authors)))

  (defun g/magit-log-line-author (style)
    (save-match-data
      (when (looking-at (pcase style
                          ('log        magit-log-heading-re)
                          ('cherry     magit-log-cherry-re)
                          ('module     magit-log-module-re)
                          ('reflog     magit-log-reflog-re)
                          ('stash      magit-log-stash-re)
                          ('bisect-vis magit-log-bisect-vis-re)
                          ('bisect-log magit-log-bisect-log-re)))
        (match-string-no-properties 5))))

  (defun g/magit-log-highlight-own-commits (orig-fn style abbrev)
    (let ((line-start (point))
          (author (g/magit-log-line-author style)))
      (prog1 (funcall orig-fn style abbrev)
        (when (and (derived-mode-p 'magit-log-mode)
                   (g/magit-log-own-author-p author))
          (save-excursion
            (goto-char line-start)
            (add-face-text-property line-start
                                    (line-end-position)
                                    'g/magit-log-own-commit
                                    'append))))))

  (add-hook 'magit-log-mode-hook #'g/magit-log-refresh-own-authors)

  (defun g/magit-refs-line-has-upstream-p (line)
    (let ((upstream (nth 7 line)))
      (and upstream
           (not (eq (get-text-property 0 'face upstream) 'error)))))

  (defun g/magit-refs-sort-local-branches-by-upstream (lines)
    "Place branches with an upstream before branches without one."
    (let (with-upstream without-upstream)
      (dolist (line lines)
        (if (g/magit-refs-line-has-upstream-p line)
            (push line with-upstream)
          (push line without-upstream)))
      (nconc (nreverse with-upstream)
             (nreverse without-upstream))))

  (advice-remove #'magit-refs--format-local-branches
                 #'g/magit-refs-sort-local-branches-by-upstream)
  (advice-add #'magit-refs--format-local-branches
              :filter-return
              #'g/magit-refs-sort-local-branches-by-upstream
              '((name . g/magit-refs-sort-local-branches-by-upstream)))

  (advice-remove #'magit-log-wash-rev
                 #'g/magit-log-highlight-own-commits)
  (advice-add #'magit-log-wash-rev
              :around
              #'g/magit-log-highlight-own-commits
              '((name . g/magit-log-highlight-own-commits))))

;; (on-linux
;;  (use-package difftastic
;;    :config
;;    (eval-after-load 'magit-diff
;;      '(transient-append-suffix 'magit-diff '(-1 -1)
;;      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;          ("S" "Difftastic show" difftastic-magit-show)]))))

(g/up ediff :ensure nil
  :ensure magit
  :config
  (dolist (face-map '((ediff-even-diff-A           . magit-diff-context-highlight)
                      (ediff-even-diff-Ancestor    . magit-diff-context)
                      (ediff-even-diff-B           . magit-diff-context-highlight)
                      (ediff-even-diff-C           . magit-diff-context-highlight)
                      (ediff-odd-diff-A            . magit-diff-context-highlight)
                      (ediff-odd-diff-Ancestor     . magit-diff-context)
                      (ediff-odd-diff-B            . magit-diff-context-highlight)
                      (ediff-odd-diff-C            . magit-diff-context-highlight)
                      (ediff-current-diff-A        . magit-diff-our)
                      (ediff-current-diff-Ancestor . magit-diff-base)
                      (ediff-current-diff-B        . magit-diff-their)
                      (ediff-fine-diff-A           . magit-diff-removed-highlight)
                      (ediff-fine-diff-Ancestor    . magit-diff-base-highlight)
                      (ediff-fine-diff-B           . magit-diff-added-highlight)))
    (let* ((face (car face-map))
           (alias (cdr face-map)))
      (put face 'theme-face nil)
      (put face 'face-alias alias)))

  ;; Setting this to t will only show two panes.
  ;; This set to nil can be useful when dealing wih merge conflicts.
  (setq magit-ediff-dwim-show-on-hunks t)

  ;; turn off whitespace checking:
  (setq ediff-diff-options "-w")

  ;; Don't spawn new window for ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; split window horizontally
  (setq ediff-split-window-function 'split-window-horizontally)

  ;; Since edif colours really don't play nicely with dark themes, we'll just overload them
  ;; with magit colours. (This hack is taken from https://github.com/bbatsov/solarized-emacs/issues/194)
  (dolist (entry '((ediff-current-diff-C . ((((class color) (background light))
                                             (:background "#DDEEFF" :foreground "#005588"))
                                            (((class color) (background dark))
                                             (:background "#005588" :foreground "#DDEEFF"))))
                   (ediff-fine-diff-C . ((((class color) (background light))
                                          (:background "#EEFFFF" :foreground "#006699"))
                                         (((class color) (background dark))
                                          (:background "#006699" :foreground "#EEFFFF"))))))
    (let ((face (car entry))
          (spec (cdr entry)))
      (put face 'theme-face nil)
      (face-spec-set face spec)))

  ;; This makes ediff usable with org mode
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all)))

(g/up git-modes)
