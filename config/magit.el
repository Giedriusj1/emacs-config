;;; -*- lexical-binding: t -*-

(g/up magit
  :init (define-key tab-map (kbd "m") 'g/magit-transient)
  (transient-define-prefix g/magit-transient ()
    ["show"
     ("l" "log" magit-log)
     ("m" "status" magit-status)
     ("D" "diff" magit-diff)
     ("b" "blame" magit-blame)
     ("r" "refs" magit-show-refs)]
    ["git"
     ("p" "pull" magit-pull)
     ("P" "push" magit-push)
     ("c" "commit" magit-commit)
     ("d" "diff-dwim" magit-diff-dwim)
     ]))

;; (on-linux
;;  (use-package difftastic
;;    :config
;;    (eval-after-load 'magit-diff
;;      '(transient-append-suffix 'magit-diff '(-1 -1)
;; 	[("D" "Difftastic diff (dwim)" difftastic-magit-diff)
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
