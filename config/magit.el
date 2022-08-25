(define-key tab-map (kbd "m") 'magit-hydra/body)

(use-package magit
  :pretty-hydra ((:color blue)
		 ("show"
		  (("l" magit-log "log")
		   ("m" magit-status "status")
		   ("D" magit-diff "diff")
		   ("b" magit-blame "blame")
		   ("r" magit-show-refs "refs"))
		  "git"
		  (("p" magit-pull "pull")
		   ("P" magit-push "push")
		   ("c" magit-commit "commit")
		   ("d" magit-diff-dwim "diff-dwim")))))

(use-package ediff :ensure nil
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


(use-package git-modes)
