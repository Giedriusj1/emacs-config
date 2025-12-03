;;; -*- lexical-binding: t -*-

(require 'bind-key)
(override-global-mode)

(define-prefix-command 'control-semi-map)
(define-prefix-command 'tab-map)

(on-mac (progn
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier 'super)))

(bind-keys*
 ("C-1" . window-swap-states)
 ("C-2" . windmove-up)
 ("C-3" . windmove-right)

 ("C-j" . consult-buffer)
 ("C-M-j" . consult-project-buffer)
 ("<C-tab>" . tab-bar-switch-to-next-tab)

 ("M-h" . open-line)
 ("M-," . backward-kill-word)
 ("M-." . kill-word)
 ("C-," . delete-backward-char)
 ("C-." . delete-char)
 ("C-d" . tab-map)
 ("C-;" . control-semi-map)
 ("<tab>" . tab-map)
 ("M-;" . tab-map)
 ("C-u" . backward-char)
 ("C-o" . forward-char)
 ("M-u" . backward-word)
 ("M-o" . forward-word)
 ("C-M-u" . backward-sexp)
 ("C-M-o" . forward-sexp)
 ("C-a" . Control-X-prefix)
 ("C-q" . beginning-of-line)
 ("C-w" . back-to-indentation)
 ("C-f" . cua-exchange-point-and-mark)
 ("C-v" . yank)

 ("<f10>" . toggle-truncate-lines)
 ("<f11>" . toggle-frame-fullscreen)
 ("<C-f12>" . whitespace-mode)

 ( "<up>" . scroll-down-line)
 ( "<down>" . scroll-up-line)
 ( "M-n" . forward-paragraph)
 ( "M-," . backward-kill-word)
 ( "<down>" . scroll-up-line)
 ( "C-M-SPC" . rectangle-mark-mode)

 ("<C-tab>" . tab-bar-switch-to-next-tab)
 ("<C-iso-lefttab>" . tab-bar-switch-to-prev-tab)
 ("<C-S-tab>" . tab-bar-switch-to-prev-tab)
 ("M-SPC" . g/frame-helper-transient))

(bind-keys
 ( "M-p" . backward-paragraph))

(bind-keys* :map control-semi-map
            ("SPC" . point-to-register)
            ("C-SPC" . point-to-register)
            ("j" . jump-to-register)
            ("h" . unhighlight-regexp)
            ("C-h" . highlight-symbol-at-point)
            ("q" . consult-goto-line)

            ("C-s" . consult-imenu)
            ("s" . consult-imenu-multi)

            ("C-j" . jump-to-register)
            ("C-q" . consult-goto-line)
            ("C-l" . execute-extended-command)
            ("C-2" . split-window-below))

(define-key control-semi-map (kbd "C-;") 'consult-line-empty)

(define-key control-semi-map (kbd "C-m") 'consult-line)

(define-key control-semi-map (kbd "C-2") 'split-window-below)

(define-key control-semi-map (kbd "C-3") 'split-window-right)

(define-key control-semi-map (kbd "C-0") 'delete-window)

(define-key control-semi-map (kbd "C-d") 'follow-mode)

(define-key control-semi-map (kbd "C-1") 'g-toggle-delete-other-windows)

(bind-keys* :map tab-map
            ("TAB" . comment-dwim)
            ("M- ." 'comment-dwim)
            ("C- ." 'comment-dwim)
            ("M- ." 'comment-dwim)
            ("u" . universal-argument)
            ("C-f" . find-file)
            ("C-d" . dired-jump))

(define-minor-mode swiftly-mode
  "Move around swiftly"
  :lighter " === SWIFT ==="
  :keymap (let ((map (make-sparse-keymap)))

            ;;disable all self inserting keys
            (suppress-keymap map t)

            ;; movement
            (define-key map (kbd "i") (i-lambda () (dotimes (bind 2)
                                                     (scroll-down-line)
                                                     (previous-line))))

            (define-key map (kbd "k") (i-lambda () (dotimes (bind 2)
                                                     (scroll-up-line)
                                                     (next-line))))

            (define-key map (kbd "o") (i-lambda () (progn
                                                     (scroll-down-line)
                                                     (previous-line))))

            (define-key map (kbd "l") (i-lambda () (progn
                                                     (scroll-up-line)
                                                     (next-line))))

            (define-key map (kbd "p") 'beginning-of-defun)
            (define-key map (kbd "n") 'end-of-defun)

            (define-key map (kbd "u") 'cua-scroll-down)

            (define-key map (kbd "j") 'cua-scroll-up)

            (define-key map (kbd "s") 'avy-goto-char-timer)

            ;; cua mode
            (define-key map (kbd "C-z") 'toggle-swiftly-mode)
            (define-key map (kbd "C-x") 'kill-region)
            (define-key map (kbd "C-c") 'kill-ring-save)
            (define-key map (kbd "C-v") 'yank)

            (define-key map (kbd "C-g") 'global-swiftly-mode)

            map)


  (if (eq global-swiftly-mode t)
      (custom-set-faces '(cursor ((t (:background "orange")))))
    (custom-set-faces '(cursor ((t (:inherit cursor-orig)))))))

(define-globalized-minor-mode global-swiftly-mode swiftly-mode swiftly-mode)

(define-key control-semi-map (kbd "C-f") 'global-swiftly-mode)
