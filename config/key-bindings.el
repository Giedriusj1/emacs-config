;;; -*- lexical-binding: t -*-

(require 'bind-key)
(override-global-mode)

(define-prefix-command 'control-semi-map)
(define-prefix-command 'tab-map)
(define-prefix-command 'lisp-playground-map)

(bind-keys*
 ("C-1" . window-swap-states)
 ("C-2" . windmove-up)
 ("C-3" . windmove-right)

 ("C-j" . consult-buffer)
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
 ("<f12>" . whitespace-mode)

 ( "<Scroll_Lock>" . scroll-lock-mode)
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
            ("q" . goto-line)

            ("C-j" . jump-to-register)
            ("C-q" . goto-line)
            ("C-l" . execute-extended-command)
            ("C-2" . split-window-below))

(define-key control-semi-map (kbd "C-;") 'consult-line-empty)

(define-key control-semi-map (kbd "C-m") 'consult-line)

(define-key control-semi-map (kbd "C-2") (i-lambda ()
                                           (split-window-below)
                                           (balance-windows)))

(define-key control-semi-map (kbd "C-3") (i-lambda ()
                                           (split-window-right)
                                           (balance-windows)))


(define-key control-semi-map (kbd "C-0") (i-lambda ()
                                           (delete-window)
                                           (balance-windows)))

(define-key control-semi-map (kbd "C-4") 'balance-windows)
(define-key control-semi-map (kbd "C-d") 'follow-mode)

(define-key control-semi-map (kbd "C-1") 'g-toggle-delete-other-windows)

(bind-keys* :map tab-map
            ("SPC" . lisp-playground-map)
            ("TAB" . comment-dwim)
            ("M- ." 'comment-dwim)
            ("C- ." 'comment-dwim)
            ("M- ." 'comment-dwim)
            ("u" . universal-argument)
            ("C-f" . find-file)
            ("C-d" . dired-jump))
