(define-prefix-command 'control-semi-map)
(define-prefix-command 'tab-map)
(define-prefix-command 'lisp-playground-map)

(bind-keys*
 ("C-j" . helm-mini)
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
 
 ("<f9>" . toggle-font-size)
 ("<f10>" . toggle-truncate-lines)
 ("<f11>" . toggle-frame-fullscreen)
 ("<f12>" . whitespace-mode)

 ( "<Scroll_Lock>" . scroll-lock-mode)
 ( "<up>" . scroll-down-line)
 ( "<down>" . scroll-up-line)
 ( "M-p" . backward-paragraph)
 ( "M-n" . forward-paragraph)
 ( "M-," . backward-kill-word)
 ( "<down>" . scroll-up-line)
 ( "C-M-SPC" . rectangle-mark-mode))

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


(bind-keys* :map tab-map
            ("SPC" . lisp-playground-map)
            ("TAB" . comment-dwim)
            ("M- ." 'comment-dwim)
            ("C- ." 'comment-dwim)
            ("M- ." 'comment-dwim)
            ("u" . universal-argument)
            ("C-f" . helm-find-files)
            ("C-d" . dired-jump))

