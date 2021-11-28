(define-prefix-command 'control-semi-map)
(define-prefix-command 'tab-map)

(bind-key* "C-d" 'tab-map)
(bind-key* "C-;" 'control-semi-map)
(bind-key* "<tab>" 'tab-map)
(bind-key* "M-;" 'tab-map)

(global-set-key [f9] 'toggle-font-size)
(global-set-key [f10] 'toggle-truncate-lines)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [f12] 'whitespace-mode)

(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(global-set-key (kbd "<up>") 'scroll-down-line)
(global-set-key (kbd "<down>") 'scroll-up-line)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(bind-key* "M-," 'backward-kill-word)
(bind-key* "M-." 'kill-word)

(global-set-key (kbd "M-,") 'backward-kill-word)
(global-set-key (kbd "<down>") 'scroll-up-line)

(global-set-key (kbd "C-d") 'global-superword-mode)
(global-set-key (kbd "C-M-SPC") 'rectangle-mark-mode)

(bind-key* "C-," 'delete-backward-char)
(bind-key* "C-." 'delete-char)

(bind-key* "M-h" 'open-line)

(bind-key* "C-u" 'backward-char)
(bind-key* "C-o" 'forward-char)

(bind-key* "M-u" 'backward-word)
(bind-key* "M-o" 'forward-word)

(bind-key* "C-M-u" 'backward-sexp)
(bind-key* "C-M-o" 'forward-sexp)

(bind-key* "C-a" 'Control-X-prefix)

(bind-key* "C-q" 'beginning-of-line)
(bind-key* "C-w" 'back-to-indentation)

(define-key control-semi-map (kbd "SPC") 'point-to-register)
(define-key control-semi-map (kbd "C-SPC") 'point-to-register)
(define-key control-semi-map (kbd "j") 'jump-to-register)
(define-key control-semi-map (kbd "h") 'unhighlight-regexp)
(define-key control-semi-map (kbd "C-h") 'highlight-symbol-at-point)
(define-key control-semi-map (kbd "q") 'goto-line)

(define-key control-semi-map (kbd "C-j") 'jump-to-register)
(define-key control-semi-map (kbd "C-q") 'goto-line)
(define-key control-semi-map (kbd "C-l") 'execute-extended-command)
(define-key control-semi-map (kbd "C-2") 'split-window-below)

(define-key control-semi-map (kbd "C-2") (lambda ()
                                           (interactive)
                                           (split-window-below)
                                           (balance-windows)))

(define-key control-semi-map (kbd "C-3") (lambda ()
                                           (interactive)
                                           (split-window-right)
                                           (balance-windows)))


(define-key control-semi-map (kbd "C-0") (lambda ()
                                           (interactive)
                                           (delete-window)
                                           (balance-windows)))

(define-key control-semi-map (kbd "C-4") 'balance-windows)

(define-key control-semi-map (kbd "C-d") 'follow-mode)

(define-key tab-map (kbd "TAB") 'comment-dwim)
(define-key tab-map (kbd "M-;") 'comment-dwim)

(define-key tab-map (kbd "C-d") 'comment-dwim)
(define-key tab-map (kbd "M-;") 'comment-dwim)

(define-key tab-map (kbd "u") 'universal-argument)
(define-key tab-map "\C-f" 'helm-find-files)
(define-key tab-map "\C-d" 'dired-jump)
