;;; -*- lexical-binding: t -*-

(defun swift-up(&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-down-line)
    (previous-line)))

(defun swift-down(&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-up-line)
    (next-line)))

(define-key control-semi-map (kbd "C-f") 'toggle-swift-mode)

(defvar swift-command-map
  (let ((map (make-sparse-keymap)))
    ;; movement
    (define-key map (kbd "i") (i-lambda () (swift-up 2)))

    (define-key map (kbd "k") (i-lambda () (swift-down 2)))

    (define-key map (kbd "o") 'swift-up)
    (define-key map (kbd "l") 'swift-down)

    (define-key map (kbd "p") 'beginning-of-defun)
    (define-key map (kbd "n") 'end-of-defun)

    (define-key map (kbd "u") 'cua-scroll-down)
    (define-key map (kbd "j") 'cua-scroll-up)

    ;; cua mode
    (define-key map (kbd "C-z") 'toggle-swift-mode)
    (define-key map (kbd "C-x") 'kill-region)
    (define-key map (kbd "C-c") 'kill-ring-save)
    (define-key map (kbd "C-v") 'yank)
    map))

(define-minor-mode swift-mode
  "Toggle SWIFT buffer mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " SWIFT"
  ;; The minor mode bindings.
  :keymap swift-command-map)

(define-globalized-minor-mode global-swift-mode swift-mode
  swift-mode
  :init-value nil)

(defun toggle-swift-mode()
  (interactive)
  (if (eq global-swift-mode t)
      (progn ;; turning mode off
        (custom-set-faces '(cursor ((t (:background "OrangeRed")))))
	    (set-face-attribute 'mode-line nil
                            :foreground "Black"
                            :background "DarkOrange3"
                            :box nil)
        (global-swift-mode -1))

    (progn ;; turning mode off
      (custom-set-faces '(cursor ((t (:background "blue")))))
      (custom-set-faces '(mode-line ((t (:background "#333377")))))
      (global-swift-mode))))
