;;; -*- lexical-binding: t -*-

(i-defun swift-up(&optional arg)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-down-line)
    (previous-line)))

(i-defun swift-down(&optional arg)
  (or arg (setq arg 1))
  (dotimes (bind arg)
    (scroll-up-line)
    (next-line)))

(define-key control-semi-map (kbd "C-f") 'global-swift-mode)

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
  :lighter " === SWIFT ==="
  :keymap swift-command-map
  (if (eq global-swift-mode t)
      (custom-set-faces '(cursor ((t (:background "blue")))))
    (custom-set-faces '(cursor ((t (:background "OrangeRed")))))))

(define-globalized-minor-mode global-swift-mode swift-mode swift-mode)
