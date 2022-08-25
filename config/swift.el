;;; -*- lexical-binding: t -*-

(define-key control-semi-map (kbd "C-f") 'global-swift-mode)

(define-minor-mode swift-mode
  "Move around swiftly"
  :lighter " === SWIFT ==="
  :keymap (let ((map (make-sparse-keymap)))
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

	    ;; cua mode
	    (define-key map (kbd "C-z") 'toggle-swift-mode)
	    (define-key map (kbd "C-x") 'kill-region)
	    (define-key map (kbd "C-c") 'kill-ring-save)
	    (define-key map (kbd "C-v") 'yank)
	    map)

  
  (if (eq global-swift-mode t)
      (custom-set-faces '(cursor ((t (:background "blue")))))
    (custom-set-faces '(cursor ((t (:background "OrangeRed")))))))

(define-globalized-minor-mode global-swift-mode swift-mode swift-mode)
