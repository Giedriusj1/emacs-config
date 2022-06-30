;;; -*- lexical-binding: t -*-

(use-package hydra :ensure t :defer t)

(defun spawn-local-mode-hydra ()
  (interactive)
  (cond (( string= "org-mode" major-mode)
	     (hydra-org/body))
	    (( string= "c-mode" major-mode)
	     (hydra-c/body))
	    (( string= "c++-mode" major-mode)
	     (hydra-c/body))
	    (( string= "python-mode" major-mode)
	     (hydra-python/body))
	    (( string= "emacs-lisp-mode" major-mode)
	     (hydra-emacs-lisp/body))
	    (( string= "rust-mode" major-mode)
	     (hydra-rust/body))
	    (( string= "rustic-mode" major-mode)
	     (hydra-rust/body))
	    (( string= "go-mode" major-mode)
	     (hydra-go/body))
	    (t (message "Argh...hydra for your current mode does not exist :("))))

(define-key tab-map (kbd "j") 'spawn-local-mode-hydra)

(define-key tab-map (kbd "o") 'hydra-lsp/body)
