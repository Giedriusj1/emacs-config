(use-package hydra  :ensure t :defer t)
(use-package pretty-hydra :ensure t :defer t)

(use-package posframe :defer t)

(require 'hydra-posframe)

(customize-set-variable 'hydra-posframe-border-width 4)
(hydra-posframe-mode)

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

(defhydra hydra-search-helper
  (:color blue)
  "
[_q_] update tags        [_o_] find gtag
[_c_] create gtag        [_p_] hydra-lsp
"
  ("q" ggtags-update-tags nil)
  ("c" ggtags-create-tags nil)
  ("o" ggtags-find-tag-dwim nil)
  ("p" hydra-lsp/body nil))

(define-key tab-map (kbd "o") 'hydra-search-helper/body)
