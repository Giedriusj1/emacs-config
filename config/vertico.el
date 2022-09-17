;;; -*- lexical-binding: t -*-

(use-package vertico
  :init
  (vertico-mode)

  (setq vertico-count 20
	vertico-resize nil))

(use-package marginalia
  :bind (:map minibuffer-local-map ("C-l" . marginalia-cycle))
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult-projectile)
