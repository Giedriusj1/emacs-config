;;; -*- lexical-binding: t -*-

(use-package yasnippet :defer t
  :ensure yasnippet-snippets
  :ensure yasnippet-classic-snippets
  :ensure helm-c-yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(use-package company-quickhelp :defer t)

(use-package company :defer t
  :bind ( :map company-active-map
          (("C-n" . company-select-next)
           ("C-p" . company-select-previous))
          :map control-semi-map
          (("n" . company-complete)
           ("C-n" . dabbrev-expand)))
  :diminish company-mode
  :config
  (global-company-mode t)
  (company-quickhelp-mode)

  (setq pos-tip-background-color "#121717")

  (setq company-tooltip-limit 25))
