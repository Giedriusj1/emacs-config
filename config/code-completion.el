;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure yasnippet-snippets
  :ensure yasnippet-classic-snippets
  :ensure helm-c-yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(on-linux
 (use-package company-quickhelp))

(use-package company :diminish
  :bind ( :map company-active-map
          (("C-n" . company-select-next)
           ("C-p" . company-select-previous))
          :map control-semi-map
          (("n" . company-complete)
           ("C-n" . dabbrev-expand)))
  :config
  (global-company-mode t)
  (company-quickhelp-mode)

  (setq pos-tip-background-color "#202221")

  (setq company-tooltip-limit 25))
