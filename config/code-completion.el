;;; -*- lexical-binding: t -*-

(use-package yasnippet :defer t
  :ensure yasnippet-snippets
  :ensure yasnippet-classic-snippets
  :ensure helm-c-yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(when (display-graphic-p)
  (use-package  company-box :defer t
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors
          '((company-lsp .
                         ( :selected (:background "orange"  :foreground "black")))
            (company-capf .
			              ( :selected (:background "orange" :foreground "black")))))
    (setq company-box-doc-delay 1)))

(use-package company :defer t
  :bind ( :map company-active-map
          (("C-n" . company-select-next)
           ("C-p" . company-select-previous))
          :map control-semi-map
          (("n" . company-complete)
           ("C-n" . dabbrev-expand)))
  :diminish company-mode
  :config
  ;; (when (display-graphic-p)
  ;;   (require 'color)

  ;;   (let ((bg (face-attribute 'default :background)))
  ;;     (custom-set-faces
  ;;      `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
  ;;      `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  ;;      `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  ;;      `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  ;;      `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

  (global-company-mode t)

  (setq company-tooltip-limit 25))
