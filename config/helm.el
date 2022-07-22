;;; -*- lexical-binding: t -*-

(use-package helm :defer 2
  :diminish helm-minor-mode
  :diminish helm-mode
  :bind
  (("C-j" . helm-mini))
  (:map control-semi-map
        (( "C-s" . g/control-semi-map-helm-tree-sitter-or-imenu)
         ( "s" .   g/control-semi-map-helm-tree-sitter-debug)
         ( "l" . helm-M-x)
         ( "r" . helm-mark-ring)
         ( "C-r" . helm-global-mark-ring)
         ( "b" . helm-resume)
         ( "C-b" . helm-resume)))

  (:map lisp-interaction-mode-map (("C-j" . helm-mini)))
  :config

  (defun g/control-semi-map-helm-tree-sitter-debug ()
    (interactive)
    (require 'helm-tree-sitter-debug)
    (tree-sitter-mode)
    (helm-tree-sitter-debug))

  (defun g/control-semi-map-helm-tree-sitter-or-imenu ()
    (interactive)
    (require 'helm-tree-sitter)
    (tree-sitter-mode)
    (helm-tree-sitter-or-imenu))

  (setq helm-candidate-number-limit 500)
  (setq helm-buffer-max-length 60)

  (custom-set-faces '(helm-rg-file-match-face ((t (:foreground "purple" :background "black" :weight bold))))))

(use-package helm-swoop :defer 3
  :bind (:map control-semi-map
              (("C-m" . helm-swoop)
               ("m" . helm-multi-swoop-all))))

(use-package swiper-helm :defer 3
  :bind (:map control-semi-map (())
              ("o" . swiper-helm)
              ("C-;" . swiper-helm))
  :config
  (require 'swiper))

(use-package helm-rg :defer 3)

(use-package view :defer t :pin manual
  :bind (:map view-mode-map
              ("C-j" . helm-mini)))
