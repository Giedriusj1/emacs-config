;;; -*- lexical-binding: t -*-

(use-package helm :defer 1
  :diminish helm-minor-mode
  :diminish helm-mode
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (diminish 'helm-mode)))

  :bind
  (:map control-semi-map
        (( "C-s" . g/control-semi-map-helm-tree-sitter-or-imenu)
         ( "s" .   g/control-semi-map-helm-tree-sitter-debug)
         ( "l" . helm-M-x)
         ( "r" . helm-mark-ring)
         ( "C-r" . helm-global-mark-ring)
         ( "b" . helm-resume)
         ( "C-b" . helm-resume)))

  :config
  (i-defun g/control-semi-map-helm-tree-sitter-debug ()
    (require 'helm-tree-sitter-debug)
    (tree-sitter-mode)
    (helm-tree-sitter-debug))

  (i-defun g/control-semi-map-helm-tree-sitter-or-imenu ()
    (require 'helm-tree-sitter)
    (tree-sitter-mode)
    (helm-tree-sitter-or-imenu))

  (setq helm-candidate-number-limit 500)
  (setq helm-buffer-max-length 60)

  (custom-set-faces '(helm-rg-file-match-face ((t (:foreground "purple" :background "black" :weight bold))))))

(use-package helm-swoop :defer 1
  :bind (:map control-semi-map
              (("C-m" . helm-swoop)
               ("m" . helm-multi-swoop-all))))

(use-package swiper-helm :defer 1
  :bind (:map control-semi-map (())
              ("o" . swiper-helm)
              ("C-;" . swiper-helm))
  :config
  (require 'swiper))

(use-package helm-rg :defer 3)
