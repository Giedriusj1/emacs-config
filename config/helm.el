(use-package helm :defer 1
  :bind
  (("C-j" . helm-mini))
  (:map control-semi-map
        (( "C-s" . g/helm-semantic-or-imenu)
         ( "l" . helm-M-x)
         ( "r" . helm-mark-ring)
         ( "C-r" . helm-global-mark-ring)
         ( "b" . helm-resume)
         ( "C-b" . helm-resume)))

  (:map lisp-interaction-mode-map (("C-j" . helm-mini)))
  :config
  (semantic-mode 1) ;; global mode
  (setq helm-candidate-number-limit 500)
  (setq helm-buffer-max-length 60)

  (custom-set-faces '(helm-rg-file-match-face ((t (:foreground "purple" :background "black" :weight bold))))))

(use-package helm-swoop :defer 2
  :bind (:map control-semi-map
              (("C-m" . helm-swoop)
               ("m" . helm-multi-swoop-all))))


(use-package swiper-helm :defer 2
  :bind (:map control-semi-map (())
              ("o" . swiper-helm)
              ("C-;" . swiper-helm))
  :config
  (require 'swiper))

(use-package helm-rg :defer 10)

;; This effectively disables idle reparsing for all files
(setq semantic-idle-scheduler-max-buffer-size 1)

;; We don't care about saving db when exiting emacs
(remove-hook 'kill-emacs-hook #'semanticdb-kill-emacs-hook)

(defun ds () t)
(add-hook 'semantic-inhibit-functions  #'ds)

(defun g/helm-semantic-or-imenu (arg)
  (interactive "P")
  (remove-hook 'semantic-inhibit-functions #'ds)
  (semantic-new-buffer-fcn)
  (helm-semantic-or-imenu arg)
  (add-hook 'semantic-inhibit-functions  #'ds))


(use-package view :defer t :pin manual
  :bind (:map view-mode-map
              ("C-j" . helm-mini)))
