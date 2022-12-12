;;; -*- lexical-binding: t -*-

(use-package dired :ensure nil :defer 5
  :config
  (require 'dired-x)

  ;; (add-hook 'dired-omit-mode-hook
  ;;           (lambda ()
  ;;             (diminish 'dired-omit-mode)))

  (define-key dired-mode-map (kbd "(") (i-lambda ()
                                         (if dired-hide-details-mode
                                             (dired-hide-details-mode -1)
                                           (dired-hide-details-mode ))
                                         (if dired-omit-mode
                                               (dired-omit-mode -1)
                                             (dired-omit-mode))))

  (define-key dired-mode-map (kbd "l") 'dired-up-directory)
  (define-key dired-mode-map (kbd "r") 'dired-do-redisplay)

  (setq dired-listing-switches "-alFh")

  (on-linux
   (use-package dired-toggle-sudo)
   (setq dired-listing-switches
         (concat dired-listing-switches " --group-directories-first -v")))

  (setq dired-dwim-target t)

  (add-hook 'dired-mode-hook
	        (lambda ()
	          (dired-hide-details-mode t)
              (dired-omit-mode))))
