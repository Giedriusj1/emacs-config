;;; -*- lexical-binding: t -*-

(use-package projectile :diminish
  :ensure projectile-ripgrep
  :ensure consult-projectile
  :bind (:map tab-map ("p" . projectile-hydra/body))
  :pretty-hydra
  ((:pre (progn
	   (projectile-global-mode t))
	 :color blue)
   ("projectile"
    (("p" consult-projectile-switch-project "projects")
     ("q" projectile-invalidate-cache "invalidate cache")
     ("j" consult-projectile-find-file "find file")
     ("d" projectile-dired "dired"))
    "projectile search"
    (("r" projectile-ripgrep "ripgrep")
     ("R" consult-ripgrep "consult ripgrep")))))
