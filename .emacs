(package-initialize)
(setq debug-on-error t)
(if (file-exists-p "~/.emacs.d/emacs.org")
    (message "Seems like emacs.org is already in place, no need to fetch one")
  (url-copy-file "https://raw.githubusercontent.com/Giedriusj1/emacs-config/master/emacs.org" "~/.emacs.d/emacs.org")
  )

(require 'org)
;;(org-babel-load-file "~/.emacs.d/emacs.org")
