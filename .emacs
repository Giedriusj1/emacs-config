(package-initialize)

(url-copy-file "https://raw.githubusercontent.com/Giedriusj1/emacs-config/master/emacs.org" "~/.emacs.d/emacs.org" 1)

(require 'org)

(org-babel-load-file "~/.emacs.d/emacs.org")