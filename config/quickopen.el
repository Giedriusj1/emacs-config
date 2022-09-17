;;; -*- lexical-binding: t -*-

;; Ignored types
;; '("*.doc" "*.ovpn" "*.pcap" "*.pcapng" "*.png" "*.pem" )


(pretty-hydra-define hydra-quickopen (:color blue)
  ("quickopen"
   (("t" (lambda ()
	   (interactive)
	   (find-file "~/private-sync/temp.org")) "~/private-sync/temp.org")
    ("c" (lambda ()
           (interactive)
           (find-file "~/.emacs.d/init.el")) "~/.emacs.d/init.el")
    ("l" (lambda ()
           (interactive)
           (progn (zygospore-toggle-delete-other-windows)
	          (dired "~/private-sync")
	          (find-file default-directory))) "dired ~/private-sync/")
    (";" (lambda ()
           (interactive)
           (consult-ripgrep "~/private-sync")) "grep notes"))))

(define-key tab-map (kbd ";") 'hydra-quickopen/body)
