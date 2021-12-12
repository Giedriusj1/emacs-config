(define-key lisp-playground-map (kbd "SPC") 'load-listp-playground)

(defun load-listp-playground (args)
  "docstring"
  (interactive "P")
  (dolist (element (seq-filter (lambda (item)
                                 (and (string-match-p ".el$" item)
                                      (not (member item basic-load-sequence))))
                               (directory-files "~/private-sync/projects/lisp-playground")))
    (load (concat "~/private-sync/projects/lisp-playground/" element))))





