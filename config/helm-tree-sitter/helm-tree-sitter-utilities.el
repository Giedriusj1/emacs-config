(defun hts-strip-newlines-and-whitespaces (string)
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (replace-regexp-in-string "\s" ""
                            (replace-regexp-in-string "\n" ""
                                                      string)))

(provide 'helm-tree-sitter-utilities)
