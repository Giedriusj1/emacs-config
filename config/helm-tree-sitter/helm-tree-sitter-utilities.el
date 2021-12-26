(defun hts/strip-newlines-and-whitespaces (string)
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (replace-regexp-in-string "\s" ""
                            (replace-regexp-in-string "\n" ""
                                                      string)))

;; Copy text from buffer between node-start-byte and node-end-byte.
;; We use this instead of (tsc-node-text node), because this way
;; we can get fontified text.
(defun hts/get-node-text (node)

  (if (tsc-node-p node)
      ;; Cool, let's copy the fontified text
      (buffer-substring
       (tsc-node-start-position node)
       (tsc-node-end-position node) )

    ;; else just return an empty string
    ""
    ))


(defun hts/append-space-if-not-empty (str)
  (if (not (= (length str) 0))
      (concat str " ")
    ""
    ))

(defun hts/prepend-if-not-empty (str prepend)
  (if (not (= (length str) 0))
      (concat prepend str)
    ""
    ))

(defun hts/strip-newlines (string)
  (replace-regexp-in-string "\n" "" string)
  )



(provide 'helm-tree-sitter-utilities)
