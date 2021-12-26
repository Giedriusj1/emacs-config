(defun hts/strip-newlines-and-whitespaces (string)
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))

  (replace-regexp-in-string
   "\s" ""
   (replace-regexp-in-string
    "\n" ""
    string)))

;; Copy text from buffer between node-start-byte and node-end-byte.
;; We use this instead of (tsc-node-text node), because this way
;; we can get fontified text.
(defun hts/get-node-text (node)
  (if (tsc-node-p node)
      (buffer-substring
       (tsc-node-start-position node)
       (tsc-node-end-position node) )
    ""
    ))

;; Same as function above, but we'll return nil if no node is
;; provided.
(defun hts/get-node-text-or-nil (node)
  (if (tsc-node-p node)
      (buffer-substring
       (tsc-node-start-position node)
       (tsc-node-end-position node) )
    nil))

(defun hts/prepend-if-not-empty (str prepend)
  (if (not (= (length str) 0))
      (concat prepend str)))

(defun hts/strip-newlines (string)
  (replace-regexp-in-string "\n" "" string))

(defun hts/empty-string (str)
  (if (stringp str)
      (= (length str) 0)))

(provide 'helm-tree-sitter-utilities)
