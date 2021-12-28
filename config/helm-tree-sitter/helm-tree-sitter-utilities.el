(defun hts/strip-newlines-and-whitespaces (str)
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))

  (replace-regexp-in-string
   "\s" ""
   (replace-regexp-in-string
    "\n" ""
    str)))

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

(defun hts/append-space-if-not-empty(str)
  (if (not (hts/empty-string str))
      (concat str " ") str))

(defun hts/prepend-if-not-empty (str prepend)
  (if (not (= (length str) 0))
      (concat prepend str)))

(defun hts/strip-newlines (str)
  (replace-regexp-in-string "\n" "" str))

(defun hts/empty-string (str)
  (if (stringp str)
      (= (length str) 0)))

(provide 'helm-tree-sitter-utilities)
