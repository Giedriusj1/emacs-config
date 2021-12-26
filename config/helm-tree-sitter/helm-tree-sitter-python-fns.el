(setq hts/python-candidate-producer
      '(("import_statement"      . hts/python-import-statement-fn)
        ("import_from_statement" . hts/python-import-statement-fn)
        ("function_definition"   . hts/python-function-definition-fn)
        ("class_definition"      . hts/python-class-definition-fn)))

(defun hts/python-import-statement-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (concat
   (propertize "Dependency / "
               'face 'italic)

   (hts/get-node-text (hts/elem-node x))))

(defun hts/python-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier (hts/get-node-text (alist-get 'identifier children-alist)))
         (parameters (hts/get-node-text (alist-get 'parameters children-alist))))

    (concat
     (propertize "Function / "
                 'face 'italic)
     (concat
      identifier
      (hts/strip-newlines-and-whitespaces parameters)))))


(defun hts/python-class-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier (hts/get-node-text (alist-get 'identifier children-alist))))

    (concat
     (propertize "Class / "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-python-fns)
