(setq hts/python-candidate-producer
      '(("import_statement"    . hts/python-import-statement-fn)
        ("function_definition" . hts/python-function-definition-fn)
        ("class_definition"    . hts/python-class-definition-fn)))

(defun hts/python-import-statement-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'dotted_name children-alist)))

    (concat
     (propertize "Dependency / "
                 'face 'italic)
     (tsc-node-text (hts/elem-node x)))))

(defun hts/python-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'identifier children-alist))
         (parameters-node (alist-get 'parameters children-alist)))

    (concat
     (propertize "Function / "
                 'face 'italic)
     (concat
      (if (tsc-node-p identifier-node)
          (format "%s" (tsc-node-text identifier-node )) "" )
      (if (tsc-node-p parameters-node)
          (hts-strip-newlines-and-whitespaces (format "%s" (tsc-node-text parameters-node))) "" )))))


(defun hts/python-class-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'identifier children-alist)))

    (concat
     (propertize "Class definition / "
                 'face 'italic)
     (tsc-node-text identifier-node))))


(provide 'helm-tree-sitter-python-fns)
