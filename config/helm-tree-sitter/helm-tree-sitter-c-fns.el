(setq hts/c-candidate-producer
      '(("function_definition" . hts/c-function-definition-fn)
        ("preproc_include" . hts/c-preproc-include-fn)
        ("declaration" . hts/c-declaration-fn)
        ("struct_specifier" . hts/c-struct-specifier-fn)))

(defun hts/c-preproc-include-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (system-lib-node (alist-get 'system_lib_string children-alist))
         (string-literal-node (alist-get 'string_literal children-alist)))

    (concat
     (propertize "Include / "
                 'face 'italic)

     (concat
      (if (tsc-node-p system-lib-node)
          (format "%s" (tsc-node-text system-lib-node )) "" )
      (if (tsc-node-p string-literal-node)
          (format "%s" (tsc-node-text string-literal-node)) "" )))))

(defun hts/c-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'identifier children-alist))
         (parameters-node (alist-get 'parameters children-alist))
         (function-declarator-node (alist-get 'function_declarator children-alist))
         (function-pointer-declarator-node (alist-get 'pointer_declarator children-alist)))

    (concat
     (propertize "Function aa / "
                 'face 'italic)
     (concat
      (if (tsc-node-p function-pointer-declarator-node)
          (format "%s" (tsc-node-text function-pointer-declarator-node )) "" )
      (if (tsc-node-p function-declarator-node)
          (format "%s" (tsc-node-text function-declarator-node )) "" )
      (if (tsc-node-p identifier-node)
          (format "%s" (tsc-node-text identifier-node )) "" )
      (if (tsc-node-p parameters-node)
          (format "%s" (tsc-node-text parameters-node)) "" )))))

(defun hts/c-declaration-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (concat
   (propertize "Declaration / "
               'face 'italic)
   (tsc-node-text (hts/elem-node x))))

(defun hts/c-struct-specifier-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'type_identifier children-alist)))

    (concat
     (propertize "Struct / "
                 'face 'italic)
     (tsc-node-text identifier-node))))


(provide 'helm-tree-sitter-c-fns)
