(setq hts/cpp-candidate-producer
      '(
        ;; We'll borrow some function from C
        ("preproc_include"     . hts/c-preproc-include-fn)
        ("enum_specifier" . hts/c-enum-specifier-fn)
        ("union_specifier" . hts/c-union-specifier-fn)

        ;; Stuff that is unique for C++
        ("function_definition" . hts/cpp-function-definition-fn)
        ("class_specifier"     . hts/cpp-class-specifier-fn)
        

        ;; We get very spammy output if we try to show every declaration,
        ;; so we'll just ignore them for now.
        ;; ("declaration" . hts/cpp-declaration-fn)
        ))

(defun hts/cpp-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         ;; Let's get the return type of the function.
         ;; Only one kind will be present.

         ;; Something like boost::shared_ptr<type> fn()
         (template-type (hts/get-node-text-or-nil (alist-get 'template_type children-alist)))
         
         ;; We would have this with namespace::type fn()
         (scoped-type (hts/get-node-text-or-nil (alist-get 'scoped_type_identifier children-alist)))
         
         ;; We would have this with type fn()
         (type-identifier (hts/get-node-text-or-nil (alist-get 'type_identifier children-alist)))
         
         ;; We would have this with int fn()
         (primitive-type (hts/get-node-text-or-nil (alist-get 'primitive_type children-alist)))

         (function-declarator (hts/get-node-text (alist-get 'function_declarator children-alist)))
         (function-reference-declarator (hts/get-node-text (alist-get 'reference_declarator children-alist)))
         (function-pointer-declarator (hts/get-node-text (alist-get 'pointer_declarator children-alist))))

    (concat
     (propertize "Function / "
                 'face 'italic)

     (concat
      (let* ((type (or template-type
                       scoped-type
                       type-identifier
                       primitive-type)))
        (if type
            (concat type " ")))

      function-pointer-declarator
      function-reference-declarator
      function-declarator))))



(defun hts/cpp-class-specifier-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (type-identifier (hts/get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Class specifier / "
                 'face 'italic)
     type-identifier)))

(provide 'helm-tree-sitter-cpp-fns)
