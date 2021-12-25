(setq hts/cpp-candidate-producer
      '(("function_definition" . hts/cpp-function-definition-fn)
        ("preproc_include" . hts/cpp-preproc-include-fn)
        ("class_specifier" . hts/cpp-class-specifier-fn)
        ("declaration" . hts/cpp-declaration-fn)))

(defun hts/cpp-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         ;; Let's get the return type of the function.
         ;; Only one kind will be present.

         ;; Something like boost::shared_ptr<type> fn()
         (template-type-node (alist-get 'template_type children-alist))
         
         ;; We would have this with namespace::type fn()
         (scoped-type-node (alist-get 'scoped_type_identifier children-alist))
         
         ;; We would have this with type fn()
         (type-identifier-node (alist-get 'type_identifier children-alist))

         ;; We would have this with int fn()
         (primitive-type-identifier-node (alist-get 'primitive_type children-alist))
         
         (function-declarator-node (alist-get 'function_declarator children-alist))
         (function-reference-declarator-node (alist-get 'reference_declarator children-alist))
         (function-pointer-declarator-node (alist-get 'pointer_declarator children-alist)))

    (concat
     (propertize "Function / "
                 'face 'italic)

     (concat
      
      (if (tsc-node-p template-type-node)
          (format "%s" (tsc-node-text template-type-node )) "" )
      (if (tsc-node-p scoped-type-node)
          (format "%s" (tsc-node-text scoped-type-node )) "" )
      (if (tsc-node-p type-identifier-node)
          (format "%s" (tsc-node-text type-identifier-node )) "" )
      (if (tsc-node-p primitive-type-identifier-node)
          (format "%s" (tsc-node-text primitive-type-identifier-node )) "" )
      
      " "
      
      (if (tsc-node-p function-pointer-declarator-node)
          (format "%s" (tsc-node-text function-pointer-declarator-node )) "" )
      (if (tsc-node-p function-reference-declarator-node)
          (format "%s" (tsc-node-text function-reference-declarator-node )) "" )
      (if (tsc-node-p function-declarator-node)
          (format "%s" (tsc-node-text function-declarator-node )) "" )))))

(defun hts/cpp-preproc-include-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (system-lib-node (alist-get 'system_lib_string children-alist))
         (string-literal-node (alist-get 'string_literal children-alist)))

    (concat
     (propertize "Include  / "
                 'face 'italic)

     (concat
      (if (tsc-node-p system-lib-node)
          (format "%s" (tsc-node-text system-lib-node )) "" )
      (if (tsc-node-p string-literal-node)
          (format "%s" (tsc-node-text string-literal-node)) "" )))))

(defun hts/cpp-class-specifier-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'type_identifier children-alist)))

    (concat
     (propertize "Class specifier / "
                 'face 'italic)
     (tsc-node-text identifier-node))))

(defun hts/cpp-declaration-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (concat
   (propertize "Declaration / "
               'face 'italic)
   (tsc-node-text (hts/elem-node x))))

(provide 'helm-tree-sitter-cpp-fns)
