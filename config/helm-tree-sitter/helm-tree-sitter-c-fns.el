(setq hts/c-candidate-producer
      '(("function_definition" . hts/c-function-definition-fn)
        ("preproc_include" . hts/c-preproc-include-fn)
        ("struct_specifier" . hts/c-struct-specifier-fn)

        ;; We get very spammy output if we try to show every declaration,
        ;; so we'll just ignore them for now.
        ;; ("declaration" . hts/c-declaration-fn)
        ))

(defun hts/c-preproc-include-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (system-lib (hts/get-node-text (alist-get 'system_lib_string children-alist)))
         (string-literal (hts/get-node-text (alist-get 'string_literal children-alist))))

    (concat
     (propertize "Include / "
                 'face 'italic)
     (concat
      system-lib
      (replace-regexp-in-string "\"" "" string-literal)))))


(defun hts/c-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier (hts/get-node-text (alist-get 'identifier children-alist)))
         (parameters (hts/get-node-text (alist-get 'parameters children-alist)))
         (function-declarator (hts/get-node-text (alist-get 'function_declarator children-alist)))
         (function-pointer-declarator (hts/get-node-text (alist-get 'pointer_declarator children-alist))))

    (concat
     (propertize "Function / "
                 'face 'italic)
     (concat
      identifier
      (hts/strip-newlines function-declarator)
      (hts/strip-newlines function-pointer-declarator)))))

(defun hts/c-struct-specifier-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (type-identifier (hts/get-node-text (alist-get 'type_identifier children-alist)))
         (field-declaration-list-node (alist-get 'field_declaration_list children-alist)))

    ;; To prevent output from being too verbose, we'll only show structs that have
    ;; field declarations too.
    (if (tsc-node-p field-declaration-list-node)

        (if (not (string= "" type-identifier))
            (concat
             (propertize "Struct / "
                         'face 'italic)

             type-identifier)

          ;; We are dealing with struct that has a field declaration list, but no type-identifier...
          ;; This must be a typedef case.
          ;; Let's check if our parent has a type_identifier:
          (let* ((parent-node (tsc-get-parent (hts/elem-node x))))
            (when parent-node
              (let* ((parent-children-alist (hts/node-children-to-alist parent-node))
                     (parent-type-identifier (hts/get-node-text (alist-get 'type_identifier parent-children-alist))))
                (concat
                 (propertize "typedef Struct / "
                             'face 'italic)

                 parent-type-identifier)))
            ))

      ;; We failed to construct something sensible, so better not show anything
      nil )))

(provide 'helm-tree-sitter-c-fns)
