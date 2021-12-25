(setq hts/rust-candidate-producer
      '(("use_declaration" . hts/rust-use-declaration-fn)
        ("struct_item" . hts/rust-struct-item-fn)
        ("function_item" . hts/rust-function-definition-fn)
        ("impl_item" . hts/rust-impl-item-fn)))

(defun hts/rust-use-declaration-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

    (concat
     (propertize "Use / "
                 'face 'italic)
     (tsc-node-text (hts/elem-node x))))

(defun hts/rust-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (visibility-modifier-node (alist-get 'visibility_modifier children-alist))
         (identifier-node (alist-get 'identifier children-alist))
         (parameters-node (alist-get 'parameters children-alist)))

    (concat
     (propertize "Function rust / "
                 'face 'italic)

     (concat
      (if (tsc-node-p visibility-modifier-node)
          (format "%s " (tsc-node-text visibility-modifier-node )) "" )

      "fn "
      
      (if (tsc-node-p identifier-node)
          (format "%s" (tsc-node-text identifier-node )) "" )
      (if (tsc-node-p parameters-node)
          (format "%s" (tsc-node-text parameters-node)) "" )))))

(defun hts/rust-struct-item-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'type_identifier children-alist)))

    (concat
     (propertize "Struct / "
                 'face 'italic)
     (tsc-node-text identifier-node))))

(defun hts/rust-impl-item-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier-node (alist-get 'type_identifier children-alist)))

    (concat
     (propertize "Impl / "
                 'face 'italic)
     (tsc-node-text identifier-node))))

(provide 'helm-tree-sitter-rust-fns)
