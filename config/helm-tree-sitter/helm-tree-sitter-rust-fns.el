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

   (hts/get-node-text (hts/elem-node x ))))

(defun hts/rust-function-definition-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (visibility-modifier (hts/get-node-text (alist-get 'visibility_modifier children-alist)))
         (identifier (hts/get-node-text (alist-get 'identifier children-alist)))
         (type-identifier (hts/get-node-text (alist-get 'type_identifier children-alist)))
         (parameters (hts/get-node-text (alist-get 'parameters children-alist))))

    (concat
     (propertize "Fn / "
                 'face 'italic)

     (concat
      (hts/append-space-if-not-empty visibility-modifier)
      (hts/append-space-if-not-empty type-identifier)
      identifier
      parameters

      (hts/prepend-if-not-empty type-identifier " -> ")))))

(defun hts/rust-struct-item-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier (hts/get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Struct / "
                 'face 'italic)

     identifier)))

(defun hts/rust-impl-item-fn (x)
  (unless (hts/elem-p x)
    (signal 'wrong-type-argument (list 'hts/elem-p x)))

  (let* ((children-alist (hts/node-children-to-alist (hts/elem-node x)))
         (identifier (hts/get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Impl / "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-rust-fns)
