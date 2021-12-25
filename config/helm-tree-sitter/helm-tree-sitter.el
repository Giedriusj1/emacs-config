;;; helm-tree-sitter.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: todo

;; Copyright (C) 2021 Giedrius Jonikas

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; URL: todo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'tree-sitter)
(require 'helm)

(require 'helm-tree-sitter-utilities)

(provide 'helm-tree-sitter-c-fns)
(require 'helm-tree-sitter-cpp-fns)
(require 'helm-tree-sitter-python-fns)
(require 'helm-tree-sitter-rust-fns)

;; tree-sitter element. Basically holds everything we care about each of the elements.
(cl-defstruct hts/elem node node-type node-text start-pos depth)

;; ;TODO: maybe defcustom?
(setq hts-producer-mode-maps
      '((python-mode . hts/python-candidate-producer)
        (c++-mode . hts/cpp-candidate-producer)
        (c-mode . hts/c-candidate-producer)
        (rust-mode . hts/rust-candidate-producer)
        (rustic-mode . hts/rust-candidate-producer)))

;; Out main entry funtion
;;;###autoload
(defun helm-tree-sitter ()
  (interactive)
  (helm :sources `(((name . "Tree-sitter")
                    (candidates . ,(hts/elements-to-helm-candidates (hts/build-node-list (tsc-root-node tree-sitter-tree) 0)))
                    (action . (("goto" .
                                (lambda (x)
                                  (goto-char (hts/elem-start-pos x))))
                               ("show node text" .
                                (lambda (x)
                                  (message "%s" (format "%s"(tsc-node-text x )) x)))))))))

(defun hts/get-candidate-producer-for-current-mode ()
  (let* ((our-producer (symbol-value (assoc-default major-mode hts-producer-mode-maps)) ))
    (if our-producer
        our-producer
      (progn
        ;TODO: signal that mode is not supported
        nil
        )
      
      
      )))

(defun hts/elements-to-helm-candidates (elements)
  (remq nil
        (mapcar
         (lambda (arg)
           (let* ((my-fn (assoc-default
                          (format "%s" (hts/elem-node-type arg))
                          (hts/get-candidate-producer-for-current-mode))))
             (when my-fn
               ;; Each candidate will consist of a list containing (text-string . tree)
               (cons
                (funcall my-fn arg) ; Let's get the actual text
                arg                 ; Store the tree too, so additional actions can be performed
                ))))
         elements )))

;; Inspect the tree-sitter-tree and build a flat list with all the nodes.
;; This will later be used to build helm candidates.
(defun hts/build-node-list (node depth)
  (let* (elements '())
    ;; Add the current node
    (add-to-list 'elements
                 (make-hts/elem
                  :node node
                  :node-type (tsc-node-type node)
                  :node-text (tsc-node-text node)
                  :start-pos (tsc-node-start-position node)
                  :depth depth))

    ;; And now all the child nodes..
    (dotimes (e (tsc-count-named-children node))
      (setq elements (append  elements (hts/build-node-list (tsc-get-nth-named-child node e) (1+ depth)))))

    elements))


(defun hts/node-children-to-alist (node)
  (let ((pl '()))
    (dotimes (e (tsc-count-named-children node))
      (let* ((child-node (tsc-get-nth-named-child node e)))
        (setf (alist-get (tsc-node-type child-node) pl) child-node)))
    pl))

(provide 'helm-tree-sitter)
