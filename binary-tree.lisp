;;;; BINARY-TREE -- an immutable binary tree for Common Lisp
;;;; by David Sorokin <david.sorokin@gmail.com>, 2011
;;;;
;;;; Licence:
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :binary-tree
  (:use :cl)
  (:nicknames :btree)
  (:export
   #:btree-find
   #:btree-insert
   #:btree-remove
   #:btree->list
   #:list->btree
   #:btree-length
   #:btree-depth
   #:map-btree
   #:reduce-btree))

(in-package :binary-tree)

(defstruct (node (:print-function
		  (lambda (n s d)
		    (declare (ignore d))
		    (print-unreadable-object (n s)
		      (format s "BTREE ~s" (node-elt n))))))
  color elt (l nil) (r nil))

(defun btree-find (obj-key btree predicate &key (key #'identity) (test #'eql))
  "Find an object with the specified key in the binary tree."
  (if (null btree)
      (values nil nil)
      (let* ((elt (node-elt btree))
	     (elt-key (funcall key elt)))
	(if (funcall test obj-key elt-key)
	    (values (node-elt btree) t)
	    (if (funcall predicate obj-key elt-key)
		(btree-find obj-key (node-l btree) predicate :key key)
		(btree-find obj-key (node-r btree) predicate :key key))))))

(defun btree-insert (obj btree predicate &key (key #'identity) (test #'eql) (replaceable nil))
  "Insert an object in the binary tree and then return a new binary tree. 
The source tree doesn't change. It is treated as an immutable value."
  (let ((n (%btree-insert obj (funcall key obj) btree predicate key test replaceable)))
    (make-node :color :black 
	       :elt (node-elt n)
	       :l (node-l n) 
	       :r (node-r n))))

(defun btree-remove (obj-key btree predicate &key (key #'identity) (test #'eql))
  "Remove an object with the specified key from the binary tree and
return a new tree. The source tree doesn't change. It is treated as 
an immutable value."
  (let ((n (%btree-remove obj-key btree predicate key test)))
    (if n
	(make-node :color :black
		   :elt (node-elt n)
		   :l (node-l n)
		   :r (node-r n)))))

(defun %btree-insert (obj obj-key btree predicate key test replaceable)
  "A helper function to insert the object in the tree."
  (if (null btree)
      (make-node :color :red :elt obj :l nil :r nil)
      (let* ((elt (node-elt btree))
	     (elt-key (funcall key elt)))
	(if (funcall test obj-key elt-key)
            (if (not replaceable)
                btree
              (make-node :color (node-color btree)
                         :elt obj
                         :l (node-l btree)
                         :r (node-r btree)))
	    (if (funcall predicate obj-key elt-key)
		(balance-node (node-color btree) elt
			      (%btree-insert obj obj-key (node-l btree) 
					     predicate key test replaceable)
			      (node-r btree))
		(balance-node (node-color btree) elt
			      (node-l btree)
			      (%btree-insert obj obj-key (node-r btree) 
					     predicate key test replaceable)))))))

(defun %btree-remove (obj-key btree predicate key test)
  "A helper function to remove the object from the tree."
  (when btree
    (let* ((elt (node-elt btree))
	   (elt-key (funcall key elt)))
      (if (funcall test obj-key elt-key)
	  (behead btree)
	  (if (funcall predicate obj-key elt-key)
	      (balance-node (node-color btree) elt
			    (%btree-remove obj-key (node-l btree)
					   predicate key test)
			    (node-r btree))
	      (balance-node (node-color btree) elt
			    (node-l btree)
			    (%btree-remove obj-key (node-r btree)
					   predicate key test)))))))

(defun behead (btree)
  "Behead the specified tree."
  (cond ((null (node-l btree))
	 (node-r btree))
	((null (node-r btree))
	 (node-l btree))
	(t
	 (balance-node (node-color (node-r btree))
		       (node-elt (node-r btree))
		       (node-l btree)
		       (behead (node-r btree))))))

(defun balance-node (color elt l r)
  "Return a balanced node with the specified initial parameters."
  (macrolet ((make-nodes (&key x y z a b c d)
	       `(make-node :color :red
			   :elt ,y
			   :l (make-node :color :black
					 :elt ,x :l ,a :r ,b)
			   :r (make-node :color :black
					 :elt ,z :l ,c :r ,d))))
    (if (eql color :black)
	(if (and l (eql (node-color l) :red))
	    (let ((l-l (node-l l)))
	      (if (and l-l (eql (node-color l-l) :red))
		  (make-nodes :x (node-elt l-l) :y (node-elt l) 
			      :z elt
			      :a (node-l l-l) :b (node-r l-l) 
			      :c (node-r l) :d r)
		  (let ((l-r (node-r l)))
		    (if (and l-r (eql (node-color l-r) :red))
			(make-nodes :x (node-elt l) :y (node-elt l-r) 
				    :z elt
				    :a l-l :b (node-l l-r)
				    :c (node-r l-r) :d r)
			(make-node :color color :elt elt :l l :r r)))))
	    (if (and r (eql (node-color r) :red))
		(let ((r-l (node-l r)))
		  (if (and r-l (eql (node-color r-l) :red))
		      (make-nodes :x elt :y (node-elt r-l)
				  :z (node-elt r)
				  :a l :b (node-l r-l)
				  :c (node-r r-l) :d (node-r r))
		      (let ((r-r (node-r r)))
			  (if (and r-r (eql (node-color r-r) :red))
			      (make-nodes :x elt :y (node-elt r)
					  :z (node-elt r-r)
					  :a l :b r-l
					  :c (node-l r-r) :d (node-r r-r))
			      (make-node :color color :elt elt :l l :r r)))))
		(make-node :color color :elt elt :l l :r r)))
	(make-node :color color :elt elt :l l :r r))))

(defun map-btree (function btree)
  "Apply the specified FUNCTION to elements of the binary tree."
  (when btree
    (map-btree function (node-l btree))
    (funcall function (node-elt btree))
    (map-btree function (node-r btree))))

(defun reduce-btree (function btree &key (key #'identity) 
		     from-end (initial-value nil initial-value-p))
  "Reduce the elements of the binary tree using the specified FUNCTION."
  (cond
    ((null btree)
     (assert initial-value-p nil "Must be supplied an initial value.")
     initial-value)
    (from-end
     (reduce-btree function (node-l btree) :key key :from-end from-end
		   :initial-value 
		   (if initial-value-p
		       (funcall function 
				(funcall key (node-elt btree))
				(reduce-btree function (node-r btree)
					      :key key :from-end from-end
					      :initial-value initial-value))
		       (if (null (node-r btree)) (funcall key (node-elt btree))
			   (funcall function 
				    (funcall key (node-elt btree))
				    (reduce-btree function (node-r btree)
						  :key key :from-end from-end))))))
    (t
     (reduce-btree function (node-r btree) :key key :from-end from-end
		   :initial-value
		   (if initial-value-p
		       (funcall function
				(reduce-btree function (node-l btree)
					      :key key :from-end from-end
					      :initial-value initial-value)
				(funcall key (node-elt btree)))
		       (if (null (node-l btree)) (funcall key (node-elt btree))
			   (funcall function
				    (reduce-btree function (node-l btree)
						  :key key :from-end from-end)
				    (funcall key (node-elt btree)))))))))
			   
(defun btree->list (btree &key (key #'identity))
  "Return the list of elements of the binary tree."
  (let ((list nil))
    (map-btree (lambda (n) (push (funcall key n) list))
	       btree)
    (nreverse list)))

(defun list->btree (list predicate &key (key #'identity) (test #'eql))
  "Return the binary tree by the specified list of elements."
  (let ((btree nil))
    (loop for item in list
       do (setf btree (btree-insert item btree predicate :key key :test test)))
    btree))

(defun btree-length (btree)
  "Return the number of elements in the binary tree."
  (if (null btree)
      0
      (+ (btree-length (node-l btree))
	 1
	 (btree-length (node-r btree)))))

(defun btree-depth (btree)
  "Return the depth of the binary tree."
  (if (null btree)
      0
      (1+ (max (btree-depth (node-l btree))
	       (btree-depth (node-r btree))))))