;created by Zixuan Wang

;Overall, this homework implements nine functions in lisp.
;Mostly, they are implemented in a recursive way and constructed in a base case and recursive calls.
;Some of them use functions other than themselves specified by the professor.


;TREE-CONTAINS checks whether number N appears in the ordered tree TREE
;Arguments: N, a number. TREE, an ordered tree
;Return values: T if N is in TREE and NIL if N is not in TREE
;Conditions: 1. if tree is empty, returns nil.
;            2. if tree is an atom, check if n equals it.
;            3. if tree is a list and the second element is n, returns t.
;            4. if tree is a list and n is smaller than the second element, check the left subtree.
;            5. if tree is a list and n is larger than the second element, check the right subtree.
(defun TREE-CONTAINS (N TREE)
	(cond ((null TREE) NIL)
		  ((atom TREE) (= N TREE))
		  ((= N (second TREE)) t)
		  ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
		  ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))))

;TREE-MIN returns the minimum number appearing in the ordered tree TREE
;Arguments: TREE, an ordered tree
;Return values: the minimum number appearing in TREE
;Conditions: 1. if tree is empty, returns nil.
;            2. if tree is a number, returns TREE.
;            3. if tree is a list, checks the left subtree.
(defun TREE-MIN (TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) TREE)
		  (t (TREE-MIN (first TREE)))))

;TREE-ORDER returns an pre-ordered list of the numbers appearing in the ordered tree TREE
;Arguments: TREE, an ordered tree
;Return values: an pre-ordered list of the numbers in TREE
;Conditions: 1. if tree is empty, returns nil.
;            2. if tree is a number, returns a list containing only this number.
;            3. if tree is a list, concatenates root, left, right, recursively in pre-order.
(defun TREE-ORDER (TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) (list TREE))
		  (t (append (TREE-ORDER (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))))

;SUB-LIST returns the sub-list of input list L starting at position START and having length LEN
;Arguments: L, a list. START, a non-negative integer. LEN, a non-negative integer
;Return values: a sublist of L starting at START with a length of LEN
;Conditions: 1. if L is empty, returns nil.
;            2. if LEN is zero, returns nil.
;            3. if START is zero, constructs the first element and the rest of L recursively with a length of LEN -1.
;            4. otherwise, recursively calls SUB-LIST on the rest starting at START -1.
(defun SUB-LIST (L START LEN)
	(cond ((null L) NIL)
		  ((= 0 LEN) NIL)
		  ((= 0 START) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
		  (t (SUB-LIST (cdr L) (- START 1) LEN))))

;SPLIT-LIST returns two sublists of the original list and the firt sublist contains 0 or 1 element more than the second
;Arguments: L, a list
;Return values: a list of two sublists of L and the first sublist is 0 or 1 element longer than the second
;let: we bind the length of L to the variable len and then use list to construct two sublists with the SUB-LIST function.
;if: 1. if len is even, then START of L1 is 0, START of L2 is len/2, LEN of L1 and L2 are both len/2.
;    2. if len is odd, then START of L1 is 0, START of L2 is (len+1)/2, LEN of L1 is (len+1)/2 and LEN of L2 is (len-1)/2.
(defun SPLIT-LIST (L)
	(let ((len (length L)))
		(list (SUB-LIST L 0 (if (evenp len) (/ len 2) (/ (+ 1 len) 2)))
			  (SUB-LIST L (if (evenp len) (/ len 2) (/ (+ 1 len) 2)) (if (evenp len) (/ len 2) (/ (- len 1) 2))))))

;BTREE-HEIGHTS returns the height of a binary tree
;Arguments: TREE, a binary tree
;Return values: the height of TREE
;Conditions: 1. if tree is empty, returns 0.
;            2. if tree is an atom, returns 0.
;            3. otherwise, we recursively calculate and compare the height of left subtree and the height of right subtree, adding 1 to the larger height.
;Note: lheight means height of the left subtree and rheight means height of the right subtree
(defun BTREE-HEIGHT (TREE)
	(cond ((null TREE) 0)
		  ((atom TREE) 0)
		  (t (let ((lheight (BTREE-HEIGHT (first TREE)))
		  	       (rheight (BTREE-HEIGHT (second TREE))))
		         (if (> lheight rheight) (+ lheight 1) (+ rheight 1))))))

;LIST2BTREE returns a balanced binary tree whose tree leaves are the elements of input list
;Arguments: LEAVES, a non-empty list of atoms
;Return values: a balanced binary tree whose leaves are elements in LEAVES
;Conditions: 1. if LEAVES is empty, returns nil.
;            2. if LEAVES contains only one element, returns that element.
;            3. if LEAVES contains two elements, returns a list of those two elements.
;            4. otherwise, calls SPLIT-LIST on LEAVES to split LEAVES into two sublists and work on each.
(defun LIST2BTREE (LEAVES)
	(cond ((null LEAVES) NIL)
		  ((= 1 (length LEAVES)) (car LEAVES))
		  ((= 2 (length LEAVES)) LEAVES)
		  (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES)))))))

;BTREE2LIST returns a list of atoms corresponding to the input binary tree, an invers of LIST2BTREE
;Arguments: TREE, a binary tree
;Return values: a list of atoms associated with TREE
;Conditions: 1. if tree is empty, returns nil.
;            2. if tree is a number, returns a list containing only that number.
;            3. if tree is a list, concatenates the lift got from the left subtree and that from the right subtree.
(defun BTREE2LIST (TREE)
	(cond ((null TREE) NIL)
		  ((numberp TREE) (list TREE))
		  (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))

;IS-SAME checks whether two number-only expreesions are identical
;Arguments: E1, an expression whose atoms are all numbers. E2, an expression whose atoms are all numbers
;Return values: T if E1 and E2 are identical and NIL otherwise
;Conditions: 1. if both E1 and E2 are empty, returns t.
;            2. if one of them is empty and the other is not, returns nil.
;            3. if both of them are only one atom (a number), checks if the two numbers are equal.
;            4. if both of them are lists, checks the first elements and then the rest.
(defun IS-SAME (E1 E2)
	(cond ((and (null E1) (null E2)) t)
		  ((or (null E1) (null E2)) NIL)
		  ((and (atom E1) (atom E2)) (= E1 E2))
		  ((and (listp E1) (listp E2)) (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
		  (t NIL)))
