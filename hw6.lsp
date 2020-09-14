;created by Zixuan Wang
;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
; This function is used to calculate the variable index following the given formula
; Arguments: n, node index. c, color index. k, the maximum color index
; Returns: the index of the propositional variable
(defun node2var (n c k)
  (+ (* (- n 1) k) c))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; This function is used to get a clause meaning the constraint, node n must be colored with at least one color
; Arguments: n, node index. c, color index. k, the maximum color index
; Returns: the clause that represents the at-least-one-color constraint
(defun at-least-one-color (n c k)
  (cond ((> c k) nil)
        ((> 1 k) nil)
        ((= c k) (list (node2var n c k)))
        (t (append (list (node2var n c k)) (at-least-one-color n (+ 1 c) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
; This function is used to get a clause meaning the constraint, node n must be colored with at most one color
; Arguments: n, node index. c, color index. k, the maximum color index
; Returns: the clause that represents the at-least-one-color constraint
(defun at-most-one-color (n c k)
  (cond ((> c k) nil)
        ((> 1 k) nil)
        (t (let* ((index (node2var n c k)))
                  (cond ((= 1 (- k c)) (list (list (* -1 index) (* -1 (+ 1 index)))))
                        (t (append (at-most-one-color-helpler (- k c) 1 index) (at-most-one-color n (+ 1 c) k))))))))


; This is a helpler function to help us complete the at-most-one-color task
(defun at-most-one-color-helpler (m s n)
  (cond ((= 1 m) (list (cons (* -1 n) (list (* -1 (+ s n))))))
        (t (append (list (cons (* -1 n) (list (* -1 (+ s n))))) (at-most-one-color-helpler (- m 1) (+ 1 s) n)))))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; This function is used to get clauses meaning the constraint, node n is clored with exactly one color
; Arguments: n, node index. k, the maximum color index
; Returns: a list of clauses that represents the appropriate color index for nodes
(defun generate-node-clauses (n k)
  (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k)))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; This function is used to get clauses meaning the constraint, the ends of an edge cannot have the same color
; Arguments: e, edge representation. k, the maximum color index
; Returns: a list of clauses that represents the constraints for the two ends of an edge
(defun generate-edge-clauses (e k)
  (cond ((null e) nil)
        (t (generate-edge-clauses-helpler e k 1))))

; This is a helpler function to help us complete the generate-edge-clauses task
(defun generate-edge-clauses-helpler (e k c)
  (cond ((> c k) nil)
        (t (append (list (cons (* -1 (node2var (first e) c k)) (list (* -1 (node2var (second e) c k))))) (generate-edge-clauses-helpler e k (+ 1 c))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
