;Created by Zixuan Wang

;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists

; this is the entry point to use our algorithm to solve the sat problem
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (solve-sat (initial-list n) delta nil))

; this function creates a list of (1 ... n) given the integer n
; param n: an integer related to the number of variables in delta
; if n is one, return a list containing only one, otherwise, recursively create the list
(defun initial-list (n)
  (cond ((<= n 0) nil)
        ((= 1 n) (list 1))
        (t (append (initial-list (- n 1)) (list n)))))

; this function checks if there is a single clause is empty
; Note that if after removing the literals that can be assigned a value, there is still a nonempty clause, delta cannot be satisfied
; param delta: a CNF represented as a list of lists
; if there is an empty clause, returns true, otherwise, check the rest of delta
(defun impossible (delta)
  (cond ((null delta) nil)
        ((null (car delta)) t)
        (t (impossible (cdr delta)))))

; this function checks if a specific lietral exists in a specific clause
; param literal: the literal to be checked
; param clause: the clause to be checked
; if we find the literal, returns true, otherwise, check the rest of the clause
(defun positive-literal-exist (literal clause)
  (cond ((null literal) nil)
        ((null clause) nil)
        ((= literal (car clause)) t)
        (t (positive-literal-exist literal (cdr clause)))))

; this function removes the negative literals in a clause
; param literal: the literal to be checked
; param clause: the clause to be checked
; if we find the negative literal, remove it and check the rest of clause
; otherwise, we keep the literal and check the rest of clause
(defun negative-literal-exist (literal clause)
  (cond ((null literal) nil)
        ((null clause) nil)
        (t (let* ((negative-literal (- literal)))
                 (cond ((= negative-literal (car clause)) (negative-literal-exist literal (cdr clause)))
                       (t (cons (car clause) (negative-literal-exist literal (cdr clause)))))))))

; this function removes the clauses in the delta if cur-literal exists and removes the negative literal from clause
; param cur-literal: the literal to be checked
; param delta: our CNF represented as list of lists
; if the literal is not in the clause, checks the negative version of the literal and sees if we can remove it
; otherwise, we delete the whole clause
(defun cur-delta (cur-literal delta)
  (cond ((null cur-literal) nil)
        ((null delta) nil)
        ((not (positive-literal-exist cur-literal (car delta))) (cons (negative-literal-exist cur-literal (car delta)) (cur-delta cur-literal (cdr delta))))
        (t (cur-delta cur-literal (cdr delta)))))

; this function is 
; param init: our list of (1 ... n), where n is related to the number of variables in delta
; param delta: a CNF represented as list of lists
; param cur-model: our current assignment of variables
; this function checks whether CNF can be satisfied or not with both positive and negative literals
; I would like to briefly explain what is going on: because a clause is a disjunction of literals,
; we can remove the whole clause if any one of the literal (positive or negative) is true
; in other words, any of the literal in a single clause which is true affects our evaluation, but the false literal does not have effect
; consequently, if the CNF finally becomes empty, that means all the clauses are satisfied and removed and we are done. otherwise, we get nil
(defun solve-sat (init delta cur-model)
  (cond ((impossible delta) nil)
        ((null delta) (append cur-model init))
        (t (let* ((positive-next (car init)) (negative-next (- positive-next)))
                 (or (solve-sat (cdr init) (cur-delta positive-next delta) (append cur-model (list positive-next)))
                     (solve-sat (cdr init) (cur-delta negative-next delta) (append cur-model (list negative-next))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

