;created by Zixuan Wang

;;;BFS: performs a breadth-first search of a tree
;;;Arguments: TREE, a list representation of the tree
;;;Return values: a list of the terminal nodes in the order they would be visted by a left-to-right breadth-first search
(defun BFS (TREE)
	(cond ((null TREE) nil) ; if tree is null, returns nil
		  ((atom TREE) (list TREE)) ; if tree is an atom, returns a list of this atom
		  ((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE)))) ; if tree is a list and the first element is an atom, then does BFS on the rest
		  (t (BFS (append (cdr TREE) (car TREE)))))) ; if tree is a list and the first element is a list, checks if the rest has atoms and does BFS

;;;DFS: performs a depth-first search of a tree
;;;Arguments: TREE, a list representation of the tree
;;;Return values: a list of the terminal nodes in the order they would be visited by a right-to-left depth-first search
(defun DFS (TREE)
	(cond ((null TREE) nil) ; if tree is null, returns nil
		  ((atom TREE) (list TREE)) ; if tree is an atom, returns a list of this atom
		  (t (append (DFS (cdr TREE)) (DFS (car TREE)))))) ; if tree is a list, does DFS right-to-left

;;;DFID: performs a depth-first iterative-deepening search of a tree, this is the main entry point of the implementation
;;;Arguments: TREE, a list representation of the tree. DEPTH, an integer representing the maximum depth of the tree
;;;Return values: a list of the terminal nodes in the order that they would be visited by a left-to-right search
(defun DFID (TREE DEPTH)
	(cond ((null TREE) nil) ; if tree is null, returns nil
		  ((< DEPTH 0) nil) ; if depth is less than zero, returns nil
		  (t (DFID-IDS TREE DEPTH 0)))) ; otherwise, does a DFID on the tree starting from the root

;;;DFID-IDS: performs the iterative deepening search with a specific limit, this is one of the helpler function for DFID
;;;Arguments: TREE, a list representation of the tree. DEPTH, an integer representing the maximum depth. LIMIT, an integer representing the current level
;;;Return values: a list of the terminal nodes in the order that they would be visited by a left-to-right search
(defun DFID-IDS (TREE DEPTH LIMIT)
	(cond ((null TREE) nil) ; if tree is null, returns nil
		  ((< DEPTH 0) nil) ; if depth is less than zero, returns nil
		  ((< LIMIT 0) nil) ; if limit is less than zero, returns nil
		  ((< DEPTH LIMIT) nil) ; if depth is less than limit, returns nil
		  (t (append (DFID-DLS TREE LIMIT) (DFID-IDS TREE DEPTH (+ 1 LIMIT)))))) ; otherwise, does a depth-limited search on the tree and iteratively increase the limit by 1

;;;DFID-DLS: performs a depth-limited search to a specified level, this is the other helpler function for DFID
;;;Arguments: TREE, a list representation of the tree. DEPTH, an integer representing the level until which we need to search
;;;Return values: a list of the terminal nodes in the order that they would be visited by a left-to-right search
(defun DFID-DLS (TREE DEPTH)
	(cond ((null TREE) nil) ; if tree is null, returns nil
		  ((< DEPTH 0) nil) ; if depth if less than zero, returns nil
		  ((atom TREE) (list TREE)) ; if tree is an atom, returns a list of this atom
		  (t (append (DFID-DLS (car TREE) (- DEPTH 1)) (DFID-DLS (cdr TREE) DEPTH))))) ; otherwise, does the depth-limited search to the next level of the visited and then the rest

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond ((equal '(3 3 nil) s) t) ; if the current state is the goal state, returns t
		  (t nil))) ; otherwise, returns nil

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(cond ((null s) nil) ; if the current state is null, returns nil
		  ((> (+ m c) 2) nil) ; if the boat carries more than two people, returns nil
		  ((> m (first s)) nil) ; if it moves more missionaries than are on this side of the river, returns nil
		  ((> c (second s)) nil) ; if it moves more cannibals than are on this side of the river, returns nil
		  ((and (> (- (second s) c) (- (first s) m)) (> (- (first s) m) 0)) nil) ; if there are more cannibals than missionaries on this side of the river, returns nil
		  ((and (> (- 3 (- (second s) c)) (- 3 (- (first s) m))) (> (- 3 (- (first s) m)) 0)) nil) ; if there are more cannibals than missionaries on the other side, returns nil
		  (t (list (list (- 3 (- (first s) m)) (- 3 (- (second s) c)) (not (third s))))))) ; otherwise, returns a list containing the successor state

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(append (next-state s 1 0) (next-state s 0 1) (next-state s 2 0) (next-state s 0 2) (next-state s 1 1))) ; append all possible next states

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond ((null states) nil) ; if the stack of states is null, returns nil
		  ((equal (first states) s) t) ; if the current state matches the top state on the stack, returns t
		  (t (on-path s (rest states))))) ; otherwise, checks the rest states on the stack

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
	(cond ((null states) nil) ; if the stack of states is null, returns nil
		  ((null (mc-dfs (first states) path)) (mult-dfs (rest states) path)) ; if there is no path from the top state on the stack, checks the rest
		  (t (mc-dfs (first states) path)))) ; otherwise, searches the path from the top state on the stack

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond ((final-state s) (cons s path)) ; if the current state is already the goal state, appends it to the front of path
		  ((on-path s path) nil) ; if the current state is on the stack of visited states, returns nil
		  (t (mult-dfs (succ-fn s) (cons s path))))) ; otherwise, does a depth first search for each of possible legal successor states and constructs the path



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
