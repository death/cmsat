;;;; +----------------------------------------------------------------+
;;;; | CMSAT                                                          |
;;;; +----------------------------------------------------------------+

(in-package #:cmsat)

;; This is the Sudoku solving example from
;; https://codingnest.com/modern-sat-solvers-fast-neat-underused-part-1-of-n/
;;
;; The puzzle is from
;; http://elmo.sbs.arizona.edu/sandiway/sudoku/examples.html

(defvar *sudoku-rows* 9
  "The number of rows in the Sudoku board.")

(defvar *sudoku-cols* 9
  "The number of columns in the Sudoku board.")

(defvar *sudoku-vals* 9
  "The number of possible values in a Sudoku cell.")

(defun to-var (row col val)
  "Return a variable designation for the supplied coordinates and
value."
  (+ (* row *sudoku-cols* *sudoku-vals*)
     (* col *sudoku-vals*)
     val
     1))

(defvar *clauses* '()
  "List of conjuncts.")

(defvar *clause* '()
  "List of disjuncts.")

(defun lit (x)
  "Add a literal to the current clause."
  (push x *clause*))

(defun clause ()
  "Add the current clause to the list of clauses."
  (when *clause*
    (push *clause* *clauses*)
    (setf *clause* '())))

(defun exactly-one-true ()
  "Add clauses ensuring exactly one of the literals in the current
clause is true."
  (let ((literals *clause*))
    (clause)
    (do ((i literals (cdr i)))
        ((null i))
      (do ((j (cdr i) (cdr j)))
          ((null j))
        (lit (- (car i)))
        (lit (- (car j)))
        (clause)))))

(defun rule-1 ()
  "No row contains duplicate numbers."
  (dotimes (row *sudoku-rows*)
    (dotimes (val *sudoku-vals*)
      (dotimes (col *sudoku-cols*)
        (lit (to-var row col val)))
      (exactly-one-true))))

(defun rule-2 ()
  "No column contains duplicate numbers."
  (dotimes (col *sudoku-cols*)
    (dotimes (val *sudoku-vals*)
      (dotimes (row *sudoku-rows*)
        (lit (to-var row col val)))
      (exactly-one-true))))

(defun rule-3 ()
  "None of the 3x3 boxes contain duplicate numbers."
  (do ((r 0 (+ r 3)))
      ((= r *sudoku-rows*))
    (do ((c 0 (+ c 3)))
        ((= c *sudoku-cols*))
      (dotimes (val *sudoku-vals*)
        (dotimes (rr 3)
          (dotimes (cc 3)
            (lit (to-var (+ r rr) (+ c cc) val))))
        (exactly-one-true)))))

(defun rule-4 ()
  "Each position contains exactly one number."
  (dotimes (row *sudoku-rows*)
    (dotimes (col *sudoku-cols*)
      (dotimes (val *sudoku-vals*)
        (lit (to-var row col val)))
      (exactly-one-true))))

(defun apply-board (initial-board)
  "Add clauses for initial Sudoku board configuration."
  (let ((row 0)
        (col 0))
    (dolist (val initial-board)
      (unless (eq val '_)
        (lit (to-var row col (1- val)))
        (clause))
      (incf col)
      (when (= col *sudoku-cols*)
        (setf col 0)
        (incf row)))))

(defun sudoku-clauses (initial-board)
  "Return the list of SAT clauses representing the Sudoku problem for
the initial board supplied."
  (let ((*clauses* '())
        (*clause* '()))
    (rule-1)
    (rule-2)
    (rule-3)
    (rule-4)
    (apply-board initial-board)
    *clauses*))

(defun print-board (assignments)
  "Print a Sudoku board for the supplied variable assignment vector."
  (dotimes (row *sudoku-rows*)
    (dotimes (col *sudoku-cols*)
      (dotimes (val *sudoku-vals*)
        (let ((assignment (aref assignments (to-var row col val))))
          (when assignment
            (format t " ~D" (1+ val))
            (return)))))
    (terpri)))

(defun sudoku (initial-board)
  "Print a solved Sudoku board for the supplied initial board
configuration."
  (print-board (solve (sudoku-clauses initial-board))))

(defvar *example-board*
  '(_ 2 _ _ _ _ _ _ _
    _ _ _ 6 _ _ _ _ 3
    _ 7 4 _ 8 _ _ _ _
    _ _ _ _ _ 3 _ _ 2
    _ 8 _ _ 4 _ _ 1 _
    6 _ _ 5 _ _ _ _ _
    _ _ _ _ 1 _ 7 8 _
    5 _ _ _ _ 9 _ _ _
    _ _ _ _ _ _ _ 4 _)
  "A particularly fun puzzle.")

(defun example ()
  "Demonstrate Sudoku-as-SAT-problem."
  (sudoku *example-board*))
