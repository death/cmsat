;;;; +----------------------------------------------------------------+
;;;; | CMSAT                                                          |
;;;; +----------------------------------------------------------------+

(in-package #:cmsat)

;; Low level

(define-foreign-library cmsat
  (t "libcryptominisat5.so"))

(use-foreign-library cmsat)

(defctype sat-solver :pointer)

(defcfun cmsat-new sat-solver)

(defcfun cmsat-free :void
  (s sat-solver))

(defcfun cmsat-nvars :unsigned-int
  (s sat-solver))

(defcfun cmsat-add-clause bool
  (s sat-solver)
  (lits :pointer)
  (num-lits size-t))

(defcfun cmsat-add-xor-clause bool
  (s sat-solver)
  (vars :pointer)
  (num-vars size-t)
  (rhs bool))

(defcfun cmsat-new-vars :void
  (s sat-solver)
  (n size-t))

(defcstruct c-lit
  (x :uint32))

(defcstruct c-lbool
  (x :uint8))

(defcstruct slice-lit
  (vals :pointer)
  (num-vals size-t))

(defcstruct slice-lbool
  (vals :pointer)
  (num-vals size-t))

(defcfun cmsat-solve (:struct c-lbool)
  (s sat-solver))

(defcfun cmsat-solve-with-assumptions (:struct c-lbool)
  (s sat-solver)
  (assumptions :pointer)
  (num-assumptions size-t))

(defcfun cmsat-get-model (:struct slice-lbool)
  (s sat-solver))

(defcfun cmsat-get-conflict (:struct slice-lit)
  (s sat-solver))

(defcfun cmsat-set-num-threads :void
  (s sat-solver)
  (n :unsigned-int))

;; High level

(defun add-clauses (clauses solver)
  (let ((num-lits 0)
        (num-vars 0))
    (dolist (clause clauses)
      (let ((i 0))
        (dolist (x clause)
          (setf num-vars (max num-vars (abs x)))
          (incf i))
        (setf num-lits (max num-lits i))))
    (cmsat-new-vars solver num-vars)
    (with-foreign-object (lits '(:struct c-lit) num-lits)
      (dolist (clause clauses)
        (let ((i 0))
          (dolist (x clause)
            (let ((ptr (mem-aptr lits '(:struct c-lit) i)))
              (setf (foreign-slot-value ptr '(:struct c-lit) 'x)
                    (logior (ash (1- (abs x)) 1) (if (minusp x) 1 0))))
            (incf i))
          (unless (= 1 (cmsat-add-clause solver lits i))
            (warn "Inconsistency when adding clause ~S." clause)))))))

(defun get-model (solver return-undefs)
  (let* ((lbools (cmsat-get-model solver))
         (num-vals (getf lbools 'num-vals))
         (vals (getf lbools 'vals))
         (bools (make-array (1+ num-vals) :initial-element nil))
         (undefs (and return-undefs (make-array (1+ num-vals) :initial-element nil))))
    (dotimes (i num-vals)
      (let ((ptr (mem-aptr vals '(:struct c-lbool) i)))
        (multiple-value-bind (bool undef)
            (case (foreign-slot-value ptr '(:struct c-lbool) 'x)
              (0 (values t nil))
              (1 (values nil nil))
              (t (values nil t)))
          (setf (aref bools (1+ i)) bool)
          (when undefs
            (setf (aref undefs (1+ i)) undef)))))
    (values bools undefs)))

(defun solve (clauses &key (num-threads nil)
                           (return-undefs nil))
  "Solve the SAT problem defined by the supplied CLAUSES.

CLAUSES should be a list of clauses in conjunctive normal form.  Each
clause is represented as a list of disjuncts.  Each disjunct
designates a variable and whether or not it should be true or false.
The first variable is designated by the integer 1 and the rest are
designated by its successors.  If the variable is constrained to be
true, the sign is positive, and if it is constrained to be false, the
sign is negative.

NUM-THREADS is the number of threads to be used by the solver.  If its
value is NIL, the default number of threads is used.

If the problem is satisfiable, the solution is returned as two values.
The primary value is a vector of value assignments for the (1-based)
variables.  True assignments are T, and false and undefined
assignments are NIL.  If RETURN-UNDEFS is true, the secondary value is
another vector with T for undefined assignments and NIL otherwise.  If
RETURN-UNDEFS is false, the secondary value is NIL.

If the problem is not satisfiable, NIL and T are the two values
returned.

If the problem is undefined, NIL and NIL are the two values returned."
  (let ((solver (cmsat-new)))
    (unwind-protect
         (progn
           (when num-threads
             (cmsat-set-num-threads solver num-threads))
           (add-clauses clauses solver)
           (let ((lbool (cmsat-solve solver)))
             (case (getf lbool 'x)
               (0 (get-model solver return-undefs))
               (1 (values nil t))
               (t (values nil nil)))))
      (cmsat-free solver))))
