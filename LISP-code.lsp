;;Upward-CS3210
(defun set-member (set item)
  ;; Return T if item is a member of set.
  ;; Return NIL if item is not a member of set.
  ;; The type of set is list.
  ;; Examples:
  ;; (set-member '(1 2) 1) => T
  ;; (set-member '(1 2) 3) => NIL
  (let ((result (custom-member set item)))
    (if result
        T
        NIL)))

(defun custom-member (set item)
  ;; Custom implementation of member function
  (cond
    ((null set) NIL)
    ((equal item (car set)) T)
    (T (custom-member (cdr set) item))))

(defun set-union (set-1 set-2)
  ;; Return the union of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-union '(1 2) '(2 4)) => '(1 2 4)
  (custom-union set-1 set-2))

(defun custom-union (set1 set2)
  ;; Custom implementation of union function
  (cond
    ((null set1) set2)
    ((custom-member set2 (car set1)) (custom-union (cdr set1) set2))
    (T (cons (car set1) (custom-union (cdr set1) set2)))))

(defun set-intersection (set-1 set-2)
  ;; Return the intersection of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-intersection '(1 2) '(2 4)) => '(2)
  (custom-intersection set-1 set-2))

(defun custom-intersection (set1 set2)
  ;; Custom implementation of intersection function
  (cond
    ((null set1) '())
    ((custom-member set2 (car set1)) (cons (car set1) (custom-intersection (cdr set1) set2)))
    (T (custom-intersection (cdr set1) set2))))

(defun set-diff (set-1 set-2)
  ;; Return the difference of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-diff '(1 2) '(2 4)) => '(1)
  (custom-difference set-1 set-2))

(defun custom-difference (set1 set2)
  ;; Custom implementation of difference function
  (cond
    ((null set1) '())
    ((custom-member set2 (car set1)) (custom-difference (cdr set1) set2))
    (T (cons (car set1) (custom-difference (cdr set1) set2)))))

(defun boolean-xor (a b)
  ;; Return the exclusive or of a and b
  ;; Examples:
  ;; (boolean-xor t nil) => t
  ;; (boolean-xor nil nil) => nil
  (if (and (not a) b)
      T
      (if (and a (not b))
          T
          NIL)))

(defun boolean-implies (a b)
  ;; Return the implication of a and b
  ;; Examples:
  ;; (boolean-implies t nil) => nil
  ;; (boolean-implies nil nil) => t
  (if (or (not a) b)
      T
      NIL))

(defun boolean-iff (a b)
  ;; Return the bi-implication (if and only if) of a and b
  ;; Examples:
  ;; (boolean-iff t nil) => nil
  ;; (boolean-iff nil nil) => t
  (if (equal a b)
      T
      NIL))

(defun boolean-eval (exp)
  ;; Evaluate a boolean expression.
  ;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
  ;; Examples:
  ;; (boolean-eval '(and t nil)) => nil
  ;; (boolean-eval '(and t (or nil t)) => t
  (cond
    ((atom exp) exp)
    ((equal (car exp) 'not) (not (boolean-eval (cadr exp))))
    ((equal (car exp) 'and) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'or) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'xor) (boolean-xor (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'implies) (boolean-implies (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'iff) (boolean-iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    (T NIL)))

;;TESTS
;;SET MEMBER
(print "Testing: Set-Member with params '(1 2) 1")
(print(set-member '(1 2) 1)) ;T 
(print "Testing: Set-Member with params '(1 2) 3")
(print(set-member '(1 2) 3)) ;NIL

;;SET-UNION
(print "Testing: Set-Union with params '(1 2) '(2 4)")
(print(set-union '(1 2) '(2 4))) ;'(1 2 4)

;;SET-INTERSECTION
(print "Testing: Set-Intersection with params '(1 2) '(2 4)")
(print(set-intersection '(1 2) '(2 4))) ; '(2)

;;SET-DIFF
(print "Testing: Set-Diff with params '(1 2) '(2 4)")
(print(set-diff '(1 2) '(2 4))) ; '(1)

;;BOOLEAN-XOR
(print "Testing: Boolean-XOR with params '(1 2 3) '(1 2 3)")
(print(boolean-xor T NIL)) ; T
(print "Testing: Boolean-XOR with params nil nil")
(print(boolean-xor NIL NIL)) ; NIL

;;BOOLEAN-IMPLIES
(print "Testing: Boolean-implies with params T NIL")
(print(boolean-implies T NIL)) ; NIL
(print "Testing: Boolean-implies with params NIL NIL")
(print(boolean-implies NIL NIL)) ; T

;;BOOLEAN-IFF
(print "Testing: Boolean-iff with params T NIL")
(print (boolean-iff T NIL)) ; NIL
(print "Testing: Boolean-iff with params NIL NIL")
(print (boolean-iff NIL NIL)) ; T

;;BOOLEAN-EVAL
(print "Testing: Boolean-eval with params '(and T NIL)")
(print(boolean-eval '(and t nil))) ;NIL
(print "Testing: Boolean-eval with params '(and t (or nil t))")
(print(boolean-eval '(and t (or nil t)))) ;T