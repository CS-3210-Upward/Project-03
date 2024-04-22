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
