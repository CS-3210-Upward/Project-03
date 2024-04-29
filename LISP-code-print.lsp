;;Upward-CS3210
(defun set-member (set item)
  ;; Return T if item is a member of set.
  ;; Return NIL if item is not a member of set.
  ;; The type of set is list.
  ;; Examples:
  ;; (set-member '(1 2) 1) => T
  ;; (set-member '(1 2) 3) => NIL
  (print (format "Checking if %s is in set %s" item set))
  (let ((result (custom-member set item)))
    (if result
        (progn
          (print "Item found in set")
          T)
        (progn
          (print "Item not found in set")
          NIL))))

(defun custom-member (set item)
  ;; Custom implementation of member function with print statements
  (print (format "Checking if %s is in set %s" item set))
  (cond
    ;; Base case: if set is empty, item is not found
    ((null set)
     (progn
       (print (format "Item %s not found in set" item))
       NIL))
    ;; If the current element of set matches item, item is found
    ((equal item (car set))
     (progn
       (print (format "Item %s found in set" item))
       T))
    ;; Recursive case: search for item in the rest of the list
    (T
     (progn
       (print (format "Checking rest of set: %s" (cdr set)))
       (custom-member (cdr set) item)))))


(defun set-union (set-1 set-2)
  ;; Return the union of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-union '(1 2) '(2 4)) => '(1 2 4)
  (print (format "Computing union of sets %s and %s" set-1 set-2))
  (custom-union set-1 set-2))

(defun custom-union (set1 set2)
  ;; Custom implementation of union function with print statements
  (print (format "Computing union of sets %s and %s" set1 set2))
  (cond
    ;; Base case: if set1 is empty, union is set2
    ((null set1) set2)
    ;; If the first element of set1 is already in set2, skip it
    ((custom-member set2 (car set1)) (custom-union (cdr set1) set2))
    ;; Otherwise, add the first element of set1 to the union and continue with the rest
    (T (cons (car set1) (custom-union (cdr set1) set2)))))


(defun set-intersection (set-1 set-2)
  ;; Return the intersection of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-intersection '(1 2) '(2 4)) => '(2)
  (print (format "Computing intersection of sets %s and %s" set-1 set-2))
  (custom-intersection set-1 set-2))

(defun custom-intersection (set1 set2)
  ;; Custom implementation of intersection function with print statements
  (print (format "Computing intersection of sets %s and %s" set1 set2))
  (cond
    ;; Base case: if set1 is empty, intersection is empty
    ((null set1) '())
    ;; If the first element of set1 is in set2, include it in the intersection and continue
    ((custom-member set2 (car set1)) (cons (car set1) (custom-intersection (cdr set1) set2)))
    ;; Otherwise, continue with the rest of set1
    (T (custom-intersection (cdr set1) set2))))

(defun set-diff (set-1 set-2)
  ;; Return the difference of set-1 and set-2.
  ;; The result should contain no duplicates.
  ;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
  ;; Examples:
  ;; (set-diff '(1 2) '(2 4)) => '(1)
  (print (format "Computing difference of sets %s and %s" set-1 set-2))
  (custom-difference set-1 set-2))

(defun custom-difference (set1 set2)
  ;; Custom implementation of difference function with print statements
  (print (format "Computing difference of sets %s and %s" set1 set2))
  (cond
    ;; Base case: if set1 is empty, difference is empty
    ((null set1) '())
    ;; If the first element of set1 is in set2, skip it and continue with the rest
    ((custom-member set2 (car set1)) (custom-difference (cdr set1) set2))
    ;; Otherwise, include the first element of set1 in the difference and continue
    (T (cons (car set1) (custom-difference (cdr set1) set2)))))

(defun boolean-xor (a b)
  ;; Return the exclusive or of a and b
  ;; Examples:
  ;; (boolean-xor t nil) => t
  ;; (boolean-xor nil nil) => nil
  (print (format "Computing XOR of %s and %s" a b))
  (if (and (not a) b)
      (progn
        (print "XOR result: T")
        T)
      (if (and a (not b))
          (progn
            (print "XOR result: T")
            T)
          (progn
            (print "XOR result: NIL")
            NIL))))

(defun boolean-implies (a b)
  ;; Return the implication of a and b
  ;; Examples:
  ;; (boolean-implies t nil) => nil
  ;; (boolean-implies nil nil) => t
  (print (format "Computing implication of %s and %s" a b))
  (if (or (not a) b)
      (progn
        (print "Implication result: T")
        T)
      (progn
        (print "Implication result: NIL")
        NIL)))

(defun boolean-iff (a b)
  ;; Return the bi-implication (if and only if) of a and b
  ;; Examples:
  ;; (boolean-iff t nil) => nil
  ;; (boolean-iff nil nil) => t
  (print (format "Computing bi-implication of %s and %s" a b))
  (if (equal a b)
      (progn
        (print "Bi-implication result: T")
        T)
      (progn
        (print "Bi-implication result: NIL")
        NIL)))

(defun boolean-eval (exp)
  ;; Evaluate a boolean expression.
  ;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
  ;; Examples:
  ;; (boolean-eval '(and t nil)) => nil
  ;; (boolean-eval '(and t (or nil t)) => t
  (print (format "Evaluating boolean expression: %s" exp))
  (cond
    ((atom exp) exp)
    ((equal (car exp) 'not) (not (boolean-eval (cadr exp))))
    ((equal (car exp) 'and) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'or) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'xor) (boolean-xor (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'implies) (boolean-implies (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal (car exp) 'iff) (boolean-iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    (T NIL)))
