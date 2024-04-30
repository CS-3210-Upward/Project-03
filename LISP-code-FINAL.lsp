;;Upward-CS3210


;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;; Examples:
;; (set-member '(1 2) 1) => T
;; (set-member '(1 2) 3) => NIL
(defun set-member (set item)
  (cond
    ((null set) NIL) ; check if set is empty. if so, return NIL
    ((equal item (car set)) T) ; check if current item matches target item
    ((set-member (cdr set) item)))) ; not a match? recursively search the rest of the list


;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;; (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
 (cond
    ((null set-1) set-2) ; if set 1 is empty return set 2
    ((set-member set-2 (car set-1)) ; checks if first element in set-1 present in set-2. If it is...vvv 
     (set-union (cdr set-1) set-2)) ; skips adding this element to the result to call 'set-union' again, avoids duplicates
    (T (cons (car set-1) (set-union (cdr set-1) set-2))))) ; like elseif, if first element of set-1 is not in set-2. adds first element of set-1 to the result using cons, recursive calls to set-union for rest of set-1 and set-2


;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;; (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2)
  (cond
    ((null set-1) '())  ; if set-1 is empty return an empty list
    ((set-member set-2 (car set-1))
     (cons (car set-1) (set-intersection (cdr set-1) set-2))) ; Add the first element of set-1 to result if it's also present in set-2, and recursively call set-intersection for rest of sets
    (T (set-intersection (cdr set-1) set-2)))) ; if first element of set-1 is not present in set-2, skip and move to next element in list
    

;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Keeps value in set 1 if not in set 2. 
;; Examples:
;; (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
  (cond
    ((null set-1) '())  ; if set-1 is empty return an empty list
    ((set-member set-2 (car set-1)) ; check if first item of set-1 exists in set-2.
     (set-diff (cdr set-1) set-2)) ; If it is, recursively call set-diff with rest of set-1 and set-2
    (T (cons (car set-1) (set-diff (cdr set-1) set-2)))))  ;if first item of set-1 is not in set-2, adds to result, recursively calls itself 


;; Return the exclusive or of a and b
;; Examples:
;; (boolean-xor t nil) => t
;; (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
  (if (and (not a) b)  ; If a is false and b is true
      T
      (and a (not b))))  ; If a is true and b is false


;; Return the implication of a and b
;; Examples:
;; (boolean-implies t nil) => nil
;; (boolean-implies nil nil) => t
(defun boolean-implies (a b)
  (if (or (not a) b) ; check if either a is false or b is true
      T ; if either condition is true, return true
      NIL)) ; neither condition is true? return false


;; Return the bi-implication (if and only if) of a and b
;; Examples:
;; (boolean-iff t nil) => nil
;; (boolean-iff nil nil) => t
(defun boolean-iff (a b)
(if (equal a b) ; If and only if a = b
      T 
      NIL))


;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;; Examples:
;; (boolean-eval '(and t nil)) => nil
;; (boolean-eval '(and t (or nil t)) => t
(defun boolean-eval (exp)
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