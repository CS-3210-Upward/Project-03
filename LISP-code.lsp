;;Upward-CS3210
;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;; Examples:
;; (set-member '(1 2) 1) => T
;; (set-member '(1 2) 3) => NIL
(defun set-member (set item)
  (cond
    ((null set) NIL)
    ((equal item (car set)) T)
    (T (set-member (cdr set) item))))

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
;; Examples:
;; (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
  (cond
    ((null set-1) '())  ; if set-1 is empty return an empty list
    ((set-member set-2 (car set-1))
     (set-diff (cdr set-1) set-2))
    (T (cons (car set-1) (set-diff (cdr set-1) set-2))))) 

;; Return the exclusive or of a and b
;; Examples:
;; (boolean-xor t nil) => t
;; (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
  (if (equal a b)
      T
      NIL))

;; Return the implication of a and b
;; Examples:
;; (boolean-implies t nil) => nil
;; (boolean-implies nil nil) => t
(defun boolean-implies (a b)
  (if (or (not a) b)
      T
      NIL))

;; Return the bi-implication (if and only if) of a and b
;; Examples:
;; (boolean-iff t nil) => nil
;; (boolean-iff nil nil) => t
(defun boolean-iff (a b)
(cond ((and (equal a T) (equal b NIL)) NIL)
        ((and (equal a NIL) (equal b T)) NIL)
        ((or (and (equal a T) (equal b T))
             (and (equal a NIL) (equal b NIL))) T)
        (T NIL)))




;  (if (eql a b)
;      T
;     NIL))

;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;; Examples:
;; (boolean-eval '(and t nil)) => nil
;; (boolean-eval '(and t (or nil t)) => t
(defun boolean-eval (exp)
  (cond
    ((atom exp) exp)
    ((eq (car exp) 'not) (not (boolean-eval (cadr exp))))
    ((eq (car exp) 'and) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'or) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'xor) (boolean-xor (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'implies) (boolean-implies (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'iff) (boolean-iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    (T NIL)))

(print "Testing: Set-Member with params '(1 2) 1")
(print(set-member '(1 2) 1)) ;T 
(print "Testing: Set-Member with params '(1 2) 3")
(print(set-member '(1 2) 3)) ;NIL


(print "Testing: Set-Union with params '(1 2) '(2 4)")
(print(set-union '(1 2) '(2 4))) ;'(1 2 4)


(print "Testing: Set-Intersection with params '(1 2) '(2 4)")
(print(set-intersection '(1 2) '(2 4))) ; '(2)



(print "Testing: Set-Diff with params '(1 2) '(2 4)")
(print(set-diff '(1 2) '(2 4))) ; '(1)



(print "Testing: boolean-iff with params T NIL")
(print(boolean-iff t nil)) ; NIL
(print "Testing: boolean-iff with params NIL NIL")
(print(boolean-iff nil nil)) ; T



(print "Testing: Boolean-XOR with params '(1 2 3) '(1 2 3)")
(print(boolean-xor '(1 2 3) '(1 2 3))) ; T
(print "Testing: Boolean-XOR with params nil nil")
(print(boolean-xor nil nil)) ; NIL



(print "Testing: Boolean-iff with params T NIL")
(print (boolean-iff T NIL))
(print "Testing: Boolean-iff with params NIL NIL")
(print (boolean-iff NIL NIL))

