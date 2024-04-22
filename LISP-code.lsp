;;Upward-CS3210
;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;; Examples:
;; (set-member '(1 2) 1) => T
;; (set-member '(1 2) 3) => NIL
(defun set-member (set item)
  (print (format "Checking if %s is in set %s" item set))
  (if (member item set)
      (progn
        (print "Item found in set")
        T)
      (progn
        (print "Item not found in set")
        NIL)))

;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;; (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
  (print (format "Computing union of sets %s and %s" set-1 set-2))
  (union set-1 set-2))

;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;; (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2)
  (print (format "Computing intersection of sets %s and %s" set-1 set-2))
  (intersection set-1 set-2))

;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;; (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
  (print (format "Computing difference of sets %s and %s" set-1 set-2))
  (set-difference set-1 set-2))

;; Return the exclusive or of a and b
;; Examples:
;; (boolean-xor t nil) => t
;; (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
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

;; Return the implication of a and b
;; Examples:
;; (boolean-implies t nil) => nil
;; (boolean-implies nil nil) => t
(defun boolean-implies (a b)
  (print (format "Computing implication of %s and %s" a b))
  (if (or (not a) b)
      (progn
        (print "Implication result: T")
        T)
      (progn
        (print "Implication result: NIL")
        NIL)))

;; Return the bi-implication (if and only if) of a and b
;; Examples:
;; (boolean-iff t nil) => nil
;; (boolean-iff nil nil) => t
(defun boolean-iff (a b)
  (print (format "Computing bi-implication of %s and %s" a b))
  (if (eql a b)
      (progn
        (print "Bi-implication result: T")
        T)
      (progn
        (print "Bi-implication result: NIL")
        NIL)))

;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;; Examples:
;; (boolean-eval '(and t nil)) => nil
;; (boolean-eval '(and t (or nil t)) => t
(defun boolean-eval (exp)
  (print (format "Evaluating boolean expression: %s" exp))
  (cond
    ((atom exp) exp)
    ((eq (car exp) 'not) (not (boolean-eval (cadr exp))))
    ((eq (car exp) 'and) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'or) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'xor) (boolean-xor (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'implies) (boolean-implies (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((eq (car exp) 'iff) (boolean-iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    (T NIL))))
