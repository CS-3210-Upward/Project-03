function set-member(set, item):
    if item is in set:
        return True
    else:
        return False

function set-union(set-1, set-2):
    return union of set-1 and set-2

function set-intersection(set-1, set-2):
    return intersection of set-1 and set-2

function set-diff(set-1, set-2):
    return difference of set-1 and set-2

function boolean-xor(a, b):
    if a is True and b is False, or a is False and b is True:
        return True
    else:
        return False

function boolean-implies(a, b):
    if a is False or b is True:
        return True
    else:
        return False

function boolean-iff(a, b):
    if a is equal to b:
        return True
    else:
        return False

function boolean-eval(exp):
    if exp is an atom:
        return exp
    else if exp starts with 'not':
        return not boolean-eval(second element of exp)
    else if exp starts with 'and':
        return boolean-eval(second element of exp) AND boolean-eval(third element of exp)
    else if exp starts with 'or':
        return boolean-eval(second element of exp) OR boolean-eval(third element of exp)
    else if exp starts with 'xor':
        return boolean-xor(boolean-eval(second element of exp), boolean-eval(third element of exp))
    else if exp starts with 'implies':
        return boolean-implies(boolean-eval(second element of exp), boolean-eval(third element of exp))
    else if exp starts with 'iff':
        return boolean-iff(boolean-eval(second element of exp), boolean-eval(third element of exp))
    else:
        return False

