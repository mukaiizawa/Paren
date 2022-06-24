# NAME
with - creating an implicit context for the dictionary.

# SYNOPSIS

    (with DICT
        EXPR ...)

# DESCRIPTION
The macro `with` generates a context for simplified referencing and updating of dictionary properties.

Symbols in the context that would be keys in a dictionary are resolved to their values.

Symbols in assignment expressions are also resolved appropriately, so that updating dictionary values can be described concisely.

# RETURN VALUE
Returns result of evaluation of the last `EXPR`.

# EXAMPLES

    ) (<- d1 #{ a 3 b 4 c 5 } d2 #{ c 6 d 7 } c true)
    true
    
    ) (with d1 (list a b c))
    (3 4 5)
    
    ) (with d1 (<- a 10))
    10
    ) ([] d1 'a)
    10
    
    ) (with d1 (<- a (+ b c)))
    9
    ) ([] d1 'a)
    9
    
    ) (with d1 (<- a '(+ a b)))
    (+ a b)
    ) ([] d1 'a)
    (+ a b)
    
    ) (with d1 (select1 (f (x) (= x b)) (.. 9)))
    4
    ) (with d1 (select1 (f (b) (= b c)) (.. 9)))
    5
    ) (with d1 (select1 (f (x) (= (++ b) c)) (.. 9)))
    0
    
    ) (with d1 (let (x a) x))
    (+ a b)
    ) (with d1 (let (c (+ b c)) c))
    9
    
    ) (with d1 (+ c (with d2 c)))
    11

# SEE ALSO
- `[](3)`
- `in?(3)`
