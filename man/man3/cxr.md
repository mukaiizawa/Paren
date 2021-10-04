# NAME
car, cdr, caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr, caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar, cdaadr, cdadar, cdaddr, cddaar, cddadr, cdddar, cddddr - cons accessors.

# SYNOPSIS

    (car X)
    
    (cdr X)
    
    (caar X)
    
    (cadr X)
    
    (cdar X)
    
    (cddr X)
    
    (caaar X)
    
    (caadr X)
    
    (cadar X)
    
    (caddr X)
    
    (cdaar X)
    
    (cdadr X)
    
    (cddar X)
    
    (cdddr X)
    
    (caaaar X)
    
    (caaadr X)
    
    (caadar X)
    
    (caaddr X)
    
    (cadaar X)
    
    (cadadr X)
    
    (caddar X)
    
    (cadddr X)
    
    (cdaaar X)
    
    (cdaadr X)
    
    (cdadar X)
    
    (cdaddr X)
    
    (cddaar X)
    
    (cddadr X)
    
    (cdddar X)
    
    (cddddr X)

# DESCRIPTION
These functions are for accessing elements at specific locations in cons `X`.

Functions are provided which perform compositions of up to four car and cdr operations. 

# RETURN VALUE
The function `car` returns the car of that cons `X`.

The function `cdr` returns the cdr of that cons `X`.

If `X` is nil, `car` and `cdr` returns `nil`.

# ERRORS
Error if `X` is not a list.

# NOTES

    (caar x) <=> (car (car x))
    (cadr x) <=> (car (cdr x))
    (cdar x) <=> (cdr (car x))
    (cddr x) <=> (cdr (cdr x))
    (caaar x) <=> (car (car (car x)))
    (caadr x) <=> (car (car (cdr x)))
    (cadar x) <=> (car (cdr (car x)))
    (caddr x) <=> (car (cdr (cdr x)))
    (cdaar x) <=> (cdr (car (car x)))
    (cdadr x) <=> (cdr (car (cdr x)))
    (cddar x) <=> (cdr (cdr (car x)))
    (cdddr x) <=> (cdr (cdr (cdr x)))
    (caaaar x) <=> (car (car (car (car x))))
    (caaadr x) <=> (car (car (car (cdr x))))
    (caadar x) <=> (car (car (cdr (car x))))
    (caaddr x) <=> (car (car (cdr (cdr x))))
    (cadaar x) <=> (car (cdr (car (car x))))
    (cadadr x) <=> (car (cdr (car (cdr x))))
    (caddar x) <=> (car (cdr (cdr (car x))))
    (cadddr x) <=> (car (cdr (cdr (cdr x))))
    (cdaaar x) <=> (cdr (car (car (car x))))
    (cdaadr x) <=> (cdr (car (car (cdr x))))
    (cdadar x) <=> (cdr (car (cdr (car x))))
    (cdaddr x) <=> (cdr (car (cdr (cdr x))))
    (cddaar x) <=> (cdr (cdr (car (car x))))
    (cddadr x) <=> (cdr (cdr (car (cdr x))))
    (cdddar x) <=> (cdr (cdr (cdr (car x))))
    (cddddr x) <=> (cdr (cdr (cdr (cdr x))))

# EXAMPLES

    ) (car nil)
    nil
    ) (car '(1 2))
    1

    ) (cdr nil)
    nil
    ) (cdr '(1 2))
    (2)

# SEE ALSO
- car!(3)
- cdr!(3)
- cons(3)
- list(3)
- cons(7)
- list(7)
