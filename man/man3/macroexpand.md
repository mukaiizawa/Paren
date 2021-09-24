# NAME
macroexpand-1, macroexpand - expand a macro.

# SYNOPSIS

    (macroexpand-1 EXPR)
    
    (macroexpand EXPR)

# DESCRIPTION
These functions expand the macro.

# RETURN VALUE
`macroexpand-1` returns an expression that is a macro expansion of `EXPR`.

If the `EXPR` is not a macro expression, returns `EXPR`.

`macroexpand` is the same as `macroexpand` except that it expands recursively.

Also, macro expansion works properly even if it contains special-operators.

# NOTES
These macros do not expand `EXPR` that are not evaluable.

    ) (macroexpand '((&& 1 (&& 2 (&& 3))))))
    ((&& 1 (&& 2 (&& 3))))

# EXAMPLES

    ) (macroexpand-1 :foo)
    :foo
    ) (macroexpand :foo)
    :foo
    ) (macroexpand-1 '(+ 1 2 3))
    (+ 1 2 3)
    ) (macroexpand '(+ 1 2 3))
    (+ 1 2 3)
    ) (macroexpand-1 '(when 1 (when 2 3)))
    (if 1 (begin (when 2 3)))
    ) (macroexpand '(when 1 (when 2 3)))
    (if 1 (begin (if 2 (begin 3))))
    ) (macroexpand-1 '(&& 1 (&& 2 (&& 3))))
    (if 1 (&& 2 (&& 3)))
    ) (macroexpand '(&& 1 (&& 2 (&& 3))))
    (if 1 (if 2 3))

# SEE ALSO
- macro(3)
- special-operater(7)
