# NAME
!, &&, || - logical operators.

# SYNOPSIS

    (! EXPR)
    
    (&& [EXPR ...])
    
    (|| [EXPR ...])

# DESCRIPTION
Function `!` inverts the truth value of EXPR.

Macro `&&` and `||` evaluates each EXPR one at a time from left to right.

`&&` finishes the evaluation as soon as any EXPR evaluates to nil.

`|| finishes the evaluation as soon as any EXPR evaluates to not nil.

# RETURN VALUE
`!` returns whether EXPR is nil or not.

`&&` returns last evaluated value.

If there are no arguments, returns true.

`||` returns the first non-nil evaluation result.

If there are no arguments, returns nil.

# NOTES
`&&` and `||` are so-called Short-circuit evaluation.

# EXAMPLES

    ) (! nil)
    true
    ) (! 1)
    nil
    ) (! true)
    nil

    ) (&&)
    true
    ) (&& (<- i 0) (<- i 1) (<- i 2))
    2
    ) i
    2
    ) (&& (<- i 0) nil (<- i 2))
    nil
    ) i
    0

    ) (||)
    nil
    ) (|| nil nil (<- i 2))
    2
    ) i
    2
    ) (|| (<- i 0) nil (<- i 2))
    0
    ) i
    0
    )

# SEE ALSO
- if(3)
- boolean(7)
