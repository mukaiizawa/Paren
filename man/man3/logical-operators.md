# NAME
!, &&, || - logical operators.

# SYNOPSIS

    (! X)
    (&& [EXPR ...])
    (|| [EXPR ...])

# DESCRIPTION
The function `!` inverts the logical value.

The macros `&&` and || combine logical values.

# RETURN VALUE
The function `!` returns whether `X` is `nil`.

The macro `&&` evaluates each `EXPR` one at a time from left to right. As soon as any `EXPR` evaluates to `nil`, returns `nil` without evaluating the remaining `EXPRs`. If all `EXPRs` but the last evaluate to `non-nil`, returns the results produced by evaluating the last `EXPR`. If no `EXPRs` are supplied, returns `true`.

The macro `||` evaluates each `EXPR`, one at a time, from left to right. The evaluation of all `EXPRs` terminates when a `EXPR` evaluates to `non-nil`. If the evaluation of any `EXPR` other than the last returns `non-nil`, immediately returns that value  without evaluating the remaining `EXPRs`. If every `EXPR` but the last returns `nil`, returns all values returned by the last `EXPR`. If no `EXPRs` are supplied, returns `nil`.

# NOTES
The macros `&&` and `||` are so-called Short-circuit evaluation.

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

# SEE ALSO
- if(3)
- when(3)
