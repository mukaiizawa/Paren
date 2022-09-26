# NAME
eval, apply - evaluation and application.

# SYNOPSIS

    (eval EXPR)
    (apply FN ARGS)

# DESCRIPTION
The function `eval` evaluates expression `EXPR` in the current dynamic environment.

The function `apply` applies the function `FN` to the `ARGS`.

# RETURN VALUE
Returns evaluation or application result.

# NOTES

    (apply fn args) <=> (eval (cons fn args))

# EXAMPLES

    ) (eval '(+ 1 2))
    3
    
    ) (apply + '(1 2))
    3

# SEE ALSO
- `import(3)`
- `load(3)`
- `read(3)`
- `repl(3)`
