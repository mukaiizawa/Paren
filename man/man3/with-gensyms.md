# NAME
with-gensyms - create a context for unbound symbols.

# SYNOPSIS

    (with-gensyms (SYMBOL ...)
        EXPR ...)

# DESCRIPTION
The macro `with-gensyms` create an environment in which `SYMBOLs` are bound by the symbols created by `gensyms`, and evaluate `EXPRs` in left to right.

Generally used in macro definitions to avoid unintended symbol binding.

# RETURN VALUE
Returns evaluation result of last `EXPRs`.

# NOTES

    (with-gensyms (a b c) ...)
    <=> (let (a (gensym) b (gensym) c (gensym)) ...)

# SEE ALSO
- macro(3)
- special-operator(7)
