# NAME
atom? - determines whether argument is an atom.

# SYNOPSIS

    (atom? X)

# DESCRIPTION
The function `atom?` returns whether argument is an atom.

# RETURN VALUE
Returns whether `X` is an atom.

# NOTES

   (atom? x) <=> (! (cons? x))

# EXAMPLES

    ) (atom? nil)
    true
    ) (atom? 1)
    true
    ) (atom? (cons 1 nil))
    nil

# SEE ALSO
- `cons?(3)`
- `list?(3)`
