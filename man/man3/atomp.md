# NAME
atom? - determines whether argument is atom.

# SYNOPSIS

    (atom? X)

# DESCRIPTION
The function `atom?` returns whether argument is atom.

# RETURN VALUE
Returns whether `X` is atom.

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
- list?(3)
- cons?(3)
