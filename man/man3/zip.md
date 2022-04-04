# NAME
zip - rows into columns.

# SYNOPSIS

    (zip LIST ...)

# DESCRIPTION
The function `zip` returns the transposed result of the argument as a matrix.

# RETURN VALUE
Returns the transposed result of the argument as a matrix.

# NOTES
As with `map(3)`, if the number of elements in each argument list differs, it is aligned with the length of the first list.

# EXAMPLES

    ) (zip '("Alice" "Bob" "Charlie") '(10 32 24) '(m f f))
    (("Alice" 10 m) ("Bob" 32 f) ("Charlie" 24 f))
    ) (apply zip '(("Alice" 10 m) ("Bob" 32 f) ("Charlie" 24 f)))
    (("Alice" "Bob" "Charlie") (10 32 24) (m f f))

# SEE ALSO
- `dolist(3)`
- `foreach(3)`
- `map(3)`
