# NAME
is-a? - test if an object is of a given type.

# SYNOPSIS

    (is-a? OBJECT CLASS)

# DESCRIPTION
The function `is-a?` determines whether an object is an instance of a given class.

# RETURN VALUE
Returns whether the `OBJECT` regarded as the specified `CLASS's` instance.

# EXAMPLES

    ) (is-a? (.new Object) Object)
    true
    ) (is-a? (.new Object) Error)
    nil
    ) (is-a? (.new Error) Object)
    true

# SEE ALSO
- `class(3)`
- `object-system(7)`
