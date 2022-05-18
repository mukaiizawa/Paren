# NAME
rename - rename a file.

# SYNOPSIS

    (rename SRC DST)

# DESCRIPTION
The function `rename` shall change the name of a file.

The `SRC` argument points to the pathname of the file to be renamed. The `DST` argument points to the new pathname of the file.

# RETURN VALUE
Returns `nil`.

# ERRORS
OSError can occur in any of the following cases or for any reason.

- `SRC` already exists.
- `SRC` is a non-empty directory.

# SEE ALSO
- `path(3)`
- `remove(3)`
