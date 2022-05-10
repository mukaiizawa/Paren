# NAME
remove - remove a file.

# SYNOPSIS

    (remove PATH)

# DESCRIPTION
The function remove attempts to delete the file indicated by `PATH`.

# RETURN VALUE
Returns `nil`.

# ERRORS
OSError may occur for some reason.

It is also an error if the deletion target `PATH` is not a non-empty directory.

# SEE ALSO
- `path(3)`
- `mkdir(3)`
- `rename(3)`
