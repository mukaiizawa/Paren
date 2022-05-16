# NAME
mkdir - make a directory.

# SYNOPSIS

    (mkdir PATH)

# DESCRIPTION
The function `mkdir` shall create a new directory with name `PATH`.

# RETURN VALUE
Returns `nil`.

# ERRORS
OSError may occur for some reason.

Error if `PATH` already exists.

# SEE ALSO
- `path(3)`
- `rename(3)`
- `remove(3)`
