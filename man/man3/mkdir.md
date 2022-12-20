# NAME
mkdir - make a directory.

# SYNOPSIS

    (mkdir PATH)

# DESCRIPTION
The function `mkdir` shall create a new directory with name `PATH`.

# RETURN VALUE
Returns the `nil`.

# ERRORS
## OSError
May occur if the `PATH` already exists or for some other reason.

# SEE ALSO
- `path(3)`
- `rename(3)`
- `remove(3)`
