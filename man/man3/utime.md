# NAME
utime - set file access and modification times.

# SYNOPSIS

    (utime PATH UNIX-TIME)

# DESCRIPTION
The function `utime` changes the access time and modification time of the file specified by `PATH` to the specified `UNIX-TIME`.

# RETURN VALUE
Returns `nil`.

# ERRORS
OSError may occur for some reason.

# SEE ALSO
- `path(3)`
- `stat(3)`
