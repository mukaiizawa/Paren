# NAME
stat - get file status.

# SYNOPSIS

    (stat PATH)

# DESCRIPTION
The function `stat` get the file status.

# RETURN VALUE
Returns the file status array of length `3` indicated `PATH`.

    0 -- file type and mode.
        1 none
        2 file
        4 dir
        8 other
        16 readable
        32 writable
    1 -- file size
    2 -- modification timestamp

# ERRORS
OSError may occur for some reason.

# SEE ALSO
- `path(3)`
