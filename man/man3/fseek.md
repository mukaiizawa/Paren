# NAME
fseek - reposition a stream.

# SYNOPSIS

    (fseek FP OFFSET)

# DESCRIPTION
The function `fseek` sets the file position indicator of the stream pointed to by `FP` to byte `OFFSET` from the beginning.

If `-1` is specified, move to the end of the file.

# RETURN VALUE
Returns `nil`.

# ERRORS
OSError may occur for some reason.

# NOTES
`FP` is created by `fopen` or `fp`.

# SEE ALSO
- `fopen(3)`
- `fp(3)`
- `ftell(3)`
