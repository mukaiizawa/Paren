# NAME
ftell - current position of stream.

# SYNOPSIS

    (ftell FP)

# DESCRIPTION
The function `ftell` obtains of the file position indicator for the stream pointed to by `FP`.

# RETURN VALUE
Returns the current value of the file position indicator for the stream pointed to by `FP`.

# ERRORS
## OSError
May occur for some reason.

# NOTES
`FP` is created by `fopen` or `fp`.

# SEE ALSO
- `fopen(3)`
- `fp(3)`
- `fseek(3)`
