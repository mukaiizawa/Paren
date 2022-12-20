# NAME
fgetc, fgets - input of characters and strings.

# SYNOPSIS

    (fgetc FP)
    (fgets FP)

# DESCRIPTION
These functions read byte/line from the stream associated with the file pointer `FP`.

# RETURN VALUE
The function `fgetc` returns read byte. If the stream reaches EOF, return `-1`.

The function `fgets` returns read line. The return value does not include the newline character. If the stream reaches EOF, returl `nil`.

# ERRORS
## OSError
May occur for some reason.

# NOTES
`FP` is created by `fopen` or `fp`.

# SEE ALSO
- `fopen(3)`
- `fp(3)`
- `fputc(3)`
