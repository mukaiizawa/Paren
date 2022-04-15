# NAME
fputc - output of character.

# SYNOPSIS

    (fputc CH FP)

# DESCRIPTION
The function `fputc` write the byte specified by `CH` to the output stream pointed to by `FP`.

# RETURN VALUE
Returns `CH`.

# ERRORS
OSError may occur for some reason.

# NOTES
`FP` is created by `fopen` or `fp`.

# SEE ALSO
- `fgetc(3)`
- `fgets(3)`
- `fopen(3)`
- `fp(3)`
