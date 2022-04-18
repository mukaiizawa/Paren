# NAME
fread, fwrite - binary stream input/output.

# SYNOPSIS

    (fread BUF FROM SIZE FP)
    (fwrite BUF FROM SIZE FP)

# DESCRIPTION
The function `fwrite` writes the `FROM-th` to `SIZE` byte of `BUF` to the stream associated with the file pointer `FP`.

The function `fread` reads up to `SIZE` bytes from the stream associated with the file pointer `FP` and copy it to the `FROM-th` to length of the data read byte of `BUF`.

# RETURN VALUE
Returns the number of bytes read and written.

# ERRORS
OSError may occur for some reason.

# NOTES
`FP` is created by `fopen` or `fp`.

# SEE ALSO
- `fgetc(3)`
- `fgets(3)`
- `fopen(3)`
- `fp(3)`
- `fputc(3)`
