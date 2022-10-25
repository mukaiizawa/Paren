# NAME
fopen, fclose - open and close the file.

# SYNOPSIS

    (fopen FILE MODE)
    (fclose FP)

# DESCRIPTION
The function `fopen` Opens the `FILE` in the specified `MODE`.

The argument `MODE` can specify bellow value.

     0 -- Open file for reading.
     1 -- Open file for writing.
     2 -- Open file for appending
     3 -- Open file for reading and writing.

The function `fclose` flushes the stream pointed to by `FP` (writing any buffered output data) and closes the underlying file descriptor.

# RETURN VALUE
The function `fopen` returns file poiner for the opened `FILE`.

The function `fopen` returns the `nil`.

# NOTES
In general, the `with-open` macro is used, so it is not called directly.

# EXAMPLES

    ) (fopen "foo.txt" 0)
    1979991648
    ) (fclose 1979991648)
    nil

# SEE ALSO
- `popen(3)`
- `pclose(3)`
- `with-open(3)`
