# NAME
fclose - close the file.

# SYNOPSIS

    (fclose FP)

# DESCRIPTION
Flushes the stream pointed to by `FP` (writing any buffered output data) and closes the underlying file descriptor.

# RETURN VALUE
Returns nil.

# NOTES
In general, the `with-open` macro is used, so it is not called directly.

# EXAMPLES

    ) (fopen "foo.txt" 0)
    1979991648
    ) (fclose 1979991648)
    nil

# SEE ALSO
- fopen(3)
- with-open(3)
