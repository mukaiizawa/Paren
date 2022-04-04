# NAME
fopen - open the file.

# SYNOPSIS

    (fopen FILE MODE)

# DESCRIPTION
Opens the `FILE` in the specified `MODE`.

The argument mode can specify bellow value.

     0 -- Open file for reading.
     1 -- Open file for writing.
     2 -- Open file for appending
     3 -- Open file for reading and writing.

# RETURN VALUE
Returns file poiner for the opened `FILE`.

# EXAMPLES

    ) (fopen "foo.txt" 0)
    1979991648
    ) (fclose 1979991648)
    nil

# SEE ALSO
- `fclose(3)`
- `with-open(3)`
