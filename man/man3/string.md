# NAME
string, string! - make a string.

# SYNOPSIS

    (string [NAME [START [STOP]]])
    (string! BYTES)

# DESCRIPTION
These functions are create a string.

# RETURN VALUE
The function `string` returns a string consisting of the `START-th` through `(STOP - 1)-th` bytes-like object `NAME`.

If `STOP` is omitted, the corresponding string from the `START` to the end is returned.

If `START` is omitted, a string whose name is `NAME` is returned.

The function `string!` same as `string` except that it destructively modifies the specified bytes `BYTES`.

# NOTES
The function `string!` only destructively converts the bytes type to the string type, and does not check whether the contents are valid utf-8 byte strings.

In general, `string!` is faster than `string` because it only changes arguments destructively.

# EXAMPLES

    ) (begin0 (<- val (bytes 3)) ([] val 0 0x30) ([] val 1 0x31) ([] val 2 0x32))
    #[ 0x30 0x31 0x32 ]

    ) (string val)
    "012"
    ) val
    #[ 0x30 0x31 0x32 ]

    ) (string! val)
    "012"
    ) val
    "012"

# SEE ALSO
- str(3)
- bytes-like(7)
