# NAME
slice - create a sub-sequence from a sequence.

# SYNOPSIS

    (slice SEQUENCE [START [STOP]])

# DESCRIPTION
The function `slice` create a sub-sequence.

A sub-sequence is always created. Therefore, it can be used for the purpose of copying `SEQUENCE`. On the other hand, care must be taken when calling it on a huge `SEQUENCE`.

# RETURN VALUE
Returns a sub-sequence of `SEQUENCE`. The sub-sequence begins at the `START` and extends to the element at index `STOP - 1`.

If `START` is omitted, it defaults to `0`.

If `STOP` is omitted, it defaults to length of `SEQUENCE`.

# NOTES

    (slice seq)
    <=> (slice seq 0)
    <=> (slice seq 0 (len seq))

    (len (slice seq a b))
    <=> (- b a)

# EXAMPLES

    ) (slice nil)
    nil
    ) (slice (.. 3))
    (0 1 2)
    ) (slice (.. 3) 0)
    (0 1 2)
    ) (slice (.. 3) 0 3)
    (0 1 2)
    ) (slice (.. 3) 1 2)
    (1)
    ) (slice (.. 3) 1 3)
    (1 2)

    ) (slice (bytes 3) 1 2)
    #[ 0x00 ]

    ) (slice (array 3) 1 2)
    #[ nil ]

    ) (slice "foo" 1 2)
    "o"

# SEE ALSO
- take(3)
- drop(3)
