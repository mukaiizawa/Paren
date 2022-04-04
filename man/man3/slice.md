# NAME
slice - create a sub-sequence from a sequence.

# SYNOPSIS

    (slice LIST [START [STOP]])
    (slice ARRAY [START [STOP]])
    (slice BYTES [START [STOP]])
    (slice STRING [START [STOP]])

# DESCRIPTION
The function `slice` create a sub-sequence.

A sub-sequence is always created. Therefore, it can be used for the purpose of copying the first argument.

# RETURN VALUE
Returns a sub-sequence of the first argument. The sub-sequence begins at the `START` and extends to the element at index `STOP - 1`.

If `START` is omitted, it defaults to `0`.

If `STOP` is omitted, it defaults to length of the first argument.

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

    ) (slice (array 3) 1 2)
    #[ nil ]

    ) (slice "foo" 1 2)
    "o"

# SEE ALSO
- `take(3)`
- `drop(3)`
