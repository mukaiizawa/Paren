# NAME
concat - concatenate the sequences.

# SYNOPSIS

    (concat [SEQUENCE ...])

# DESCRIPTION
The function `concat` concatenate the sequences.

# RETURN VALUE
Returns a sequence of concatenated arguments in the order they are supplied.

If there is no argument, `nil` is returned.

# NOTES
The return value will be created anew and none of the arguments will be changed.

# ERRORS
Error if the argument data types are not all the same. However, the symbol `nil` also behaves as an empty list.

# EXAMPLES

    ) (concat)
    nil

    ) (concat nil)
    nil
    ) (concat nil nil)
    nil
    ) (concat '(1 2) '(3))
    (1 2 3)
    ) (concat '(1) '(2))
    (1 2)
    ) (concat nil '(1) '(2))
    (1 2)

    ) (concat "0" "1" "2")
    "012"

    ) (concat (bytes 1) (bytes 2))
    #[ 0x00 0x00 0x00 ]

    ) (concat (array 1) (array 2))
    #[ nil nil nil ]

# SEE ALSO
- sequence(7)
