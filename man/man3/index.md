# NAME
index, last-index - locate a element.

# SYNOPSIS

    (index X ARRAY [START [END]])
    (index X BYTES [START [END]])
    (index X LIST [START [END]])
    (index X STRING [START [END]])
    
    (last-index X ARRAY [START [END]])
    (last-index X BYTES [START [END]])
    (last-index X LIST [START [END]])
    (last-index X STRING [START [END]])

# DESCRIPTION
The function `index` returns the key of the argument corresponding to `X`.

The function `last-index` is identical to `index` except that it searches from the tail. However, for dictionaries without the concept of order, it is identical to `index`.

If `START` and `END` are specified, the search is performed over a range equivalent to `(slice x START `END`)`. Their initial values are `0` and `(len x)`

# RETURN VALUE
If the argument is `ARRAY` or `BYTES` or `LIST`, the position of `X` is returned.

If the argument is `STRING`, returns the index within this string of the first occurrence of the specified substring `X`.

If `X` is not associated, returns `nil`.

# EXAMPLES

    ) (index 0 #[ 0 1 0 ])
    0
    ) (index 0 #[ 0 1 0 ] 1)
    2
    ) (index 0 #[ 0 1 0 ] 1 2)
    nil
    ) (index 1 #[ 0 1 0 ])
    1
    ) (index 10 #[ 0 1 0 ])
    nil
    ) (last-index 0 #[ 0 1 0 ])
    2
    ) (last-index 0 #[ 0 1 0 ] 0 1)
    0
    ) (last-index 0 #[ 0 1 0 ] 1 2)
    nil
    ) (last-index 1 #[ 0 1 0 ])
    1
    ) (last-index 10 #[ 0 1 0 ])
    nil

    ) (index 0x01 (bytes 3))
    nil
    ) (index 0x00 (bytes 3))
    0
    ) (last-index 0x01 (bytes 3))
    nil
    ) (last-index 0x00 (bytes 3))
    2

    ) (index 0 '(0 1 0))
    0
    ) (index 0 '(0 1 0) 1)
    2
    ) (index 0 '(0 1 0) 1 2)
    nil
    ) (index 1 '(0 1 0))
    1
    ) (index 2 '(0 1 0))
    nil
    ) (last-index 0 '(0 1 0))
    2
    ) (last-index 0 '(0 1 0) 0 1)
    0
    ) (last-index 0 '(0 1 0) 1 2)
    nil
    ) (last-index 1 '(0 1 0))
    1
    ) (last-index 2 '(0 1 0))
    nil

    ) (index "a" "abcab")
    0
    ) (index "a" "abcab" 1)
    3
    ) (index "a" "abcab" 1 2)
    nil
    ) (index "b" "abcab")
    1
    ) (index "d" "abcab")
    nil
    ) (index "ab" "abcab")
    0
    ) (index "bc" "abcab")
    1
    ) (last-index "a" "abcab")
    3
    ) (last-index "a" "abcab" 0 2)
    0
    ) (last-index "a" "abcab" 1 2)
    nil
    ) (last-index "b" "abcab")
    4
    ) (last-index "d" "abcab")
    nil
    ) (last-index "ab" "abcab")
    3
    ) (last-index "bc" "abcab")
    1

# SEE ALSO
- `[]`(3)
- in?(3)
- position(3)
