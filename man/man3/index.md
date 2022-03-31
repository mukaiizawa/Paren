# NAME
index, last-index - locate a element.

# SYNOPSIS

    (index ARRAY VAL [START [END]])
    (index BYTES VAL [START [END]])
    (index DICTIONARY VAL)
    (index LIST VAL [START [END]])
    (index STRING VAL [START [END]])
    
    (last-index ARRAY VAL [START [END]])
    (last-index BYTES VAL [START [END]])
    (last-index DICTIONARY VAL)
    (last-index LIST VAL [START [END]])
    (last-index STRING VAL [START [END]])

# DESCRIPTION
The function `index` returns the key of the argument corresponding to `VAL`.

The function `last-index` is identical to `index` except that it searches from the tail. However, for dictionaries without the concept of order, it is identical to `index`.

If `START` and `END` are specified, the search is performed over a range equivalent to `(slice x START `END`)`. Their initial values are `0` and `(len x)`

# RETURN VALUE
If the argument is `ARRAY` or `BYTES` or `LIST`, the position of `VAL` is returned.

If the argument is `STRING`, returns the index within this string of the first occurrence of the specified substring `VAL`.

If the argument is a `DICTIONARY`, returns the key corresponding to `VAL`. If there are multiple identical `VAL`, the value returned is undefined.

If `VAL` is not associated, returns `nil`.

# EXAMPLES

    ) (index #[ 0 1 0 ] 0)
    0
    ) (index #[ 0 1 0 ] 0 1)
    2
    ) (index #[ 0 1 0 ] 0 1 2)
    nil
    ) (index #[ 0 1 0 ] 1)
    1
    ) (index #[ 0 1 0 ] 10)
    nil
    ) (last-index #[ 0 1 0 ] 0)
    2
    ) (last-index #[ 0 1 0 ] 1)
    1
    ) (last-index #[ 0 1 0 ] 10)
    nil

    ) (index (bytes 3) 0x01)
    nil
    ) (index (bytes 3) 0x00)
    0
    ) (last-index (bytes 3) 0x01)
    nil
    ) (last-index (bytes 3) 0x00)
    2

    ) (index #{ :foo 0 :bar 1 } 0)
    :foo
    ) (index #{ :foo 0 :bar 1 } 1)
    :bar
    ) (index #{ :foo 0 :bar 1 } 2)
    nil

    ) (index '(0 1 0) 0)
    0
    ) (index '(0 1 0) 0 1)
    2
    ) (index '(0 1 0) 0 1 2)
    nil
    ) (index '(0 1 0) 1)
    1
    ) (index '(0 1 0) 2)
    nil
    ) (last-index '(0 1 0) 0)
    2
    ) (last-index '(0 1 0) 1)
    1
    ) (last-index '(0 1 0) 2)
    nil

    ) (index "abcab" "a")
    0
    ) (index "abcab" "a" 1)
    3
    ) (index "abcab" "a" 1 2)
    nil
    ) (index "abcab" "b")
    1
    ) (index "abcab" "d")
    nil
    ) (index "abcab" "ab")
    0
    ) (index "abcab" "bc")
    1
    ) (last-index "abcab" "a")
    3
    ) (last-index "abcab" "b")
    4
    ) (last-index "abcab" "d")
    nil
    ) (last-index "abcab" "ab")
    3
    ) (last-index "abcab" "bc")
    1

# SEE ALSO
- [](3)
- in?(3)
- position(3)
