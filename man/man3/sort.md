# NAME
sort! - sorting a sequence.

# SYNOPSIS

    (sort! SEQUENCE [ :sorter SORTER ] [ :key KEY ] [ :start START ] [ :end END ])

# DESCRIPTION
The function `sort!` destructively sort the `SEQUENCE`.

The `SORTER` parameter can specify a comparison function to determine the order. The sorted sequence will be in such an order that two adjacent elements are the same or the comparison function returns `true`. By default, the comparison function `<` is used.

The `KEY` parameter can be used to specify the function to be applied before comparison for sorting.

The `START` and `END` parameters can be used to specify the range to sort. If specified, the range from the `START-th` to the `(END - 1)-th` is targeted. By default, the entire sequence is target. In other words, `START` is `0` and `END` is same as `(len SEQUENCE)`.

# RETURN VALUE
Returns sorted sequence.

# ERRORS
Error if `SEQUENCE` is not a mutable sequence.

# NOTES
The `SEQUENCE` is modified. If you want to keep the original sequence, copy it like `(sort! (slice seq))`.

# EXAMPLES

    ) (sort! '(2 0 1))
    (0 1 2)
    
    ) (sort! '("foo" "bar" "buzz"))
    ("bar" "buzz" "foo")

    ) (sort! '(2 0 1) :sorter >)
    (2 1 0)

    ) (sort! '((2 :a) (0 :b) (1 :c)) :key car)
    ((0 :b) (1 :c) (2 :a))
    
    )  (sort! '((2 :a) (0 :b) (1 :c)) :key cadr)
    ((2 :a) (0 :b) (1 :c))

    ) (sort! '(2 4 1 3) :start 1)
    (2 1 3 4)
    
    ) (sort! '(2 4 1 3) :end 3)
    (1 2 4 3)
    
    ) (sort! '(2 4 1 3) :start 1 :end 3)
    (2 1 4 3)

# SEE ALSO
- <(3)
