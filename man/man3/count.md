# NAME
count - count the elements that satisfy the condition.

# SYNOPSIS

    (count FN LIST)

# DESCRIPTION
The function `count` count the elements that satisfy the predicate.

# RETURN VALUE
Returns the number of elements in the `LIST` that satisfy the predicate function `FN`.

# NOTES

    (count fn lis) <=> (len (select fn lis))

# EXAMPLES

    ) (count even? (.. 10))
    5
    ) (count odd? (.. 10))
    5
    ) (count zero? (.. 10))
    1

# SEE ALSO
- len(3)
- select(3)
