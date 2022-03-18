# NAME
select, select1, reject - create a sublist that meets the conditions.

# SYNOPSIS

    (select FN LIST)
    (select1 FN LIST)
    (reject FN LIST)

# DESCRIPTION
These functions return a sublist of the `LIST` elements based on the result of applying the function `FN`.

For all these functions, elements not removed occur in the same order in the result as they did in sequence.

# RETURN VALUE
The function `select` returns a list of elements for which `FN` returned `non-nil`.

The function `select1` returns the first element that satisfies the predicate.

The function `reject` returns a list of elements for which `FN` returned `nil`.

# NOTES

    (select fn list) <=> { x ∊ list | (! (nil? (fn x))) }
    (reject fn list) <=> { x ∊ list | (nil? (fn x)) }
    (select fn list) ∪ (reject fn list) = list

    (select1 fn list) <=> (car (select fn list))

# EXAMPLES

    ) (<- l '(1 (2) 3 (4)))
    (1 (2) 3 (4))
    ) (select atom? l)
    (1 3)
    ) (select1 atom? l)
    1
    ) (reject atom? l)
    ((2) (4))

# SEE ALSO
- complement(3)
- compose(3)
- keep(3)
- keep1(3)
- partial(3)
