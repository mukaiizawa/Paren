# NAME
select, reject - create a sublist that meets the conditions.

# SYNOPSIS

    (select FX LIST)
    (reject FX LIST)

# DESCRIPTION
These functions return a sublist of the `LIST` elements based on the result of applying the function `FX`.

For all these functions, elements not removed occur in the same order in the result as they did in sequence.

# RETURN VALUE
The function `select` returns a list of elements for which `FX` returned `non-nil`.

The function `reject` returns a list of elements for which `FX` returned `nil`.

# NOTES

    (select fx list) <=> { x ∊ list | (! (nil? (fx x))) }
    (reject fx list) <=> { x ∊ list | (nil? (fx x)) }
    (select fx list) ∪ (reject fx list) = list

# EXAMPLES

    ) (<- l '(1 (2) 3 (4)))
    (1 (2) 3 (4))

    ) (select atom? l)
    (1 3)

    ) (reject atom? l)
    ((2) (4))
