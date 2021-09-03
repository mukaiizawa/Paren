# NAME
select, reject - create a sublist that meets the conditions.

# SYNOPSIS

    (select FN LIST)
    
    (reject FN LIST)

# DESCRIPTION
These functions return a sublist of the LIST elements based on the result of applying the FN function.

For all these functions, elements not removed occur in the same order in the result as they did in sequence.

# RETURN VALUE
The function select(3) returns a list of elements for which FN returned not-nil.

The function reject(3) returns a list of elements for which FN returned nil.

# NOTES

    (select FN LIST) <=> { x ∊ LIST | (! (nil? (FN x))) }
    (reject FN LIST) <=> { x ∊ LIST | (nil? (FN x)) }
    (select FN LIST) ∪ (reject FN LIST) = LIST

# EXAMPLES

    ) (<- l '(1 (2) 3 (4)))
    (1 (2) 3 (4))
    ) (select atom? l)
    (1 3)
    ) (reject atom? l)
    ((2) (4))
