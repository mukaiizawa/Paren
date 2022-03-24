# NAME
group-by - grouping the list into sublists by function return value.

# SYNOPSIS

    (group-by FN LIST)

# DESCRIPTION
The function `group-by` group the list into sublists by function return value.

The order of the return values guarantees the order of the original list.

# RETURN VALUE
Returns the `LIST` into sublists by function `FN` return value.

# EXAMPLES

    ) (group-by odd? '(1 1 1 2 2 3 3))
    (true (1 1 1 3 3) nil (2 2))
    ) (group-by identity (list... "hello world"))
    '("h" ("h") "e" ("e") "l" ("l" "l" "l") "o" ("o" "o") " " (" ") "w" ("w") "r" ("r") "d" ("d"))
    ) (group-by (f (x) (% x 3)) (.. 10))
    (0 (0 3 6 9) 1 (1 4 7) 2 (2 5 8))

# SEE ALSO
- chunk(3)
- group(3)
- split-with(3)
