# NAME
chunk - grouping the list into sublists by function return value.

# SYNOPSIS

    (chunk FN LIST)

# DESCRIPTION
The function `chunk` returns elements organized into chunks as specified by the given function.

# RETURN VALUE
Returns the `LIST` into sublists by function `FN` return value.

# EXAMPLES

    ) (chunk odd? '(1 1 1 2 2 3 3))
    ((true (1 1 1)) (nil (2 2)) (true (3 3)))
    ) (chunk identity (list... "hello world"))
    (("h" ("h")) ("e" ("e")) ("l" ("l" "l")) ("o" ("o")) (" " (" ")) ("w" ("w")) ("o " ("o")) ("r" ("r")) ("l" ("l")) ("d" ("d")))
    ) (chunk (f (x) (% x 3)) (.. 10))
    ((0 (0)) (1 (1)) (2 (2)) (0 (3)) (1 (4)) (2 (5)) (0 (6)) (1 (7)) (2 (8)) (0 (9))

# SEE ALSO
- group(3)
- group-by(3)
- split-with(3)
