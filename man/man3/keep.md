# NAME
keep1, keep - keep the element.

# SYNOPSIS

    (keep FN LIST)
    (keep1 FN LIST)

# DESCRIPTION
The function `keep` keep the element in the list `LIST` where the function `FN` returns not `nil`.

# RETURN VALUE
Returns the return value of the function `FN` that did not return `nil` first.

If there is no such element, returns `nil`.

    (keep fn list) <=> (reject nil? (map fn list))
    (keep1 fn list) <=> (car (keep fn lis))

# EXAMPLES

    ) (<- l (.. 10))
    (0 1 2 3 4 5 6 7 8 9)
    ) (keep (f (x) (if (> x 5) (* x x))) l)
    (36 49 64 81)
    ) (keep1 (f (x) (if (> x 5) (* x x))) l)
    36

# SEE ALSO
- `complement(3)`
- `compose(3)`
- `map(3)`
- `reject(3)`
- `select(3)`
- `select1(3)`
- `partial(3)`
