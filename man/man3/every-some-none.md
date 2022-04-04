# NAME
every?, some?, none? - test elements of list for satisfaction of a given predicate.

# SYNOPSIS

    (every? FN LIST)
    (some? FN LIST)
    (none? FN LIST)

# DESCRIPTION
These functions are test elements of `LIST` for satisfaction of a given predicate `FN`.

# RETURN VALUE
The function `every?` returns whether the result of applying the function `FN` to each element of `LIST` is all `true` or not. If `LIST` is `nil`, returns `true`.

The function `some?` returns whether any of the results of applying the function `FN` to each element of `LIST` is `true` or not. If `LIST` is `nil`, returns `nil`.

The function `none?` returns whether the result of applying the function `FN` to each element of `LIST` is all `nil` or not. If `LIST` is `nil`, returns `true`.

# EXAMPLES

    ) (every? nil? nil)
    true
    ) (some? nil? nil)
    nil
    ) (none? nil? nil)
    true

    ) (every? nil? '(nil))
    true
    ) (some? nil? '(nil))
    true
    ) (none? nil? '(nil))
    nil

    ) (every? nil? '(1))
    nil
    ) (some? nil? '(1))
    nil
    ) (none? nil? '(1))
    true

    ) (every? nil? '(1 nil))
    nil
    ) (some? nil? '(1 nil))
    true
    ) (none? nil? '(1 nil))
    nil

# SEE ALSO
- `every-adjacent?(3)`
