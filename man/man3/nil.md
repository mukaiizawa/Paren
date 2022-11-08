# NAME
nil - symbol nil.

# DESCRIPTION
The symbol `nil` represents both boolean `false` and the empty list.

The `nil` is bound itself.

# EXAMPLES

    ) nil
    nil
    ) (nil? nil)
    true
    ) (list? nil)
    true
    ) (cons? nil)
    nil
    ) (len nil)
    0

# SEE ALSO
- `if(3)`
- `true(3)`
- `symbol(7)`
