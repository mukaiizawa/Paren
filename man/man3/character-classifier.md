# NAME
ascii?, alnum?, alpha?, digit?, space?, print?, lower?, upper?, title? - character classification routines.

# SYNOPSIS

    (ascii? TEXT)
    (alnum? TEXT)
    (alpha? TEXT)
    (digit? TEXT)
    (space? TEXT)
    (print? TEXT)
    (lower? TEXT)
    (upper? TEXT)
    (title? TEXT)

# DESCRIPTION
These functions check whether `TEXT` falls into a certain character class.

# RETURN VALUE
The function `ascii?` returns whether all characters in the `TEXT` are ASCII.

The function `alnum?` returns whether all characters in the `TEXT` are alphanumeric.

The function `alpha?` returns whether all characters in the `TEXT` are alphabetic ASCII characters.

The function `digit?` returns whether all characters in the `TEXT` are ASCII decimal digits.

The function `space?` returns whether all characters in the `TEXT` are whitespace.

The function `print?` returns whether all characters in the `TEXT` are printable.

The function `lower?` returns whether all characters in the `TEXT` are ASCII lowercase.

The function `upper?` returns whether all characters in the `TEXT` are ASCII uppercase.

The function `title?` returns whether the `TEXT` is a titlecased string.

If `TEXT` is empty, these functions returns `nil`.

# EXAMPLES

    ) (ascii? "abc")
    true
    ) (ascii? "あいう")
    nil

    ) (alnum? "abc123")
    true
    ) (alnum? " ")
    nil

    ) (alpha? "abc")
    true
    ) (alpha? "123")
    nil

    ) (digit? "0123456789")
    true
    ) (digit? "abc")
    nil

    ) (space? " \t\r\n")
    true
    ) (space? "")
    nil

    ) (print? " ")
    true
    ) (print? "\e")
    nil

    ) (lower? "abc")
    true
    ) (lower? "ABC")
    nil

    ) (upper? "ABC")
    true
    ) (upper? "abc")
    nil

# SEE ALSO
- lower(3)
- title(3)
- upper(3)
