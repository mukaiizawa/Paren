# NAME
symbol, keyword - symbol data type.

# DESCRIPTION
A symbol is a data type that has references to other data (variables in general programming languages).

When a symbol is evaluated, it returns the value it holds. If it does not hold a value (initial state), an error occurs.

The following symbols are exceptions, which themselves are retained from the beginning.

- nil (the only value treated as false)
- true (representative value of true)
- Symbols beginning with `:` (called keywords)

Use special operator `<-` to bind the symbol.

Symbols with the same name are guaranteed to be identical, so identity verification can be done by address comparison.

# NOTES
Unlike Common Lisp, there is no concept of packaging, so symbols in the system are in only one namespace.

See `lang(7)` for names that can be used for symbols.

# EXAMPLES

    ) nil
    nil
    
    ) true
    true
    
    ) :foo
    :foo
    
    ) foo
    StateError -- unbound symbol
      at: foo
      at: (repl)
      at: (boot nil)
    
    ) (<- foo "foo")
    "foo"
    
    ) foo
    "foo"

# SEE ALSO
- `<-(3)`
- `byte-len(3)`
- `symbol(3)`
- `keyword(3)`
- `data-types(7)`
- `lang(7)`
