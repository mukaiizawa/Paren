# NAME
symbol - symbol data type.

# DESCRIPTION
Symbols are used for their object identity to name various entities in Paren.

Use special operator `<-` to bind the symbol.

There is only one symbol with the same name in the system, so identity verification can be done by address comparison.

# NOTES
Unlike Common Lisp, there is no concept of packaging, so symbols in the system are in only one namespace.

See `lang(7)` for names that can be used for symbols.

# SEE ALSO
- <-(3)
- symbol(3)
- data-types(7)
- lang(7)
