# NAME
environment - set of bindings.

# DESCRIPTION
A binding is an association between a symbol and that which the symbol denotes.

Bindings are established by particular special-operators.

A single name can simultaneously have more than one associated binding per environment.

The environment holds a reference to the parent's environment in addition to binding.

When a symbol is evaluated, it is resolved by following the bindings of the parent's environment, starting with the current environment.

# SEE ALSO
- <-(3)
- let(3)
