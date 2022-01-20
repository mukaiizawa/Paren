# NAME
environment - set of bindings.

# DESCRIPTION
An environment is a collection of associations between a symbol and what that symbol represents.

Environment are established by particular special operators.

A single name can simultaneously have more than one associated binding per environment.

The environment holds a reference to the parent's environment in addition to binding.

When a symbol is evaluated, it is resolved by following the bindings of the parent's environment, starting with the current environment.

# SEE ALSO
- <-(3)
- let(3)
- special-operator(7)
