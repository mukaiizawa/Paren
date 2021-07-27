# NAME
module - Paren modules.

# DESCRIPTION
`$paren-home/modules` holds Paren's standard modules.

The module becomes available by calling the `import` function.

Specify the keyword excluding the extension of the target module in the import function as EXAMPLES.

Since Paren does not have a package system, the modules are made with the following policy so as not to pollute the namespace.

- Prefix the symbol with the module name.
- Give the class a unique name for the module.

# EXAMPLES
import rand module.

    (import :rand)

# SEE ALSO
- import(3)
- $paren-home(3)
