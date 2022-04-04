# NAME
module - module file.

# DESCRIPTION
A module is a component or part of a program that contains one or more routines.

The built-in module files are in the `$paren-home/modules` directory.

Module files can be imported with the `import` function.

Since Paren does not have a package system, the modules are made with the following policy so as not to pollute the namespace.

- Prefix the symbol with the module name.
- Give the class a unique name for the module.

The module file must be written in UTF-8.

# SEE ALSO
- `import(3)`
- `$paren-home(3)`
