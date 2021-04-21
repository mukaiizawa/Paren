module

# Overview
This directory holds Parent's standard modules.

# Usage
The module becomes available by calling the import function.

Specify the keyword excluding the extension of the target module in the import function as follows.

    (import :rand)

Import to the same module is done only once regardless of the number of calls.

# Policy
Since Paren does not have a package system, the modules are made with the following policy so as not to pollute the namespace.

- Prefix the symbol with the module name
- Give the class a unique name for the module

Since Paren has no visibility mechanism, the following policy is used.

- Add comments to the published interface
- Do not comment on functions that are not exposed, or start with `;;`
