# NAME
array - array data type.

# DESCRIPTION
An array data type is a data structure consisting of a collection of elements (values or variables), each identified by at least one array index or key. 

Since the array is guaranteed to have contiguous memory, any element can be accessed with `O(1)`.

The array can use the functions listed in `SEE ALSO` section, and can also use the functions supported by `sequence` and `collection` api.

Any data type can be treated as a elements.

# EXAMPLES

    ) (<- a (array 3))
    #[ nil nil nil ]
    ) ([] a 0 :foo)
    :foo
    ) ([] a 1 'bar)
    bar
    ) ([] a 0)
    :foo

# SEE ALSO
- array(3)
- collection(7)
- data-types(7)
- sequence(7)
