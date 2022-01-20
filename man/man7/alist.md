# NAME
association-list - a list of conses representing an association.

# DESCRIPTION
An association list is a list of conces constructed for the purpose of representing a `key-value` pair.

The even-numbered elements corresponds to the key and the odd-numbered elements corresponds to the value.

Since the search time is `O(n)`, use dictionary when dealing with large elements.

# NOTES
The definition is different from the association list of Common Lisp.

# EXAMPLES

    ) (<- alist (list :foo 0 :bar 1 :buzz 3))
    (:foo 0 :bar 1 :buzz 3)
    ) (assoc alist :foo)
    0
    ) (assoc alist :buzz)
    3
    ) (assoc alist :foobar)
    nil

# SEE ALSO
- assoc(3)
- cons(7)
- dictionary(7)
- list(7)
