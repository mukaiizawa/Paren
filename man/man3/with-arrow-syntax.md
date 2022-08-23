# NAME
with-arrow-syntax - creating an implicit context for the object.

# SYNOPSIS

    (with-arrow-syntax EXPR ...)

# DESCRIPTION
The macro `with-arrow-syntax` generates a context for simplified referencing and updating of object properties.

Symbols containing `->` in `with-arrow-syntax` are converted to evaluation of function `[]` as follows.

    ;; Referring to property y of object x
    x->y
    => ([] x :y)

    ;; Referring to property z of object x property y
    x->y->z
    => ([] ([] x :y) :z)

The left-hand side values in the special operator `<-` are properly expanded into assignment expressions.

    ;; Assign object a property b to object x property y
    (<- x->y a->b)
    => ([] x :y ([] a :b))

# RETURN VALUE
Returns result of evaluation of the last `EXPR`.

# NOTES
The macro `with-arrow-syntax` is implicitly applied within the macro `method`.  Thus, it can be used as follows:

    (class Dog () name)
    
    (method Dog .init (name)
      (<- self->name name)
      self)
    
    (method Dog .name ()
      self->name)
    
    (.name (.init (.new Dog) "foo")) ;; "foo"

# SEE ALSO
- `[](3)`
- `in?(3)`
- `method(3)`
