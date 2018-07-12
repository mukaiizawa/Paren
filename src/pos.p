; paren object system

(macro class ())
(macro method ())

(class Object ())

(method Object .init ()
        self)

(method Object .equals ())

(class Class ())

(method Class .new ()
        (.init self))

(method Class .type ())

(method Class .fields ())

(method Class .methods ())
