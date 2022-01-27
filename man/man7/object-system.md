# NAME
object-system - object system.

# DESCRIPTION
Paren has a simple object system consisting of dictionary data types and macros. Also, some built-in features such as file paths, streams, and exception handling are built using the object system.

In the following, we will use a common `Shape` class as an example to illustrate Paren's object system. The entire executable program can be viewed in the `EXAMPLES` section.

## class
A class is defined by the macro `class`.

For example, a `Shape` class that has only one field named `name` is defined as follows.

    (class Shape ()
      name)

## instance
The `Class` class has a `.new` method defined to create an instance. Therefore, in the previous example, we can create an instance of the `Shape` class as follows.

    ) (.new Shape)
    #{ :class Object :name nil }

As the evaluation results show, an instance is just a dictionary.

## accessors
When a class is defined, accessor methods are automatically generated.

    ) (<- s (.new Shape))
    #{ :class Shape :name nil }
    
    ) (&name! s :unknown)    ; Set the `name` field to `:unknown`.
    #{ :class Shape :name :unknown }
    
    ) (&name s)    ; Get the `name` field.
    :unknown

Since setter returns itself, it can also be written as follows.

    ) (&name (&name! (<- s (.new Shape)) :unknown))
    :unknown

The field immediately after instance creation is `nil`.

    ) (&name (.new Shape))
    nil

## method
A method is defined by the macro `method`.

In the method, this instance is implicitly bound to the symbol `self`.

For example, a method that returns the `name` field is defined as follows.

    (method Shape .name ()
      (&name self))

The defined method is called as follows.

    ) (.name (&name! (.new Shape) :unknown))
    :unknown

The method is equivalent to the macro `function`, except that the first argument is implicitly bound to `self`. Therefore, flexible parameters can be specified, like the optional parameter.

## initialization
The initialization method is defined as `.init`. The `.init` method, which has no arguments, is called automatically after it is created by the `.new` method.

    (method Shape .init ()
      (&name! self :unknown))

For example, the following can be used to specify the initial value at the time of instance creation.

    ) (.name (.new Shape))
    :unknown    ; not nil!

Initialization methods with arguments need to be called explicitly.

## superclass
In Paren, only single inheritance is allowed.  Like most object-oriented languages, it inherits the fields and methods of its superclass.

For example, a `Rectangle` class that inherits from the Shape class.

    (class Rectangle (Shape)
      width height)
    
    (method Rectangle .init (width height)
      (&name! self :rectangle))
    
    (method Rectangle .area ()
      (* (&self width) (&height self)))
    
    (<- r (.init (.new Rectangle) 3 5))
    (.area r)
    => 15
    (.name r)    ; inherit
    => :rectangle

Class `Object` is the root of the class hierarchy. Every class has `Object` as a superclass. Therefore, if you omit the superclass when declaring the `Shape` class, as in the previous example, it is considered to inherit from the `Object` class.

## method overloading
Method overloading is not supported in Paren.

This is because methods are managed by the global symbol `<class-name> + <method name>`.

However, even without this feature, it is not a problem because methods can be parameterized in a flexible way.

## encapsulation
Encapsulation is not supported in Paren. That is, accessor can be called at any point.

As a rule of thumb, do not call accessors of other classes directly. This implies that state updates should be done by message passing.

# EXAMPLES
An executable program that is a modified version of the one illustrated in the `DESCRIPTION` section is shown below.

    (class Shape ()
      name)
    
    (method Shape .init (name)
      (&name! self name))
    
    (method Shape .area ()
      (assert nil))    ; should be implemented.
    
    (method Shape .inspect ()
      (list :name (&name self) :area (.area self)))
    
    (class Circle (Shape)
      radius)
    
    (method Circle .init (r)
      (Shape.init self :circle)
      (&radius! self r))
    
    (method Circle .area ()
      (* 3.14 (pow (&radius self) 2)))
    
    (class Rectangle (Shape)
      width height)
    
    (method Rectangle .init (w h)
      (Shape.init self :rectangle)
      (&width! self w)
      (&height! self h))
    
    (method Rectangle .area ()
      (* (&width self) (&height self)))

Examples of use are shown below.

    (foreach write
             (map .inspect
                  (list (.init (.new Circle) 1)
                        (.init (.new Rectangle) 1 1))))

The evaluation results are as follows.

    (:name :circle :area 3.14)
    (:name :rectangle :area 1)

# SEE ALSO
- Class(3)
- Object(3)
- catch(3)
- class(3)
- method(3)
- throw(3)
