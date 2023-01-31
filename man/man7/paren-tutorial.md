# NAME
paren-tutorial - a tutorial introduction to Paren.

# DESCRIPTION
## Introduction
This is a brief tutorial on Paren, a dialect of Lisp. It's intended for readers with no Lisp experience.

Programs consist of expressions. The simplest expressions are things like numbers and strings, which evaluate to themselves.

    ) 25
    25
    ) "foo"
    "foo"

Several expressions enclosed within parentheses are also an expression. These are called lists. When a list is evaluated, the elements are evaluated from left to right, and the value of the first (presumably a function) is passed the values of the rest. Whatever it returns is returned as the value of the expression.

    ) (+ 1 2)
    3

Here's what just happened. First `+`, `1`, and `2` were evaluated, returning the `plus function`, `1`, and `2` respectively. `1` and `2` were then passed to the `plus function`, which returned `3`, which was returned as the value of the whole expression. (Macros introduce a twist, because they transform lists before they're evaluated. We'll get to them later.)

Since expression and evaluation are both defined recursively, programs can be as complex as you want:

    ) (+ (+ 1 2) (+ 3 (+ 4 5)))
    15

Putting the `+` before the numbers looks odd when you're used to writing `1 + 2`, but it has the advantage that `+` can now take any number of arguments, not just two:

    ) (+)
    0
    ) (+ 1)
    1
    ) (+ 1 2)
    3
    ) (+ 1 2 3)
    6

This turns out to be a convenient property, especially when generating code, which is a common thing to do in Lisp.

Lisp dialects like Paren have a data type most languages don't: symbols. We've already seen one: `+` is a symbol. Symbols don't evaluate to themselves the way numbers and strings do. They return whatever value they've been assigned.

If we give `foo` the value `13`, it will return `13` when evaluated:

    ) (<- foo 13)
    13
    ) foo
    13

You can turn off evaluation by putting a single quote character before an expression. So `'foo` returns the symbol `foo`.

    ) 'foo
    foo

Particularly observant readers may be wondering how we got away with using `foo` as the first argument to `<-`  If the arguments are evaluated left to right, why didn't this cause an error when `foo` was evaluated?  There are some operators that violate the usual evaluation rule, and `<-` is one of them. Its first argument isn't evaluated.

If you quote a list, you get back the list itself.

    ) (+ 1 2)
    3
    ) '(+ 1 2)
    (+ 1 2)

The first expression returns the number `3`. The second, because it was quoted, returns a list consisting of the symbol `+` and the numbers `1` and `2`.

You can build up lists with `cons`, which returns a list with a new element on the front:

    ) (cons 'f '(a b))
    (f a b)

It doesn't change the original list:

    ) (<- x '(a b))
    (a b)
    ) (cons 'f x)
    (f a b)
    ) x
    (a b)

The empty list is represented by the symbol `nil`, which is defined to evaluate to itself. So to make a list of one element you say:

    ) (cons 'a nil)
    (a)

You can take lists apart with `car` and `cdr`, which return the first element and everything but the first element respectively:

    ) (car '(a b c))
    a
    ) (cdr '(a b c))
    (b c)

To create a list with many elements use `list`, which does a series of conses:

    ) (list 'a 1 "foo" '(b))
    (a 1 "foo" (b))
    ) (cons 'a (cons 1 (cons "foo" (cons '(b) nil))))
    (a 1 "foo" (b))

Notice that lists can contain elements of any type.

There are 4 parentheses at the end of that call to `cons`. How do Lisp programmers deal with this? They don't. You could add or subtract a right paren from that expression and most wouldn't notice.

Lisp programmers don't count parens. They read code by indentation, not parens, and when writing code they let the editor match parens.

Lists are useful in exploratory programming because they're so flexible. You don't have to commit in advance to exactly what a list represents. For example, you can use a list of two numbers to represent a point on a plane. Some would think it more proper to define a point object with two fields, `x` and `y`. But if you use lists to represent points, then when you expand your program to deal with n dimensions, all you have to do is make the new code default to zero for missing coordinates, and any remaining planar code will continue to work.

Or if you decide to expand in another direction and allow partially evaluated points, you can start using symbols representing variables as components of points, and once again, all the existing code will continue to work.

In exploratory programming, it's as important to avoid premature specification as premature optimization.

The most exciting thing lists can represent is code. The lists you build with `cons` are the same things programs are made out of. This means you can write programs that write programs. The usual way to do this is with something called a macro. We'll get to those later. First, functions.

## Function
We've already seen some functions: `+`, `cons`, `car`, and `cdr`. You can define new ones with `function`, which takes a symbol to use as the name, a list of symbols representing the parameters, and then zero or more expressions called the body. When the function is called, those expressions will be evaluated in order with the symbols in the body temporarily set ("bound") to the corresponding argument. Whatever the last expression returns will be returned as the value of the call.

Here's a function that takes two numbers and returns their average:

    ) (function average (x y)
        (/ (+ x y) 2))
    average
    ) (average 2 4)
    3

The body of the function consists of one expression, `(/ (+ x y) 2)`. It's common for functions to consist of one expression; in purely functional code (code with no side-effects) they always do.

Notice that `function`, like `<-` doesn't evaluate all its arguments. It is another of those operators with its own evaluation rule.

In Paren, as in most Lisps, functions are a data type, just like numbers or strings. As the literal representation of a string is a series of characters surrounded by double quotes, the literal representation of a function is a list consisting of the symbol `f`, followed by its parameters, followed by its body. So you could represent a function to return the average of two numbers as:

    ) average
    (f (x y) (/ (+ x y) 2))

There's nothing semantically special about named functions as there is in some other languages. All function does is basically this:

    ) (<- average (f (x y) (/ (+ x y) 2)))
    (f (x y) (/ (+ x y) 2))
    ) (average 2 4)
    3

There is an operator `let` to set temporary variables.

    ) (let (x 1)
        (+ x (* x 2)))
    3
    ) (let (x 3 y 4)
        (sqrt (+ (pow x 2) (pow y 2))))
    5

So far we've only had things printed out implicity as a result of evaluating them. The standard way to print things out in the middle of evaluation is with `print` or `println`. They take multiple arguments and print them in order; `println` also prints a newline at the end. Here's a variant of average that tells us what its arguments were:

    ) (function average2 (x y)
            (println "my arguments were: " (list x y))
            (/ (+ x y) 2))
    average2
    ) (average2 100 200)
    my arguments were: (100 200)
    150

## Function2 TOOD
Functions can have as many optional parameters as you want, but they have to come at the end of the parameter list.

If you put an expression after the name of an optional parameter, it will be evaluated if necessary to produce a default value. The expression can refer to preceding parameters.

) (function greet (name (o punc (case name who #\? #\!)))
       (string "hello " name punc))
*** redefining greet
#<procedure: greet>
) (greet 'who)
"hello who?"

To make a function that takes any number of arguments, put a period and a space before the last parameter, and it will get bound to a list of the values of all the remaining arguments:

) (function foo (x y . z)
       (list x y z))
#<procedure: foo>
) (foo (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8))
(3 7 (11 15))

This type of parameter is called a `rest parameter` because it gets the rest of the arguments. If you want all the arguments to a function to be collected in one parameter, just use it in place of the whole parameter list. (These conventions are not as random as they seem. The parameter list mirrors the form of the arguments, and a list terminated by something other than nil is represented as e.g. (a b . c).)

To supply a list of arguments to a function, use apply:

) (apply + '(1 2 3))
6

Now that we have rest parameters and apply, we can write a version of average that takes any number of arguments.

) (function average args
       (/ (apply + args) (len args)))
#<procedure: average>
) (average 1 2 3)
2

## Conditional operators
The standard conditional operator is `if`. Like `<-` and `function`, it doesn't evaluate all its arguments. When given three arguments, it evaluates the first, and if that returns true, it returns the value of the second, otherwise the value of the third:

    ) (if (odd? 1) 'a 'b)
    a
    ) (if (odd? 2) 'a 'b)
    b

Returning `true` means returning anything except `nil`. Nil is conventionally used to represent falsity as well as the empty list. The symbol `true` (which like nil evaluates to itself) is often used to represent truth, but any value other than `nil` would serve just as well.

    ) (odd? 1)
    true
    ) (odd? 2)
    nil

It sometimes causes confusion to use the same thing for falsity and the empty list, but many years of Lisp programming have convinced me it's a net win, because the empty list is set-theoretic false, and many Lisp programs think in sets.

If the third argument is missing it defaults to `nil`.

    ) (if (odd? 2) 'a)
    nil

An if with more than three arguments is equivalent to a nested if.

    (if a b c d e)

is equivalent to

    (if a
        b
        (if c
            d
            e))

If you're used to languages with `elseif`, this pattern will be familiar.

Each argument to `if` is a single expression, so if you want to do multiple things depending on the result of a test, combine them into one expression with `begin`.

    ) (begin
        (println "hello")
        (+ 2 3))
    hello
    5

If you just want several expressions to be evaluated when some condition is true, you could say

    (if a (begin b c))

but this situation is so common there's a separate operator for it.

    (when a
      b
      c)

The `&&` and `||` operators are like conditionals because they don't evaluate more arguments than they have to.

    ) (&& nil (print "you'll never see this"))
    nil

The negation operator is `!`, a name that also works when talking about `nil` as the empty list. Here's a function to return the length of a list:

    ) (function mylen (xs)
        (if (! xs) 0
            (+ 1 (mylen (cdr xs)))))
    mylen

If the list is `nil` the function will immediately return `0`. Otherwise it returns `1` more than the length of the rest of the list.

    ) (mylen nil)
    0
    ) (mylen '(a b))
    2

I called it mylen because there's already a function called `len` for this.

The standard comparison operator is `=`, which returns `true` if its arguments are identical or, if strings, have the same characters.

    ) (= 'a 'a)
    true
    ) (= 'a 'b)
    nil
    ) (= "foo" "foo")
    true
    ) (= (list 'a) (list 'a))
    true

If you want to test whether something is one of several alternatives, you could say `(|| (= x y) (= x z) ...)`, but this situation is common enough that there's an operator for it.

    ) (in 'a '(a b c))
    true

## Loops and iteration
Paren has a variety of iteration operators. For a range of numbers, use `for`.

    ) (for (i 1) (<= i 10) (i (++ i))
        (print i " "))
    1 2 3 4 5 6 7 8 9 10 nil

To iterate through the elements of a list, use `dolist`.

    ) (dolist (x '(a b c d e))
        (print x " "))
    a b c d e nil

Those nils you see at the end each time are not printed out by the code in the loop. They're the return values of the iteration expressions.

To continue iterating while some condition is true, use `while`.

    ) (let (x 10)
        (while (> x 5)
          (<- x (- x 1))
          (print x)))
    98765nil

The `map` function takes a function and a list and returns the result of applying the function to successive elements.

    ) (map (f (x) (+ x 10)) '(1 2 3))
    (11 12 13)

There are a number of functions like `map` that apply functions to successive elements of a sequence. The most commonly used is `select`, which returns the elements satisfying some test.

    ) (select odd? '(1 2 3 4 5 6 7))
    (1 3 5 7)

Others include `reject`, which does the opposite of `select`; `every?`, which returns true if the function is true of every element; `some?`, which returns true if the function is true of any element; `position`, which returns the position of the first element for which the function returns true; and `keep`, which returns a list of all the non-nil return values:

    ) (reject odd? '(1 2 3 4 5 6))
    (2 4 6)
    ) (every? odd? '(1 3 5 7))
    true
    ) (some? even? '(1 3 5 7))
    nil
    ) (position even? '(1 2 3 4 5))
    1
    ) (keep (f (x) (if (odd? x) (+ x 10)))
            '(1 2 3 4 5))
    (11 13 15)

## Dictionary
Lists can be used to represent a wide variety of data structures, but if you want to store key/value pairs efficiently, Paren also has dictionary.

    ) (<- airports (dict))
    #{ }
    ) ([] airports "Boston" 'bos)
    bos
    ) ([] airports "Boston")
    bos
    ) airports
    #{ "Boston" bos }

The function `keys` returns the keys in a dictionary, and `vals` returns the values.

    ) (keys airports)
    ("Boston")
    ) (vals airports)
    (bos)

## String
There are a couple operators for building strings. The most general is `str`, which takes any number of arguments and mushes them into a string:

    ) (str 99 " bottles of " nil 'beer)
    "99 bottles of beer"

Every argument will appear as it would look if printed out by `print`, except `nil`, which is ignored.

## Macros
We know enough now to start writing macros. Macros are basically functions that generate code. Of course, generating code is easy; just call list.

) (list '+ 1 2)
(+ 1 2)

What macros offer is a way of getting code generated this way into your programs. Here's a (rather stupid) macro definition:

) (mac foo ()
       (list '+ 1 2))
*** redefining foo
#3(tagged mac #<procedure>)

Notice that a macro definition looks exactly like a function definition, but with function replaced by mac.

What this macro says is that whenever the expression (foo) occurs in your code, it shouldn't be evaluated in the normal way like a function call. Instead it should be replaced by the result of evaluating the body of the macro definition, (list '+ 1 2). This is called the "expansion" of the macro call.

In other words, if you've defined foo as above, putting (foo) anywhere in your code is equivalent to putting (+ 1 2) there.

) (+ 10 (foo))
13

This is a rather useless macro, because it doesn't take any arguments. Here's a more useful one:

) (mac when (test . body)
       (list 'if test (cons 'begin body)))
*** redefining when
#3(tagged mac #<procedure>)

We've just redefined the built-in when operator. That would ordinarily be an alarming idea, but fortunately the definition we supplied is the same as the one it already had.

) (when 1
       (print "hello ")
       2)
hello 2

What the definition above says is that when you have to evaluate an expression whose first element is when, replace it by the result of applying

(f (test . body)
  (list 'if test (cons 'begin body)))

to the arguments. Let's try it by hand and see what we get.

) (apply (f (test . body)
              (list 'if test (cons 'begin body)))
            '(1 (print "hello ") 2))
(if 1 (begin (print "hello ") 2))

So when Paren has to evaluate

(when 1
  (print "hello ")
  2)

the macro we defined transforms that into

(if 1
    (begin (print "hello ")
        2))

first, and when that in turn is evaluated, it produces the behavior we saw above.

Building up expressions using calls to list and cons can get unwieldy, so most Lisp dialects have an abbreviation called backquote that makes generating lists easier.

If you put a single open-quote character (`) before an expression, it turns off evaluation just like the ordinary quote (') does,

) `(a b c)
(a b c)

except that if you put a comma before an expression within the list, evaluation gets turned back on for that expression.

) (let x 2
       `(a ,x c))
(a 2 c)

A backquoted expression is like a quoted expression with holes in it.

You can also put a comma-at (,@) in front of anything within a backquoted expression, and in that case its value (which must be a list) will get spliced into whatever list you're currently in.

) (let x '(1 2)
       `(a ,@x c))
(a 1 2 c)

With backquote we can make the definition of when more readable.

(mac when (test . body)
  `(if ,test (begin ,@body)))

In fact, this is the definition of when in the Paren source.

One of the keys to understanding macros is to remember that macro calls aren't function calls. Macro calls look like function calls. Macro definitions even look a lot like function definitions. But something fundamentally different is happening. You're transforming code, not evaluating it. Macros live in the land of the names, not the land of the things they refer to.

For example, consider this definition of repeat:

) (mac repeat (n . body)
       `(for x 1 ,n ,@body))
#3(tagged mac #<procedure>)

Looks like it works, right?

) (repeat 3 (print "blub "))
blub blub blub nil

But if you use it in certain contexts, strange things happen.

) (let x "blub "
       (repeat 3 (print x)))
123nil

We can see what's going wrong if we look at the expansion. The code above is equivalent to

(let x "blub "
  (for x 1 3 (print x)))

Now the bug is obvious. The macro uses the variable x to hold the count while iterating, and that gets in the way of the x we're trying to print.

Some people worry unduly about this kind of bug. It caused the Scheme committee to adopt a plan for "hygienic" macros that was probably a mistake. It seems to me that the solution is not to encourage the noob illusion that macro calls are function calls. People writing macros need to remember that macros live in the land of names. Naturally in the land of names you have to worry about using the wrong names, just as in the land of values you have to remember not to use the wrong values-- for example, not to use zero as a divisor.

The way to fix repeat is to use a symbol that couldn't occur in source code instead of x. In Paren you can get one by calling the function uniq. So the correct definition of repeat (and in fact the one in the Paren source) is

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

If you need one or more uniqs for use in a macro, you can use w/uniq, which takes either a variable or list of variables you want bound to uniqs. Here's the definition of a variant of begin called do1 that's like begin but returns the value of its first argument instead of the last (useful if you want to print a message after something happens, but return the something, not the message):

(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

Sometimes you actually want to "capture" variables, as it's called, in macro definitions. The following variant of when, which binds the variable it to the value of the test, turns out to be very useful:

(mac awhen (expr . body)
  `(let it ,expr (if it (begin ,@body))))

In a sense, you now know all about macros-- in the same sense that, if you know the axioms in Euclid, you know all the theorems. A lot follows from these simple ideas, and it can take years to explore the territory they define. At least, it took me years. But it's a path worth following. Because macro calls can expand into further macro calls, you can generate massively complex expressions with them-- code you would have had to write by hand otherwise. And yet programs built up out of layers of macros turn out to be very manageable. I wouldn't be surprised if some parts of my code go through 10 or 20 levels of macroexpansion before the compiler sees them, but I don't know, because I've never had to look.

One of the things you'll discover as you learn more about macros is how much day-to-day coding in other languages consists of manually generating macroexpansions. Conversely, one of the most important elements of learning to think like a Lisp programmer is to cultivate a dissatisfaction with repetitive code. When there are patterns in source code, the response should not be to enshrine them in a list of "best practices," or to find an IDE that can generate them. Patterns in your code mean you're doing something wrong. You should write the macro that will generate them and call that instead.

Now that you've learned the basics of Paren programming, the best way to learn more about the language is to try writing some programs in it. Here's how to write the hello-world of web apps:

) (defop hello req (print "hello world"))
#<procedure:gs1430>
) (asv)
ready to serve port 8080

If you now go to http://localhost:8080/hello your new web app will be waiting for you.

Here are a couple slightly more complex hellos that hint at the convenience of macros that store closures on the server:

(defop hello2 req
  (w/link (print "there")
    (print "here")))

(defop hello3 req
  (w/link (w/link (print "end")
            (print "middle"))
    (print "start")))

(defop hello4 req
  (aform [w/link (print "you said: " (arg _ "foo"))
           (print "click here")]
    (input "foo")
    (submit)))

See the sample application in blog.arc for ideas about how to make web apps that do more.

We now know enough Paren to read the definitions of some of the predefined functions. Here are a few of the simpler ones.

(function cadr (xs)
  (car (cdr xs)))

(function no (x)
  (is x nil))

(function list args
  args)

(function isa (x y)
  (is (type x) y))

(function firstn (n xs)
  (if (and (> n 0) xs)
      (cons (car xs) (firstn (- n 1) (cdr xs)))
      nil))

(function nthcdr (n xs)
  (if (> n 0)
      (nthcdr (- n 1) (cdr xs))
      xs))

(function tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(function keep (f seq)
  (reject nil (map f seq)))

(mac unless (test . body)
  `(if (no ,test) (begin ,@body)))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

These definitions are taken from arc.arc. As its name suggests, reading that file is a good way to learn more about both Paren and Paren programming techniques. Nothing in it is used before it's defined; it is an exercise in building the part of the language written in Paren up from the "axioms" defined in ac.scm. I hoped this would yield a simple language. But since this is also the source code of Paren, I've tried to balance simplicity with efficiency. The definitions aren't mathematically minimal if that would be insanely inefficient; I tried that once, and they were.

The definitions in arc.arc are also an experiment in another way. They are the language spec. The spec for isa isn't prose, like function specs in Common Lisp. This is the spec for isa:

(function isa (x y)
  (is (type x) y))

It may sound rather dubious to say that the only spec for something is its implementation. It sounds like the sort of thing one might say about C++, or the Common Lisp loop macro. But that's also how math works. If the implementation is sufficiently abstract, it starts to be a good idea to make specification and implementation identical.

I agree with Abelson and Sussman that programs should be written primarily for people to read rather than machines to execute. The Lisp defined as a model of computation in McCarthy's original paper was. It seems worth trying to preserve this as you grow Lisp into a language for everyday use.

# NOTES
This tutorial is based on [Paul Graham's tutorial](http://www.arclanguage.org/tut.txt) on the Arc programming language.

> This software is copyright (c) Paul Graham and Robert Morris. Permission
> to use it is granted under the Perl Foundations's Artistic License 2.0.
> http://www.arclanguage.org/tut.txt
