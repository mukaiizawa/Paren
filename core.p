; Paren core library.

; special operator

(macro special-operator (name syntax)
  ; A special operator is a operator with special evaluation rules, possibly manipulating the evaluation environment, control flow, or both.
  name)

(special-operator let
  ; Special operator let create new environment and bind symbol then execute a series of expression that use these bindings.
  ; First evaluates the expression init-expr1, then binds the symbol sym1 to that value,  then it evaluates init-expr2 and binds sym2, and so on.
  ; init-expr is evaluated under the newly created environment.
  ; Therefore, init-expr-n is affected by previously bound symbols(sym1, sym2, ... sym-n-1).
  ; Then, evaluate expressions from left to right.
  ; Returns the last evaluation result.
  (let (sym1 init-expr1 sym2 init-expr2 ...)
    expr1
    expr2
    ...))

(special-operator <-
  ; Special operator `<-` is the symbol binding statement of Paren.
  ; First evaluate expr1 and bind sym1 to the result, and so on.
  ; If the symbol is not bound to the current environment, the parent environment is searched in turn.
  ; If it is not bound to the global environment, bind to the global environment.
  ; If the symbol is already bound, it will be bound again with the result of the evaluation.
  ; Returns the last evaluation result.
  (<- sym1 expr1 sym2 expr2 ...))

(special-operator begin
  ; Special operator progn evaluates expressions, in the order in which they are given.
  ; Returns the last evaluation result.
  (begin
    expr1
    expr2
    ...))

(special-operator quote
  ; Special operator quote returns just expr.
  (quote expr))

(special-operator if
  ; Special operator if allows the execution of exprs to be dependent on test.
  ; Statements are evaluated one at a time in the order in which they are given in the expression list until a stmt is found that evaluates to true.
  ; Once one stmt has evaluated to true, no additional stmt are evaluated.
  ; If no stmt yields true, nil is returned.
  ; An odd number of arguments can be passed, in which case last argument act as a default phrase.
  ; Returns the last evaluation result.
  (if stmt1 expr1
      stmt2 expr2
      ...
      stmt-n expr-n
      [default-phrase]))

(special-operator lambda
  ; Special operator lambda creates an anonymous function.
  ; There are the following types of parameters.
  ; - required parameter
  ; - optional parameter
  ; - keyword parameter
  ; - rest parameter
  ; Required parameters are a parameter that results in an error if not specified when calling the function.
  ; Optional parameters are parameters that need not be specified when calling the function.
  ; Keyword parameters are specified with names without regard to order when calling the function.
  ; Rest parameters implement variable length arguments.
  ; Returns the anonymous function.
  (lambda ([required_param] ...
           [:opt optional_param ...]
           [{ :rest rest_param | :key keyword_param ... }] )
    expr1
    expr2
    ...))

(special-operator return
  ; Special operator return escapes from current lambda context.
  ; Returns the result of evaluating the argument val.
  (return val))

(special-operator macro
  ; Special operator macro creates macro named the specified name.
  ; Macro expands without evaluating its arguments.
  ; The macro-parameters that can be specified for macros differ in that macro-parameters can be specified recursively instead of required parameters.
  ; Returns the macro.
  (macro name ([{ param | required_param } ...]
               [:opt optional_param ...]
               [{ :rest rest_param | :key keyword_param ... }])
    expr1
    expr2
    ...))

(special-operator unwind-protect
  ; Special operator unwind-protect evaluates protected-expr and guarantees that cleanup-exprs are executed before unwind-protect exits, whether it terminates normally or is aborted by a control transfer of some kind.
  ; unwind-protect is intended to be used to make sure that certain side effects take place after the evaluation of protected-expr.
  (unwind-protect protected-expr
                  cleanup-expr
                  ...))

(special-operator goto
  ; Special operator goto is used in the context of labels.
  ; label must be a keyword and can only jump within the most recent labels context.
  ; Labels and gotos are rarely used directly and are used to define macros that create iteration contexts.
  (goto label))

(special-operator labels
  ; Special operator labels create a context for jumping with goto expressions.
  ; When a goto is evaluated in the labels context, transfer control to the location of the specified expr that matches the specified keyword.
  (labels expr1
          expr2
          ...))

(special-operator throw
  ; Special operator throw provide a mechanism to control global escape.
  ; By using special operator catch, it is possible to catch the occurrence of exception during evaluation.
  ; The throwing object must be an instance of the Paren object system.
  (throw expr))

(special-operator catch
  ; Special operator catch evaluate expr in order.
  ; If an error is thrown by the throw operator during expr evaluation, control is transferred to the handler and processing is performed.
  ; Handler must be a function with one required parameter.
  ; Since paren often uses the catch macro wrapped in the object system, it is not used directly.
  (catch (Error1 handler1 Error2 handler2 ...)
    expr1
    expr2
    ...))

(special-operator assert
  ; Evaluates the specified expression and kill the system if the results is nil.
  ; Not executed if not in debug mode.
  ; It is used when an argument or an internal state is abnormal, or a process that can not be reached is executed.
  (assert expr))

(special-operator dynamic
  ; Evaluate symbols with a dynamic scope.
  ; Used when dynamically binding the standard input.
  (dynamic sym))

; fundamental macro

(macro function! (name args :rest body)
  ; Bind a lambda to a specified symbol name.
  ; Same as function macro except for the following points.
  ; - No error even if the symbol is already bound.
  ; - Do not inline macros
  (list <- name (cons lambda (cons args body))))

(macro builtin-function (name args :rest body)
  ; Describes the specification of a built-in function and is used to describe unit tests.
  ; Built-in function is no different from user-defined function.
  (cons begin body))

(macro with-gensyms ((:rest syms) :rest body)
  ; Create the new let context which the specified syms bind with symbols which generated by gensyms and under the let context evaluate the specified body.
  ; Macro expansion image is as follows.
  ;     (with-gensyms (a b c)
  ;       ...)
  ;     (let (a (gensym) b (gensym) c (gensym))
  ;       ...)
  (let (rec (lambda (syms)
              (if syms (cons (car syms) (cons '(gensym) (rec (cdr syms)))))))
    (cons let (cons (rec syms) body))))

(macro begin0 (:rest body)
  ; Evaluate each of the specified body and return the first evaluated value.
  ; Macro expansion image is as follows.
  ;     (begin0 expr1 expr2 ...)
  ;     (let (x expr1)
  ;         (begin expr2
  ;                 ...)
  ;         x)
  (with-gensyms (val)
    (list let (list val (car body))
          (cons begin (cdr body))
          val)))

(macro when (test :rest body)
  ; Evaluate the specified test and if it is not nil then evaluate each of the specified body.
  (list if test (cons begin body)))

(macro || (:rest args)
  ; Evaluate each of the specified args, one at a time, from left to right.
  ; The evaluation of all args terminates when a args evaluates to true.
  ; Return last evaluated value.
  ; If args is nil, returns nil.
  (if (nil? args) nil
      (with-gensyms (g)
        (let (rec (lambda (l)
                    (if (nil? l) nil
                        (cons (list <- g (car l)) (cons g (rec (cdr l)))))))
          (list let (list g nil)
                (cons if (rec args)))))))

(macro && (:rest args)
  ; Evaluate each of the specified args, one at a time, from left to right.
  ; As soon as any form evaluates to nil, and returns nil without evaluating the remaining forms.
  ; If all args but the last evaluate to true values, and returns the results produced by evaluating the last args.
  ; If no args are supplied, returns true.
  (if (nil? args) true
      (let (rec (lambda (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))

(macro switch (expr :rest body)
  ; The switch macro help control complex conditional and branching operations.
  ; As shown below, the switch macro consists of one expr and multiple branches.
  ;     (switch expr
  ;       <branch>
  ;       <branch>
  ;       ...)
  ;     <branch> ::= <labels> expr
  ;     <labels> -- list of candidates for address comparison. Generally a symbol or keyword.
  ; You can also pass an atom in <labels>, in which case the elements are converted internally as a list.
  ; expr is evaluated only once and compared from left to right for matching elements in the <labels> of each branch.
  ; If there is a matching element, control is transferred to that branch.
  ; Otherwise, an error will occur.
  ; If you specify keyword `:default` (-- must be atom) in <labels>, you can unconditionally transfer control to that branch regardless of the value of expr.
  ; Macro expansion image is as follows.
  ;     (switch :a
  ;       :a "a"
  ;       (:b) "b"
  ;       (:c :d) "c or d"
  ;       :default "others")
  ;     (if (|| (eq? :a :a)) "a"
  ;         (|| (eq? :a :b)) "b"
  ;         (|| (eq? :a :c) (eq? :a :d)) "c or d"
  ;         "others")
  (with-gensyms (gexpr branches)
    (let (branches (group body 2)
                   candidates (list 'flatten (list 'map car (list quote branches)))
                   parse-branch (lambda (branches)
                                  (if (nil? branches)
                                      (list true (list 'error gexpr " not included in " candidates))
                                      (let (label (caar branches) then (cadar branches))
                                        (cons (if (eq? label :default) (return (list true then))
                                                  (cons '|| (map (lambda (label)
                                                                   (list eq? (list quote label) gexpr))
                                                                 (->list label))))
                                              (cons then
                                                    (parse-branch (cdr branches))))))))
      (list let (list gexpr expr)
            (cons if (parse-branch branches))))))

(macro break ()
  ; The break macro is expected evaluated in the iteration context like a for, while.
  ; Jump to :break label which causes the inner-most loop to be terminated immediately when executed.
  ; If you create new iteration macro, desirable to support it.
  '(goto :break))

(macro continue ()
  ; The continue macro is expected evaluated in the iteration context like a for, while.
  ; Jump to :continue label which will move at once to the next iteration without further progress through the loop body for the current iteration.
  ; If you create new iteration macro, desirable to support it.
  '(goto :continue))

(macro for (binding test update :rest body)
  ; The for macro creates a general-purpose iteration context and evaluates the specified body.
  ; Returns nil.
  ; Macro expansion image is as follows.
  ;     (for (i 0) (< i 10) (<- i (++ i))
  ;         expr1
  ;         expr2
  ;         ...)
  ;     (let (i 0)
  ;        (labels :start
  ;                (if (! test) (goto :break))
  ;                expr1
  ;                expr2
  ;                ...
  ;                :continue
  ;                (<- i (++ i))
  ;                (goto :start)
  ;                :break)
  ;         nil)
  (list let binding
        (list labels
              :start
              (list if (list ! test) '(goto :break))
              (cons begin body)
              :continue
              update
              '(goto :start)
              :break)
        nil))

(macro while (test :rest body)
  ; The specified test is evaluated, and if the specified test is true, each of the specified body is evaluated.
  ; This repeats until the test becomes nil.
  ; Supports break, continue macro.
  ; Returns nil.
  ;     (while test
  ;        expr1
  ;        expr2
  ;        ...)
  ;     (for nil test nil
  ;        expr1
  ;        expr2
  ;        ...)
  (list labels
        :start
        (list if (list ! test) '(goto :break))
        (cons begin body)
        :continue
        '(goto :start)
        :break
        nil))

(macro dolist ((i l) :rest body)
  ; Iterates over the elements of the specified list l, with index the specified i.
  ; Evaluate each of the specified body once for each element in list l, with index i bound to the element.
  ; Supports break, continue macro.
  ; Returns nil.
  (with-gensyms (gl)
    (list 'for (list gl l i (list car gl)) gl (list <- gl (list cdr gl) i (list car gl))
          (cons begin body))))

(macro dotimes ((i n) :rest body)
  ; Iterates over a series of integers, from 0 to the specified n.
  ; The specified body once for each integer from 0 up to but not including the value of n, with the specified i bound to each integer.
  ; Supports break, continue macro.
  ; Returns nil.
  (with-gensyms (gn)
    (list 'for (list i 0 gn n) (list < i gn) (list <- i (list '++ i))
          (cons begin body))))

(macro timeit (:rest body)
  ; Clock the time it takes to evaluate the specified body.
  ; Returns evaluation result of the last element of body.
  (with-gensyms (clock-offset cycle-offset)
    (list let (list clock-offset '(clock) cycle-offset '(cycle))
          (list 'begin0
                (cons begin body)
                (list 'write-bytes (list 'string
                                         "time=" (list '- '(clock) clock-offset)
                                         ",cycle=" (list '- '(cycle) cycle-offset)))
                '(write-line)))))

(builtin-function expand-macro (expr)
  ; Returns the result of expanding the macro when expr is a list and car is a macro.
  ; Otherwise returns expr.
  (assert (eq? (car (expand-macro '(begin0 1 2 3))) let)))

(function! expand-macro-all (expr)
  ; Same as expand-macro except that it executes recursively.
  (let (expand1 (lambda (expr)
                  (if (cons? expr) (each-expand (expand-macro expr))
                      expr))
                each-expand (lambda (expr)
                              (if expr (cons (expand1 (car expr))
                                             (each-expand (cdr expr))))))
    (expand1 expr)))

; fundamental function

(macro function (name args :rest body)
  ; Returns a function which parameter list the specified args and lambda function body the specified body.
  ; The macro in the function body is expanded.
  ; The created function binds the name symbol.
  ; Error if name is already bound.
  (if (bound? name) (error name " already bound")
      (list begin0 (list quote name)
            (list <- name (cons lambda (cons args (expand-macro-all body)))))))

(builtin-function eq? (x y :rest args)
  ; Returns whether all arguments are the same object.
  (assert (! (eq? 'x 'y 'z)))
  (assert (eq? 'x 'x 'x)))

(builtin-function neq? (x y :rest args)
  ; Same as (! (eq? x y)).
  (assert (neq? 'x 'y 'z))
  (assert (! (neq? 'x 'x 'x))))

(builtin-function address (x)
  ; Returns address of the specified x.
  ; The addresses of symbols or keywords with the same name are always equal.
  (assert (= (address 'x) (address 'x))))

(builtin-function ! (x)
  ; Returns whether the x is nil.
  (assert (! (eq? 'x 'y 'z)))
  (assert (! nil))
  (assert (eq? (! true) nil)))

(function nil? (x)
  ; Same as (! x).
  (! x))

(builtin-function cons? (x)
  ; Returns whether the x is a cons.
  (assert (cons? '(1)))
  (assert (! (cons? nil)))
  (assert (! (cons? '()))))

(function atom? (x)
  ; Returns whether the x is an atom.
  ; It means x is cons or not.
  (! (cons? x)))

(builtin-function function? (x)
  ; Returns whether the x is a lambda.
  (assert (function? (lambda (x) x)))
  (assert (! (function? begin0))))

(builtin-function macro? (x)
  ; Returns whether the x is a macro.
  (assert (macro? begin0))
  (assert (! (macro? begin))))

; symbol & keyword

(builtin-function symbol? (x)
  ; Returns whether the x is symbol.
  (assert (symbol? 'foo))
  (assert (! (symbol? :foo)))
  (assert (! (symbol? (bytes 3)))))

(builtin-function keyword? (x)
  ; Returns whether the x is keyword.
  (assert (keyword? :foo))
  (assert (! (keyword? 'foo)))
  (assert (! (keyword? (bytes 3)))))

(builtin-function bound? (sym)
  ; Returns whether the x is bound.
  (assert (bound? 'bound?))
  (assert (bound? 'nil)))

(builtin-function gensym ()
  ; Returns a numbered symbol starting with `$G-`.
  ; gensim only guarantees that the symbols generated with each gensim call will not collide.
  ; There is no inconvenience unless intentionally generating symbols starting with `$G-`.
  (assert (neq? (gensym) (gensym))))

; list

(builtin-function cons (x y)
  ; Returns a cons such that the specified x is the car part and y is the cdr part.
  ; Error if y is not cons.
  (assert (eq? (car (cons 'x nil)) 'x))
  (assert (nil? (cdr (cons 'x nil)))))

(builtin-function car (x)
  ; Returns car of the specified cons x.
  ; If x is nil, returns nil.
  ; Error if x is not list.
  (assert (= (car '(1 2 3)) 1))
  (assert (nil? (car '()))))

(builtin-function car! (x v)
  ; Destructively change the car of the specified cons x to the specified v.
  ; Returns v.
  ; Error if x is not cons.
  (let (x '(1 2 3))
    (assert (eq? (car! x 'one) 'one))
    (assert (eq? (car x) 'one))))

(builtin-function cdr (x)
  ; Returns cdr of the specified cons x.
  ; If x is nil, returns nil.
  ; Error if x is not list.
  (assert (= (car (cdr '(1 2 3))) 2))
  (assert (nil? (cdr '()))))

(builtin-function cdr! (x v)
  ; Destructively changes the cdr of the specified cons to the specified v.
  ; Returns v.
  ; Error if x is not cons or v is not list.
  (let (x '(1 2 3))
    (cdr! x '(two))
    (assert (eq? (car (cdr x)) 'two))))

(function caar (x)
  ; Same as (car (car x)).
  (car (car x)))

(function cadr (x)
  ; Same as (car (cdr x)).
  (car (cdr x)))

(function cdar (x)
  ; Same as (cdr (car x)).
  (cdr (car x)))

(function cddr (x)
  ; Same as (cdr (cdr x)).
  (cdr (cdr x)))

(function caaar (x)
  ; Same as (car (caar x)).
  (car (caar x)))

(function caadr (x)
  ; Same as (car (cadr x)).
  (car (cadr x)))

(function cadar (x)
  ; Same as (car (cdar x)).
  (car (cdar x)))

(function caddr (x)
  ; Same as (car (cddr x)).
  (car (cddr x)))

(function cdaar (x)
  ; Same as (cdr (caar x)).
  (cdr (caar x)))

(function cdadr (x)
  ; Same as (cdr (cadr x)).
  (cdr (cadr x)))

(function cddar (x)
  ; Same as (cdr (cdar x)).
  (cdr (cdar x)))

(function cdddr (x)
  ; Same as (cdr (cddr x)).
  (cdr (cddr x)))

(function caaaar (x)
  ; Same as (car (caaar x)).
  (car (caaar x)))

(function caaadr (x)
  ; Same as (car (caadr x)).
  (car (caadr x)))

(function caadar (x)
  ; Same as (car (cadar x)).
  (car (cadar x)))

(function caaddr (x)
  ; Same as (car (caddr x)).
  (car (caddr x)))

(function cadaar (x)
  ; Same as (car (cdaar x)).
  (car (cdaar x)))

(function cadadr (x)
  ; Same as (car (cdadr x)).
  (car (cdadr x)))

(function caddar (x)
  ; Same as (car (cddar x)).
  (car (cddar x)))

(function cadddr (x)
  ; Same as (car (cdddr x)).
  (car (cdddr x)))

(function cdaaar (x)
  ; Same as (cdr (caaar x)).
  (cdr (caaar x)))

(function cdaadr (x)
  ; Same as (cdr (caadr x)).
  (cdr (caadr x)))

(function cdadar (x)
  ; Same as (cdr (cadar x)).
  (cdr (cadar x)))

(function cdaddr (x)
  ; Same as (cdr (caddr x)).
  (cdr (caddr x)))

(function cddaar (x)
  ; Same as (cdr (cdaar x)).
  (cdr (cdaar x)))

(function cddadr (x)
  ; Same as (cdr (cdadr x)).
  (cdr (cdadr x)))

(function cdddar (x)
  ; Same as (cdr (cddar x)).
  (cdr (cddar x)))

(function cddddr (x)
  ; Same as (cdr (cdddr x)).
  (cdr (cdddr x)))

(builtin-function list (:rest args)
  ; Returns a list whose elements are the specified args.
  ; If args is nil, returns nil.
  (assert (= (car '(1 2 3)) 1))
  (assert (nil? (car '()))))

(function list? (x)
  ; Returns whether the x is a list.
  ; Same as (|| (nil? x) (cons? x)).
  (|| (nil? x) (cons? x)))

(function ->list (x)
  ; Returns a list with x as the only element.
  ; If x is a list, returns x.
  (if (list? x) x
      (list x)))

(function list->string (l :opt delim)
  ; Returns a new string of the specified list elements joined together with of the specified delimiter.
  ; If delim is not specified, consider an empty string to be specified.
  (if delim (reduce (lambda (x y) (string x delim y)) l)
      (apply bytes-concat (cons "" l))))

(builtin-function length (l)
  ; Returns the length of the specified list l.
  (assert (= (length nil) 0))
  (assert (= (length '(1)) 1)))

(builtin-function last-cons (x)
  ; Returns the terminal cons.
  ; If x is nil, returns nil.
  ; Error if x is not cons.
  (assert (= (car (last-cons '(1 2 3))) 3))
  (assert (nil? (last-cons nil))))

(function nth (l n)
  ; Returns the nth element of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (car (nthcdr l n)))

(function nthcdr (l n)
  ; Returns the the specified nth cons of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (for (i 0) (&& l (< i n)) (<- i (++ i))
    (<- l (cdr l)))
  l)

(function first (x)
  ; Same as (nth x 0).
  (nth x 0))

(function second (x)
  ; Same as (nth x 1).
  (nth x 1))

(function third (x)
  ; Same as (nth x 2).
  (nth x 2))

(function last (x)
  ; Same as (nth x (-- (length x))).
  (nth x (-- (length x))))

(function butlast (l)
  ; Returns a list excluding the last element of the specified list l.
  (let (rec (lambda (rest)
              (if (cdr rest) (cons (car rest) (rec (cdr rest)))
                  nil)))
    (rec l)))

(builtin-function copy (l)
  ; Returns a list that is a copy of the argument list l.
  ; The copy target is only cons, the element is not copied.
  )

(function .. (s e :opt step)
  ; Returns a list with the specified step increments from the specified integer s to the specified integer e.
  (let (acc nil step (|| step 1))
    (while (<= s e)
      (push! acc s)
      (<- s (+ s step)))
    (reverse! acc)))

(function group (l n)
  ; Returns a list in which the elements of l are grouped into sublists of length n.
  ; Error if the list length is not a multiple of n.
  (if (<= n 0) (error "illegal arguments")
      (let (lis nil)
        (while l
          (let (sublis nil)
            (dotimes (i n)
              (if (nil? l) (error "indivisible by " n))
              (push! sublis (car l))
              (<- l (cdr l)))
            (push! lis (reverse! sublis))))
        (reverse! lis))))

(function reverse (l)
  ; Returns a list with the elements of list l reversed.
  (let (acc nil)
    (dolist (x l)
      (<- acc (cons x acc)))
    acc))

(builtin-function reverse! (l)
  ; Same as reverse except that it destructively modifies the argument list.
  ; Generally faster than reverse.
  (assert (nil? (reverse! nil)))
  (assert (= (car (reverse! '(0 1))) 1)))

(builtin-function append (:rest args)
  ; Returns a new list with each argument element as an element.
  ; If args is nil, returns nil.
  ; Error if all arguments are not list.
  (assert (nil? (append)))
  (assert (nil? (append nil)))
  (assert (nil? (append nil nil)))
  (assert (= (length (append '(1 2) '(3))) 3))
  (assert (= (length (append '(1) '(2))) 2))
  (assert (= (length (append nil '(1) '(2))) 2)))

(macro push! (sym x)
  ; Destructively add the specified element x to the top of the specified list that binds the specified symbol sym.
  ; Returns x.
  (with-gensyms (y)
    (list let (list y x)
          (list <- sym (list cons y sym))
          y)))

(macro pop! (sym)
  ; Returns the head of the list that binds the specified symbol sym and rebinds sym with the cdr of the list.
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))

(builtin-function assoc (al k)
  ; Returns a value corresponding to the specified key k of the specified asoociate list al.
  ; Error if there is no key.
  (assert (= (assoc '(:one 1 :two 2 :three 3) :one) 1)))

(builtin-function assoc! (al k v)
  ; Change the value corresponding to the specified key k in the specified association list al to the specified vlaue v.
  ; Error if there is no key.
  (assert (eq? (assoc! '(:one 1 :two 2 :three 3) :one 'one) 'one)))

(function flatten (l)
  ; Returns a list in which the car parts of all cons that make up the specified list l are elements.
  (let (acc nil rec (lambda (x)
                      (if (atom? x) (push! acc x)
                          (dolist (i x) (rec i)))))
    (rec l)
    (reverse! acc)))

(function map (f args)
  ; Returns a list of the results of mapping each element of the specified list args with the specified function f.
  (let (acc nil)
    (while args
      (push! acc (f (car args)))
      (<- args (cdr args)))
    (reverse! acc)))

(function reduce (f l)
  ; Returns the value that apply function of two arguments cumulatively to the elements of the list l, from left to right.
  (let (rec (lambda (l)
              (if (nil? (cdr l)) (car l)
                  (rec (cons (f (car l) (cadr l)) (cddr l))))))
    (rec l)))

(function find-cons-if (f l)
  ; Returns the first cons for which the result of applying the function f to the list elements in order from left to right is true.
  ; If there is no such cons, returns nil.
  (while l
    (if (f (car l)) (return l)
        (<- l (cdr l)))))

(function find-if (f l)
  ; Returns the first element for which the result of applying the function f to the list elements in order from left to right is true.
  ; If there is no such cons, returns nil.
  (car (find-cons-if f l)))

(function remove-if (f l)
  ; Returns a list with the elements for which the result of applying the function f is true removed.
  (let (acc nil)
    (dolist (x l)
      (if (nil? (f x)) (push! acc x)))
    (reverse! acc)))

(function all-satisfy? (f l)
  ; Returns whether the result of the function f applied to all the elements of the list is true.
  ; If x is nil, returns true.
  ; As soon as any element evaluates to nil, and returns nil without evaluating the remaining elements.
  (while l
    (if (f (car l)) (<- l (cdr l))
        (return nil)))
  true)

(function all-adjacent-satisfy? (f l)
  ; Returns whether each adjacent element of the specified list l returns true when evaluated as an argument to the specified function f
  (while (cdr l)
    (if (f (car l) (cadr l)) (<- l (cdr l))
        (return nil)))
  true)

(function any-satisfy? (f l)
  ; Returns whether the function f applied to any element of the list is true.
  ; If x is nil, returns nil.
  ; As soon as any element evaluates to not nil, and returns it without evaluating the remaining elements.
  (while l
    (if (f (car l)) (return true)
        (<- l (cdr l)))))

; number

(builtin-function number? (x)
  ; Returns whether the x is a number.
  (assert (number? 1))
  (assert (number? 3.14))
  (assert (number? 0x20))
  (assert (nil? (number? 'x))))

(builtin-function int? (x)
  ; Returns whether the x is a integer.
  (assert (int? 1))
  (assert (nil? (int? 3.14)))
  (assert (nil? (int? 'x))))

(function byte? (x)
  ; Returns whether the x is a integer and between 0 and 255.
  (&& (int? x) (<= 0 x 255)))

(function byte-space? (b)
  ; Returns whether byte b is a space character.
  (|| (= b 0x09)
      (= b 0x0a)
      (= b 0x0d)
      (= b 0x20)))

(function byte-alpha? (b)
  ; Returns whether byte b is an alphabetic character.
  (|| (<= 0x41 b 0x5a) (<= 0x61 b 0x7a)))

(function byte-digit? (b)
  ; Returns whether byte b is a digit character.
  (<= 0x30 b 0x39))

(function int->string (i :key radix padding)
  ; Returns string of i.
  (with-memory-stream (out)
    (.write-int out i :radix radix :padding padding)))

(function byte-lower (b)
  ; Returns lowercase if byte b is an alphabetic character.
  ; Otherwise returns b.
  (if (&& (byte-alpha? b) (<= 0x41 b 0x5a)) (+ b 0x20)
      b))

(function byte-print? (b)
  ; Returns whether b is printable.
  (&& (byte? b) (<= 0x20 b 0x7e)))

(function byte-upper (b)
  ; Returns uppercase if byte b is an alphabetic character.
  ; Otherwise returns b.
  (if (&& (byte-alpha? b) (<= 0x61 b 0x7a)) (- b 0x20)
      b))

(function byte->digit (b :opt radix)
  ; Returns byte b as a number.
  ; If the radix is not specified, 10 is assumed to be specified.
  (let (n (if (byte-digit? b) (- b 0x30)
              (byte-alpha? b) (+ (- (byte-lower b) 0x61) 10)))
    (if (|| (nil? n) (>= n (|| radix 10))) (error "not numeric char")
        n)))

(function digit->byte (n)
  ; Returns the byte representation of a number.
  (if (< n 10) (+ n 0x30)
      (+ n 0x61 -10)))

(builtin-function = (x y :rest args)
  ; Returns whether all arguments are the same value.
  ; However, arguments for which the function `eq?` returns true will always return true.
  (assert (= 1 1 1))
  (assert (= 1.0 1.0 1.0))
  (assert (= 1 1.00 1))
  (assert (! (= 10 20)))
  (assert (= 'x 'x))
  (assert (! (= 'x 'y))))

(builtin-function & (x y)
  ; bitwise and.
  (assert (= (& 0x333333333 0x555555555) 0x111111111)))

(builtin-function | (x y)
  ; bitwise or.
  (assert (= (| 0x333333333 0x555555555) 0x777777777)))

(builtin-function << (x y)
  ; bitwise left shift.
  (assert (= (<< 3 2) 12)))

(function >> (x y)
  ; bitwise right shift.
  (<< x (- y)))

(builtin-function ^ (x y)
  ; bitwise xor.
  (assert (= (^ 3 0x500000000) 0x500000003))
  (assert (= (^ 0x500000000 0x500000003) 3)))

(function /= (x y)
  ; Same as (! (= x y))).
  (! (= x y)))

(builtin-function + (x :rest args)
  ; Returns the sum of the args.
  (assert (= (+ 1) 1))
  (assert (= (+ 1 2 3) 6))
  (assert (= (+ 1 2.0 3.0) 6)))

(function - (x :rest args)
  ; Returns the value of the specified x minus the sum of the specified args.
  ; If args is nil, returns inverted value of the x.
  (if (nil? args) (* x -1)
      (+ x (- (apply + args)))))

(builtin-function * (x :rest args)
  ; Returns the product of the arguments.
  (assert (= (* 1 2 3) 6))
  (assert (= (* 1.0 2.0 3.0) 6))
  (assert (= (* 1 2.0 3.0) 6)))

(builtin-function / (x :rest args)
  ; Returns the quotient of the x divided by the each args.
  ; If args is nil, returns the reciprocal of x.
  (assert (= (/ 2) 0.5))
  (assert (= (/ 12 2 3) 2))
  (assert (= (/ 3 2 5) 0.3)))

(builtin-function // (x y)
  ; Returns the quotient of the x divided by the y.
  (assert (= (// 2 1) 2))
  (assert (= (// 2 2) 1))
  (assert (= (// 2 3) 0)))

(builtin-function mod (x y)
  ; Returns the remainder of dividing x by y.
  (assert (= (mod 4 5) 4))
  (assert (= (mod 4 3) 1))
  (assert (= (mod 4 2) 0)))

(builtin-function < (:rest args)
  ; Returns whether the each of the specified args are in monotonically decreasing order.
  (assert (< 0 1 2))
  (assert (< 0 1.0 2))
  (assert (nil? (< 0 0 1))))

(function > (:rest args)
  ; Returns whether the each of the specified args are in monotonically increasing order.
  (all-adjacent-satisfy? (lambda (x y) (< y x)) args))

(function <= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nondecreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (< y x))) args))

(function >= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nonincreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (< x y))) args))

(function ++ (x)
  ; Same as (+ x 1).
  (+ x 1))

(function -- (x)
  ; Same as (- x 1).
  (- x 1))

(function abs (x)
  ; Returns the absolute value of the specified number x.
  (if (< x 0) (- x)
      x))

(function exp (base power)
  ; Returns base-number raised to the power power-number.
  (let (val 1.0)
    (dotimes (i (abs power))
      (<- val (* val base)))
    (if (> power 0) val
        (/ val))))

; string

(function string (:rest args)
  ; Returns concatenated string which each of the specified args as string.
  (with-memory-stream (ms)
    (dolist (arg args)
      (if (string? arg) (write-bytes arg ms)
          arg (write arg ms :end "")))))

(builtin-function string? (x)
  ; Returns whether the x is a string.
  (assert (string? ""))
  (assert (string? "aaa"))
  (assert (! (string? (bytes 1)))))

(function string->number (s)
  ; Returns a string as a number.
  (.skip-number (.init (.new AheadReader) s)))

(function string->code (s)
  ; Returns the code point of string s.
  (let (b 0 val 0)
    (with-memory-stream (in s)
      (while (/= (<- b (read-byte in)) -1)
        (<- val (| (<< val 8) b))))
    val))

(function code->string (i)
  ; Returns string of code point.
  (with-memory-stream (out)
    (while (/= i 0)
      (write-byte (& i 0xff) out)
      (<- i (>> i 8)))))

(function string->array (s)
  ; Returns a character array of string s.
  (let (a (.new Array) c nil)
    (with-memory-stream (in s)
      (while (<- c (read-char in))
        (.add a c)))
    (.to-a a)))

(function string= (x y)
  ; Same as (bytes= x y).
  (bytes= x y))

(function string/= (x y)
  ; Same as (bytes/= x y).
  (bytes/= x y))

(function string< (:rest args)
  ; Returns whether the each of the specified args are in monotonically decreasing order.
  (apply < (map string->code args)))

(function string> (:rest args)
  ; Returns whether the each of the specified args are in monotonically increasing order.
  (all-adjacent-satisfy? (lambda (x y) (string< y x)) args))

(function string<= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nondecreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (string< y x))) args))

(function string>= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nonincreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (string< x y))) args))

(function string-prefix? (s prefix)
  ; Returns whether the string x with the specified prefix.
  (&& (>= (bytes-length s) (bytes-length prefix))
      (bytes-index s prefix 0 (bytes-length prefix))))

(function string-suffix? (s suffix)
  ; Returns whether the string x with the specified suffix.
    (&& (>= (bytes-length s) (bytes-length suffix))
        (bytes-index s suffix (- (bytes-length s) (bytes-length suffix)))))

(function string-slice (s start :opt end)
  ; Returns a string that is a substring of the specified string s.
  ; The substring begins at the specified start and extends to the character at index end - 1.
  ; Thus the length of the substring is `end - start`.
  (let (ms (.new MemoryStream))
    (if (< start 0) (error "illegal start " start))
    (.write-bytes ms s)
    (dotimes (i start)
      (if (nil? (.read-char ms)) (error "illegal start " start)))
    (if (nil? end) (bytes-slice s (.tell ms))
        (let (pos (.tell ms))
          (dotimes (i (- end start))
            (if (nil? (.read-char ms)) (error "illegal end " end)))
          (bytes-slice s pos (.tell ms))))))

(function string-at (s i)
  ; Returns the i-th character of string s.
  ([] (string->array s) i))

(function string-length (s)
  ; Returns the number of characters in string s.
  (array-length (string->array s)))

(function string-index (s pat :opt start)
  ; Returns the position where the substring pat appears first in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  ; If start is specified, search for substring pat from start-th of the string s.
  (let (start (|| start 0) sa (string->array s) slen (array-length sa)
              pa (string->array pat) plen (array-length pa))
    (if (< (- slen start) 0) (error "illegal start")
        (= plen 0) (return 0))
    (for (i start end (- slen plen) p0 ([] pa 0)) (<= i end) (<- i (++ i))
      (when (bytes= ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (bytes= ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

(function string-last-index (s pat)
  ; Returns the position where the substring pat appears last in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  (let (sa (string->array s) slen (array-length sa)
           pa (string->array pat) plen (array-length pa))
    (if (= plen 0) (return (-- slen)))
    (for (i (- slen plen) p0 ([] pa 0)) (>= i 0) (<- i (-- i))
      (when (bytes= ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (bytes= ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

(function string->list (s :opt delim)
  ; Returns a list of characters in string s.
  ; If delim is specified, returns a list of strings s delimited by delimiter.
  (if (nil? delim) (array->list (string->array s))
      (let (i 0 lis nil chars nil
              sa (string->array s) salen (array-length sa)
              da (string->array delim) dalen (array-length da) end (- salen dalen)
              match? (lambda ()
                       (dotimes (j dalen)
                         (if (! (bytes= ([] sa (+ i j)) ([] da j))) (return nil)))
                       true)
              join-chars (lambda () (if chars (apply bytes-concat (reverse! chars)) "")))
        (while (<= i end)
          (if (match?) (<- lis (cons (join-chars) lis)
                           chars nil
                           i (+ i dalen))
              (<- chars (cons ([] sa i) chars)
                  i (++ i))))
        (while (< i salen)
          (<- chars (cons ([] sa i) chars)
              i (++ i)))
        (reverse! (cons (join-chars) lis)))))

; bytes

(builtin-function bytes (size)
  ; Returns a bytes of size the specified size.
  ; The element is cleared to 0.
  )

(builtin-function bytes? (x)
  ; Returns whether the x is bytes.
  ; symbols, keywords, and strings are acceptable as arguments for some bytes api, but this function returns nil.
  (assert (bytes? (bytes 3)))
  (assert (! (bytes? 'foo)))
  (assert (! (bytes? :foo)))
  (assert (! (bytes? "foo")))
  (assert (! (bytes? (array 3)))))

(builtin-function bytes= (x y)
  ; Returns whether x arguments are the same bytes.
  ; This function also accepts symbols, keywords and strings.
  (assert (bytes= :foo :foo))
  (assert (! (bytes= "foo" "bar"))))

(function bytes/= (x y)
  ; Same as (! (bytes= x y)).
  (! (bytes= x y)))

(builtin-function ->bytes (x :opt i size)
  ; Returns bytes corresponding to x.
  ; If i is supplied, returns string of partial byte sequence from i of x.
  ; If size is supplied, returns string of partial byte sequence from i to (size -1) of x.
  (assert (eq? (bytes->symbol "foo") 'foo)))

(builtin-function bytes->symbol (x :opt i size)
  ; Same as (->bytes x) except returns symbol.
  (assert (eq? (bytes->symbol "foo") 'foo)))

(builtin-function bytes->keyword (x)
  ; Same as (->bytes x) except returns keyword.
  (assert (eq? (bytes->keyword "foo") :foo)))

(builtin-function bytes->string (x :opt i size)
  ; Same as (->bytes x) except returns string.
  (assert (bytes= (bytes->string 'foo) "foo"))
  (assert (bytes= (bytes->string 'foo 1) "oo"))
  (assert (bytes= (bytes->string 'foo 1 1) "o")))

(builtin-function bytes->string! (x)
  ; Same as (bytes->string x), except that it destructively modifies the x.
  ; Generally faster than bytes->string.
  ; This function only allows bytes.
  (assert (let (x (bytes 1))
            ([]<- x 0 0x01)
            (bytes= (bytes->string! x) "\x01"))))

(function bytes->list (s :opt delim)
  ; Returns a list of bytes delimited by bytes s.
  ; If delim is specified, returns a list of strings s delimited by delimiter.
  (if (nil? delim) (string->list s)
      (let (acc nil i 0 pos nil slen (bytes-length s) dlen (bytes-length delim))
        (assert (> dlen 0))
        (while (&& (< i slen) (<- pos (bytes-index s delim i)))
          (push! acc (bytes-slice s i pos))
          (<- i (+ pos dlen)))
        (push! acc (bytes-slice s i slen))
        (reverse! acc))))

(builtin-function bytes-length (x)
  ; Returns the size of the bytes x.
  ; This function also accepts symbols, keywords and strings.
  (assert (= (bytes-length "") 0))
  (assert (= (bytes-length "012") 3)))

(builtin-function bytes-index (x b :opt start end)
  ; Returns the position where the b appears first in the bytes x.
  ; If the b is not appeared, returns nil.
  ; If b is bytes, returns the position where the partial bytes appears first in the bytes x.
  ; If start is specified, search from start-th of the bytes x.
  ; If end is specified, search untile end-th of the bytes x.
  ; This function also accepts symbols, keywords and strings.
  (assert (= (bytes-index "012" 0x31 1) 1))
  (assert (= (bytes-index "012" 0x31 0 3) 1))
  (assert (= (bytes-index "012" 0x31 0 3) 1))
  (assert (= (bytes-index "012" "12" 0 3) 1)))

(builtin-function bytes-copy (src src-i dst dst-i size)
  ; Copy size elements from the `src-i`th element of the src bytes to the dst bytes `dst-i`th element and beyond.
  ; Returns dst.
  ; Even if the areas to be copied overlap, it operates correctly.
  ; This function also accepts strings.
  (assert (let (s "foo" d "bar")
            (bytes= (bytes-copy s 1 d 1 2) "boo"))))

(builtin-function bytes-slice (x start :opt end)
  ; Returns the partial byte sequence starting from start.
  ; If end is specified, returns the partial byte sequence from the i th to (end-1) th.
  ; This function also accepts strings.
  (assert (bytes= (bytes-slice "012" 0) "012"))
  (assert (bytes= (bytes-slice "012" 1) "12"))
  (assert (bytes= (bytes-slice "012" 1 2) "1")))

(builtin-function bytes-concat (x :rest args)
  ; Returns the result of combining each args with x.
  ; This function also accepts symbols, keywords and strings.
  (assert (bytes= (bytes-concat "0" "1" "2") "012")))

; array

(builtin-function array (size)
  ; Returns an array of length size.
  (assert (array 1)))

(builtin-function array? (x)
  ; Returns whether the x is an array.
  ; However, bytes are not considered as arrays.
  (assert (array? (array 3)))
  (assert (! (array? (bytes 3)))))

(function array->list (x)
  ; Returns array as a list.
  (let (acc nil)
    (dotimes (i (array-length x))
      (push! acc ([] x i)))
    (reverse! acc)))

(builtin-function [] (x i)
  ; Returns the i-th element of the array x.
  ; This function can also be applied to bytes.
  (assert (nil? ([] (array 1) 0)))
  (assert (= ([] (bytes 1) 0) 0)))

(builtin-function []<- (x i v)
  ; Update the i-th element of array x to v.
  ; Returns v.
  ; This function can also be applied to bytes.
  (assert (let (a (array 1) b (bytes 1))
            (&& ([]<- a 0 true)
                ([] a 0)
                ([]<- b 0 0xff)
                (= ([] b 0) 0xff)))))

(builtin-function array-length (x)
  ; Returns the length of the specified array x.
  (assert (= (array-length (array 3)) 3)))

(builtin-function array-copy (src src-i dst dst-i size)
  ; Copy size elements from the `src-i`th element of the src bytes to the dst bytes `dst-i`th element and beyond.
  ; Returns dst.
  ; Even if the areas to be copied overlap, it operates correctly.
  ; This function also accepts strings.
  (assert (let (s (array 1) d (array 2))
            ([]<- s 0 1)
            ([]<- d 0 :zero)
            ([]<- d 1 :one)
            (&& (= ([] (array-copy s 0 d 1 1) 1) 1)
                (eq? ([] d 0) :zero)
                (= ([] d 1) 1)))))

(function array-slice (x start :opt end)
  ; Returns a new array object selected from start to end (end not included) where start and end represent the index of items in that array x.
  (let (xlen (array-length x))
    (if (< start 0) (error "illegal start")
        (nil? end) (<- end xlen)
        (> end xlen) (error "illegal end"))
    (let (new-len (- end start) new-array (array new-len))
      (array-copy x start new-array 0 new-len))))

; os

(builtin-function fp (fd)
  ; Returns the file pointer associated with the file descriptor.
  ; The argument fd can specify bellow value.
  ;      0 -- stdin
  ;      1 -- stdout
  ;      2 -- stderr
  )

(builtin-function fopen (filename mode)
  ; Opens the file whose name is the string pointed to by filename and associates a stream with it.
  ; Returns file poiner for the opened file.
  ; The argument mode can specify bellow value.
  ;      0 -- Open file for reading.
  ;      1 -- Open file for writing.
  ;      2 -- Open file for appending
  ;      3 -- Open file for reading and writing.
  )

(builtin-function fgetc (fp)
  ; Read byte from the stream associated with the file pointer fp.
  ; Returns read byte.
  ; If stream reached EOF, returns -1.
  )

(builtin-function fputc (c fp)
  ; Write the byte specified by c to the output stream pointed to by fp. 
  ; Returns written byte.
  )

(builtin-function fgets (fp)
  ; Read a line from the steream pointed to by fp and return it.
  ; Do not include newline characters.
  ; Returns nil if EOF.
  )

(builtin-function fread (buf from size fp)
  ; Reads size bytes of data from the stream pointed to by fp, storing them at the location given by byte-array buf offset from.
  ; Returns size;
  )

(builtin-function fwrite (buf from size fp)
  ; Writes size bytes of data to the stream pointed to by fp, obtaining them at the location given by byte-array buf offset from.
  ; Returns size;
  )

(builtin-function fseek (fp)
  ; Sets the file position indicator for the stream pointed to by fp
  ; Returns nil.
  )

(builtin-function ftell (fp)
  ; Returns the current value of the file position indicator for the stream pointed to by fp.
  )

(builtin-function fclose (fp)
  ; Flushes the stream pointed to by fp (writing any buffered output data) and closes the underlying file descriptor.
  ; Returns nil.
  )

(builtin-function stat (filename)
  ; Returns the file status indicated filename.
  ; The return value is an array of length 3.
  ;     0 -- file type and mode.
  ;         1 none
  ;         2 file
  ;         4 dir
  ;         8 other
  ;         16 readable
  ;         32 writable
  ;     1 -- file size
  ;     2 -- modification timestamp
  )

(builtin-function utime (filename unix-time)
  ; Change the access time and modification time of the file indicated filename to the specified unix-time in times.
  ; Returns nil.
  )

(builtin-function getcwd ()
  ; Returns a string containing the absolute filename of the current working directory.
  )

(builtin-function chdir (filename)
  ; Change the current working directory to the directory specified in filename
  ; Returns nil.
  )

(builtin-function readdir (filename)
  ; Return the contents of the directory indicated by filename as a character string delimited by a newline character.
  )

(builtin-function remove (filename)
  ; Attempts to remove a file whose name is pathname.
  ; Returns nil.
  )

(builtin-function mkdir (filename)
  ; Attempts to create a directory whose name is pathname.
  ; It is an error if filename already exists.
  ; Returns nil.
  )

(builtin-function rename (src dst)
  ; Rename the file and move between directories if necessary.
  ; Returns nil.
  )

(builtin-function time ()
  ; Returns the number of seconds relative to the unix epoch (January 1, 1970, 00:00:00 UTC).
  )

(builtin-function clock ()
  ; Returns the approximate processor time[sec] used by the program.
  )

(builtin-function cycle ()
  ; Returns the cycle of the internal virtual machine.
  )

(builtin-function utcoffset ()
  ; Returns the difference in hours and minutes from Coordinated Universal Time (UTC) for time zone set in the host system.
  )

(builtin-function sleep (sec)
  ; Sleep for a specified number of seconds.
  ; Returns nil.
  )

(builtin-function getenv (name)
  ; Looks up the environment variable named name in the environment list and returns value string.
  ; Returns nil if not found.
  )

(builtin-function putenv (name value)
  ; Add environment variables or change values.
  ; If name does not exist in the environment, name-value is added to the environment.
  ; If name exists in the environment, the value of name is changed to value.
  ; Returns nil.
  )

; Paren object system

(builtin-function object? (x)
  ; Returns whether the x is an object.
  (assert (object? '(:class Object))))

(builtin-function is-a? (o cls)
  ; Returns whether the specified object o regarded as the specified class cls's instance.
  (assert (is-a? '(:class Object)
                 '(:class Class :symbol Object :super nil :features nil :fields (class)))))

(builtin-function find-class (cls-sym)
  ; Returns the class corresponding to the specified symbol cls_sym.
  )

(builtin-function find-method (cls-sym method-sym)
  ; Returns the method by which an instance of the class name cls-sym is dispatched.
  )

(function error-if-not-object (o)
  ; Returns the method by which an instance of the class name cls-sym is dispatched.
  (if (! (object? o)) (error "expected object")))

(macro make-accessor (field)
  ; Returns an expression that binds getter and setter.
  ; If field name is 'xxx', bind a getter named `&xxx` and setter named `&xxx<-`.
  ; Works faster than method which defined with the `method` macro.
  (with-gensyms (receiver val)
    (let (key (bytes->keyword field)
              field (bytes->string field)
              getter (bytes-concat '& field)
              setter (bytes-concat '& field '<-))
      (list begin
            (list if (list ! (list 'bound? (list quote getter)))
                  (list 'function getter (list receiver)
                        (list 'error-if-not-object receiver)
                        (list 'assoc receiver key)))
            (list if (list ! (list 'bound? (list quote setter)))
                  (list 'function setter (list receiver val)
                        (list 'error-if-not-object receiver)
                        (list begin (list 'assoc! receiver key val) receiver)))))))

(macro &<- (object :rest pairs)
  (with-gensyms (go)
    (cons let (cons (list go object)
                    (map (lambda (pair)
                           (list (bytes->symbol (bytes-concat '& (car pair) '<-)) go (cadr pair)))
                         (group pairs 2))))))

(macro make-method-dispatcher (method-sym)
  (when (! (bound? method-sym))
    (with-gensyms (receiver args)
      (list 'function method-sym (list receiver :rest args)
            (list 'error-if-not-object receiver)
            (list 'apply
                  (list 'find-method
                        (list 'cadr receiver)    ; <=> (assoc cls :class)
                        (list quote method-sym))
                  (list 'cons receiver args))))))

(macro class (cls-sym (:opt super :rest features) :rest fields)
  ; Returns expression that create class named cls-sym.
  (if (! (all-satisfy? symbol? fields)) (error "fields must be symbol")
      (bound? cls-sym) (error cls-sym " already bound"))
  (list begin0
        (list quote cls-sym)
        (list <- cls-sym (list quote (list :class 'Class
                                           :symbol cls-sym
                                           :super (if (eq? cls-sym 'Object) nil (|| super 'Object))
                                           :features features
                                           :fields fields)))
        (cons begin
              (map (lambda (field) (list 'make-accessor field)) fields))))

(macro method (cls-sym method-sym args :rest body)
  (let (global-sym (bytes-concat cls-sym method-sym))
    (if (! (find-class cls-sym)) (error "unbound class")
        (bound? global-sym) (error global-sym " already bound"))
    (list begin
          (list 'make-method-dispatcher method-sym)
          (cons 'function (cons global-sym (cons (cons 'self args) body))))))

(class Object ()
  ; Object is a class that is the basis of all class hierarchies.
  ; This class provides basic functionality common to all objects.
  ; It is the only class that does not have a superclass.
  class)

(method Object .init ()
  ; Initialize receiver.
  ; Method executed when an object is created by .new method.
  ; Overwrite this method if there is class-specific initialization processing.
  ; If overwrote .init method has argument, must call manually.
  self)

(method Object .class ()
  ; Returns the class of receiver.
  (find-class (&class self)))

(method Object .eq? (o)
  ; Returns whether the o is equals of this object.
  ; Overwrite this method if there is class-specific comparisons.
  (eq? self o))

(method Object .to-s ()
  ; Returns a String representing the receiver.
  (string "<" (&symbol (.class self)) ":" (address self)  ">"))

(class Class ()
  ; Class of class object.
  ; All class objects are instances of Class class.
  symbol super features fields methods)

(method Class .new ()
  ; Construct an instance.
  ; If .init method has argument, must invoke after create an instance.
  ; Otherwise automatically invoke .init method.
  (let (o nil)
    (for (cls self) cls (<- cls (&& (assoc cls :super) (find-class (assoc cls :super))))
      (dolist (field (reverse! (map bytes->keyword (assoc cls :fields))))
        (push! o nil)
        (push! o field)))
    (car! (cdr o) (assoc self :symbol))
    (if (= (length (lambda-parameter (find-method (assoc o :class) '.init))) 1)
        (.init o)
        o)))

(method Class .super ()
  ; Returns the class representing the superclass of the receiver.
  (find-class (&super self)))

(method Class .features ()
  ; Returns the feature list representing the feature of the receiver.
  (map find-class (&features self)))

(method Class .methods ()
  ; Returns method list of this class, but excluding inherited methods.
  (&methods self))

;; exception

(class Exception ()
  ; The Exception class is the superclass of all exceptions.
  ; It is recommended that new exceptions derive from the Error class or one of its subclasses.
  ; Do not derive from Exception.
  message stack-trace)

(method Exception .message (message)
  ; Message accessors.
  (&message<- self message))

(method Exception .to-s ()
  ; Returns a String representing the receiver.
  (let (class-name (bytes->string (&class self)) msg (&message self))
    (if msg (bytes-concat class-name " -- " msg)
        class-name)))

(method Exception .stack-trace ()
  (&stack-trace self))

(method Exception .print-stack-trace ()
  (write-bytes (.to-s self))
  (write-line)
  (dolist (x (.stack-trace self))
    (write-bytes "\tat: ") (write x)))

(class SystemExit (Exception)
  ; Dispatched to shut down the Paren system.
  ; In principle, this exception is not caught.
  )

(class Interrrupt (Exception)
  ; Dispatched when the user presses the interrupt key (usually Ctrl-c).
  )

(class Error (Exception)
  )

(function error (:rest args)
  (throw (.message (.new Error) (apply string args))))

(class Array ()
  size elt)

(method Array .init ()
  (&size<- self 0)
  (&elt<- self (array 4)))

(method Array .size ()
  (&size self))

(method Array .at (i)
  ([] (&elt self) i))

(method Array .put (i val)
  ([]<- (&elt self) i val))

(method Array .reserve (size)
  (let (req (+ (&size self) size) elt-size (array-length (&elt self)))
    (when (< elt-size req)
      (while (< (<- elt-size (* elt-size 2)) req))
      (let (elt (array elt-size))
        (array-copy (&elt self) 0 elt 0 (&size self))
        (&elt<- self elt)))
    self))

(method Array .add (val)
  (let (i (&size self))
    (.reserve self 1)
    (&size<- self (++ i))
    ([]<- (&elt self) i val))
  self)

(method Array .to-a ()
  (let (size (&size self) a (array size))
    (array-copy (&elt self) 0 a 0 size)
    a))

(class Comparable ()
  ; A feature that provides comparison operators.
  )

(method Comparable .lt? (:rest args)
  ; Returns whether the each of the specified args are in monotonically decreasing order.
  (assert nil))

(method Comparable .gt? (:rest args)
  ; Returns whether the each of the specified args are in monotonically increasing order.
  (all-adjacent-satisfy? (lambda (x y) (.lt? y x)) (cons self args)))

(method Comparable .le? (:rest args)
  ; Returns whether the each of the specified args are in monotonically nondecreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (.lt? y x))) (cons self args)))

(method Comparable .ge? (:rest args)
  ; Returns whether the each of the specified args are in monotonically nonincreasing order.
  (all-adjacent-satisfy? (lambda (x y) (! (.lt? x y))) (cons self args)))

(class Path ()
  ; A class that handles a file path.
  ; Construct with Path.of function.
  ; It should not be construct by new.
  ; The corresponding file does not have to exist.
  ; You can read and write files to the corresponding path as needed.
  path)

(<- Path.separator "/")

(function Path.of (path-name)
  ; Constructs and returns the path object corresponding to path-name.
  ; Internally it just holds the string path-name as a list of filenames.
  ;     (Path.of "foo/bar/buzz") -- ("foo" "bar" "buzz")
  ;     (Path.of "/etc") -- ("/" "etc")
  ; The first `~` expands to the home directory using the environment variable.
  ; Any `~` other than the beginning is ignored.
  ;     (Path.of "~/.vimrc") <=> ("home" "foo" ".vimrc")
  ;     (Path.of "~/~.vimrc") <=> ("home" "foo" ".vimrc")
  ; All characters `\` in path-name are replaced with `/` for processing.
  ;     (Path.of "C:\\foo") <=> ("C:" "foo")
  ; `.` and `..` included in path-name are not treated specially.
  ; Path class places the highest priority on keeping the implementation simple, and assumes that these features are implemente where necessary.
  ;     (Path.of "foo/bar/../buzz") <=> ("foo" "bar" ".." "buzz")
  ; Two or more consecutive `/`s or trailing `/`s are ignored.
  ;     (Path.of "foo//bar/") <=> ("foo" "bar")
  (if (is-a? path-name Path) path-name
      (let (c nil path nil first-letter (string-at path-name 0) root? nil)
        (if (string= first-letter "~")
            (<- path-name (bytes-concat
                            (if (eq? $host-name :windows)
                                (bytes-concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))
                                (getenv "HOME"))
                            Path.separator path-name))
            (string= first-letter Path.separator)
            (<- root? true))
        (<- path (remove-if (lambda (file-name)
                              (|| (string= file-name "") (string= file-name "~")))
                            (string->list
                              (with-memory-stream (out)
                                (with-memory-stream (in path-name)
                                  (while (<- c (read-char in))
                                    (if (string= c "\\") (write-bytes Path.separator out)
                                        (write-bytes c out)))))
                              Path.separator)))
        (if root? (<- path (cons Path.separator path)))
        (&path<- (.new Path) path))))

(function Path.getcwd ()
  ; Returns the path corresponding to the current directory.
  (Path.of (getcwd)))

(method Path .name ()
  ; Returns file name.
  (last (&path self)))

(method Path .base-name ()
  ; Returns base name (the string up to the first dot).
  ; If not including dot, returns the entire name.
  (let (name (.name self) i (string-index name "."))
    (if i (string-slice name 0 i)
        name)))

(method Path .suffix ()
  ; Returns the suffix (the string after the last dot).
  ; If not including dot, returns nil.
  (let (name (.name self) i (string-last-index name "."))
    (if i (string-slice name (++ i)))))

(method Path .but-suffix ()
  ; Returns the name without the suffix.
  (let (name (.name self) i (string-last-index name "."))
    (if i (string-slice name 0 i)
        name)))

(method Path .root? ()
  ; Returns whether this object is a root directory.
  (&& (.absolute? self) (nil? (.parent self))))

(method Path .parent ()
  ; Returns the parent path
  ; If this object is root directory, returns nil.
  ; However, this object is relative path, non-root directory may return nil.
  (let (path (butlast (&path self)))
    (if path (&path<- (.new Path) path))))

(method Path .resolve (path)
  ; Resolve the given path against this path.
  ; If the argument is a character string, convert it to a path object before processing.
  ; If the path parameter is an absolute path then this method trivially returns path
  ; Otherwise this method concatenate this path and the speciifed path.
  ; `.` and `..` included in path-name are not treated specially.
  (if (string? path) (<- path (Path.of path)))
  (if (.absolute? path) path
      (Path.of (bytes-concat (.to-s self) Path.separator (.to-s path)))))

(method Path .absolute? ()
  ; Returns whether this path regarded as the absolute path.
  (let (first-file (car (&path self)))
    (if (eq? $host-name :windows)
        (&& (= (bytes-length first-file) 2)
            (bytes-index first-file ":" 1 2))
        (string= first-file Path.separator))))

(method Path .relative? ()
  ; Same as (! (.absolute? self))
  (! (.absolute? self)))

(method Path .to-l ()
  (with-open (in self :read)
    (return (.read-lines in))))

(method Path .to-s ()
  (reduce (lambda (acc rest)
            (bytes-concat (if (string= acc Path.separator) "" acc) Path.separator rest))
          (&path self)))

(method Path .open (mode)
  ; Returns a stream that reads the contents of the receiver.
  (.init (.new FileStream) (fopen (.to-s self)
                                  (switch mode
                                    :read 0
                                    :write 1
                                    :append 2
                                    :update 3))))

(method Path .remove ()
  ; Deletes the file corresponding to this object.
  ; Returns this object.
  (remove (.to-s self))
  self)

(method Path .stat ()
  ; Returns stat of this object.
  (let (stat-array (stat (.to-s self)))
    (if stat-array stat-array
        (begin (<- stat-array (array 3))
               ([]<- stat-array 0 1)
               ([]<- stat-array 1 0)
               stat-array))))

(method Path .mode ()
  ; Returns whether this object is a regular file.
  ([] (.stat self) 0))

(method Path .none? ()
  ; Returns whether this object is not exits.
  (/= (& (.mode self) 1) 0))

(method Path .file? ()
  ; Returns whether this object is a regular file.
  (/= (& (.mode self) 2) 0))

(method Path .dir? ()
  ; Returns whether this object is a directory.
  (/= (& (.mode self) 4) 0))

(method Path .other? ()
  ; Returns whether this object is neither a regular file nor a directory.
  (/= (& (.mode self) 8) 0))

(method Path .readable? ()
  ; Returns whether this object is readable.
  (/= (& (.mode self) 16) 0))

(method Path .writable? ()
  ; Returns whether this object is writable.
  (/= (& (.mode self) 32) 0))

(method Path .size ()
  ; Returns the size of this object.
  ([] (.stat self) 1))

(method Path .mtime ()
  ; Returns the last update time of this object.
  ([] (.stat self) 2))

(method Path .utime (time)
  ; Update the last update time of this object.
  ; Returns this object.
  (utime (.to-s self) time))

(method Path .children ()
  (map (lambda (child)
         (.resolve self child))
       (string->list (readdir (.to-s self)) "\n")))

;; stream I/O

(class Stream ()
  ; Abstract class for reading and writing streams.
  )

(method Stream .read-byte (:rest args)
  ; Read 1byte from stream.
  ; Returns -1 when the stream reaches the end.
  ; Must be implemented in the inherited class.
  (assert nil))

(method Stream .illegal-character (:rest seq)
  (error "illegal byte sequence -- " seq))

(method Stream .trail? (b)
  (let (encoding (dynamic $external-encoding))
    (switch encoding
      :UTF-8 (= (& b 0xc0) 0x80)
      :SJIS (|| (<= 0x81 b 0x9f) (<= 0xe0 b 0xfc)))))

(method Stream .read-char ()
  ; Read 1 character from stream.
  ; Returns nil when the stream reaches the end.
  (let (encoding (dynamic $external-encoding) b1 (.read-byte self) b2 nil b3 nil b4 nil size 0)
    (switch encoding
      :UTF-8 (if (< b1 0) (return nil)
                 (< b1 0x80) (<- size 1)
                 (< b1 0xc2) (.illegal-character self b1)
                 (< b1 0xe0) (if (|| (= (& b1 0x3e) 0)
                                     (! (.trail? self (<- b2 (.read-byte self)))))
                                 (.illegal-character self b1 b2)
                                 (<- size 2))
                 (< b1 0xf0) (if (|| (! (.trail? self (<- b2 (.read-byte self))))
                                     (&& (= b1 0xe0)
                                         (= (& b2 0x20) 0))
                                     (! (.trail? self (<- b3 (.read-byte self)))))
                                 (.illegal-character self b1 b2 b3)
                                 (<- size 3))
                 (< b1 0xf8) (if (|| (! (.trail? self (<- b2 (.read-byte self))))
                                     (&& (= b1 0xf0)
                                         (= (& b2 0x30) 0))
                                     (! (.trail? self (<- b3 (.read-byte self))))
                                     (! (.trail? self (<- b4 (.read-byte self)))))
                                 (.illegal-character self b1 b2 b3 b4)
                                 (<- size 4))
                 (.illegal-character self b1))
      :SJIS (if (< b1 0) (return nil)
                (< b1 0x80) (<- size 1)
                (< 0x80 b1 0xa0) (<- b2 (.read-byte self) size 2)
                (< 0xa0 b1 0xe0) (<- size 1)
                (< b1 0xfd) (<- b2 (.read-byte self) size 2)
                (.illegal-character self b1)))
    (let (c (bytes size))
      (if (= size 1) ([]<- c 0 b1)
          (= size 2) (begin ([]<- c 0 b1) ([]<- c 1 b2))
          (= size 3) (begin ([]<- c 0 b1) ([]<- c 1 b2) ([]<- c 2 b3))
          (= size 4) (begin ([]<- c 0 b1) ([]<- c 1 b2) ([]<- c 2 b3) ([]<- c 3 b4)))
      (bytes->string! c))))

(method Stream .read ()
  ; Read expression from the specified stream.
  ; Returns nil if eof reached.
  (.read (.init (.new ParenReader) self)))

(method Stream .read-line ()
  ; Input one line from this object.
  ; Returns read line.
  ; If stream reached eof, returns nil.
  (let (c nil)
    (with-memory-stream (out)
      (while true
        (if (= (<- c (.read-byte self)) -1) (return nil)
            (= c 0x0a) (break)
            (.write-byte out c))))))

(method Stream .read-lines ()
  ; Return the rest of the stream as a list whose elements are rows.
  (let (line nil lines nil)
    (while (<- line (.read-line self))
      (push! lines line))
    (reverse! lines)))

(method Stream .write-line (:opt bytes)
  (if bytes (.write-bytes self bytes))
  (.write-byte self 0x0a))

(method Stream .write-lines (lines)
  (dolist (line lines)
    (.write-line self line)))

(method Stream .write-int (n :key radix padding)
  ; Write integer to stream.
  (let (write1 (lambda (n depth)
                 (if (< n 0) (begin
                               (.write-byte self 0x2d)
                               (write1 (- n) (++ depth)))
                     (let (upper (// n radix))
                       (if (/= upper 0) (write1 upper (++ depth))
                           (dotimes (i (- padding depth 1))
                             (.write-byte self 0x30)))
                       (.write-byte self (digit->byte (mod n radix)))))))
    (<- radix (|| radix 10)
        padding (|| padding 0))
    (write1 n 0)))

(method Stream .write-number (n)
  (if (int? n) (.write-int self n)
      (= n 0.0) (.write-byte self 0x30)
      (let (mant n exp 8)
        (let (write-mant1
               (lambda ()
                 (let (upper (// (number->int mant) 100000000))
                   (.write-int self upper)
                   (<- mant (* (- mant (* upper 100000000)) 10))))
               write-fraction
               (lambda (n)
                 (write-mant1)
                 (dotimes (i (-- n))
                   (if (= mant 0) (break)
                       (write-mant1)))))
          (when (< mant 0)
            (.write-byte self 0x2d)
            (<- mant (- mant)))
          (while (>= mant 1000000000)
            (<- mant (/ mant 10) exp (++ exp)))
          (while (< mant 100000000)
            (<- mant (* mant 10) exp (-- exp)))
          (if (<= 0 exp 6)
              (begin
                (dotimes (i (++ exp))
                  (write-mant1))
                (.write-byte self 0x2e)
                (write-fraction (- 16 exp 1)))
              (<= -3 exp -1)
              (begin
                (.write-bytes self "0.")
                (dotimes (i (- (- exp) 1))
                  (.write-byte self 0x30))
                (write-fraction 16))
              (begin
                (write-mant1)
                (.write-byte self 0x2e)
                (write-fraction 15)
                (.write-byte self 0x65)
                (.write-int self exp)))))))

(method Stream .write (x :key start end)
  ; Write the specified x as a readable format.
  ; Returns x.
  (if start (.write-bytes self start))
  (if (cons? x)
      (let (ope (car x))
        (if (&& (eq? ope 'quote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x27) (.write self (cadr x) :end ""))
            (&& (eq? ope 'quasiquote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x60) (.write self (cadr x) :end ""))
            (&& (eq? ope 'unquote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x2c) (.write self (cadr x) :end ""))
            (&& (eq? ope 'unquote-splicing) (nil? (cddr x)))
            (begin
              (.write-bytes self ",@") (.write self (cadr x) :end ""))
            (begin
              (.write-byte self 0x28)
              (.write self (car x) :end "")
              (while (<- x (cdr x))
                (.write self (car x) :start " " :end ""))
              (.write-byte self 0x29))))
      (builtin? x)
      (.write-bytes self (builtin-name x))
      (string? x)
      (begin
        (.write-byte self 0x22)
        (.write-bytes self x)
        (.write-byte self 0x22))
      (symbol? x)
      (.write-bytes self x)
      (keyword? x)
      (begin
        (.write-byte self 0x3a)
        (.write-bytes self x))
      (number? x)
      (.write-number self x)
      (bytes? x)
      (begin
        (.write-bytes self "#b[")
        (dotimes (i (bytes-length x))
          (if (/= i 0) (.write-byte self 0x20))
          (.write-bytes self "0x")
          (.write-int self ([] x i) :radix 16 :padding 2))
        (.write-byte self 0x5d))
      (array? x)
      (begin
        (.write-bytes self "#a[")
        (dotimes (i (array-length x))
          (.write self ([] x i) :start (&& (/= i 0) " ") :end ""))
        (.write-byte self 0x5d))
      (|| (macro? x)
          (function? x))
      (begin
        (if (macro? x) (.write-bytes self "(macro")
            (.write-bytes self "(lambda"))
        (.write-byte self 0x20)
        (.write self (lambda-parameter x) :end "")
        (dolist (body (lambda-body x))
          (.write self body :start " " :end ""))
        (.write-byte self 0x29))
      (assert nil))
  (.write-bytes self (|| end "\n"))
  x)

(class MemoryStream (Stream)
  ; A stream whose contents are held in memory.
  buf rdpos wrpos)

(method MemoryStream .init ()
  (&buf<- self (bytes 64))
  (&rdpos<- self 0)
  (&wrpos<- self 0))

(method MemoryStream .size ()
  ; Returns the number of bytes written to the stream.
  (&wrpos self))

(method MemoryStream .reserve (size)
  (let (req (+ (&wrpos self) size) buf-size (bytes-length (&buf self)))
    (when (< buf-size req)
      (while (< (<- buf-size (* buf-size 2)) req))
      (let (buf (bytes buf-size))
        (bytes-copy (&buf self) 0 buf 0 (&wrpos self))
        (&buf<- self buf)))
    self))

(method MemoryStream .read-byte ()
  (let (rdpos (&rdpos self))
    (if (= rdpos (&wrpos self)) -1
        (begin0 ([] (&buf self) rdpos)
                (&rdpos<- self (++ rdpos))))))

(method MemoryStream .read-bytes (buf from size)
  (let (rest (- (&wrpos self) (&rdpos self)))
    (if (< rest size) (<- size rest))
    (bytes-copy (&buf self) (&rdpos self) buf (&wrpos self) size)
    size))

(method MemoryStream .write-byte (byte)
  (let (wrpos (&wrpos self))
    (.reserve self 1)
    ([]<- (&buf self) wrpos byte)
    (&wrpos<- self (++ wrpos))))

(method MemoryStream .write-bytes (bytes :opt from size)
  (.reserve self (|| size (<- size (bytes-length bytes))))
  (bytes-copy bytes (|| from 0) (&buf self) (&wrpos self) size)
  (&wrpos<- self (+ (&wrpos self) size))
  size)

(method MemoryStream .seek (offset)
  (if (! (<= 0 offset (&wrpos self))) (error "index outof bound"))
  (&rdpos<- self offset))

(method MemoryStream .tell ()
  (&rdpos self))

(method MemoryStream .to-s ()
  ; Returns the contents written to the stream as a string.
  (let (size (&wrpos self))
    (if (= size 0) ""
        (bytes->string (&buf self) 0 size))))

(method MemoryStream .reset ()
  ; Empty the contents of the stream.
  (&rdpos<- self 0)
  (&wrpos<- self 0))

(class FileStream (Stream)
  ; Provides I/O functions for files.
  ; Construct with methods such as File.open-read, File.open-write.
  ; It should not be construct by new.
  fp)

(method FileStream .init (fp)
  (&fp<- self fp))

(method FileStream .read-byte ()
  (fgetc (&fp self)))

(method FileStream .read-bytes (buf from size)
  (fread buf from size (&fp self)))

(method FileStream .read-line ()
  (fgets (&fp self)))

(method FileStream .write-byte (byte)
  (fputc byte (&fp self)))

(method FileStream .write-bytes (x :opt from size)
  (fwrite x (|| from 0) (|| size (bytes-length x)) (&fp self)))

(method FileStream .seek (offset)
  (fseek (&fp self) offset))

(method FileStream .tell ()
  (ftell (&fp self)))

(method FileStream .close ()
  (fclose (&fp self)))

(class AheadReader ()
  ; A one-character look-ahead reader.
  ; While prefetching one character at a time from a character string or Reader, if necessary, cut out a part as a token.
  ; Can be used as a syllable reader or lexical analyzer.
  stream next token)

(method AheadReader .init (stream)
  (if (string? stream) (let (s stream) (.write-bytes (<- stream (.new MemoryStream)) s))
      (nil? stream) (<- stream (dynamic $stdin)))
  (&<- self
       :stream stream
       :next (.read-char stream)
       :token (.new MemoryStream)))

(method AheadReader .next ()
  ; Returns a pre-read character.
  (&next self))

(method AheadReader .alpha? ()
  ; Returns true if next character is alphabetic.
  (byte-alpha? (string->code (&next self))))

(method AheadReader .digit? ()
  ; Returns true if next character is digit.
  (byte-digit? (string->code (&next self))))

(method AheadReader .numeric-alpha? ()
  ; Returns true if next character is digit or alphabetic.
  (let (b (string->code (&next self)))
    (|| (byte-digit? b)
        (byte-alpha? b))))

(method AheadReader .skip (:opt expected)
  ; Skip next character and returns it.
  ; Error if expected is specified and the next character is not the same as the expected.
  (let (next (&next self))
    (if (nil? next) (error "unexpected EOF")
        (&& expected (string/= next expected)) (error "unexpected token")
        (&next<- self (.read-char (&stream self))))
    next))

(method AheadReader .skip-escape ()
  (let (c (.skip self))
    (if (string/= c "\\") c
        (string= (<- c (.skip self)) "a") 0x07
        (string= c "b") 0x08
        (string= c "c") (if (<= 0x40 (<- c (byte-upper (string->code (.skip self)))) 0x5f)
                            (& c 0x1f)
                            (error "illegal ctrl char"))
        (string= c "e") 0x1b
        (string= c "f") 0x0c
        (string= c "n") 0x0a
        (string= c "r") 0x0d
        (string= c "t") 0x09
        (string= c "v") 0x0b
        (string= c "x") (+ (* 16 (.skip-digit self 16)) (.skip-digit self 16))
        c)))

(method AheadReader .skip-line ()
  (while (&next self)
    (if (string= (.skip self) "\n") (break)))
  self)

(method AheadReader .skip-space ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (byte-space? (string->code (&next self)))
    (.skip self))
  self)

(method AheadReader .skip-sign ()
  (let (next (&next self))
    (if (string= next "+") (begin (.skip self) nil)
        (string= next "-") (begin (.skip self) true)
        nil)))

(method AheadReader .skip-digit (:opt radix)
  (byte->digit (string->code (.skip self)) (|| radix 10)))

(method AheadReader .skip-uint ()
  (if (! (.digit? self)) (error "missing digits")
      (let (val 0)
        (while (.digit? self)
          (<- val (+ (* val 10) (.skip-digit self))))
        val)))

(method AheadReader .skip-int ()
  (let (minus? (.skip-sign self) val (.skip-uint self))
    (if minus? (- val)
        val)))

(method AheadReader .skip-unumber ()
  (let (val (.skip-uint self))
    (if (string= (&next self) "x")
        (let (radix (if (= val 0) 16 val))
          (<- val 0)
          (.skip self)
          (if (! (.numeric-alpha? self)) (error "missing lower or digits")
              (while (.numeric-alpha? self)
                (<- val (+ (* val radix) (.skip-digit self 16))))))
        (string= (&next self) ".")
        (let (factor 0.1)
          (.skip self)
          (while (.digit? self)
            (<- val (+ val (* factor (.skip-digit self 16)))
                factor (/ factor 10)))
          (when (= (&next self) 0x65)
            (.skip self)
            (<- val (* val (exp 10 (.skip-int self)))))))
    val))

(method AheadReader .skip-number ()
  (let (minus? (.skip-sign (.skip-space self)) val (.skip-unumber self))
    (if minus? (- val)
        val)))

(method AheadReader .get ()
  ; Append next character to token and returns it.
  (let (c (.skip self))
    (.put self c)
    c))

(method AheadReader .get-escape ()
  (let (c (.skip-escape self))
    (.put self c)
    c))

(method AheadReader .put (o)
  ; Put the o to the end of the token regardless of the stream.
  (if (byte? o) (.write-byte (&token self) o)
      (begin0 o
              (.write-bytes (&token self) o))))

(method AheadReader .token ()
  ; Returns the token string currently cut out.
  (.to-s (&token self)))

(method AheadReader .reset ()
  ; Reset token and returns self.
  (.reset (&token self))
  self)

(method AheadReader .stream ()
  ; Returns the stream held by this object.
  (&stream self))

; Paren reader

(class ParenLexer (AheadReader))

(method ParenLexer .identifier-symbol-alpha? ()
  (|| (bytes-index "!#$%&*./<=>?^[]_{|}" (.next self))
      (.alpha? self)))

(method ParenLexer .identifier-sign? ()
  (bytes-index "+-" (.next self)))

(method ParenLexer .identifier-trail? ()
  (|| (.identifier-symbol-alpha? self)
      (.identifier-sign? self)
      (.digit? self)))

(method ParenLexer .get-identifier-sign ()
  (when (|| (.identifier-sign? self) (.identifier-symbol-alpha? self))
    (while (.identifier-trail? self) (.get self)))
  self)

(method ParenLexer .get-identifier ()
  (if (.identifier-sign? self)
      (begin (.get self)
             (.get-identifier-sign self))
      (.identifier-symbol-alpha? self)
      (begin (while (.identifier-trail? self)
               (.get self))
             self)
      (error "illegal identifier '" (.token self) "'")))

(method ParenLexer .lex-sign ()
  (let (sign (.get self))
    (if (.digit? self)
        (let (val (.skip-number self))
          (if (string= sign "-") (- val) val))
        (bytes->symbol (.token (.get-identifier-sign self))))))

(method ParenLexer .lex-symbol ()
  (bytes->symbol (.token (.get-identifier self))))

(method ParenLexer .lex-keyword ()
  (.skip self)
  (bytes->keyword (.token (.get-identifier self))))

(method ParenLexer .lex-string ()
  (.skip self)
  (while (string/= (&next self) "\"")
    (.get-escape self))
  (.skip self "\"")
  (.token self))

(method ParenLexer .lex ()
  (.skip-space (.reset self))
  (let (next (&next self))
    (if (nil? next) '(:EOF)
        (string= next "(") (begin (.skip self) '(:open-paren))
        (string= next ")") (begin (.skip self) '(:close-paren))
        (string= next "'") (begin (.skip self) '(:quote))
        (string= next "`") (begin (.skip self) '(:backquote))
        (string= next ",") (begin (.skip self)
                                  (if (string= (&next self) "@") (begin (.skip self)
                                                                        '(:unquote-splicing))
                                      '(:unquote)))
        (string= next "\"") (list :atom (.lex-string self))
        (string= next ":") (list :atom (.lex-keyword self))
        (string= next ";") (.lex (.skip-line self))
        (string= next "#") (begin (.skip self) (list :read-macro (bytes->symbol (.next self))))
        (|| (string= next "+")
            (string= next "-")) (list :atom (.lex-sign self))
        (.digit? self) (list :atom (.skip-number self))
        (list :atom (.lex-symbol self)))))

(class ParenReader ()
  lexer token token-type)

(method ParenReader .init (stream)
  (&lexer<- self (.init (.new ParenLexer) stream)))

(method ParenReader .scan ()
  (let (x (.lex (&lexer self)))
    (&token-type<- self (car x))
    (&token<- self (cadr x))))

(method ParenReader .parse-list ()
  (.scan self)
  (if (eq? (&token-type self) :close-paren) nil
      (eq? (&token-type self) :EOF) (error "missing close-paren")
      (cons (.parse self) (.parse-list self))))

(method ParenReader .parse ()
  (switch (&token-type self)
    :EOF nil
    :atom (&token self)
    :open-paren (.parse-list self)
    :quote (list quote (.parse (.scan self)))
    :backquote (list 'quasiquote (.parse (.scan self)))
    :unquote (list 'unquote (.parse (.scan self)))
    :unquote-splicing (list 'unquote-splicing (.parse (.scan self)))
    :read-macro ((assoc $read-table (string->code (&token self))) self)
    :default (error "syntax error")))

(macro unquote (expr)
  (list 'error "unexpected unquote -- ," expr))

(macro unquote-splicing (expr)
  (list 'error "unexpected unquote-splicing -- ,@" expr))

(macro quasiquote (expr)
  (let (descend (lambda (x level)
                  (if (atom? x) (list quote x)
                      (switch (car x)
                        quasiquote (list cons
                                         '(quote quasiquote)
                                         (descend (cdr x) (++ level)))
                        unquote (if (= level 0) (cadr x)
                                    (list cons
                                          '(quote unquote)
                                          (descend (cdr x) (-- level))))
                        unquote-splicing (if (= level 0) (cadr x)
                                             (list cons
                                                   '(quote unquote-splicing)
                                                   (descend (cdr x) (-- level))))
                        :default (list append
                                       (descend-car (car x) level)
                                       (descend (cdr x) level)))))
                descend-car (lambda (x level)
                              (if (atom? x) (list quote (list x))
                                  (switch (car x)
                                    quasiquote (list list (list cons
                                                                '(quote quasiquote)
                                                                (descend (cdr x) (++ level))))
                                    unquote (if (= level 0) (cons list (cdr x))
                                                (list list (list cons
                                                                 '(quote unquote)
                                                                 (descend (cdr x) (-- level)))))
                                    unquote-splicing (if (= level 0) (cons append (cdr x))
                                                         (list list (list cons
                                                                          '(quote unquote-splicing)
                                                                          (descend (cdr x) (-- level)))))
                                    :default (list list (list append
                                                              (descend-car (car x) level)
                                                              (descend (cdr x) level)))))))
    (descend expr 0)))

(method ParenReader .read ()
  (.parse (.scan self)))

(macro reader-macro (next params :rest body)
  ; Define a reader macro starting with `# + next`.
  ; next must be a single character string.
  ; When the reserved character string is read, the processing moves to the specified function f and the evaluation result is expanded.
  ; Returns nil.
  (with-gensyms (f)
    (list let (list f (cons lambda (cons params body)))
          (list 'push! '$read-table f)
          (list 'push! '$read-table (list 'string->code (list quote next))))))

(function read-byte (:opt stream)
  ; Read 1byte from the specified stream.
  ; Returns -1 when the stream reaches the end.
  (.read-byte (|| stream (dynamic $stdin))))

(function read-char (:opt stream)
  ; Read 1character from the specified stream.
  (.read-char (|| stream (dynamic $stdin))))

(function read-line (:opt stream)
  ; Read line from the specified stream.
  (.read-line (|| stream (dynamic $stdin))))

(function read-lines (:opt stream)
  ; Return the rest of the stream as a list whose elements are rows.
  (.read-lines (|| stream (dynamic $stdin))))

(function read (:opt stream)
  (.read (|| stream (dynamic $stdin))))

(function write-byte (byte :opt stream)
  ; Write 1byte to the specified stream.
  (.write-byte (|| stream (dynamic $stdout)) byte))

(function write-bytes (bytes :opt stream)
  ; Write the specified stirng bytes to the specified stream.
  (.write-bytes (|| stream (dynamic $stdout)) bytes))

(function write-line (:opt bytes stream)
  (.write-line (|| stream (dynamic $stdout)) bytes))

(function write-lines (lines :opt stream)
  (.write-lines (|| stream (dynamic $stdout)) lines))

(function write (x :opt stream :key start end)
  ; Write the specified x as a readable format.
  (.write (|| stream (dynamic $stdout)) x :start start :end end))

(macro with-memory-stream ((ms :opt s) :rest body)
  ; Create memory stream context.
  ; If the string s is specified, construct an input stream with s as the source.
  ; Returns nil.
  ; Otherwise act as an output stream.
  ; Returns the string written to the output stream.
  ;     (with-memory-stream (ms s)
  ;        expr1 expr2 ...)
  ;     (let (ms (.new MemoryStream))
  ;        (if s (.write-bytes ms s))
  ;        expr1 expr2 ...
  ;        (if s (.to-s ms)))
  (with-gensyms (g)
    (list let (list ms '(.new MemoryStream) g s)
          (list if g (list '.write-bytes ms g))
          (cons begin body)
          (list if (list ! g) (list '.to-s ms)))))

(macro with-open ((sym path mode) :rest body)
  (with-gensyms (gsym)
    (list let (list gsym nil)
          (list unwind-protect
                (cons let (cons (list sym (list <- gsym (list '.open (list 'Path.of path) mode)))
                                body))
                (list if gsym (list '.close gsym))))))

; execution

(builtin-function eval (expr)
  ; Evaluates the specified expression and returns a value.
  (assert (nil? (eval 'nil))))

(builtin-function apply (f args)
  ; Evaluates the specified expression and returns a value.
  ; Applies the function to the args.
  (assert (= (apply car '((1))) 1)))

(function repl ()
  ; Enter repl(read eval print loop) mode.
  ; Executed when there is no command line argument when paren starts.
  (let (expr nil)
    (catch (SystemExit (lambda (e) (return true)))
      (while true
        (catch (Error (lambda (e) (.print-stack-trace e)))
          (write-bytes ") ")
          (if (<- expr (read)) (write (eval (expand-macro-all expr)))
              (break)))))))

(function quit ()
  ; Quit the system.
  (throw (.new SystemExit)))

(function load (path)
  ; Load the specified file.
  ; Returns true if successfully loaded.
  (with-open (in path :read)
    (let (expr nil)
      (while (<- expr (read in))
        (eval expr))))
  true)

(function import (key)
  ; Load the file corresponding to the specified keyword.
  ; Search the $paren-home directory.
  ; Bind main to nil after processing.
  ; Returns true if successfully loaded.
  (if (find-if (lambda (x) (eq? x key)) $import) true
      (let (p (Path.of (string (bytes->symbol key) ".p")))
        (if (|| (.readable? p) (.readable? (<- p (.resolve $paren-home p))))
            (begin0 (load p)
                    (<- main nil)
                    (push! $import key))
            (error "unreadable module " key)))))

(function boot (args)
  ; Executed when paren is executed.
  ; Invoke repl if there are no command line arguments that bound to the symbol $args.
  ; If command line arguments are specified, read the first argument as the script file name and execute main.
  (if (nil? args) (repl)
      (let (script (Path.of (car args)))
        (if (&& (! (.readable? script))
                (! (.readable? (<- script (.resolve $paren-home script)))))
            (error "unreadable file " (car args))
            (&& (load script) (bound? 'main)) (main args)))))

(<- $import '(:core)
    $read-table nil
    $stdin (.init (.new FileStream) (fp 0))
    $stdout (.init (.new FileStream) (fp 1))
    $external-encoding (if (eq? $host-name :windows) :SJIS :UTF-8)
    $paren-home (.parent (.resolve (Path.getcwd) core.p)))

(reader-macro a (reader)
  ; Define an array literal.
  ; Array elements are not evaluated.
  (let (lexer (&lexer reader) a (.new Array) expr nil)
    (.skip lexer)
    (.skip lexer "[")
    (while (string/= (.next lexer) "]") (.get lexer))
    (.skip lexer)
    (with-memory-stream (in (.token lexer))
      (while (<- expr (read in))
        (.add a expr)))
    (.to-a a)))

(reader-macro b (reader)
  ; Define an bytes literal.
  (let (lexer (&lexer reader) expr nil)
    (.skip lexer)
    (.skip lexer "[")
    (while (string/= (.next lexer) "]") (.get lexer))
    (.skip lexer)
    (->bytes
      (with-memory-stream (out)
        (with-memory-stream (in (.token lexer))
          (let (expr nil)
            (while (<- expr (read in))
              (.write-byte out expr))))))))

(reader-macro m (reader)
  ; Define expand-macro-all reader.
  (let (lexer (&lexer reader))
    (.skip lexer)
    (list 'write (list 'expand-macro-all (list quote (.read reader))))))

(boot $args)
