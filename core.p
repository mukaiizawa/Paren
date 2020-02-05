; Paren core library.

; special operator

(macro special-operator (name :rest syntax)
  ; A special operator is a operator with special syntax, special evaluation rules, or both, possibly manipulating the evaluation environment, control flow, or both.
  name)

(special-operator let
  ; Special operator let create new environment and bind symbol then execute a series of expression that use these bindings.
  ; First evaluates the expression init-expr1, then binds the symbol sym1 to that value,  then it evaluates init-expr2 and binds sym2, and so on.
  ; The expressions are then evaluated in order.
  ; The values of all but the last are discarded.
  (let (sym1 init-expr1
        sym2 init-expr2
        ...)
    expr1
    expr2
    ...))

(special-operator <-
  ; Special operator '<-' is the simple symbol binding statement of Paren.
  ; First expr1 is evaluated and the bind sym1 with result, and so on.
  ; This special operator may be used for lexical and dynamic binding.
  (<- sym1 expr1
      sym2 expr2
      ...))

(special-operator begin
  ; Special operator progn evaluates expressions, in the order in which they are given.
  ; The values of each form but the last are discarded.
  (begin expr1
         expr2
         ...))

(special-operator quote
  ; Special operator quote returns just expr.
  (quote expr))

(special-operator if
  ; Special operator if allows the execution of exprs to be dependent on test.
  ; Test-expressions are evaluated one at a time in the order in which they are given in the expression list until a test-expr is found that evaluates to true.
  ; Once one test-expr has yielded true, no additional test-exprs are evaluated.
  ; If no test-expr yields true, nil is returned.
  ; The last even-numbered expression is optional, in which case the evaluation result of odd-numberd is returned.
  (if test1 then1
      test2 then2
      ...))

(special-operator lambda
  ; Special operator lambda creates an  anonymous function.
  ; Parameters include required parameters, optional parameters, keyword parameters and rest parameters.
  ; Required parameters are a parameter that results in an error if not specified when calling the function.
  ; Optional parameters are parameters that need not be specified when calling the function.
  ; Keyword parameters are specified with names without regard to order when calling the function.
  ; Rest parameters implement variable length arguments.
  ;     <lambda_parameter> ::= [<required_params>]
  ;                            [:opt <xparams>]
  ;                            { [:rest <param>] | [:key <xparams>] }
  ;     <required_params> ::= <param> <param> ...
  ;     <xparams> ::= <xparam> <xparam> ...
  ;     <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
  (lambda <lambda_parameter>
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
  ;     <macro_parameter> ::= [<macro_parameter> || <required_params>]
  ;                           [:opt <xparams>]
  ;                           { [:rest <param>] | [:key <xparams>] }
  ;     <required_params> ::= <param> <param> ...
  ;     <xparams> ::= <xparam> <xparam> ...
  ;     <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
  (macro name <macro_parameter>
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
  (catch (Error1 handler1
          Error2 handler2
          ...)
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

(macro global-symbol (s v)
  ; In Paren, explicitly binding symbols to the global environment is rare and bad practice.
  ; It is the programmer's responsibility to call it in the global environment because it is only macro-expanded into a special operator '<-'.
  ; By convention, the binding symbol name starts with '$'.
  ; Macro expansion image is as follows.
  ;     (global-symbol s v)
  ;     (<- s v)
  (list <- s v))

(macro function (name args :rest body)
  ; Create a lambda function which parameter list the specified args and lambda function body the specified body.
  ; Then, bind a created lambda function with the specified name.
  ; If the specified name is bound, throw error.
  ; Expand the macro inline.
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
  (let (rec (lambda (syms :opt acc)
              (if (not syms) acc
                  (cons (car syms) (cons '(gensym) (rec (cdr syms) acc))))))
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

(macro unless (test :rest body)
  ; Evaluate the specified test and if it is nil then evaluate each of the specified body.
  (list if (list nil? test) (cons begin body)))

(macro or (:rest args)
  ; Evaluate each of the specified args, one at a time, from left to right.
  ; The evaluation of all args terminates when a args evaluates to true.
  ; Return last evaluated value.
  ; If args is nil then return nil.
  (if (nil? args) nil
      (with-gensyms (g)
        (let (rec (lambda (l)
                    (if (nil? l) nil
                        (cons (list <- g (car l)) (cons g (rec (cdr l)))))))
          (list let (list g nil)
                (cons if (rec args)))))))

(macro and (:rest args)
  ; Evaluate each of the specified args, one at a time, from left to right.
  ; As soon as any form evaluates to nil, and returns nil without evaluating the remaining forms.
  ; If all args but the last evaluate to true values, and returns the results produced by evaluating the last args.
  ; If no args are supplied, returns true.
  (if (nil? args) true
      (let (rec (lambda (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))

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
  ;                (if (not test) (goto :break))
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
              (list if (list not test) '(goto :break))
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
        (list if (list not test) '(goto :break))
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
  (if (not (symbol? i)) (error "require symbol"))
  (with-gensyms (gl)
    (list 'for (list gl l i (list car gl)) gl (list <- gl (list cdr gl)
                                                        i (list car gl))
          (cons begin body))))

(macro dotimes ((i n) :rest body)
  ; Iterates over a series of integers, from 0 to the specified n.
  ; The specified body once for each integer from 0 up to but not including the value of n, with the specified i bound to each integer.
  ; Supports break, continue macro.
  ; Returns nil.
  (if (not (symbol? i)) (error "require symbol"))
  (with-gensyms (gn)
    (list 'for (list i 0 gn n) (list < i gn) (list <- i (list '++ i))
          (cons begin body))))

(macro measure (:rest body)
  ; Measure the time it takes to evaluate the specified body.
  ; Macro expansion image is as follows.
  ;     (measure expr1 expr2 ...)
  ;     (let (s (clock))
  ;       (begin0 (begin expr1 expr2 ...)
  ;               (print (- (clock) s))))
  (with-gensyms (s)
    (list let (list s (list clock))
          (list 'begin0
                (cons begin body)
                (list 'write-line
                      (list 'string "time=" (list '- (list clock) s)))))))

(builtin-function expand-macro (expr)
  ; Expand macro the specified expression expr.
  (assert (same? (car (expand-macro '(begin0 1 2 3))) let)))

(function expand-macro-all (expr)
  ; Expand macro the specified expression expr recursively.
  (let (expand-each-element (lambda (expr)
                              (if expr (cons (expand-expr (car expr))
                                             (expand-each-element (cdr expr)))))
        expand-expr (lambda (expr)
                      (if (not (cons? expr)) expr
                          (expand-each-element (expand-macro expr)))))
    (expand-expr expr)))

(macro assert-error (expr)
  ; Evaluates the specified expression and kill the system if no exception is thrown.
  ; Not executed if not in debug mode.
  ; It is used when an argument or an internal state is abnormal, or a process that can not be reached is executed.
  (list assert (list (list lambda '()
                           (list catch '(Error (lambda (e) true))
                                 expr
                                 '(return nil))
                           true))))

; fundamental function

(macro function (name args :rest body)
  (if (bound? name) (error (concat (symbol->string name) " already bound"))
      (list <- name (cons lambda (cons args (expand-macro-all body))))))

(builtin-function same? (x y)
  ; Returns true if the specified x is same object.
  (assert (not (same? 'x 'y)))
  (assert (same? 'x 'x)))

(builtin-function address (x)
  ; Returns address of the specified x.
  ; The addresses of symbols or keywords with the same name are always equal.
  (assert (= (address 'x) (address 'x)))
  (assert (not (= (address 'x) (address 'y)))))

(function different? (x y)
  ; Same as (not (same? x y)).
  (not (same? x y)))

(builtin-function not (x)
  ; Returns true if the specified x is nil.
  (assert (not nil))
  (assert (same? (not true) nil)))

(function nil? (x)
  ; Alias for not.
  (not x))

(builtin-function cons? (x)
  ; Returns true if the specified x is cons.
  (assert (cons? '(1)))
  (assert (not (cons? nil)))
  (assert (not (cons? '()))))

(function atom? (x)
  ; Returns true if the specified x is of type atom.
  ; It means x is cons or not.
  (not (cons? x)))

; list

(builtin-function cons (x y)
  ; Creates a cons such that the specified x is the car part and y is the cdr part.
  ; Returns nil if x is nil.
  ; If x is not cons treated as an error.
  (assert (same? (car (cons 'x nil)) 'x))
  (assert (nil? (cdr (cons 'x nil))))
  (assert-error (cons 'x 'y)))

(builtin-function car (x)
  ; Returns car of the specified cons x.
  ; Returns nil if x is nil.
  ; If x is not cons treated as an error.
  (assert (= (car '(1 2 3)) 1))
  (assert (nil? (car '())))
  (assert-error (car 1)))

(builtin-function car! (x val)
  ; Destructively change the car part of the specified cons x to the specified val.
  ; Returns val
  ; Error if x is not cons.
  (let (x '(1 2 3))
    (assert (same? (car! x 'one) 'one))
    (assert (same? (car x) 'one))
    (assert-error (car! nil 1))))

(builtin-function cdr (x)
  ; Returns cdr of the specified cons x.
  ; Returns nil if x is nil.
  ; If x is not cons treated as an error.
  (assert (= (car (cdr '(1 2 3))) 2))
  (assert (nil? (cdr '())))
  (assert-error (cdr 1)))

(builtin-function cdr! (x val)
  ; Destructively change the car part of the specified cons x to the specified val.
  ; Returns val
  ; Error if x or val is not cons.
  (let (x '(1 2 3))
    (assert (nil? (cdr! x nil)))
    (assert (nil? (cdr x)))
    (cdr! x '(two))
    (assert (same? (car (cdr x)) 'two))
    (assert-error (cdr! '(1) 2))
    (assert-error (cdr! nil 1))))

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

(function identity (x)
  ; Returns the specified x.
  ; So-called identity function.
  x)

(builtin-function list (:rest args)
  ; Returns a list whose elements are the specified args.
  ; Returns nil if args is nil.
  ; This function is a built-in function for speeding up.
  ; In paren you can define like this.
  ;     (function list (:rest args)
  ;       args)
  (assert (= (car '(1 2 3)) 1))
  (assert (nil? (car '()))))

(function list? (x)
  ; Returns true if the specified x is of type list.
  ; Same as (or (nil? x) (cons? x)).
  (or (nil? x) (cons? x)))

(function ->list (x)
  ; Returns the specified x if x is a list, otherwise returns x as a list.
  (if (list? x) x
      (list x)))

(function list->string (l delimiter)
  ; Returns a new string of the specified list elements joined together with of the specified delimiter.
  (reduce l (lambda (x y) (string x delimiter y))))

(function list= (x y :key (test same?))
  ; Returns whether the result of comparing each element of the specified lists x and y with the specified function test is true.
  ; Always returns nil if x and y are different lengths.
  (while true
    (if (and (nil? x) (nil? y)) (return true)
        (or (nil? x) (nil? y)) (return nil)
        (test (car x) (car y)) (<- x (cdr x) y (cdr y))
        (return nil))))

(builtin-function last-cons (x)
  ; Returns the last cons to follow from the specified cons x.
  ; Error if x is not cons.
  (assert (= (car (last-cons '(1 2 3))) 3))
  (assert (nil? (last-cons nil)))
  (assert-error (last-cons 1)))

(function nthcdr (l n)
  ; Get the the specified nth cons of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (if (not (unsigned-integer? n)) (error "require unsigned integer"))
  (for (i 0) (< i n) (<- i (++ i))
    (<- l (cdr l)))
  l)

(function copy-list (l)
  ; Create and return a duplicate of the specified list l.
  ; It is shallow copy.
  (if (nil? l) nil
      (subseq l 0 (length l))))

(function last (l)
  ; Returns the last element of the specified list l.
  (car (last-cons l)))

(function butlast (l)
  ; Returns a list excluding the last element of the specified list l.
  (subseq l 0 (-- (length l))))

(function .. (s e :opt (step 1))
  ; Returns a list with the specified step increments from the specified integer s to the specified integer e.
  (let (acc nil test (if (> step 0) <= >=))
    (while (test s e)
      (push! acc s)
      (<- s (+ s step)))
    (reverse! acc)))

(function append-atom (l x)
  ; Returns a new list with the specified x appended to the end of the specified list l.
  (let (acc nil)
    (dolist (i l)
      (push! acc i))
    (push! acc x)
    (reverse! acc)))

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

(function flatten (l)
  ; Returns a new list in which the car parts of all cons that make up the specified list l are elements.
  (let (acc nil rec (lambda (x)
                      (if (atom? x) (push! acc x)
                          (dolist (i x) (rec i)))))
    (rec l)
    (reverse! acc)))

(function map (args f)
  ; Returns a list of the results of mapping each element of the specified list args with the specified function f.
  (let (acc nil)
    (while args
      (push! acc (f (car args)))
      (<- args (cdr args)))
    (reverse! acc)))

(function reduce (l f)
  ; Reduce uses the specified binary operation f, to combine the elements of the specified list l.
  ; The function must accept as arguments two elements of list or the results from combining those elements.
  ; The function must also be able to accept no arguments.
  (let (rec (lambda (l)
              (if (nil? (cdr l)) (car l)
                  (rec (cons (f (car l) (cadr l)) (cddr l))))))
    (rec l)))

(function find-cons (l e :key (test same?) (key identity))
  ; Among the cons that make up the specified list l, the cons whose car part is equal to the specified e is returned.
  ; Evaluation is performed in order from left to right.
  ; If there is no such cons, nil is returned.
  ; The comparison is done with the specified function test which default value is same?.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (while true
    (if (nil? l) (return nil)
        (test (key (car l)) e) (return l)
        (<- l (cdr l)))))

(function find-cons-if (l f :key (key identity))
  ; Returns the cons that make up the specified list l that are not nil when the car part is evaluated as an argument of the specified function f.
  ; Evaluation is performed in order from left to right.
  ; If there is no such cons, nil is returned.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (while l
    (if (f (key (car l))) (return l)
        (<- l (cdr l)))))

(function find (l e :key (test same?) (key identity))
  ; Returns an element equal to the specified element e from the beginning of the specified list l.
  ; Returns nil if e does not exist.
  ; The comparison is done with same? as a default.
  ; If test is supplied, the element is compared using the test function.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (car (find-cons l e :test test :key key)))

(function find-if (l f :key (key identity))
  ; From the beginning of the specified list l, the specified function f returns the first element that does not evaluate to nil.
  ; If no such element exists, nil is returned.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (car (find-cons-if l f :key key)))

(function all-satisfy? (l f)
  ; Returns true if all element of the specified list l returns a not nil value which evaluates as an argument to the specified function f.
  ; Otherwise returns nil.
  ; As soon as any element evaluates to nil, and returns nil without evaluating the remaining elements
  (while l
    (if (f (car l)) (<- l (cdr l))
        (return nil)))
  true)

(function any-satisfy? (l f)
  ; Returns true if any element of the specified list l returns a not nil value which evaluated as an argument to the specified function f.
  ; Otherwise returns nil.
  ; It returns nil if l is empty.
  ; As soon as any element evaluates to not nil, and returns it without evaluating the remaining elements.
  (while l
    (if (f (car l)) (return true)
        (<- l (cdr l)))))

(function each-adjacent-satisfy? (l f)
  ; Returns true if each adjacent element of the specified list l returns true when evaluated as an argument to the specified function f
  (while true
    (if (nil? (cdr l)) (return true)
        (f (car l) (cadr l)) (<- l (cdr l))
        (return nil))))

; associated list

(builtin-function assoc (al k)
  ; Returns a value corresponding to the specified key k of the specified asoociate list al.
  ; Raises an exception if there is no value.
  (assert (= (assoc '(:one 1 :two 2 :three 3) :one) 1))
  (assert-error (assoc '(:one 1 :two 2 :three 3) :four)))

(builtin-function assoc! (al k v)
  ; Change the value corresponding to the specified key k in the specified association list al to the specified vlaue v.
  ; Raises an exception if there is no value.
  (assert (same? (assoc! '(:one 1 :two 2 :three 3) :one 'one) 'one))
  (assert-error (assoc! '(:one 1 :two 2 :three 3) :four 4)))

; symbol & keyword

(builtin-function bound? (sym)
  ; Returns true if the specified symbol is bound.
  (assert (bound? 'bound?))
  (assert (bound? 'nil)))

(builtin-function gensym ()
  ; Returns a numbered symbol starting with '$G-'.
  ; Unlike common lisp, the same symbol name as the symbol name generated by gensym is the same.
  ; gensim only guarantees that the symbols generated with each gensim call will not collide.
  ; There is no inconvenience unless intentionally generating symbols starting with '$G-'.
  (assert (different? (gensym) (gensym))))

; ascii character code.

(function ascii-space? (c)
  ; Returns whether byte c can be considered a space character.
  (find '(0x09 0x0A 0x0D 0x20) c :test =))

(function ascii-alpha? (c)
  ; Returns whether byte c can be considered a alphabetic character.
  (or (<= 0x41 c 0x5A) (<= 0x61 c 0x7A)))

(function ascii-digit? (c)
  ; Returns whether byte c can be considered a digit character.
  (<= 0x30 c 0x39))

(function ascii-lower (c)
  ; Returns lowercase if byte c can be considered an alphabetic character, c otherwise.
  (if (<= 0x41 c 0x5A) (+ c 0x20)
      c))

(function ascii-upper (c)
  ; Returns uppercase if byte c can be considered an alphabetic character, c otherwise.
  (if (<= 0x61 c 0x7A) (- c 0x20)
      c))

(function ascii->digit (c :key (radix 10))
  ; Returns the numeric value when the specified byte c is regarded as the specified radix base character.
  ; Default radix is 10.
  (let (n (if (ascii-digit? c) (- c 0x30)
              (ascii-alpha? c) (+ (- (ascii-lower c) 0x61) 10)))
    (if (and n (< n radix)) n
        (error "illegal char"))))

; string

(function string (:rest args)
  ; Returns concatenated string which each of the specified args as string.
  (with-memory-stream (ms)
    (dolist (arg args)
      (if (object? arg) (<- arg (.to-s arg)))
      (if arg (write arg :stream ms)))))

'todo
(function string->list (s delimiter)
  (->list s))
; (function string->list (s delimiter)
;   ; Returns a list delimited by the specified delimiter.
;   (let (s (->byte-array s) delimiter (->byte-array delimiter) match nil acc nil)
;     (while (<- match (byte-array-index s delimiter))
;       (<- acc (cons (subseq
;     (let (rec (lambda (s acc)
;                 (let (match )
;                   (if match (begin
;                               (push! acc (byte-array->string
;                                            (byte-array-slice s 0 match)))
;                               (if (<= (+ match (length delimiter))
;                                       (length s))
;                                   (rec (byte-array-slice
;                                          s
;                                          (+ match (length delimiter))
;                                          (length s))
;                                        acc)
;                                   (begin (push! acc s)
;                                          (reverse! acc))))
;                       (begin 
;                         (push! acc s)
;                         (reverse! acc))))))
;       (rec s nil))))

; number

(builtin-function number? (x)
  ; Returns true if the specified x is a number.
  (assert (number? 1))
  (assert (number? 3.14))
  (assert (number? 0x20))
  (assert (nil? (number? 'x))))

(builtin-function integer? (x)
  ; Returns true if the specified x is a integer.
  (assert (integer? 1))
  (assert (nil? (integer? 3.14)))
  (assert (nil? (integer? 'x))))

(function even? (x)
  ; Returns true if the specified integer x is even.
  (= (mod x 2) 0))

(function odd? (x)
  ; Returns true if the specified integer x is odd
  (not (even? x)))

(function byte? (x)
  ; Returns true if the specified x is integer and between 0 and 255.
  (and (integer? x ) (<= 0 x 255)))

(function unsigned-integer? (x)
  ; Returns true if the specified x is integer and zero or positive.
  (and (integer? x) (>= x 0)))

(builtin-function = (x y)
  ; Returns true if the specified number x and y are equal.
  (assert (= 3.14 3.140))
  (assert (not (= 10 20)))
  (assert (not (= 'x 'y))))

(function /= (x y)
  ; Same as (not (= x y))).
  (not (= x y)))

(builtin-function + (x :rest args)
  ; Returns the sum of the arguments.
  (assert (= (+ 1 2 3) 6)))

(function - (x :rest args)
  ; Returns the value of the specified x minus the sum of the specified args.
  ; If args is nil, return -x.
  (if (nil? args) (* x -1)
      (+ x (- (apply + args)))))

(builtin-function * (x :rest args)
  ; Returns the product of the arguments.
  (assert (= (* 1 2 3) 6)))

(builtin-function / (x :rest args)
  ; Returns the dividing of the arguments.
  (assert (= (/ 6 2 1) 3)))

(function // (x y)
  ; Perform truncation division.
  ; Same as (truncate (/ x y))).
  (truncate (/ x y)))

(builtin-function mod (x y)
  ; Returns the remainder of dividing x by y.
  (assert (= (mod 4 5) 4))
  (assert (= (mod 4 3) 1))
  (assert (= (mod 4 2) 0)))

(builtin-function < (:rest args)
  ; Returns true if each of the specified args are in monotonically decreasing order.
  ; Otherwise returns nil.
  (assert (< 0 1 2))
  (assert (nil? (< 0 0 1))))

(function > (:rest args)
  ; Returns true if each of the specified args are in monotonically decreasing order.
  ; Otherwise returns nil.
  (each-adjacent-satisfy? args (lambda (x y) (< y x))))

(function <= (:rest args)
  ; Returns true if each of the specified args are in monotonically nondecreasing order.
  ; Otherwise returns nil.
  (each-adjacent-satisfy? args (lambda (x y) (not (< y x)))))

(function >= (:rest args)
  ; Returns true if each of the specified args are in monotonically nonincreasing order.
  ; Otherwise returns nil.
  (each-adjacent-satisfy? args (lambda (x y) (not (< x y)))))

(function ++ (x)
  ; Returns the value of the specified number x + 1.
  (+ x 1))

(function -- (x)
  ; Returns the value of the specified number x - 1.
  (- x 1))

(builtin-function < (:rest args)
  ; Returns true if each of the specified args are in monotonically decreasing order.
  ; Otherwise returns nil.
  (assert (< 0 1 2))
  (assert (nil? (< 0 0 1))))

; kernel

(builtin-function fp (fd)
  ; Returns file pointer.
  ; Used to define standard input and output.
  ; Never used directly.
  )

; Paren object system
; Paren object system is an object system implemented from a primitive paren.
; Some functions are built-in.
; Although it is a built-in function, it is only built in considering speed.

(builtin-function object? (x)
  ; Returns true if the specified x is object.
  )

(builtin-function is-a? (o cls)
  ; Returns true if the specified object o regarded as the specified class cls's instance.
  )

(builtin-function find-class (cls-sym)
  ; Returns the class corresponding to the specified symbol cls_sym.
  )

(builtin-function find-method (cls-sym method-sym)
  ; Returns the class corresponding to the specified symbol cls_sym.
  )

(macro make-accessor (field)
  ; Create accessor for the specified field.
  ; If field name is 'xxx', create accessor &xxx.
  ; Works faster than method which defined with the method macro.
  (with-gensyms (receiver val)
    (let (key (symbol->keyword field)
          field (symbol->string field)
          getter (string->symbol (concat "&" field))
          setter (string->symbol (concat "&" field "!"))
          verify (list if (list not (list 'object? receiver))
                       (list 'error "require object")))
      (list begin
            (unless (bound? getter)
              (list 'function getter (list receiver)
                    :method
                    verify
                    (list 'assoc receiver key)))
            (unless (bound? setter)
              (list 'function setter (list receiver val)
                    :method
                    verify
                    (list begin (list 'assoc! receiver key val) receiver)))))))

(macro make-method-dispatcher (method-sym)
  (unless (bound? method-sym)
    (with-gensyms (receiver args)
      (list 'function method-sym (list receiver :rest args)
            :method
            (list if (list not (list 'object? receiver))
                  (list 'error "require object"))
            (list 'apply
                  (list 'find-method
                        (list 'cadr receiver)    ; <=> (assoc cls :class)
                        (list quote method-sym))
                  (list 'cons receiver args))))))

(function method? (o)
  ; Returns true if the specified o is method.
  (and (function? o)
       (same? (car (lambda-body o)) :method)))

(macro class (cls-sym (:opt (super 'Object) :rest features) :rest fields)
  ; Create class the specified cls-sym.
  (let (Object? (same? cls-sym 'Object))
    (if (not (all-satisfy? fields symbol?)) (error "require symbol")
        (bound? cls-sym) (error (concat (symbol->string cls-sym)
                                        " already bound")))
    (list begin0
          (list quote cls-sym)
          (list <- cls-sym (list quote (list :class 'Class
                                             :symbol cls-sym
                                             :super (if (not Object?) super)
                                             :features features
                                             :fields fields)))
          (cons begin
                (map fields (lambda (field) (list 'make-accessor field)))))))

(macro method (cls-sym method-sym args :rest body)
  (let (global-sym (concat cls-sym method-sym)
        quoted-global-sym (list quote global-sym)
        method-lambda (cons lambda (cons (cons 'self args) body)))
    (if (not (find-class cls-sym)) (error "class not found")
        (bound? global-sym) (error (concat (symbol->string global-sym)
                                           " already bound")))
    (list begin0
          quoted-global-sym
          (list 'make-method-dispatcher method-sym)
          (list <- global-sym method-lambda))))

(function error (:rest args)
  (throw (.message (.new Error) (apply string args))))

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

(method Object .equal? (o)
  ; Returns true if the receiver and the specified object o are the same object.
  ; Overwrite this method if there is class-specific comparisons.
  (same? self o))

(method Object .to-s ()
  ; Returns a String representing the receiver.
  "<object>")

(class Class ()
  ; Class of class object.
  ; All class objects are instances of Class class.
  symbol super features fields methods)

(method Class .new ()
  ; Construct an instance.
  ; If .init method has argument, must invoke after create an instance.
  ; Otherwise automatically invoke .init method.
  (let (o nil)
    (for (cls self) cls (<- cls (and (assoc cls :super)
                                     (find-class (assoc cls :super))))
      (dolist (field (reverse! (map (assoc cls :fields) symbol->keyword)))
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
  (map (&features self) find-class))

(method Class .methods ()
  ; Returns method list of this class, but excluding inherited methods.
  (&methods self))

;; exception

(class Exception ()
  ; The Exception class is the superclass of all exceptions.
  ; It is recommended that new exceptions derive from the Error class or one of its subclasses.
  ; Do not derive from Exception.
  message stack-trace)

(method Exception .message (:opt (message nil message?))
  ; Message accessors.
  (if message? (&message! self message) (&message self)))

(method Exception .to-s ()
  ; Returns a String representing the receiver.
  (let (class-name (symbol->string (&class self)) msg (.message self))
    (if msg (concat class-name " -- " msg)
        class-name)))

(method Exception .stack-trace ()
  (&stack-trace self))

(method Exception .print-stack-trace ()
  (write-line (.to-s self))
  (dolist (x (.stack-trace self))
    (write-string "\tat: ") (print x)))

(class SystemExit (Exception)
  ; Dispatched to shut down the Paren system.
  ; In principle, this exception is not caught.
  )

(class Interrrupt (Exception)
  ; Dispatched when the user presses the interrupt key (usually Ctrl-c or Delete).
  )

(class Error (Exception)
  )

(class NotImplementedError (Error)
  )

;; stream I/O

(class Stream ()
  ; Abstract class for reading and writing streams.
  )

(method Stream .read-byte (:rest args)
  ; Read 1byte from stream.
  ; Returns -1 when the stream reaches the end.
  ; Must be implemented in the inherited class.
  (assert nil))

(method Stream .read-char ()
  ; Read 1character from stream.
  (let (encoding (dynamic $external-encoding))
    (if (same? encoding :UTF-8)
        (let (illegal-utf8-error (.message (.new Error) "illegal UTF-8")
              trail? (lambda (b) (= (bit-and b 0xC0) 0x80))
              ms (.new MemoryStream)
              b1 (.read-byte self) b2 nil b3 nil b4 nil)
          (if (< b1 0) (return :EOF)
              (< b1 0x80) (.write-byte ms b1)
              (< b1 0xC2) (throw illegal-utf8-error)
              (not (trail? (<- b2 (.read-byte self)))) (throw illegal-utf8-error)
              (< b1 0xE0) (begin (if (= (bit-and b1 0x3E) 0)
                                     (throw illegal-utf8-error))
                                 (.write-byte (.write-byte ms b1) b2))
              (< b1 0xF0) (begin (<- b3 (.read-byte self))
                                 (if (or (and (= b1 0xE0)
                                              (= (bit-and b2 0x20) 0))
                                         (not (trail? b3)))
                                     (throw illegal-utf8-error))
                                 (.write-byte
                                   (.write-byte
                                     (.write-byte ms b1) b2) b3))
              (< b1 0xF8) (begin (<- b3 (.read-byte self) b4 (.read-byte self))
                                 (if (or (not (trail? b3))
                                         (not (trail? b4))
                                         (and (= b1 0xf0)
                                              (= (bit-and b2 0x30) 0)))
                                     (throw illegal-utf8-error))
                                 (.write-byte
                                   (.write-byte
                                     (.write-byte
                                       (.write-byte ms b1) b2) b3) b4))
              (throw illegal-utf8-error))
          (.to-s ms))
        (throw (.message (.new NotImplementedError) "unsupport encoding")))))

(method Stream .read-line (:rest args)
  ; Read line.
  ; Must be implemented in the inherited class.
  (throw (.new NotImplementedError)))

(method Stream .read ()
  ; Read expression from the specified stream.
  ; Returns :EOF if eof reached.
  (.parse (.init (.new ParenParser) :stream self)))

(method Stream .write-byte (:rest args)
  ; Write 1byte to stream.
  ; Must be implemented in the inherited class.
  (throw (.new NotImplementedError)))

(method Stream .write-string (s)
  ; Write string to stream.
  (let (ba (->byte-array s))
    (dotimes (i (length ba))
      (.write-byte self (nth ba i))))
  self)

(method Stream .write (x :key readable? (radix 10) write-line-feed?)
  ; Write the specified x to the specified stream.
  ; write is the general entry point to the Paren printer.
  ; If readable? is supplied, write in a format understood by the Paren reader.
  ; The default keyword parameters are intended to look good to people.
  (let (write-s-expr (lambda (x)
                       (if (cons? x) (write-cons x)
                           (write-atom x)))
        write-cons (lambda (x)
                     (.write-byte self 0x28)
                     (write-s-expr (car x))
                     (map (cdr x) (lambda (x) 
                                    (.write-byte self 0x20)
                                    (write-s-expr x)))
                     (.write-byte self 0x29))
        write-operator (lambda (x name)
                         (.write-byte self 0x28)
                         (.write-string self name)
                         (.write-byte self 0x20)
                         (write-s-expr (lambda-parameter x))
                         (dolist (body (lambda-body x))
                           (.write-byte self 0x20)
                           (write-s-expr body))
                         (.write-byte self 0x29))
        write-addr (lambda (x name)
                     (.write-string self "#<")
                     (.write-string self name)
                     (.write-byte self 0x3A)
                     (write-integer (address x) 16)
                     (.write-byte self 0x3E))
        write-number (lambda (x)
                       (if (integer? x) (write-integer x radix)
                           (write-float x)))
        write-integer (lambda (x radix)
                        (if (= x 0) (.write-byte self 0x30)
                            (let (write-digit
                                   (lambda (x)
                                     (let (upper (// x radix)
                                           digit (mod x radix))
                                       (if (/= upper 0) (write-digit upper))
                                       (.write-byte self
                                                   (+ digit
                                                      (if (< digit 10) 0x30
                                                          (+ digit -10 0x41)))))))
                              (when (< x 0)
                                (.write-byte self 0x2D)
                                (<- x (- x)))
                                (write-digit x))))
        write-float (lambda (x)
                      (if (= x 0) (.write-byte self 0x30)
                          (let (mant x exp 8)
                            (let (write-mant1
                                   (lambda ()
                                     (let (upper (// (truncate mant) 100000000))
                                       (write-integer upper 10)
                                       (<- mant (* (- mant (* upper 100000000))
                                                   10))))
                                  write-fraction
                                   (lambda (x)
                                     (write-mant1)
                                     (dotimes (i (-- x))
                                       (if (= mant 0) (break)
                                           (write-mant1)))))
                            (when (< mant 0)
                              (.write-byte self 0x2D)
                              (<- mant (- mant)))
                            (while (>= mant 1000000000)
                              (<- mant (/ mant 10.0) exp (++ exp)))
                            (while (< mant 100000000)
                              (<- mant (* mant 10.0) exp (-- exp)))
                            (if (<= 0 exp 6)
                                (begin
                                  (dotimes (i (++ exp))
                                    (write-mant1))
                                  (.write-byte self 0x2E)
                                  (write-fraction (- 16 exp 1)))
                                (<= -3 exp -1)
                                (begin
                                  (.write-byte self 0x30)
                                  (.write-byte self 0x2E)
                                  (dotimes (i (- (- exp) 1))
                                    (.write-byte self 0x30))
                                  (write-fraction 16))
                                (begin
                                  (write-mant1)
                                  (.write-byte self 0x2E)
                                  (write-fraction 15)
                                  (.write-byte self 0x65)
                                  (write-integer exp 10)))))))
        write-atom (lambda (x)
                     (if (builtin? x) (write-atom (builtin-name x))
                         (macro? x) (if readable? (write-operator x "macro")
                                        (write-addr x "macro"))
                         (function? x) (if readable? (write-operator x "lamdba")
                                           (write-addr x "lambda"))
                         (string? x) (if readable?
                                         (begin (.write-byte self 0x22)
                                                (.write-string self x)
                                                (.write-byte self 0x22))
                                         (.write-string self x))
                         (symbol? x) (.write-string self (symbol->string x))
                         (keyword? x) (begin
                                        (.write-byte self 0x3A)
                                        (.write-string self
                                                      (symbol->string
                                                        (keyword->symbol x))))
                         (number? x) (write-number x)
                         (assert nil))))
    (write-s-expr x)
    (if write-line-feed? (.write-byte self 0x0A))
    x))

(method Stream .seek (:rest args)
  ; Move the read position on the stream to the specified offset.
  ; Must be implemented in the inherited class.
  (throw (.new NotImplementedError)))

(method Stream .tell (:rest args)
  ; Returns the read position on the stream as a byte offset from the beginning.
  ; Must be implemented in the inherited class.
  (throw (.new NotImplementedError)))

(class MemoryStream (Stream)
  ; A stream whose contents are held in memory.
  buf buf-size rdpos wrpos)

(method MemoryStream .init ()
  (let (buf-size 256)
    (&buf-size! self buf-size)
    (&buf! self (byte-array buf-size))
    (&rdpos! self 0)
    (&wrpos! self 0))
  self)

(method MemoryStream -extend (size)
  (let (req (+ (&wrpos self) size) new-buf nil)
    (while (< (&buf-size self) req)
      (&buf-size! self (* (&buf-size self) 2)))
    (<- new-buf (byte-array (&buf-size self)))
    (byte-array-copy (&buf self) 0 new-buf 0  (&wrpos self))
    (&buf! self new-buf))
  self)

(method MemoryStream .write-byte (byte)
  (let (wrpos (&wrpos self))
    (unless (< wrpos (&buf-size self)) (-extend self 1))
    (nth! (&buf self) wrpos byte)
    (&wrpos! self (++ wrpos))))

(method MemoryStream .read-byte ()
  (let (rdpos (&rdpos self))
    (if (= rdpos (&wrpos self)) -1
        (begin0 (nth (&buf self) rdpos)
                (&rdpos! self (++ rdpos))))))

(method MemoryStream .seek (offset)
  (if (not (<= 0 offset (&wrpos self))) (error "index outof bound"))
  (&rdpos! self offset))

(method MemoryStream .tell (offset)
  (&rdpos self))

(method MemoryStream .to-s ()
  (let (pos (&wrpos self) str (byte-array pos))
    (if (= pos 0) ""
        (byte-array->string (byte-array-copy (&buf self) 0 str 0 pos)))))

(method MemoryStream .reset ()
  ; Empty the contents of the stream.
  (&rdpos! self 0)
  (&wrpos! self 0))

(class FileStream (Stream)
  ; Provides I/O functions for files.
  ; Construct with methods such as Path.openRead, Path.openWrite.
  ; It should not be construct by new.
  fp)

(method FileStream .init (:key fp)
  (&fp! self fp))

(method FileStream .read-byte ()
  (fgetc (&fp self)))

(method FileStream .read-line ()
  (fgets (&fp self)))

(method FileStream .write-byte (byte)
  (fputc byte (&fp self)))

(method FileStream .write-string(o)
  (let (o (->byte-array o))
    (fwrite o 0 (length o) (&fp self))))

(method FileStream .seek (offset)
  (fseek (&fp self) offset))

(method FileStream .tell ()
  (ftell (&fp self)))

(method FileStream .close ()
  (fclose (&fp self)))

(class Path ()
  ; Means a file or directory and provides a series of functions such as file information acquisition, creation and deletion, and stream construction.
  ; Use '/' to separate path names regardless of the host OS.
  files mode)

(method Path .init (:rest files)
  ; Initialize by passing the specified list of files that make up this path.
  (&files! self (string->list (reduce files concat) "/"))
  self)

(method Path .parent ()
  ; Returns the parent path, or nil if this path does not have a parent.
  (&files! (.init (.new Path)) (butlast (&files self))))

(method Path .resolve (:rest body)
  ; Resolve the given path against this path.
  (&files! (.new Path) (concat (&files self)
                               (string->list (reduce body concat) "/"))))

(method Path .file-name ()
  ; Resolve file name of receiver.
  (last (&files self)))

(method Path .to-s ()
  (reduce (&files self) (lambda (acc rest) (concat acc "/" rest))))

(method Path -open (mode)
  (.init (.new FileStream) :fp (fopen (.to-s self) mode)))

(method Path .open-read ()
  ; Returns a stream that reads the contents of the receiver.
  (-open self 0))

(method Path .open-write ()
  ; Returns a stream to write to the contents of the receiver.
  (-open self 1))

(method Path .open-append ()
  ; Returns a stream to append to the receiver's content.
  (-open self 2))

(method Path .openUpdate ()
  ; Returns a stream that updates the contents of the receiver.
  ; The read/write position is at the beginning of the file.
  ; The file size cannot be reduced.
  (-open self 3))

(class AheadReader ()
  ; A one-character look-ahead reader.
  ; While prefetching one character at a time from a character string or Reader, if necessary, cut out a part as a token.
  ; Can be used as a syllable reader or lexical analyzer.
  stream next buf)

(method AheadReader .init (:key string stream)
  (when string
    (<- stream (.write-string (.new MemoryStream) string)))
  (if (not (is-a? (<- stream (or stream (dynamic $stdin))) Stream))
      (error "require instance of Stream class"))
  (&stream! self stream)
  (&next! self (.read-byte stream))
  (&buf! self (.new MemoryStream))
  self)

(method AheadReader .next ()
  ; Returns a pre-read Char type character.
  (&next self))

(method AheadReader .eof? ()
  ; Returns true if eof reached.
  (= (&next self) -1))

(method AheadReader .skip ()
  ; Skip next character and returns it.
  (if (.eof? self) (error "EOF reached"))
  (begin0 (&next self)
          (&next! self (.read-byte (&stream self)))))

(method AheadReader .get ()
  ; Append next character to token and returns it.
  (let (c (.skip self))
    (.put self c)
    c))

(method AheadReader .put (b)
  ; Put the specified byte b to the end of the token regardless of the stream.
  (.write-byte (&buf self) b))

(method AheadReader .token ()
  ; Returns the token string currently cut out.
  (.to-s (&buf self)))

(method AheadReader .reset ()
  ; Reset token and returns self.
  (.reset (&buf self))
  self)

(method AheadReader .skip-space ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (and (ascii-space? (&next self)) (not (.eof? self)))
    (.skip self))
  self)

; Paren reader

(class ParenLexer (AheadReader))

(method ParenLexer -identifier-first? ()
  (let (c (&next self))
    (or (find '(0x21 0x24 0x25 0x26 0x2A 0x2B 0x2D
                0x2F 0x3C 0x3D 0x3E 0x3F 0x5F 0x2E) c)
        (ascii-alpha? c))))

(method ParenLexer -identifier-trail? ()
  (or (-identifier-first? self) (ascii-digit? (&next self))))

(method ParenLexer -lex-comment ()
  (while (/= (&next self) 0x0A) (.skip self))
  (.lex self))

(method ParenLexer -get-identifier ()
  (unless (-identifier-first? self)
    (error "illegal identifier '" (.token self) "'"))
  (.get self)
  (-get-partial-identifier self))

(method ParenLexer -get-partial-identifier ()
  (when (-identifier-first? self)
    (.get self)
    (while (-identifier-trail? self) (.get self)))
  self)

(method ParenLexer -lex-sign ()
  (let (sign (.get self))
    (if (ascii-space? (&next self)) (-get-symbol self)
        (-identifier-first? self) (-get-symbol (-get-partial-identifier self))
        (if (= sign 0x2B) (-lex-number self)
            (list :number (- (cadr (-lex-number self))))))))

(method ParenLexer -get-symbol ()
  (list :symbol (string->symbol (.token self))))

(method ParenLexer -lex-symbol ()
  (-get-symbol (-get-identifier self)))

(method ParenLexer -lex-keyword ()
  (.skip self)
  (list :keyword (string->keyword (.token (-get-identifier self)))))

(method ParenLexer -lex-string ()
  (.skip self)
  (while (/= 0x22 (&next self))
    (if (.eof? self) (error "string not closed")
        (/= (&next self) 0x5C) (.get self)
        (begin (.skip self)
               (let (c (.skip self))
                 (if (= c 0x61) (.put self 0x07)
                     (= c 0x62) (.put self 0x08)
                     (= c 0x65) (.put self 0x1B)
                     (= c 0x66) (.put self 0x0C)
                     (= c 0x6E) (.put self 0x0A)
                     (= c 0x72) (.put self 0x0D)
                     (= c 0x74) (.put self 0x09)
                     (= c 0x76) (.put self 0x0B)
                     (= c 0x78) (.put self
                                      (+ (* 16
                                            (ascii->digit (.skip self)
                                                          :radix 16))
                                         (ascii->digit (.skip self)
                                                       :radix 16)))
                     (.put self c))))))
  (.skip self)
  (list :string (.token self)))

(method ParenLexer -lex-number ()
  (let (radix 10 factor 0 val 0)
    (while (ascii-digit? (&next self))
      (<- val (+ (* val 10) (ascii->digit (.skip self)))))
    (if (= (&next self) 0x78)
        (begin (.skip self)
               (<- radix (if (= val 0) 16 val) val 0)
               (while (or (ascii-alpha? (&next self))
                          (ascii-digit? (&next self)))
                 (<- val (+ (* val radix)
                            (ascii->digit (.skip self) :radix radix)))))
        (= (&next self) 0x2E)
        (begin (.skip self)
               (<- factor 0.1)
               (while (ascii-digit? (&next self))
                 (<- val (+ val (* factor (ascii->digit (.skip self))))
                     factor (/ factor 10)))))
    (list :number val)))

(method ParenLexer .lex ()
  (.skip-space (.reset self))
  (let (next (&next self))
    (if (.eof? self) '(:EOF)
        (= next 0x22) (-lex-string self)
        (= next 0x27) (begin (.skip self) '(:quote))
        (= next 0x28) (begin (.skip self) '(:open-paren))
        (= next 0x29) (begin (.skip self) '(:close-paren))
        (= next 0x3A) (-lex-keyword self)
        (= next 0x3B) (-lex-comment self)
        (or (= next 0x2B) (= next 0x2D)) (-lex-sign self)
        (ascii-digit? next) (-lex-number self)
        (-identifier-first? self) (-lex-symbol self)
        (error "illegal char"))))

(class ParenParser ()
  lexer token token-type)

(method ParenParser .init (:key string stream)
  (&lexer! self (.init (.new ParenLexer) :string string :stream stream))
  self)

(method ParenParser -scan ()
  (let (x (.lex (&lexer self)))
    (&token! (&token-type! self (car x)) (cadr x)))
  self)

(method ParenParser -parse-s ()
  (let (token-type (&token-type self))
    (if (same? token-type :EOF) :EOF
        (same? token-type :quote) (list quote (.parse self))
        (same? token-type :open-paren) (-parse-list self)
        (or (same? token-type :symbol)
            (same? token-type :keyword)
            (same? token-type :string)
            (same? token-type :number)) (&token self)
        (error "syntax error"))))

(method ParenParser -parse-list ()
  (-scan self)
  (if (same? (&token-type self) :close-paren) nil
      (cons (-parse-s self) (-parse-list self))))

(method ParenParser .parse ()
  (-parse-s (-scan self)))

(function read-byte (:opt stream)
  ; Read 1byte from the specified stream.
  ; Returns -1 when the stream reaches the end.
  (let (stream (or stream (dynamic $stdin)))
    (.read-byte stream)))

(function read-char (:opt stream)
  ; Read 1character from the specified stream.
  (let (stream (or stream (dynamic $stdin)))
    (.read-char stream)))

(function read-line (:opt stream)
  ; Read line from the specified stream.
  (let (stream (or stream (dynamic $stdin)))
    (.read-line stream)))

(function write-byte (byte :opt stream)
  ; Write 1byte to the specified stream.
  ; Returns byte.
  (let (stream (or stream (dynamic $stdout)))
    (.write-byte stream byte)
    byte))

(function write-string (s :opt stream)
  ; Write the specified stirng s to the specified stream.
  (let (stream (or stream (dynamic $stdout)))
    (.write-string stream s)))

(macro with-memory-stream ((ms :opt s) :rest body)
  ; Create memory stream context.
  ; If the specified string s supplied, memory stream initialize with s.
  ; (with-memory-stream (ms s)
  ;    expr1 expr2 ...)
  ; (let (ms (.new MemoryStream))
  ;    (if s (.write-string ms s))
  ;    expr1 expr2 ...
  ;    (.to-s ms))
  (with-gensyms (g)
    (list let (list ms (list '.new 'MemoryStream) g s)
          (list if g (list '.write-string ms g))
          (cons begin body)
          (list '.to-s ms))))

(function with-open-mode (sym gsym path mode body)
  (list let (list path (list if
                             (list string? path) (list '.init '(.new Path) path)
                             (list 'and (list 'object? path)
                                   (list is-a? path 'Path)) path))
    (list let (list gsym nil)
          (list unwind-protect
                (cons let (cons (list sym (list mode path))
                                (cons (list <- gsym sym)
                                      body)))
                (list if gsym (list '.close gsym))))))

(macro with-open-read ((in path) :rest body)
  (with-gensyms (stream)
    (with-open-mode in stream path '.open-read body)))

(macro with-open-write ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.open-write body)))

(macro with-open-append ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.open-append body)))

(macro with-open-update ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.openUpdate body)))

(function read (:opt stream)
  (let (stream (or stream (dynamic $stdin)))
    (.read stream)))

(function write (x :key stream readable? (radix 10) write-line-feed?)
  (let (stream (or stream (dynamic $stdout)))
    (.write stream x
            :readable? readable?
            :radix radix
            :write-line-feed? write-line-feed?)))

(function write-line (x :key stream readable? (radix 10))
  (let (stream (or stream (dynamic $stdout)))
    (.write stream x
            :readable? readable?
            :radix radix
            :write-line-feed? true)))

(function print (x :opt stream)
  ; Print the specified x as a readable format.
  (let (stream (or stream (dynamic $stdout)))
    (.write stream x :readable? true :write-line-feed? true)))

; execution

(builtin-function eval (expr)
  ; Evaluates the specified expression and returns a value.
  (assert (nil? (eval 'nil))))

(builtin-function apply (f args)
  ; Evaluates the specified expression and returns a value.
  ; Applies the function to the args.
  (assert (= (apply car '((1))) 1))
  (assert (= (apply identity '(1)) 1)))

(function repl ()
  ; Enter repl(read eval print loop) mode.
  ; Executed when there is no command line argument when paren starts.
  (with-gensyms (g)
    (while true
      (catch (SystemExit (lambda (e) (break))
              Error (lambda (e) (.print-stack-trace e)))
        (write-string ") ")
        (if (same? (<- g (read)) :EOF) (break))
        (print (eval g))))))

(function quit ()
  ; Quit the system.
  (throw (.new SystemExit)))

(function load (path)
  ; Load the specified file.
  ; Returns true if successfully loaded.
  (with-open-read (in path)
    (while (different? (eval (read in)) :EOF)))
  true)

(function import (key)
  ; Load the file corresponding to the specified keyword.
  ; Search the current directory and directories in the execution environment.
  ; Returns true if successfully loaded.
  (if (find $import key) true
      (begin0 (load (string (keyword->symbol key) ".p"))
              (push! $import key))))

(function shell ()
  ; Start paren shell.
  (let (s nil)
    (while true
      (catch (SystemExit (lambda (e) (break))
              Error (lambda (e) (.print-stack-trace e)))
        (write-string (.to-s $paren-home))
        (write-string "> ")
        (let (expr nil)
          (with-memory-stream (out (with-memory-stream (in)
                                     (write-string (read-line) in)))
            (while (different? (<- s (read out)) :EOF)
              (push! expr s)))
          (print (eval (reverse! expr))))))))

(function boot (args)
  ; Executed when paren is executed.
  ; Invoke repl if there are no command line arguments.
  ; If there are command line arguments, each is regarded as a file name and loaded in the specified order.
  (if (nil? args) (repl)
      (begin
        (dolist (arg args)
          (load arg))
        (if (bound? 'main) (main)))))

; global symbol

(global-symbol $import '(:core)
  ; List of imported modules.
  ; Referenced and updated when calling the import function.
  ; Do not update directly.
  )

(global-symbol $paren-home (.init (.new Path) $paren-home)
  ; System directory.
  ; Holds system files.
  )

(global-symbol $stdin (.init (.new FileStream) :fp (fp 0))
  ; File stream object holding the standard input.
  )

(global-symbol $stdout (.init (.new FileStream) :fp (fp 1))
  ; File stream object holding the standard ouput.
  )

(global-symbol $os $os
  ; Host os.
  ; Determined by compile-time arguments.
  ; The values to be set are as follows.
  ; - :Windows
  ; - :Linux
  ; - :Android
  ; - :Mac
  )

(global-symbol $external-encoding :UTF-8
  ; Input / output encoding.
  ; Currently supported encodings are as follows.
  ; - :UTF-8
  ; Currently dummy encodings are as follows.
  ; - :CP932
  ; A dummy encoding is an encoding for which character handling is not properly implemented.
  )

(boot $args)
