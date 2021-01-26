; Paren core library.

; special operator

(macro special-operator (name syntax)
  ; A special operator is a operator with special evaluation rules, possibly manipulating the evaluation environment, control flow, or both.
  name)

(special-operator <-
  ; Bind the bound-expr with the result of evaluating the binding-expr in order from the left to right.
  ; The symbol is bound to the already bound environment closest to the current environment.
  ; If it is not bound to the global environment, bind to the global environment.
  ; If bound-expr is a tree rather than symbol, binds the symbols specified in tree to the corresponding values in the tree structure resulting from the evaluation of expression.
  ; Returns the last evaluation result.
  (<- bound-expr1 binding-expr1 bound-expr2 binding-expr2 ...))

(special-operator let
  ; Create new environment and bind symbol then execute a series of expression in that environment.
  ; The binding mechanism is the same as special-operator '<-' except that it unconditionally binds the symbol to the newly created environment.
  ; Returns the last evaluation result.
  (let (bound-expr1 binding-expr1 bound-expr2 binding-expr2 ...)
    expr1
    expr2
    ...))

(special-operator begin
  ; Evaluate expressions in order from left to right.
  ; Returns the last evaluation result.
  (begin
    expr1
    expr2
    ...))

(special-operator quote
  ; Returns just expr.
  (quote expr))

(special-operator if
  ; Evaluate statements in order from the left to right, then evaluate the expression corresponding to the statement that returned true first.
  ; If none of the statemnet returns true, evaluate default-expr.
  ; If default-expr is omitted, it is considered to be specified by nil.
  ; Returns the last evaluation result.
  (if stmt1 expr1
      stmt2 expr2
      ...
      [default-expr]))

(special-operator f
  ; Returns an anonymous function.
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
  (f ([required_param] ...
                       [:opt optional_param ...]
                       [{ :rest rest_param | :key keyword_param ... }] )
    expr1
    expr2
    ...))

(special-operator return
  ; Special operator return escapes from current procedure(function/macro) context.
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
  ; Evaluates protected-expr and guarantees that cleanup-exprs are executed before unwind-protect exits, whether it terminates normally or is aborted by a control transfer of some kind.
  ; Returns the last expressions result of cleanup-expr.
  (unwind-protect protected-expr
                  cleanup-expr1
                  cleanup-expr2
                  ...))

(special-operator labels
  ; Create a context for jumping with goto expressions.
  ; When a goto is evaluated in the labels context, transfer control to the location of the specified expr that matches the specified keyword.
  (labels expr1
          expr2
          ...))

(special-operator goto
  ; Jump to the specified label in the most recent labels context.
  (goto label))

(special-operator throw
  ; Throw an exception.
  ; The throwing object must be an instance of the Paren object system.
  (throw expr))

(special-operator catch
  ; Special operator catch evaluate expr in order.
  ; If an error is thrown by the throw operator during expr evaluation, 
  ; Determine if the objects thrown in order from the left to right are instances of the specified class.
  ; Transfer control to the corresponding handler if there is a matching class.
  ; If not, the exception is propagated to the higher context.
  ; Handler must be a function with only one required parameter to receive the thrown object.
  (catch (Error1 handler1 Error2 handler2 ...)
    expr1
    expr2
    ...))

(special-operator assert
  ; Evaluates the specified expression and kill the system if the results is nil.
  ; If compiling with the debug option off, Returns nil and the expression is not evaluated.
  (assert expr))

(special-operator dynamic
  ; Evaluate symbols with a dynamic scope.
  (dynamic sym))

; fundamental macro

(macro function! (name args :rest body)
  ; Bind an anonimous function to a specified symbol name.
  ; Same as function macro except for the following points.
  ; - No error even if the symbol is already bound.
  ; - Do not inline macros
  ; Returns name.
  (list <- name (cons f (cons args body))))

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
  (let (rec (f (syms)
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
      (nil? (cdr args)) (car args)
      (with-gensyms (g)
        (let (rec (f (l)
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
      (nil? (cdr args)) (car args)
      (let (rec (f (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))

(macro switch (test :rest body)
  ; The switch macro help control complex conditional and branching operations.
  ; As shown below, the switch macro consists of one test and multiple branches.
  ;     (switch <test> <branches>)
  ;     <branches> ::= <branch> ...
  ;     <branch> ::= <labels> <then>
  ;     <test> -- An expression that determines which branch is evaluated.
  ;     <labels> -- An element or list of element whose address is compared to test.
  ;     <then> -- An expression that is evaluated only when control is transferred to a branch.
  ; You can also pass an atom in <labels>, in which case the elements are converted internally as a list.
  ; test is evaluated only once and compared from left to right for matching elements in the <labels> of each branch.
  ; If there is a matching element, control is transferred to that branch.
  ; If you specify keyword :default in <labels>, you can unconditionally transfer control to that branch regardless of the value of test.
  ; Error if, not specified :default and control reaches the end.
  (with-gensyms (gtest)
    (let (branches (group body 2)
                   default-branch (list 'error "switch/" gtest " not included in " (list quote (flatten (map car branches))))
                   parse-branches (f (branches)
                                    (if (nil? branches) (list default-branch)
                                        (let ((label then) (car branches))
                                          (if (eq? label :default) (return (list true then))
                                              (cons (cons '|| (map (f (x) (list eq? (list quote x) gtest))
                                                                   (->list label)))
                                                    (cons then (parse-branches (cdr branches)))))))))
      (list let (list gtest test)
            (cons if (parse-branches branches))))))

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
  (list let binding
        (list labels
              :start
              (list if (list ! test) '(goto :break))
              (cons begin body)
              :continue
              (cons <- update)
              '(goto :start)
              :break)
        nil))

(macro while (test :rest body)
  ; The specified test is evaluated, and if the specified test is true, each of the specified body is evaluated.
  ; This repeats until the test becomes nil.
  ; Supports break, continue macro.
  ; Returns nil.
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
    (list 'for (list gl l i (list car gl)) gl (list gl (list cdr gl) i (list car gl))
          (cons begin body))))

(macro dotimes ((i n) :rest body)
  ; Iterates over a series of integers, from 0 to the specified n.
  ; The specified body once for each integer from 0 up to but not including the value of n, with the specified i bound to each integer.
  ; Supports break, continue macro.
  ; Returns nil.
  (with-gensyms (gn)
    (list 'for (list i 0 gn n) (list < i gn) (list i (list '++ i))
          (cons begin body))))

(macro dostring ((c s) :rest body)
  ; Iterates over the characters of the string s.
  ; Supports break, continue macro.
  ; Returns nil.
  (with-gensyms (ga)
    (list let (list ga (list 'str->arr s))
          (list 'doarray (list c ga)
                (cons begin body)))))

(macro doarray ((i a) :rest body)
  ; Iterates over the elements of the specified array a, with index the specified i.
  ; Supports break, continue macro.
  ; Returns nil.
  (with-gensyms (ga gi glen)
    (list 'for (list gi 0 ga a glen (list 'arrlen ga)) (list < gi glen) (list gi (list '++ gi))
          (list let (list i (list [] ga gi))
                (cons begin body)))))

(macro timeit (:rest body)
  ; Clock the time it takes to evaluate the specified body.
  ; Returns evaluation result of the last element of body.
  (with-gensyms (clock-offset cycle-offset)
    (list let (list clock-offset '(clock) cycle-offset '(cycle))
          (list 'begin0
                (cons begin body)
                (list 'write (list 'list
                                   :time (list '- '(clock) clock-offset)
                                   :cycle (list '- '(cycle) cycle-offset)))))))

(builtin-function expand-macro (expr)
  ; Returns the result of expanding the macro when expr is a list and car is a macro.
  ; Otherwise returns expr.
  (assert (eq? (car (expand-macro '(begin0 1 2 3))) let)))

(function! expand-macro-all (expr)
  ; Same as expand-macro except that it executes recursively.
  (let (expand (f (x)
                 (if (cons? x)
                     (let (y (expand-macro x))
                       (if (neq? x y) (expand y)
                           (expand-cdr (cdr x) (cons (car x) nil))))
                     x))
               expand-cdr (f (x acc)
                            (if x (expand-cdr (cdr x) (cons (expand (car x)) acc))
                                (reverse! acc))))
    (expand expr)))

; fundamental function

(macro function (name args :rest body)
  ; Bind a symbol the specified name on an anonimous function whose parametes are args and whose body is body.
  ; The macro in the body is expanded.
  ; Error if name is already bound.
  ; Returns name.
  (let (expand-body (f (expr)
                      (if expr (cons (expand-macro-all (car expr)) (expand-body (cdr expr))))))
    (with-gensyms (gname)
      (list let (list gname (list quote name))
            (list if (list bound? gname) (list 'error gname " already bound")
                  (list <- name (cons f (cons args (expand-body body)))))
            gname))))

(builtin-function eq? (x y :rest args)
  ; Returns whether all arguments are the same object.
  (assert (! (eq? 'x 'y 'z)))
  (assert (eq? 'x 'x 'x)))

(builtin-function neq? (x y :rest args)
  ; Same as (! (eq? x y)).
  (assert (neq? 'x 'y 'z))
  (assert (! (neq? 'x 'x 'x))))

(builtin-function ! (x)
  ; Returns whether the x is nil.
  (assert (! (eq? 'x 'y 'z)))
  (assert (! nil))
  (assert (eq? (! true) nil)))

(builtin-function address (x)
  ; Returns address of the specified x.
  ; The addresses of symbols or keywords with the same name are always equal.
  (assert (= (address 'x) (address 'x))))

(builtin-function function? (x)
  ; Returns whether the x is a function.
  (assert (function? (f (x) x)))
  (assert (! (function? begin0))))

(builtin-function builtin? (x)
  ; Returns whether the x is a builtin-operator.
  (assert (builtin? f))
  (assert (builtin? +))
  (assert (! (builtin? builtin-function))))

(builtin-function macro? (x)
  ; Returns whether the x is a macro.
  (assert (macro? begin0))
  (assert (! (macro? begin))))

; list

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
  (if (cons? x) true
      (nil? x)))

(function ->list (x)
  ; Returns a list representation of x.
  ; If x is a list, returns x.
  ; If x is an array, returns a list containing all of the elements in x.
  ; Otherwise, returns a list with x as the only element.
  (if (list? x) x
      (array? x) (let (acc nil)
                   (doarray (e x) (push! e acc))
                   (reverse! acc))
      (list x)))

(function join (l :opt delim)
  ; Returns a new string of the specified list elements joined together with of the specified delimiter.
  ; If delim is not specified, consider an empty string to be specified.
  (if (nil? l) nil
      (nil? (cdr l)) (mem->str (car l))
      (nil? delim) (mem->str! (apply memcat l))
      (with-memory-stream ($out)
        (write-mem (car l))
        (dolist (x (cdr l))
          (write-mem delim)
          (write-mem x)))))

(function split (s :opt delim)
  ; Returns a list of characters in string s.
  ; If delim is specified, returns a list of strings s delimited by delimiter.
  (if (memempty? s) nil
      (nil? delim) (->list (str->arr s))
      (let (i 0 lis nil chars nil
              sa (str->arr s) salen (arrlen sa)
              da (str->arr delim) dalen (arrlen da) end (- salen dalen)
              match? (f ()
                       (dotimes (j dalen)
                         (if (! (memeq? ([] sa (+ i j)) ([] da j))) (return nil)))
                       true)
              join-chars (f () (if chars (apply memcat (reverse! chars)) "")))
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

(function nth (n l)
  ; Returns the nth element of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (car (nthcdr n l)))

(function nthcdr (n l)
  ; Returns the the specified nth cons of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (let (rec (f (m l)
              (if (= n m) l
                  (rec (++ m) (cdr l)))))
    (if (< n 0) (error "unexpected negative number")
        l (rec 0 l))))

(function last (x)
  ; Same as (car (last-cons x)).
  (car (last-cons x)))

(function butlast (l)
  ; Returns a list excluding the last element of the specified list l.
  (let (rec (f (rest)
              (if (cdr rest) (cons (car rest) (rec (cdr rest))))))
    (rec l)))

(function .. (s e :opt step)
  ; Returns a list with the specified step increments from the specified integer s to the specified integer e.
  (let (rec (f (s e step acc)
              (if (> s e) (reverse! acc)
                  (rec (+ s step) e step (cons s acc)))))
    (if (<= (|| step (<- step 1)) 0) (error "step must be positive")
        (rec s e step nil))))

(function group (l n)
  ; Returns a list in which the elements of l are grouped into sublists of length n.
  ; Error if the list length is not a multiple of n.
  (let (rec (f (l sublis m acc)
              (if (/= n m) (if (nil? l) (error "indivisible by " n)
                               (rec (cdr l) (cons (car l) sublis) (++ m) acc))
                  (nil? l) (reverse! (cons (reverse! sublis) acc))
                  (rec l nil 0 (cons (reverse! sublis) acc)))))
    (if (<= n 0) (error "expected positive number")
        l (rec l nil 0 nil))))

(function reverse (l)
  ; Returns a list with the elements of list l reversed.
  (let (rec (f (l acc)
              (if (nil? l) acc
                  (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))

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

(macro push! (x sym)
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

(function nilable-assoc (al k)
  ; Same as asooc except that it returns nil if the key does not exist.
  (if (nil? al) nil
      (eq? (car al) k) (cadr al)
      (nilable-assoc (cddr al) k)))

(builtin-function assoc! (al k v)
  ; Change the value corresponding to the specified key k in the specified association list al to the specified vlaue v.
  ; Error if there is no key.
  (assert (eq? (assoc! '(:one 1 :two 2 :three 3) :one 'one) 'one)))

(function flatten (l)
  ; Returns a list in which the car parts of all cons that make up the specified list l are elements.
  (let (acc nil rec (f (x)
                      (if (atom? x) (push! x acc)
                          (dolist (i x) (rec i)))))
    (rec l)
    (reverse! acc)))

(function collect (fn)
  ; Returns a list of the applied results until the function fn returns nil.
  (let (rec (f (val :opt acc)
              (if val (rec (fn) (cons val acc))
                  (reverse! acc))))
    (rec (fn))))

(function zip (:rest args)
  ; Returns a list of list, where the i-th list contains the i-th element from each of the argument.
  ; The list of return values depends on the length of the first args length.
  (let (rec (f (args acc)
              (if (nil? (car args)) (reverse! acc)
                  (rec (map cdr args) (cons (map car args) acc)))))
    (rec args nil)))

(function map (fn args :rest more-args)
  ; Returns a list of the results of mapping each element of the specified list args with the specified function fx.
  (let (map1 (f (fn args :opt acc)
               (if (nil? args) (reverse! acc)
                   (map1 fn (cdr args) (cons (fn (car args)) acc))))
             mapn (f (args-list :opt acc)
                    (if (nil? (car args-list)) (reverse! acc)
                        (mapn (map1 cdr args-list)
                              (cons (apply fn (map1 car args-list)) acc)))))
    (if (nil? more-args) (map1 fn args)
        (mapn (cons args more-args)))))

(function foreach (fn args)
  ; Apply a function to each argument.
  ; Returns nil.
  (when args
    (fn (car args))
    (foreach fn (cdr args))))

(function reduce (fn args)
  ; Returns the value that apply function of two arguments cumulatively to the elements of the list args, from left to right.
  (if (cdr args) (reduce fn (cons (fn (car args) (cadr args)) (cddr args)))
      (car args)))

(function find (fn l)
  ; Find the element in the list l where the function fn first returns not nil.
  ; Returns a following list (such-element return-value).
  ; If there is no such element, returns '(nil nil).
  (let (rec (f (l :opt e v)
              (if (&& l (! (<- e (car l) v (fn e)))) (rec (cdr l))
                  (list e v))))
    (rec l)))

(function select (fn l)
  ; Returns a list with the elements for which the result of applying the function fn is true.
  (let (rec (f (l acc)
              (if (nil? l) (reverse! acc)
                  (fn (car l)) (rec (cdr l) (cons (car l) acc))
                  (rec (cdr l) acc))))
    (rec l nil)))

(function except (fn l)
  ; Returns a list with the elements for which the result of applying the function fn is true removed.
  (let (rec (f (l acc)
              (if (nil? l) (reverse! acc)
                  (fn (car l)) (rec (cdr l) acc)
                  (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))

(function every? (fn l)
  ; Returns whether the result of the function fn applied to all the elements of the list is true.
  ; If x is nil, returns true.
  (if l (&& (fn (car l)) (every? fn (cdr l)))
      true))

(function some? (fn l)
  ; Returns whether the function fn applied to any element of the list is true.
  ; If x is nil, returns nil.
  (if (nil? l) nil
      (fn (car l)) true
      (some? fn (cdr l))))

(function none? (fn l)
  ; Returns whether the result of the function fn applied to all the elements of the list is nil.
  ; If x is nil, returns true.
  (if (nil? l) true
      (fn (car l)) nil
      (none? fn (cdr l))))

(function every-adjacent? (fn l)
  ; Returns whether each adjacent element of the specified list l returns true when evaluated as an argument to the specified function fn.
  (if (cdr l) (&& (fn (car l) (cadr l)) (every-adjacent? fn (cdr l)))
      true))

; array

(builtin-function array (size)
  ; Returns an array of length size.
  (assert (array 1)))

(builtin-function array? (x)
  ; Returns whether the x is an array.
  ; However, bytes are not considered as arrays.
  (assert (array? (array 3)))
  (assert (! (array? (bytes 3)))))

(builtin-function [] (x i :opt v)
  ; Returns the i-th element of the array x.
  ; If v is specified, update the i-th element of array x to v.
  ; Returns v.
  ; This function can also be applied to bytes.
  (assert (nil? ([] (array 1) 0)))
  (assert (= ([] (bytes 1) 0) 0))
  (assert (let (a (array 1) b (bytes 1))
            (&& ([] a 0 true)
                ([] a 0)
                ([] b 0 0xff)
                (= ([] b 0) 0xff)))))

(builtin-function arrlen (x)
  ; Returns the length of the specified array x.
  (assert (= (arrlen (array 3)) 3)))

(builtin-function arrcpy (src src-i dst dst-i size)
  ; Copy size elements from the `src-i`th element of the src bytes to the dst bytes `dst-i`th element and beyond.
  ; Returns dst.
  ; Even if the areas to be copied overlap, it operates correctly.
  ; This function also accepts strings.
  (assert (let (s (array 1) d (array 2))
            ([] s 0 1)
            ([] d 0 :zero)
            ([] d 1 :one)
            (&& (= ([] (arrcpy s 0 d 1 1) 1) 1)
                (eq? ([] d 0) :zero)
                (= ([] d 1) 1)))))

(function subarr (x start :opt end)
  ; Returns a new array object selected from start to end (end not included) where start and end represent the index of items in that array x.
  (let (xlen (arrlen x))
    (if (< start 0) (error "illegal start")
        (nil? end) (<- end xlen)
        (> end xlen) (error "illegal end"))
    (let (new-len (- end start) new-array (array new-len))
      (arrcpy x start new-array 0 new-len))))

; memory

(builtin-function memeq? (x y)
  ; Returns whether x arguments are the same byte sequence.
  (assert (memeq? :foo "foo"))
  (assert (memeq? :foo 'foo))
  (assert (! (memeq? "foo" "bar"))))

(function memneq? (x y)
  ; Same as (! (memeq? x y)).
  (! (memeq? x y)))

(builtin-function memcmp (x y)
  ; If x is equals to y, returns 0.
  ; If x is lexicographically less than y, returns -1.
  ; If x is lexicographically greater than y, returns 1.
  (assert (= (memcmp "bar" "foo") -1))
  (assert (= (memcmp "foo" "bar") 1))
  (assert (= (memcmp "foo" "foo") 0))
  (assert (= (memcmp "fo" "foo") -1))
  (assert (= (memcmp "foo" "fo") 1)))

(function memhash (x)
  ; Returns hash value of mem.
  (let (hval 17)
    (dotimes (i (memlen x))
      (if (< i 10) (<- hval (+ (* hval 31) ([] x i)))
          (break)))
    hval))

(function memempty? (s)
  ; Returns whether the s is "" or nil.
  (|| (nil? s) (= (memlen s) 0)))

(builtin-function mem->bytes (x :opt i size)
  ; Returns bytes corresponding to byte sequence x.
  ; If i is supplied, returns bytes of partial byte sequence from i of x.
  ; If size is supplied, returns string of partial byte sequence from i to (size -1) of x.
  (assert (memeq? (mem->bytes "a") "a")))

(builtin-function mem->sym (x :opt i size)
  ; Same as (mem->bytes x) except returns symbol.
  (assert (eq? (mem->sym "foo") 'foo)))

(builtin-function mem->key (x :opt i size)
  ; Same as (mem->bytes x) except returns keyword.
  (assert (eq? (mem->key "foo") :foo)))

(builtin-function mem->str (x :opt i size)
  ; Same as (mem->bytes x) except returns string.
  (assert (memeq? (mem->str 'foo) "foo"))
  (assert (memeq? (mem->str 'foo 1) "oo"))
  (assert (memeq? (mem->str 'foo 1 1) "o")))

(builtin-function mem->str! (x)
  ; Same as (mem->str x), except that it destructively modifies the specified bytes x.
  ; Generally faster than mem->str.
  (assert (let (x (bytes 1))
            ([] x 0 0x01)
            (memeq? (mem->str! x) "\x01"))))

(function memprefix? (x prefix)
  ; Returns whether the byte sequence x with the specified prefix.
  (&& (>= (memlen x) (memlen prefix))
      (memmem x prefix 0 (memlen prefix))))

(function memsuffix? (x suffix)
  ; Returns whether the byte sequence x with the specified suffix.
  (&& (>= (memlen x) (memlen suffix))
      (memmem x suffix (- (memlen x) (memlen suffix)))))

(builtin-function memlen (x)
  ; Returns the size of the byte sequence x.
  (assert (= (memlen "") 0))
  (assert (= (memlen "012") 3)))

(builtin-function memmem (x b :opt start end)
  ; Returns the position where the byte b appears first in the byte sequence x.
  ; If the b is not appeared, returns nil.
  ; If b is byte sequence, returns the position where the partial byte sequence appears first in the byte sequence x.
  ; If start is specified, search from start-th of the byte sequence x.
  ; If end is specified, search untile end-th of the byte sequence x.
  (assert (= (memmem "012" 0x31 1) 1))
  (assert (= (memmem "012" 0x31 0 3) 1))
  (assert (= (memmem "012" 0x31 0 3) 1))
  (assert (= (memmem "012" "12" 0 3) 1)))

(builtin-function memcpy (src src-i dst dst-i size)
  ; Copy size elements from the `src-i`th element of the src byte sequence to the dst byte sequence `dst-i`th element and beyond.
  ; Even if the areas to be copied overlap, it operates correctly.
  ; Returns dst.
  (assert (let (s "foo" d "bar")
            (memeq? (memcpy s 1 d 1 2) "boo"))))

(builtin-function submem (x start :opt end)
  ; Returns the partial byte sequence starting from start.
  ; If end is specified, returns the partial byte sequence from the i th to (end-1) th.
  (assert (memeq? (submem "012" 0) "012"))
  (assert (memeq? (submem "012" 1) "12"))
  (assert (memeq? (submem "012" 1 2) "1")))

(builtin-function memcat (x :rest args)
  ; Returns the result of combining each args with x.
  (assert (memeq? (memcat "0" "1" "2") "012")))

; bytes

(builtin-function bytes (size)
  ; Returns a bytes of size the specified size.
  ; The element is cleared to 0.
  (assert (= (memlen (bytes 1)) 1))
  (assert (= ([] (bytes 1) 0) 0)))

(builtin-function bytes? (x)
  ; Returns whether the x is bytes.
  ; symbols, keywords, and strings are acceptable as arguments for some bytes api, but this function returns nil.
  (assert (bytes? (bytes 3)))
  (assert (! (bytes? 'foo)))
  (assert (! (bytes? :foo)))
  (assert (! (bytes? "foo")))
  (assert (! (bytes? (array 3)))))

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

(function symcmp (x y)
  ; If x is equals to y, returns 0.
  ; If the memory address of x is less than y, returns -1.
  ; If the memory address of x is greater than y, returns 1.
  (if (eq? x y) 0
      (- (address x) (address y))))

(builtin-function bound? (sym)
  ; Returns whether the x is bound.
  (assert (bound? 'bound?))
  (assert (bound? 'nil)))

(builtin-function gensym ()
  ; Returns a numbered symbol starting with `$G-`.
  ; gensim only guarantees that the symbols generated with each gensim call will not collide.
  ; There is no inconvenience unless intentionally generating symbols starting with `$G-`.
  (assert (neq? (gensym) (gensym))))

; string

(function string (:rest args)
  ; Returns concatenated string which each of the specified args as string.
  ; Treat nil as an empty string.
  (with-memory-stream ($out)
    (dolist (arg args)
      (if (symbol? arg) (if arg (write-mem arg))
          (|| (string? arg) (keyword? arg) (bytes? arg)) (write-mem arg)
          (write arg :end "")))))

(builtin-function string? (x)
  ; Returns whether the x is a string.
  (assert (string? ""))
  (assert (string? "aaa"))
  (assert (! (string? (bytes 1)))))

(function str->num (s)
  ; Returns a string as a number.
  (with-memory-stream ($in s)
    (.skip-number (.new AheadReader))))

(function str->code (s)
  ; Returns the code point of string s.
  (let (b 0 val 0)
    (with-memory-stream ($in s)
      (while (/= (<- b (read-byte)) -1)
        (<- val (| (<< val 8) b))))
    val))

(function code->str (i)
  ; Returns string of code point.
  (with-memory-stream ($out)
    (while (/= i 0)
      (write-byte (& i 0xff))
      (<- i (>> i 8)))))

(function str->arr (s)
  ; Returns a character array of string s.
  (let (a (.new Array) c nil)
    (with-memory-stream ($in s)
      (while (<- c (read-char)) (.add a c)))
    (.to-a a)))

(function str->list (s)
  ; Returns a character list of string s.
  (let (acc nil)
    (with-memory-stream ($in s)
      (while (<- c (read-char)) (push! c acc)))
    (reverse! acc)))

(function substr (s start :opt end)
  ; Returns a string that is a substring of the specified string s.
  ; The substring begins at the specified start and extends to the character at index end - 1.
  ; Thus the length of the substring is `end - start`.
  (let (ms (.new MemoryStream))
    (if (< start 0) (error "illegal start " start))
    (.write-mem ms s)
    (dotimes (i start)
      (if (nil? (.read-char ms)) (error "illegal start " start)))
    (if (nil? end) (submem s (.tell ms))
        (let (pos (.tell ms))
          (dotimes (i (- end start))
            (if (nil? (.read-char ms)) (error "illegal end " end)))
          (submem s pos (.tell ms))))))

(function strnth (s i)
  ; Returns the i-th character of string s.
  ([] (str->arr s) i))

(function strlen (s)
  ; Returns the number of characters in string s.
  (arrlen (str->arr s)))

(function strstr (s pat :opt start)
  ; Returns the position where the substring pat appears first in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  ; If start is specified, search for substring pat from start-th of the string s.
  (let (start (|| start 0) sa (str->arr s) slen (arrlen sa)
              pa (str->arr pat) plen (arrlen pa))
    (if (< (- slen start) 0) (error "illegal start")
        (= plen 0) (return 0))
    (for (i start end (- slen plen) p0 ([] pa 0)) (<= i end) (i (++ i))
      (when (memeq? ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (memeq? ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

(function strlstr (s pat)
  ; Returns the position where the substring pat appears last in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  (let (sa (str->arr s) slen (arrlen sa)
           pa (str->arr pat) plen (arrlen pa))
    (if (= plen 0) (return (-- slen)))
    (for (i (- slen plen) p0 ([] pa 0)) (>= i 0) (i (-- i))
      (when (memeq? ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (memeq? ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

; number

(builtin-function number? (x)
  ; Returns whether the x is a number.
  (assert (number? 1))
  (assert (number? 3.14))
  (assert (number? 0x20))
  (assert (! (number? 'x))))

(builtin-function int? (x)
  ; Returns whether the x is a integer.
  (assert (int? 1))
  (assert (! (int? 3.14)))
  (assert (! (int? 'x))))

(function byte? (x)
  ; Returns whether the x is a integer and between 0 and 255.
  (&& (int? x) (<= 0 x 255)))

(function space? (b)
  ; Returns whether byte b is a space character.
  (|| (= b 0x09)
      (= b 0x0a)
      (= b 0x0d)
      (= b 0x20)))

(function print? (b)
  ; Returns whether b is printable.
  (<= 0x20 b 0x7e))

(function alpha? (b)
  ; Returns whether byte b is an alphabetic character.
  (|| (<= 0x41 b 0x5a) (<= 0x61 b 0x7a)))

(function digit? (b)
  ; Returns whether byte b is a digit character.
  (<= 0x30 b 0x39))

(function int->str (i :key radix padding)
  ; Returns string of i.
  (with-memory-stream (out)
    (.write-int out i :radix radix :padding padding)))

(function tolower (b)
  ; Returns lowercase if byte b is an alphabetic character.
  ; Otherwise returns b.
  (if (&& (alpha? b) (<= 0x41 b 0x5a)) (+ b 0x20)
      b))

(function toupper (b)
  ; Returns uppercase if byte b is an alphabetic character.
  ; Otherwise returns b.
  (if (&& (alpha? b) (<= 0x61 b 0x7a)) (- b 0x20)
      b))

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

(builtin-function // (x :opt y)
  ; Returns an integer value of the number x.
  ; If y is specified, returns the quotient of the x divided by the y.
  (assert (= (// 3) 3))
  (assert (= (// 3.14) 3))
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
  (every-adjacent? (f (x y) (< y x)) args))

(function <= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nondecreasing order.
  (every-adjacent? (f (x y) (! (< y x))) args))

(function >= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nonincreasing order.
  (every-adjacent? (f (x y) (! (< x y))) args))

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
  (let (val 1)
    (dotimes (i (abs power))
      (<- val (* val base)))
    (if (> power 0) val
        (/ val))))

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
  ; Reads size bytes of data from the stream pointed to by fp, storing them at the location given by bytes buf offset from.
  ; Returns size;
  )

(builtin-function fwrite (buf from size fp)
  ; Writes size bytes of data to the stream pointed to by fp, obtaining them at the location given by bytes buf offset from.
  ; Returns size;
  )

(builtin-function fseek (fp)
  ; Sets the file position indicator for the stream pointed to by fp
  ; Returns nil.
  )

(builtin-function ftell (fp)
  ; Returns the current value of the file position indicator for the stream pointed to by fp.
  )

(builtin-function fflush (fp)
  ; Flushes the stream pointed to by fp (writing any buffered output data).
  ; Returns nil.
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
  ; Error if filename already exists.
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

(builtin-function system (command)
  ; Execute host system commands.
  ; Returns the termination status of the child shell used to execute command.
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
  ; If cls-sym is not bound or cls-sym is not a class instance, returns nil.
  )

(builtin-function find-method (cls-sym method-sym)
  ; Returns the method by which an instance of the class name cls-sym is dispatched.
  )

(function error-if-not-object (o)
  ; Returns the method by which an instance of the class name cls-sym is dispatched.
  (if (! (object? o)) (error "expected object")))

(macro make-accessor (field)
  ; Returns an expression that binds getter and setter.
  ; If field name is 'xxx', bind a getter named `&xxx` and setter named `&xxx!`.
  ; Works faster than method which defined with the `method` macro.
  (with-gensyms (receiver val)
    (let (key (mem->key field) getter (memcat '& field) setter (memcat getter '!))
      (list begin
            (list if (list ! (list 'bound? (list quote getter)))
                  (list 'function getter (list receiver)
                        (list 'error-if-not-object receiver)
                        (list 'assoc receiver key)))
            (list if (list ! (list 'bound? (list quote setter)))
                  (list 'function setter (list receiver val)
                        (list 'error-if-not-object receiver)
                        (list begin (list 'assoc! receiver key val) receiver)))))))

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
  (if (! (every? symbol? fields)) (error "fields must be symbol")
      (bound? cls-sym) (error cls-sym " already bound"))
  (list begin0
        (list quote cls-sym)
        (list <- cls-sym (list quote (list :class 'Class
                                           :symbol cls-sym
                                           :super (if (eq? cls-sym 'Object) nil (|| super 'Object))
                                           :features features
                                           :fields fields)))
        (cons begin (map (f (field) (list 'make-accessor field)) fields))))

(macro method (cls-sym method-sym args :rest body)
  (let (global-sym (memcat cls-sym method-sym))
    (if (nil? (find-class cls-sym)) (error "unbound class")
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
  ; Returns the class of the receiver.
  (find-class (&class self)))

(method Object .eq? (o)
  ; Returns whether the o is equals of the receiver.
  ; Overwrite this method if there is class-specific comparisons.
  (eq? self o))

(method Object .to-s ()
  ; Returns a String representing the receiver.
  (string "<" (&symbol (.class self)) ":0x" (address self)  ">"))

(method Object .raise (:rest args)
  (apply error (cons (.to-s self) (cons " " args))))

(class Class ()
  ; Class of class object.
  ; All class objects are instances of Class class.
  symbol super features fields methods)

(method Class .new ()
  ; Construct an instance.
  ; If .init method has argument, must invoke after create an instance.
  ; Otherwise automatically invoke .init method.
  (let (o nil)
    (for (cls self) cls (cls (find-class (assoc cls :super)))
      (dolist (field (reverse! (map mem->key (assoc cls :fields))))
        (push! nil o)
        (push! field o)))
    (car! (cdr o) (assoc self :symbol))
    (if (= (length (procparams (find-method (assoc o :class) '.init))) 1) (.init o)
        o)))

(method Class .super ()
  ; Returns the class representing the superclass of the receiver.
  (find-class (&super self)))

(method Class .features ()
  ; Returns the feature list representing the feature of the receiver.
  (map find-class (&features self)))

;; exception

(class Exception ()
  ; The Exception class is the superclass of all exceptions.
  ; It is recommended that new exceptions derive from the Error class or one of its subclasses.
  ; Do not derive from Exception.
  message stack-trace)

(method Exception .message (message)
  ; Message accessors.
  (&message! self message))

(method Exception .to-s ()
  ; Returns a String representing the receiver.
  (let (class-name (mem->str (&class self)) msg (&message self))
    (if msg (memcat class-name " -- " msg)
        class-name)))

(method Exception .stack-trace ()
  (&stack-trace self))

(method Exception .print-stack-trace ()
  (write-mem (.to-s self))
  (write-line)
  (dolist (x (.stack-trace self))
    (write-mem "\tat: ") (write x)))

(class SystemExit (Exception)
  ; Dispatched to shut down the Paren system.
  ; In principle, this exception is not caught.
  )

(class Interrrupt (Exception)
  ; Dispatched when the user presses the interrupt key (usually Ctrl-c).
  )

(class Error (Exception))

(function error (:rest args)
  ; Throw a instance of the Error, which message is args.
  (throw (.message (.new Error) (apply string args))))

(class Comparable ()
  ; A feature that provides comparison operators.
  )

(method Comparable .cmp (:rest args)
  ; Compares the receiver with the specified object.
  ; Returns a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
  (assert nil))

(method Comparable .eq? (o)
  ; Returns whether the receiver and o is equal.
  (= (.cmp self o) 0))

(class Array ()
  ; Extensible array class.
  size elt)

(method Array .init ()
  (&size! self 0)
  (&elt! self (array 4)))

(method Array .size ()
  ; Returns size of the receiver.
  (&size self))

(method Array .at (i)
  ; Returns i-th element of the receiver.
  ([] (&elt self) i))

(method Array .put (i val)
  ; Update the i-th element of the receiver to val.
  ; Returns the receiver.
  ([] (&elt self) i val)
  self)

(method Array .reserve (size)
  (let (req (+ (&size self) size) elt-size (arrlen (&elt self)))
    (when (< elt-size req)
      (while (< (<- elt-size (* elt-size 2)) req))
      (let (elt (array elt-size))
        (arrcpy (&elt self) 0 elt 0 (&size self))
        (&elt! self elt)))
    self))

(method Array .add (val)
  ; Add an element to the end of the array.
  ; Returns the receiver.
  (let (i (&size self))
    (.reserve self 1)
    (&size! self (++ i))
    ([] (&elt self) i val))
  self)

(method Array .to-a ()
  ; Returns an array representation of the receiver.
  (let (size (&size self) a (array size))
    (arrcpy (&elt self) 0 a 0 size)
    a))

(class Path ()
  ; A class that handles a file path.
  ; Construct with Path.of function.
  ; It should not be construct by new.
  ; The corresponding file does not have to exist.
  ; You can read and write files to the corresponding path as needed.
  path)

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
      (let (c nil path nil root? nil)
        (if (memprefix? path-name "/") (<- root? true)
            (memprefix? path-name "~") (<- path-name
                                           (memcat
                                             (if (eq? $hostname :windows)
                                                 (memcat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))
                                                 (getenv "HOME"))
                                             "/" (submem path-name 1))))
        (<- path (except memempty?
                         (split
                           (with-memory-stream ($out)
                             (with-memory-stream ($in path-name)
                               (while (<- c (read-char))
                                 (if (memeq? c "\\") (write-mem "/")
                                     (write-mem c)))))
                           "/")))
        (if root? (<- path (cons "/"path)))
        (&path! (.new Path) path))))

(function Path.getcwd ()
  ; Returns the path corresponding to the current directory.
  (Path.of (getcwd)))

(method Path .name ()
  ; Returns file name.
  (last (&path self)))

(method Path .base-name ()
  ; Returns base name (the string up to the first dot).
  ; If not including dot, returns the entire name.
  (let (name (.name self) i (strstr name "."))
    (if i (substr name 0 i)
        name)))

(method Path .suffix ()
  ; Returns the suffix (the string after the last dot).
  ; If not including dot, returns nil.
  (let (name (.name self) i (strlstr name "."))
    (if i (substr name (++ i)))))

(method Path .but-suffix ()
  ; Returns the name without the suffix.
  (let (name (.name self) i (strlstr name "."))
    (if i (substr name 0 i)
        name)))

(method Path .root? ()
  ; Returns whether the receiver is a root directory.
  (&& (.absolute? self) (nil? (.parent self))))

(method Path .parent ()
  ; Returns the parent path
  ; If the receiver is root directory, returns nil.
  ; However, the receiver is relative path, non-root directory may return nil.
  (let (path (butlast (&path self)))
    (if path (&path! (.new Path) path))))

(method Path .resolve (path)
  ; Resolve the given path against this path.
  ; If the argument is a character string, convert it to a path object before processing.
  ; If the path parameter is an absolute path then this method trivially returns path
  ; Otherwise this method concatenate this path and the speciifed path.
  ; `.` and `..` included in path-name are not treated specially.
  (if (string? path) (<- path (Path.of path)))
  (if (.absolute? path) path
      (Path.of (memcat (.to-s self) "/" (.to-s path)))))

(method Path .absolute? ()
  ; Returns whether this path regarded as the absolute path.
  (let (first-file (car (&path self)))
    (if (eq? $hostname :windows) (&& (= (memlen first-file) 2)
                                     (= ([] first-file 1) 0x3a))
        (memeq? first-file "/"))))

(method Path .relative? ()
  ; Same as (! (.absolute? self))
  (! (.absolute? self)))

(method Path .to-l ()
  ; Reads the contents of the file corresponding to the receiver.
  ; Returns it as a list.
  (with-open ($in self :read)
    (return (collect read-line))))

(method Path .to-s ()
  ; Returns a string representation of the receiver.
  (reduce (f (acc rest)
            (memcat (if (memeq? acc "/") "" acc) "/" rest))
          (&path self)))

(method Path .open (mode)
  ; Returns a stream that reads the contents of the receiver.
  (.init (.new FileStream) (fopen (.to-s self)
                                  (switch mode
                                    :read 0
                                    :write 1
                                    :append 2
                                    :update 3))))

(method Path .mkdir ()
  ; Create a directory corresponding to this receiver, including any necessary but nonexistent parent directories.
  ; Returns self.
  (if (.dir? self) (return self)
      (.parent self) (.mkdir (.parent self)))
  (mkdir (.to-s self))
  self)

(method Path .remove ()
  ; Deletes the file corresponding to the receiver.
  ; Returns the receiver.
  (remove (.to-s self))
  self)

(method Path .stat ()
  ; Returns stat of the receiver.
  (let (stat-array (stat (.to-s self)))
    (if stat-array stat-array
        (begin (<- stat-array (array 3))
               ([] stat-array 0 1)
               ([] stat-array 1 0)
               stat-array))))

(method Path .mode ()
  ; Returns whether the receiver is a regular file.
  ([] (.stat self) 0))

(method Path .none? ()
  ; Returns whether the receiver is not exits.
  (/= (& (.mode self) 1) 0))

(method Path .file? ()
  ; Returns whether the receiver is a regular file.
  (/= (& (.mode self) 2) 0))

(method Path .dir? ()
  ; Returns whether the receiver is a directory.
  (/= (& (.mode self) 4) 0))

(method Path .other? ()
  ; Returns whether the receiver is neither a regular file nor a directory.
  (/= (& (.mode self) 8) 0))

(method Path .readable? ()
  ; Returns whether the receiver is readable.
  (/= (& (.mode self) 16) 0))

(method Path .writable? ()
  ; Returns whether the receiver is writable.
  (/= (& (.mode self) 32) 0))

(method Path .size ()
  ; Returns the size of the receiver.
  ([] (.stat self) 1))

(method Path .mtime ()
  ; Returns the last update time of the receiver.
  ([] (.stat self) 2))

(method Path .utime (time)
  ; Update the last update time of the receiver.
  ; Returns the receiver.
  (utime (.to-s self) time))

(method Path .children ()
  ; Returns a list of the contents of the directory corresponding to the receiver.
  (map (f (x) (.resolve self x))
       (split (readdir (.to-s self)) "\n")))

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
  (error "illegal byte sequence -- "
         (map (f (x) (string "0x" (int->str x :radix 16))) seq)))

(method Stream .trail? (b)
  (switch (dynamic $encoding)
    :UTF-8 (= (& b 0xc0) 0x80)
    :SJIS (|| (< 0x80 b1 0xa0) (< 0xdf b1))))

(method Stream .read-char ()
  ; Read 1 character from stream.
  ; Returns nil when the stream reaches the end.
  (let (b1 (.read-byte self) b2 nil b3 nil b4 nil size 0)
    (switch (dynamic $encoding)
      :UTF-8
      (if (< b1 0) (return nil)
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
      :SJIS
      (if (< b1 0) (return nil)
          (< b1 0x80) (<- size 1)
          (.trail? b1) (<- b2 (.read-byte self) size 2)
          (.illegal-character self b1)))
    (let (c (bytes size))
      (if (= size 1) ([] c 0 b1)
          (= size 2) (begin ([] c 0 b1) ([] c 1 b2))
          (= size 3) (begin ([] c 0 b1) ([] c 1 b2) ([] c 2 b3))
          (= size 4) (begin ([] c 0 b1) ([] c 1 b2) ([] c 2 b3) ([] c 3 b4)))
      (mem->str! c))))

(method Stream .read ()
  ; Read expression from the specified stream.
  ; Returns nil if eof reached.
  (let ($in self)
    (.read (.new ParenReader))))

(method Stream .read-line ()
  ; Input one line from the receiver.
  ; Returns read line.
  ; If stream reached eof, returns nil.
  (with-memory-stream (out)
    (let (c nil)
      (while true
        (if (= (<- c (.read-byte self)) -1) (return nil)
            (= c 0x0a) (break)
            (.write-byte out c))))))

(method Stream .write-line (:opt bytes)
  (if bytes (.write-mem self bytes))
  (.write-byte self 0x0a))

(method Stream .write-int (n :key radix padding)
  ; Write integer to stream.
  ; Returns n.
  (let (radix (|| radix 10) padding (|| padding 0)
              ->byte (f (x)
                       (if (< x 10) (+ x 0x30)
                           (+ x 0x61 -10)))
              write1 (f (n padding)
                       (let (upper (// n radix))
                         (if (/= upper 0) (write1 upper (-- padding))
                             (dotimes (i padding) (.write-byte self 0x30)))
                         (.write-byte self (->byte (mod n radix))))))
    (when (< n 0)
      (.write-byte self 0x2d)
      (<- n (- n) padding (-- padding)))
    (write1 n (-- padding)))
  n)

(method Stream .write-number (n)
  (if (int? n) (.write-int self n)
      (= n 0.0) (.write-byte self 0x30)
      (let (mant n exp 8)
        (let (write-mant1
               (f ()
                 (let (upper (// (// mant) 100000000))
                   (.write-int self upper)
                   (<- mant (* (- mant (* upper 100000000)) 10))))
               write-fraction
               (f (n)
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
                (.write-mem self "0.")
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
  (if start (.write-mem self start))
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
              (.write-mem self ",@") (.write self (cadr x) :end ""))
            (begin
              (.write-byte self 0x28)
              (.write self (car x) :end "")
              (dolist (x (cdr x)) (.write self x :start " " :end ""))
              (.write-byte self 0x29))))
      (builtin? x)
      (.write-mem self (builtin-name x))
      (string? x)
      (begin
        (.write-byte self 0x22)
        (dostring (c x)
          (if (memeq? c "\a") (.write-mem self "\\a")
              (memeq? c "\b") (.write-mem self "\\b")
              (memeq? c "\e") (.write-mem self "\\e")
              (memeq? c "\f") (.write-mem self "\\f")
              (memeq? c "\n") (.write-mem self "\\n")
              (memeq? c "\r") (.write-mem self "\\r")
              (memeq? c "\t") (.write-mem self "\\t")
              (memeq? c "\v") (.write-mem self "\\v")
              (memeq? c "\\") (.write-mem self "\\\\")
              (memeq? c "\"") (.write-mem self "\\\"")
              (.write-mem self c)))
        (.write-byte self 0x22))
      (symbol? x)
      (.write-mem self x)
      (keyword? x)
      (begin
        (.write-byte self 0x3a)
        (.write-mem self x))
      (number? x)
      (.write-number self x)
      (bytes? x)
      (begin
        (.write-mem self "#b[")
        (dotimes (i (memlen x))
          (if (/= i 0) (.write-byte self 0x20))
          (.write-mem self "0x")
          (.write-int self ([] x i) :radix 16 :padding 2))
        (.write-byte self 0x5d))
      (array? x)
      (begin
        (.write-mem self "#a[")
        (dotimes (i (arrlen x))
          (.write self ([] x i) :start (&& (/= i 0) " ") :end ""))
        (.write-byte self 0x5d))
      (|| (macro? x)
          (function? x))
      (begin
        (if (macro? x) (.write-mem self "(macro")
            (.write-mem self "(f"))
        (.write-byte self 0x20)
        (.write self (procparams x) :end "")
        (dolist (body (procbody x))
          (.write self body :start " " :end ""))
        (.write-byte self 0x29))
      (assert nil))
  (.write-mem self (|| end "\n"))
  x)

(class MemoryStream (Stream)
  ; A stream whose contents are held in memory.
  buf rdpos wrpos)

(method MemoryStream .init ()
  (&buf! self (bytes 64))
  (&rdpos! self 0)
  (&wrpos! self 0))

(method MemoryStream .size ()
  ; Returns the number of bytes written to the stream.
  (&wrpos self))

(method MemoryStream .reserve (size)
  (let (req (+ (&wrpos self) size) buf-size (memlen (&buf self)))
    (when (< buf-size req)
      (while (< (<- buf-size (* buf-size 2)) req))
      (let (buf (bytes buf-size))
        (memcpy (&buf self) 0 buf 0 (&wrpos self))
        (&buf! self buf)))
    self))

(method MemoryStream .read-byte ()
  (let (rdpos (&rdpos self))
    (if (= rdpos (&wrpos self)) -1
        (begin0 ([] (&buf self) rdpos)
                (&rdpos! self (++ rdpos))))))

(method MemoryStream .read-bytes (buf from size)
  (let (rest (- (&wrpos self) (&rdpos self)))
    (if (< rest size) (<- size rest))
    (memcpy (&buf self) (&rdpos self) buf (&wrpos self) size)
    size))

(method MemoryStream .write-byte (byte)
  (let (wrpos (&wrpos self))
    (.reserve self 1)
    ([] (&buf self) wrpos byte)
    (&wrpos! self (++ wrpos))
    byte))

(method MemoryStream .write-mem (mem :opt from size)
  (.reserve self (|| size (<- size (memlen mem))))
  (memcpy mem (|| from 0) (&buf self) (&wrpos self) size)
  (&wrpos! self (+ (&wrpos self) size))
  size)

(method MemoryStream .seek (offset)
  (if (! (<= 0 offset (&wrpos self))) (error "index outof bound"))
  (&rdpos! self offset))

(method MemoryStream .tell ()
  ; Returns current byte position in stream.
  (&rdpos self))

(method MemoryStream .to-s ()
  ; Returns the contents written to the stream as a string.
  (let (size (&wrpos self))
    (if (= size 0) ""
        (mem->str (&buf self) 0 size))))

(method MemoryStream .reset ()
  ; Empty the contents of the stream.
  ; Returns the receiver.
  (&rdpos! self 0)
  (&wrpos! self 0))

(class FileStream (Stream)
  ; Provides I/O functions for files.
  ; Construct with methods such as File.open-read, File.open-write.
  ; It should not be construct by new.
  fp)

(method FileStream .init (fp)
  (&fp! self fp))

(method FileStream .read-byte ()
  (fgetc (&fp self)))

(method FileStream .read-bytes (buf from size)
  (fread buf from size (&fp self)))

(method FileStream .read-line ()
  (fgets (&fp self)))

(method FileStream .write-byte (byte)
  (fputc byte (&fp self)))

(method FileStream .write-mem (x :opt from size)
  (fwrite x (|| from 0) (|| size (memlen x)) (&fp self)))

(method FileStream .seek (offset)
  ; Sets the file position indicator of the receiver.
  ; Returns the receiver.
  (fseek (&fp self) offset)
  self)

(method FileStream .tell ()
  ; Returns the current value of the file position indicator of the receiver.
  (ftell (&fp self)))

(method FileStream .flush ()
  (fflush (&fp self)))

(method FileStream .close ()
  (fclose (&fp self)))

(class AheadReader ()
  ; A one-character look-ahead reader.
  ; While prefetching one character at a time from a character string or Reader, if necessary, cut out a part as a token.
  ; Can be used as a syllable reader or lexical analyzer.
  stream next token lineno)

(method AheadReader .init ()
  ; initializing AheadReader with (dynamic $in).
  ; Returns the receiver.
  (&stream! self (dynamic $in))
  (&next! self (.read-char (&stream self)))
  (&token! self (.new MemoryStream))
  (&lineno! self 1))

(method AheadReader .next ()
  ; Returns a pre-read character.
  (&next self))

(method AheadReader .alpha? ()
  ; Returns true if next character is alphabetic.
  (alpha? (str->code (&next self))))

(method AheadReader .digit? ()
  ; Returns true if next character is digit.
  (digit? (str->code (&next self))))

(method AheadReader .space? ()
  ; Returns true if next character is space.
  (space? (str->code (&next self))))

(method AheadReader .alnum? ()
  ; Returns true if next character is digit or alphabetic.
  (let (b (str->code (&next self)))
    (|| (digit? b)
        (alpha? b))))

(method AheadReader .skip (:opt expected)
  ; Skip next character and returns it.
  ; Error if expected is specified and the next character is not the same as the expected.
  (let (next (&next self))
    (if (nil? next) (.raise self "unexpected EOF")
        (&& expected (memneq? next expected)) (.raise self
                                                      "unexpected character '" next "'. "
                                                      "expected '" expected "'")
        (memeq? next "\n") (&lineno! self (++ (&lineno self))))
    (&next! self (.read-char (&stream self)))
    next))

(method AheadReader .skip-escape ()
  (let (c (.skip self))
    (if (memneq? c "\\") c
        (memeq? (<- c (.skip self)) "a") 0x07
        (memeq? c "b") 0x08
        (memeq? c "c") (if (<= 0x40 (<- c (toupper (str->code (.skip self)))) 0x5f) (& c 0x1f)
                           (.raise self "illegal ctrl char"))
        (memeq? c "e") 0x1b
        (memeq? c "f") 0x0c
        (memeq? c "n") 0x0a
        (memeq? c "r") 0x0d
        (memeq? c "t") 0x09
        (memeq? c "v") 0x0b
        (memeq? c "x") (+ (* 16 (.skip-digit self 16)) (.skip-digit self 16))
        c)))

(method AheadReader .skip-line ()
  ; Skip line.
  ; Returns line.
  ; If stream reached eof, returns nil.
  (let (next (&next self))
    (if (nil? next) (.skip self)    ; raise error.
        (memeq? next "\n") (begin (.skip self) "")
        (let (line (.read-line (&stream self)))
          (if line (begin
                     (<- line (memcat next line))
                     (&lineno! self (++ (&lineno self)))
                     (&next! self (.read-char (&stream self))))
              (&next! self nil))
          line))))

(method AheadReader .skip-space ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (space? (str->code (&next self)))
    (.skip self))
  self)

(method AheadReader .skip-sign ()
  (let (next (&next self))
    (if (memeq? next "+") (begin (.skip self) nil)
        (memeq? next "-") (begin (.skip self) true)
        nil)))

(method AheadReader .skip-digit (:opt radix)
  (let (byte (str->code (.skip self))
             val (if (digit? byte) (- byte 0x30)
                     (alpha? byte) (+ (- (tolower byte) 0x61) 10)))
    (if (|| (nil? val) (>= val (|| radix 10))) (.raise self "illegal digit.")
        val)))

(method AheadReader .skip-uint ()
  (if (! (.digit? self)) (.raise self "missing digits")
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
    (if (memeq? (&next self) "x")
        (let (radix (if (= val 0) 16 val))
          (<- val 0)
          (.skip self)
          (if (! (.alnum? self)) (.raise self "missing lower or digits")
              (while (.alnum? self)
                (<- val (+ (* val radix) (.skip-digit self 16))))))
        (memeq? (&next self) ".")
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
  (.put self (.skip self)))

(method AheadReader .get-line ()
  ; Get line.
  ; Returns line.
  ; If stream reached eof, returns nil.
  (let (line (.skip-line self))
    (if line (.put self line))))

(method AheadReader .get-escape ()
  (.put self (.skip-escape self)))

(method AheadReader .put (o)
  ; Put the o to the end of the token regardless of the stream.
  ; Returns o;
  (if (byte? o) (.write-byte (&token self) o)
      (.write-mem (&token self) o))
  o)

(method AheadReader .token ()
  ; Returns the token string currently cut out.
  ; In the process of processing, token is initialized.
  (begin0 (.to-s (&token self))
          (.reset (&token self))))

(method AheadReader .stream ()
  ; Returns the stream held by the receiver.
  (&stream self))

(method AheadReader .to-s ()
  (string "<" (&symbol (.class self)) ":0x" (address self) " "
          (list :next (&next self) :lineno (&lineno self)) ">"))

; Paren reader

(class ParenLexer (AheadReader))

(method ParenLexer .identifier-symbol-alpha? ()
  (|| (memmem "!#$%&*./<=>?^[]_{|}" (.next self))
      (.alpha? self)))

(method ParenLexer .identifier-sign? ()
  (memmem "+-" (.next self)))

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
      (.raise self "illegal identifier")))

(method ParenLexer .lex-sign ()
  (let (sign (.skip self))
    (if (.digit? self)
        (let (val (.skip-number self))
          (if (memeq? sign "-") (- val) val))
        (begin (.put self sign)
               (mem->sym (.token (.get-identifier-sign self)))))))

(method ParenLexer .lex-symbol ()
  (mem->sym (.token (.get-identifier self))))

(method ParenLexer .lex-keyword ()
  (.skip self)
  (mem->key (.token (.get-identifier self))))

(method ParenLexer .lex-string ()
  (.skip self)
  (while (memneq? (&next self) "\"")
    (.get-escape self))
  (.skip self "\"")
  (.token self))

(method ParenLexer .lex ()
  (.skip-space self)
  (let (next (&next self))
    (if (nil? next) '(:EOF)
        (memeq? next "(") (begin (.skip self) '(:open-paren))
        (memeq? next ")") (begin (.skip self) '(:close-paren))
        (memeq? next "'") (begin (.skip self) '(:quote))
        (memeq? next "`") (begin (.skip self) '(:backquote))
        (memeq? next ",") (begin (.skip self) (if (memeq? (&next self) "@")
                                                  (begin (.skip self) '(:unquote-splicing))
                                                  '(:unquote)))
        (memeq? next "\"") (list :atom (.lex-string self))
        (memeq? next ":") (list :atom (.lex-keyword self))
        (memeq? next ";") (begin (.skip-line self) (.lex self))
        (memeq? next "#") (begin (.skip self) (list :read-macro (mem->sym (.next self))))
        (memmem "+-" next) (list :atom (.lex-sign self))
        (memmem "0123456789" next) (list :atom (.skip-number self))
        (list :atom (.lex-symbol self)))))

(class ParenReader ()
  lexer token-type token)

(method ParenReader .init ()
  (&lexer! self (.new ParenLexer)))

(method ParenReader .scan ()
  (let ((token-type :opt token) (.lex (&lexer self)))
    (&token-type! self token-type)
    (&token! self token)))

(method ParenReader .parse-list ()
  (let (parse-cdr (f (acc)
                    (.scan self)
                    (if (eq? (&token-type self) :close-paren) (reverse! acc)
                        (eq? (&token-type self) :EOF) (.raise self "missing close-paren")
                        (parse-cdr (cons (.parse self) acc)))))
    (parse-cdr nil)))

(method ParenReader .parse ()
  (switch (&token-type self)
    :EOF nil
    :atom (&token self)
    :open-paren (.parse-list self)
    :quote (list quote (.parse (.scan self)))
    :backquote (list 'quasiquote (.parse (.scan self)))
    :unquote (list 'unquote (.parse (.scan self)))
    :unquote-splicing (list 'unquote-splicing (.parse (.scan self)))
    :read-macro (apply (assoc $read-table (str->code (&token self))) (list self))
    :default (.raise self "syntax error")))

(macro unquote (expr)
  (list 'error "unexpected unquote -- ," expr))

(macro unquote-splicing (expr)
  (list 'error "unexpected unquote-splicing -- ,@" expr))

(macro quasiquote (expr)
  (let (descend (f (x level)
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
                descend-car (f (x level)
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
  (with-gensyms (g)
    (list let (list g (cons f (cons params body)))
          (list 'push! g '$read-table)
          (list 'push! (list 'str->code (list quote next)) '$read-table))))

(function read-byte ()
  ; Same as (.read-byte (dynamic $in)).
  (.read-byte (dynamic $in)))

(function read-char ()
  ; Same as (.read-char (dynamic $in)).
  (.read-char (dynamic $in)))

(function read-line ()
  ; Same as (.read-line (dynamic $in)).
  (.read-line (dynamic $in)))

(function read ()
  ; Same as (.read (dynamic $in)).
  (.read (dynamic $in)))

(function write-byte (x)
  ; Same as (.write-byte (dynamic $out) x).
  (.write-byte (dynamic $out) x))

(function write-mem (x :opt from size)
  ; Same as (.write-mem (dynamic $out) x).
  (.write-mem (dynamic $out) x from size))

(function write-line (:opt x)
  ; Same as (.write-line (dynamic $out) x).
  (.write-line (dynamic $out) x))

(function write (x :key start end)
  ; Same as (.write (dynamic $out) x :start start :end end)).
  (.write (dynamic $out) x :start start :end end))

(function flush ()
  (.flush (dynamic $out)))

(macro with-memory-stream ((ms :opt s) :rest body)
  ; Create memory stream context.
  ; If the string s is specified, construct an input stream with s as the source.
  ; Returns last evaluated value.
  ; Otherwise act as an output stream.
  ; Returns the string written to the output stream.
  ;     (with-memory-stream (ms s)
  ;        expr1 expr2 ...)
  ;     (let (ms (.new MemoryStream))
  ;        (if s (.write-mem ms s))
  ;        expr1 expr2 ...
  ;        (if s (.to-s ms)))
  (with-gensyms (g)
    (list let (list ms '(.new MemoryStream) g s)
          (list if g
                (list begin
                      (list if g (list '.write-mem ms g))
                      (cons begin body))
                (list begin
                      (cons begin body)
                      (list '.to-s ms))))))

(macro with-open ((sym path mode) :rest body)
  ; Create file stream context.
  ; The file stream is guaranteed to be closed when exiting the context.
  ; Returns nil.
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

(builtin-function apply (fn args)
  ; Evaluates the specified expression and returns a value.
  ; Applies the function to the args.
  (assert (= (apply car '((1))) 1)))

(function repl ()
  ; Enter repl(read eval print loop) mode.
  ; Executed when there is no command line argument when paren starts.
  (let (expr nil)
    (while true
      (catch (Error (f (e) (.print-stack-trace e)))
        (write-mem ") ")
        (if (<- expr (read)) (write (eval (expand-macro-all expr)))
            (break))))))

(function quit ()
  ; Quit the system.
  (throw (.new SystemExit)))

(function load (path)
  ; Load the specified file.
  ; Returns true if successfully loaded.
  (with-open ($in path :read)
    (foreach (f (x) (eval x))
             (collect read)))
  true)

(function import (key)
  ; Load the file corresponding to the specified keyword.
  ; Search the $paren-home directory.
  ; Bind main to nil after processing.
  ; Returns true if successfully loaded.
  ; Module file to read must be UTF-8.
  (let ($encoding :UTF-8)
    (if (some? (f (x) (eq? x key)) $import) true
        (let (p (Path.of (string (mem->sym key) ".p")))
          (if (|| (.readable? p) (.readable? (<- p (.resolve $paren-home p))))
              (begin0 (load p)
                      (<- main nil)
                      (push! key $import))
              (error "unreadable module " key))))))

(function boot (args)
  ; Executed when paren is executed.
  ; Invoke repl if there are no command line arguments that bound to the symbol $args.
  ; If command line arguments are specified, read the first argument as the script file name and execute main.
  (catch (SystemExit (f (e) (return true)))
    (if (nil? args) (repl)
        (let ((_ script) (find (f (dir)
                                 (let (full-path (.resolve dir (car args)))
                                   (if (.readable? full-path) full-path)))
                               (cons (Path.getcwd) $runtime-path)))
          (if (nil? script) (error "unreadable file " (car args))
              (&& (load script) (bound? 'main)) (main (cdr args)))))))

(<- $import '(:core)
    $read-table nil
    $stdin (.init (.new FileStream) (fp 0))
    $stdout (.init (.new FileStream) (fp 1))
    $in $stdin
    $out $stdout
    $encoding :UTF-8
    $paren-home (.parent (.resolve (Path.getcwd) core.p))
    $runtime-path (list (.resolve $paren-home "coreutils") $paren-home))

(reader-macro a (reader)
  ; Define an array literal.
  ; Array elements are not evaluated.
  (let (lexer (&lexer reader) a (.new Array))
    (.skip lexer)
    (.skip lexer "[")
    (while (memneq? (.next lexer) "]") (.get lexer))
    (.skip lexer)
    (with-memory-stream ($in (.token lexer))
      (foreach (f (x) (.add a x))
               (collect read)))
    (.to-a a)))

(reader-macro b (reader)
  ; Define an bytes literal.
  (let (lexer (&lexer reader))
    (.skip lexer)
    (.skip lexer "[")
    (while (memneq? (.next lexer) "]") (.get lexer))
    (.skip lexer)
    (mem->bytes
      (with-memory-stream ($out)
        (with-memory-stream ($in (.token lexer))
          (foreach write-byte (collect read)))))))

(reader-macro p (reader)
  ; Define print reader macro.
  (let (lexer (&lexer reader))
    (.skip lexer)
    (list 'write (.read reader))))

(reader-macro . (reader)
  ; Define eval reader.
  (let (lexer (&lexer reader))
    (.skip lexer)
    (eval (.read reader))))

(boot $args)
