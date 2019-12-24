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
  ;
  ; <lambda_parameter> ::= [<required_params>]
  ;                        [:opt <xparams>]
  ;                        { [:rest <param>] | [:key <xparams>] }
  ; <required_params> ::= <param> <param> ...
  ; <xparams> ::= <xparam> <xparam> ...
  ; <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
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
  ;
  ; <macro_parameter> ::= [<macro_parameter> || <required_params>]
  ;                       [:opt <xparams>]
  ;                       { [:rest <param>] | [:key <xparams>] }
  ; <required_params> ::= <param> <param> ...
  ; <xparams> ::= <xparam> <xparam> ...
  ; <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
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

(special-operator basic-throw
  ; Special operator basic-throw provide a mechanism to control global escape.
  ; By using special operator basic-catch, it is possible to catch the occurrence of exception during evaluation.
  ; Since paren often uses the throw macro wrapped in the object system, it is not used directly.
  (basic-throw expr))

(special-operator basic-catch
  ; Special operator basic-catch receives the value thrown by basic-throw by the specified handler and performs processing.
  ; Handler must be a function with one required parameter.
  ; Since paren often uses the catch macro wrapped in the object system, it is not used directly.
  (basic-catch handler))

(special-operator assert
  ; If the specified expr is nil, kill the system.
  ; Not executed if not in debug mode.
  ; It is used when an argument or an internal state is abnormal, or a process that can not be reached is executed.
  (assert expr))

(special-operator dynamic
  ; Evaluate symbols with a dynamic scope.
  ; Used when dynamically binding the standard input.
  (dynamic sym))

; fundamental macro

(macro function (name args :rest body)
  ; Create a lambda function which parameter list the specified args and lambda function body the specified body.
  ; Then, bind a created lambda function with the specified name.
  ; If the specified name is bound, throw IllegalArgumentException.
  ; This function is in beta and will be redefined later.
  (list <- name (cons lambda (cons args body))))

(macro builtin-function (name args :rest body)
  ; Describes the specification of a built-in function and is used to describe unit tests.
  ; Built-in function is no different from user-defined function.
  (cons begin body))

(macro with-gensyms ((:rest syms) :rest body)
  ; Create the new let context which the specified syms bind with symbols which generated by gensyms and under the let context evaluate the specified body.
  ; (with-gensyms (a b c)
  ;   ...)
  ; (let (a (gensym) b (gensym) c (gensym))
  ;   ...)
  (let (rec (lambda (syms :opt acc)
              (if (not syms) acc
                  (cons (car syms) (cons '(gensym) (rec (cdr syms) acc))))))
    (cons let (cons (rec syms) body))))

(macro begin0 (:rest body)
  ; Evaluate each of the specified body and return the first evaluated value.
  ; (begin0 expr1 expr2 ...)
  ; (let (x expr1)
  ;     (begin expr2
  ;             ...)
  ;     x)
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
  ; See expanded image for details.
  ; (for (i 0) (< i 10) (<- i (++ i))
  ;     expr1
  ;     expr2
  ;     ...)
  ; (let (i 0)
  ;    (labels :start
  ;            (if (not test) (goto :break))
  ;            expr1
  ;            expr2
  ;            ...
  ;            :continue
  ;            (<- i (++ i))
  ;            (goto :start)
  ;            :break)
  ;     nil)
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
  ; (while test
  ;    expr1
  ;    expr2
  ;    ...)
  ; (for nil test nil
  ;    expr1
  ;    expr2
  ;    ...)
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
  (ensure-argument (symbol? i))
  (with-gensyms (gl)
    (list 'for (list gl l i (list car gl)) gl (list <-
                                                    gl (list cdr gl)
                                                    i (list car gl))
          (cons begin body))))

(macro dotimes ((i n) :rest body)
  ; Iterates over a series of integers, from 0 to the specified n.
  ; The specified body once for each integer from 0 up to but not including the value of n, with the specified i bound to each integer.
  ; Supports break, continue macro.
  ; Returns nil.
  (ensure-argument (symbol? i))
  (with-gensyms (gn)
    (list 'for (list i 0 gn n) (list < i gn) (list <- i (list '++ i))
          (cons begin body))))

(macro measure (:rest body)
  ; Measure the time it takes to evaluate the specified body.
  ; (measure expr1 expr2 ...)
  ; (let (s (clock))
  ;   (begin0 (begin expr1 expr2 ...)
  ;           (print (- (clock) s))))
  (with-gensyms (s)
    (list let (list s (list clock))
          (list begin0 (cons begin body)
                (list 'write-string
                      (list concat "time="
                            (list number->string (list - (list clock) s))))
                (list 'write-new-line)))))

(macro ensure-argument (:rest tests)
  ; If the specified test evaluate to nil, throw IllegalArgumentException.
  (list if (list not (cons 'and tests))
        (list basic-throw (list '.new 'IllegalArgumentException))))

(function expand-macro-all (expr)
  ; Expand macro the specified expression expr recursively.
  (let (expand-each-element (lambda (expr)
                              (if expr (cons (expand-expr (car expr))
                                             (expand-each-element (cdr expr)))))
        expand-expr (lambda (expr)
                      (if (not (cons? expr)) expr
                          (expand-each-element (expand-macro expr)))))
    (expand-expr expr)))

; fundamental macro

(macro function (name args :rest body)
  ; Redefined improved function.
  ; Expand the macro inline.
  ; Throw IllegalArgumentException when the specified name already bound.
  (if (bound? name) (throw (.message (.new IllegalArgumentException)
                                     "symbol already bound"))
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
  ; Returns true if the argument is nil.
  (assert (not nil))
  (assert (same? (not true) nil)))

(function nil? (x)
  ; Alias for not.
  (not x))

(function atom? (x)
  ; Returns true if the specified x is of type atom.
  ; It means x is cons or not.
  (not (cons? x)))

; list

(function list? (x)
  ; Returns true if the specified x is of type list.
  ; Same as (or (nil? x) (cons? x)).
  (or (nil? x) (cons? x)))

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

(function ->list (x)
  ; Returns the specified x if x is a list, otherwise returns x as a list.
  (if (list? x) x
      (list x)))

(function list->string (l delimiter)
  ; Returns a new string of the specified list elements joined together with of the specified delimiter.
  (ensure-argument (list? l) (string? delimiter))
  (reduce l (lambda (x y) (->string x delimiter y))))

(function list= (x y :key (test same?))
  ; Returns whether the result of comparing each element of the specified lists x and y with the specified function test is true.
  ; Always returns nil if x and y are different lengths.
  (ensure-argument (list? x) (list? y) (function? test))
  (while true
    (if (and (nil? x) (nil? y)) (return true)
        (or (nil? x) (nil? y)) (return nil)
        (test (car x) (car y)) (<- x (cdr x) y (cdr y))
        (return nil))))

(function nthcdr (l n)
  ; Get the the specified nth cons of the specified list l.
  ; If n is greater than the length of l, nil is returned.
  (ensure-argument (list? l) (unsigned-integer? n))
  (for (i 0) (< i n) (<- i (++ i))
    (<- l (cdr l)))
  l)

(function copy-list (l)
  ; Create and return a duplicate of the specified list l.
  ; It is shallow copy.
  (ensure-argument (list? l))
  (if (nil? l) nil
      (subseq l 0 (length l))))

(function last (l)
  ; Returns the last element of the specified list l.
  (ensure-argument (list? l))
  (car (last-cons l)))

(function butlast (l)
  ; Returns a list excluding the last element of the specified list l.
  (ensure-argument (list? l))
  (subseq l 0 (-- (length l))))

(function .. (s e :opt (step 1))
  ; Returns a list with the specified step increments from the specified integer s to the specified integer e.
  (ensure-argument (number? s) (number? e) (number? step) (/= step 0)
                   (or (and (< step 0) (>= s e)) (and (> step 0) (<= s e))))
  (let (acc nil test (if (> step 0) <= >=))
    (while (test s e)
      (push! acc s)
      (<- s (+ s step)))
    (reverse! acc)))

(function append-atom (l x)
  ; Returns a new list with the specified x appended to the end of the specified list l.
  (ensure-argument (list? l))
  (let (acc nil)
    (dolist (i l)
      (push! acc i))
    (push! acc x)
    (reverse! acc)))

(macro push! (sym x)
  ; Destructively add the specified element x to the top of the specified list that binds the specified symbol sym.
  ; Returns x.
  (ensure-argument (symbol? sym))
  (with-gensyms (y)
    (list let (list y x)
          (list <- sym (list cons y sym))
          y)))

(macro pop! (sym)
  ; Returns the head of the list that binds the specified symbol sym and rebinds sym with the cdr of the list.
  (ensure-argument (symbol? sym))
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))

(function flatten (l)
  ; Returns a new list in which the car parts of all cons that make up the specified list l are elements.
  (ensure-argument (list? l))
  (let (acc nil rec (lambda (x)
                      (if (atom? x) (push! acc x)
                          (dolist (i x) (rec i)))))
    (rec l)
    (reverse! acc)))

(function map (args f)
  ; Returns a list of the results of mapping each element of the specified list args with the specified function f.
  (ensure-argument (list? args) (function? f))
  (let (acc nil)
    (while args
      (push! acc (f (car args)))
      (<- args (cdr args)))
    (reverse! acc)))

(function reduce (l f :key (identity nil identity?))
  ; Reduce uses the specified binary operation f, to combine the elements of the specified list l.
  ; The function must accept as arguments two elements of list or the results from combining those elements.
  ; The function must also be able to accept no arguments.
  ; If the specified identity is supplied, used as a unit.
  (ensure-argument (list? l))
  (let (rec (lambda (l)
              (if (nil? (cdr l)) (car l)
                  (rec (cons (f (car l) (cadr l)) (cddr l))))))
    (rec (if identity? (cons identity l) l))))

(function find-cons (l e :key (test same?) (key identity))
  ; Among the cons that make up the specified list l, the cons whose car part is equal to the specified e is returned.
  ; Evaluation is performed in order from left to right.
  ; If there is no such cons, nil is returned.
  ; The comparison is done with the specified function test which default value is same?.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (ensure-argument (list? l) (function? test))
  (while true
    (if (nil? l) (return nil)
        (test (key (car l)) e) (return l)
        (<- l (cdr l)))))

(function find-cons-if (l f :key (key identity))
  ; Returns the cons that make up the specified list l that are not nil when the car part is evaluated as an argument of the specified function f.
  ; Evaluation is performed in order from left to right.
  ; If there is no such cons, nil is returned.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (ensure-argument (list? l) (function? f))
  (while l
    (if (f (key (car l))) (return l)
        (<- l (cdr l)))))

(function find (l e :key (test same?) (key identity))
  ; Returns an element equal to the specified element e from the beginning of the specified list l.
  ; Returns nil if e does not exist.
  ; The comparison is done with same? as a default.
  ; If test is supplied, the element is compared using the test function.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (ensure-argument (list? l))
  (car (find-cons l e :test test :key key)))

(function find-if (l f :key (key identity))
  ; From the beginning of the specified list l, the specified function f returns the first element that does not evaluate to nil.
  ; If no such element exists, nil is returned.
  ; If key is supplied, the element is evaluated with the key function at first and then compared.
  (ensure-argument (list? l))
  (car (find-cons-if l f :key key)))

(function all-satisfy? (l f)
  ; Returns true if all element of the specified list l returns a not nil value which evaluates as an argument to the specified function f.
  ; Otherwise returns nil.
  ; As soon as any element evaluates to nil, and returns nil without evaluating the remaining elements
  (ensure-argument (list? l) (function? f))
  (while l
    (if (f (car l)) (<- l (cdr l))
        (return nil)))
  true)

(function any-satisfy? (l f)
  ; Returns true if any element of the specified list l returns a not nil value which evaluated as an argument to the specified function f.
  ; Otherwise returns nil.
  ; It returns nil if l is empty.
  ; As soon as any element evaluates to not nil, and returns it without evaluating the remaining elements.
  (ensure-argument (list? l) (function? f))
  (while l
    (if (f (car l)) (return true)
        (<- l (cdr l)))))

(function each-adjacent-satisfy? (l f)
  ; Returns true if each adjacent element of the specified list l returns true when evaluated as an argument to the specified function f
  (ensure-argument (list? l) (function? f))
  (while true
    (if (nil? (cdr l)) (return true)
        (f (car l) (cadr l)) (<- l (cdr l))
        (return nil))))

; associated list

(function assoc (al k)
  ; Returns a value corresponding to the specified key k of the specified asoociate list al.
  ; Raises an exception if there is no value.
  (ensure-argument (list? al) (or (keyword? k) (symbol? k)))
  (while al
    (if (same? (car al) k) (return (cadr al))
        (<- al (cddr al))))
  (throw (.message (.new IllegalArgumentException)
                   (concat "property " (symbol->string k) " not found"))))

(function assoc! (al k v)
  ; Change the value corresponding to the specified key k in the specified association list al to the specified vlaue v.
  ; Raises an exception if there is no value.
  (ensure-argument (list? al) (or (keyword? k) (symbol? k)))
  (while al
    (if (same? (car al) k) (return (car! (cdr al) v))
        (<- al (cddr al))))
  (throw (.message (.new IllegalArgumentException)
                   (concat "property " (symbol->string k) " not found"))))

; ascii character code.

(function ascii-space? (c)
  ; Returns whether byte c can be considered a space character.
  (ensure-argument (number? c))
  (find '(0x09 0x0A 0x0D 0x20) c :test =))

(function ascii-alpha? (c)
  ; Returns whether byte c can be considered a alphabetic character.
  (ensure-argument (number? c))
  (or (<= 0x41 c 0x5A) (<= 0x61 c 0x7A)))

(function ascii-digit? (c)
  ; Returns whether byte c can be considered a digit character.
  (ensure-argument (number? c))
  (<= 0x30 c 0x39))

(function ascii-lower (c)
  ; Returns lowercase if byte c can be considered an alphabetic character, c otherwise.
  (ensure-argument (byte? c))
  (if (<= 0x41 c 0x5A) (+ c 0x20)
      c))

(function ascii-upper (c)
  ; Returns uppercase if byte c can be considered an alphabetic character, c otherwise.
  (ensure-argument (byte? c))
  (if (<= 0x61 c 0x7A) (- c 0x20)
      c))

(function ascii->digit (c :key (radix 10))
  ; Returns the numeric value when the specified byte c is regarded as the specified radix base character.
  ; Default radix is 10.
  (ensure-argument (byte? c))
  (let (n (if (ascii-digit? c) (- c 0x30)
              (ascii-alpha? c) (+ (- (ascii-lower c) 0x61) 10)))
    (if (and n (< n radix)) n
        (throw (.message (.new IllegalStateException) "illegal char")))))

; string

(function ->string (:rest args)
  ; Returns concatenated string which each of the specified args as string.
  (with-memory-stream (ms)
    (dolist (arg args)
      (if arg (simple-print arg ms)))))

'todo
(function string->list (s delimiter)
  (->list s))
; (function string->list (s delimiter)
;   ; Returns a list delimited by the specified delimiter.
;   (ensure-argument (string? s) (string? delimiter))
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

(function - (x :rest args)
  ; Returns the value of the specified x minus the sum of the specified args.
  ; If args is nil, return -x.
  (ensure-argument (number? x) (all-satisfy? args number?))
  (if (nil? args) (negated x)
      (+ x (negated (reduce args +)))))

(function negated (x)
  ; Returns the inverted value of the specified x's sign.
  (* x -1))

(function // (x y)
  ; Same as (truncate (/ x y))).
  (ensure-argument (number? x) (number? y))
  (truncate (/ x y)))

(function /= (x y)
  ; Same as (not (= x y))).
  (not (= x y)))

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
  (ensure-argument (number? x))
  (+ x 1))

(function -- (x)
  ; Returns the value of the specified number x - 1.
  (ensure-argument (number? x))
  (- x 1))

(function even? (x)
  ; Returns true if the specified integer x is even.
  (ensure-argument (integer? x))
  (= (mod x 2) 0))

(function odd? (x)
  ; Returns true if the specified integer x is odd
  (ensure-argument (integer? x))
  (not (even? x)))

(function plus? (x)
  ; Returns true if the specified number x is positive.
  (ensure-argument (number? x))
  (> x 0))

(function zero? (x)
  ; Same as (= x 0).
  (ensure-argument (number? x))
  (= x 0))

(function minus? (x)
  ; Returns true if the specified number x is negative.
  (ensure-argument (number? x))
  (< x 0))

(function byte? (x)
  ; Returns true if the specified x is integer and between 0 and 255.
  (and (integer? x ) (<= 0 x 255)))

(function unsigned-integer? (x)
  ; Returns true if the specified x is integer and zero or positive.
  (and (integer? x) (not (minus? x))))

(function max (:rest args)
  (ensure-argument (all-satisfy? args number?))
  (reduce args (lambda (x y) (if (> x y) x y))))

(function min (:rest args)
  (ensure-argument (all-satisfy? args number?))
  (reduce args (lambda (x y) (if (< x y) x y))))

; splay tree

(function splay-new (:opt (comparator
                            (lambda (k1 k2)
                              (- (address k1) (address k2)))))
  (cons comparator $splay-nil))

(function splay-top (splay)
  (cdr splay))

(function splay-top! (splay top)
  (cdr! splay top))

(function splay-comparator (splay)
  (car splay))

(function splay-node-new (k v l r)
  (cons (cons k l) (cons v r)))

(function splay-node-key (splay-node)
  (caar splay-node))

(function splay-node-key! (splay-node key)
  (car! (car splay-node) key))

(function splay-node-val (splay-node)
  (cadr splay-node))

(function splay-node-left (splay-node)
  (cdar splay-node))

(function splay-node-left! (splay-node val)
  (cdr! (car splay-node) val))

(function splay-node-right (splay-node)
  (cddr splay-node))

(function splay-node-right! (splay-node val)
  (cdr! (cdr splay-node) val))

(function splay-balance (splay k)
  (let (top (splay-top splay) cmp (splay-comparator splay) p nil q nil d 0)
    (splay-node-key! $splay-nil k)
    (splay-node-left! $splay-nil $splay-nil)
    (splay-node-right! $splay-nil $splay-nil)
    (while (/= (<- d (cmp k (splay-node-key top))) 0)
      (<- p top)
      (if (< d 0)
          (begin
            (<- q (splay-node-left p))
            (if (= (<- d (cmp k (splay-node-key q))) 0)
                (begin
                  (<- top q)
                  (splay-node-left! p (splay-node-right top))
                  (splay-node-right! top p)
                  (break))
                (< d 0)
                (begin
                  (<- top (splay-node-left q))
                  (splay-node-left! q (splay-node-right top))
                  (splay-node-right! top p))
                (begin
                  (<- top (splay-node-right q))
                  (splay-node-right! q (splay-node-left top))
                  (splay-node-left! top q)
                  (splay-node-left! p (splay-node-right top))
                  (splay-node-right! top p))))
          (begin
            (<- q (splay-node-right p))
            (if (= (<- d (cmp k (splay-node-key q))) 0)
                (begin
                  (<- top q)
                  (splay-node-right! p (splay-node-left top))
                  (splay-node-left! top p)
                  (break))
                (> d 0)
                (begin
                  (<- top (splay-node-right q))
                  (splay-node-right! q (splay-node-left top))
                  (splay-node-left! top p))
                (begin
                  (<- top (splay-node-left q))
                  (splay-node-left! q (splay-node-right top))
                  (splay-node-right! top q)
                  (splay-node-right! p (splay-node-left top))
                  (splay-node-left! top p))))))
    top))

(function splay-resume (top)
  (let (l (splay-node-left top) r (splay-node-right top) p nil)
    (if (same? l $splay-nil) (return r)
        (different? r $splay-nil)
        (begin (<- p l)
               (while (different? (splay-node-right p) $splay-nil)
                 (<- p (splay-node-right p)))
               (splay-node-right! p r)))
    l))

(function splay-add (splay k v)
  ; Associates the specified v with the specified k in the specified splay.
  ; Returns the v.
  (let (top (splay-balance splay k))
    (assert (same? top $splay-nil))
    (splay-top! splay (splay-node-new k v (splay-node-left $splay-nil)
                                      (splay-node-right $splay-nil)))
    v))

(function splay-find (splay k)
  ; Returns the value to which the specified k is associated in the specified splay.
  ; If not found, return nil.
  (let (top (splay-balance splay k))
    (if (same? top $splay-nil) (begin (splay-top! splay (splay-resume top))
                                      nil)
        (begin (splay-top! splay top)
               (splay-node-val top)))))

;; End node of splay.
(<- $splay-nil (splay-node-new nil nil nil nil))

; Paren object system

(<- $class (splay-new))

(function find-class (cls-sym)
  (ensure-argument (symbol? cls-sym))
  (let (cls (splay-find $class cls-sym))
    (if cls cls
        (throw (.message (.new IllegalStateException)
                         (concat "class " (symbol->string cls-sym)
                                  " not found"))))))

(function global-method-sym (cls-sym method-sym)
  (string->symbol (concat (symbol->string cls-sym)
                          (symbol->string method-sym))))

(function find-method (cls-sym method-sym)
  (ensure-argument (symbol? cls-sym) (symbol? method-sym))
  (let (m nil
        find-class-method
            (lambda (cls-sym)
              (let (gs (global-method-sym cls-sym method-sym))
                (if (bound? gs) (eval gs))))
        rec
            (lambda (cls-sym)
              (for (cls-sym cls-sym cls (find-class cls-sym)) cls-sym
                (<- cls-sym (nth cls 5) cls (find-class cls-sym))
                (if (<- m (find-class-method cls-sym)) (return m)
                    (dolist (feature (nth cls 7))
                      (if (<- m (find-class-method feature))
                          (return m)))))))
    (if (<- m (rec cls-sym)) m
        (throw (.message (.new IllegalStateException)
                         (concat "method " (symbol->string method-sym)
                                  " not found"))))))

(macro make-accessor (field)
  ; Create accessor for the specified field.
  ; If field name is 'xxx', create accessor &xxx.
  ; Works faster than method which defined with the method macro.
  (let (key (symbol->keyword field) f (string->symbol
                                        (concat "&" (symbol->string field))))
    (unless (bound? f)
      (with-gensyms (receiver val val?)
        (list 'function f (list receiver :opt (list val nil val?))
              :method
              (list 'ensure-argument (list 'object? receiver))
              (list if val? (list begin (list 'assoc! receiver key val)
                                  receiver)
                    (list 'assoc receiver key)))))))

(macro make-method-dispatcher (method-sym)
  (unless (bound? method-sym)
    (with-gensyms (receiver args)
      (list 'function method-sym (list receiver :rest args)
            :method
            (list 'ensure-argument (list object? receiver))
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
    (ensure-argument (all-satisfy? fields symbol?) (not (bound? cls-sym)))
    (list begin0
          (list quote cls-sym)
          (list <- cls-sym (list quote (list :class 'Class
                                             :symbol cls-sym
                                             :super (if (not Object?) super)
                                             :features features
                                             :fields fields)))
          (list 'splay-add '$class (list quote cls-sym) cls-sym)
          (cons begin
                (map fields (lambda (field) (list 'make-accessor field)))))))

(macro method (cls-sym method-sym args :rest body)
  (let (global-sym (global-method-sym cls-sym method-sym)
        quoted-gloval-sym (list quote global-sym)
        method-lambda (cons lambda (cons (cons 'self args) body)))
    (ensure-argument (find-class cls-sym) (not (bound? global-sym)))
    (list begin0
          quoted-gloval-sym
          (list 'make-method-dispatcher method-sym)
          (list <- global-sym method-lambda))))

(macro throw (o)
  (with-gensyms (e)
    (list let (list e o)
          (list ensure-argument (list 'object? e) (list 'is-a? e 'Throwable))
          (list '&stack-trace e (list 'call-stack))
          (list basic-throw o))))

(macro catch ((:rest handlers) :rest body)
  ; (catch ((Exception1 (e) ...)
  ;         (Exception2 (e) ...)
  ;         (Exception3 (e) ...))
  ;   ...)
  ; (basic-catch (lambda (gsym)
  ;                (ensure-argument (object? gsym) (is-a? Throwable))
  ;                (if (is-a gsym Exception1) ((lambda (e) ...) gsym)
  ;                    (is-a gsym Exception2) ((lambda (e) ...) gsym)
  ;                    (is-a gsym Exception3) ((lambda (e) ...) gsym)
  ;                    (throw gsym)))
  ;              ...)
  (with-gensyms (gargs)
    (let (if-clause nil)
      (push! if-clause if)
      (dolist (h handlers)
        (push! if-clause (list 'is-a? gargs (car h)))
        (push! if-clause (list (cons lambda (cons (cadr h) (cddr h))) gargs)))
      (push! if-clause (list 'throw gargs))
      (list basic-catch
            (list lambda (list gargs)
                  (list begin
                        (list 'ensure-argument
                              (list 'object? gargs)
                              (list 'is-a? gargs 'Throwable))
                        (reverse! if-clause)))
            (cons begin body)))))

(function object? (x)
  ; Returns true if the specified x is object.
  (and (list? x) (same? (car x) :class)))

(function is-a? (o cls)
  ; Returns true if the specified object o regarded as the specified class cls's instance.
  (ensure-argument (object? o) (object? cls) (same? (cadr cls) 'Class))
  (let (o-cls-sym (cadr o) cls-sym (assoc cls :symbol))
    (while o-cls-sym
      (if (same? o-cls-sym cls-sym) (return true)
          (<- o-cls-sym (assoc (find-class o-cls-sym) :super))))))

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

(method Object .toString ()
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

;; error, exception

(class Throwable ()
  ; The Throwable class is the superclass of all errors and exceptions.
  message stack-trace)

(method Throwable .message (:opt (message nil message?))
  ; Set the message.
  (if message? (&message self message) (&message self)))

(method Throwable .toString ()
  ; Returns a String representing the receiver.
  (let (class-name (symbol->string (&class self)) msg (.message self))
    (if msg (concat class-name " -- " msg)
        class-name)))

(method Throwable .stackTrace ()
  (&stack-trace self))

(method Throwable .printStackTrace ()
  (write-string (.toString self))
  (write-new-line)
  (dolist (x (.stackTrace self))
    (write-string "\tat: ") (print x)))

(method Throwable .printSimpleStackTrace ()
  (write-string (.toString self))
  (write-new-line)
  (for (i 0 st (.stackTrace self)) st (<- i (++ i) st (cdr st))
    (if (< i 15) (begin (write-string "\tat: ")
                        (simple-print (car st))
                        (write-new-line))
        (begin (write-string "\t\t...\n")
               (break)))))

(class Error (Throwable)
  ; An Error is a subclass of Throwable that indicates serious problems that a reasonable application should not try to catch.
  ; Most such errors are abnormal conditions. 
  )

(class Exception (Throwable)
  ; The class Exception and its subclasses are a form of Throwable that indicates conditions that a reasonable application might want to catch.
  )

(class IllegalArgumentException (Exception)
  ; Thrown to indicate that a method has been passed an illegal or inappropriate argument.
  )

(class IllegalStateException (Exception)
  ; Signals that a method has been invoked at an illegal or inappropriate time.
  ; In other words, application is not in an appropriate state for the requested operation.
  )

(class UnimplementedException (Exception)
  ; Exception that thrown when an abstract method is not implemented.
  )

(class QuitSignal (Error)
  ; Error that terminates the system.
  ; In principle, this Error is not caught and terminates Paren system itself.
  )

;; stream I/O

(class Stream ()
  ; Abstract class for reading and writing streams.
  )

(method Stream .readByte (:rest args)
  ; Read 1byte from stream.
  ; Returns -1 when the stream reaches the end.
  (throw (.new UnimplementedException)))

(method Stream .writeByte (:rest args)
  ; Write 1byte to stream.
  (throw (.new UnimplementedException)))

(method Stream .readChar ()
  ; Read 1character from stream.
  (let (encoding (dynamic $encoding))
    (if (same? encoding :UTF-8)
        (let (utf8-exception (.message (.new IllegalStateException)
                                       "illegal UTF-8")
              trail? (lambda (b) (= (bit-and b 0xC0) 0x80))
              ms (.new MemoryStream)
              b1 (.readByte self) b2 nil b3 nil b4 nil)
          (if (< b1 0) (return :EOF)
              (< b1 0x80) (.writeByte ms b1)
              (< b1 0xC2) (throw utf8-exception)
              (not (trail? (<- b2 (.readByte self)))) (throw utf8-exception)
              (< b1 0xE0) (begin (if (= (bit-and b1 0x3E) 0)
                                     (throw utf8-exception))
                                 (.writeByte (.writeByte ms b1) b2))
              (< b1 0xF0) (begin (<- b3 (.readByte self))
                                 (if (or (and (= b1 0xE0)
                                              (= (bit-and b2 0x20) 0))
                                         (not (trail? b3)))
                                     (throw utf8-exception))
                                 (.writeByte
                                   (.writeByte
                                     (.writeByte ms b1) b2) b3))
              (< b1 0xF8) (begin (<- b3 (.readByte self) b4 (.readByte self))
                                 (if (or (not (trail? b3))
                                         (not (trail? b4))
                                         (and (= b1 0xf0)
                                              (= (bit-and b2 0x30) 0)))
                                     (throw utf8-exception))
                                 (.writeByte
                                   (.writeByte
                                     (.writeByte
                                       (.writeByte ms b1) b2) b3) b4))
              (throw utf8-exception))
          (.toString ms))
        (throw (.message (.new IllegalStateException) "unsupport encoding")))))

(method Stream .readLine (:rest args)
  ; Read line.
  (throw (.new UnimplementedException)))

(method Stream .writeString (s)
  ; Write string to stream.
  (ensure-argument (string? s))
  (let (ba (->byte-array s))
    (dotimes (i (length ba))
      (.writeByte self (nth ba i))))
  self)

(method Stream .seek (:rest args)
  ; Move the read position on the stream to the specified offset.
  (throw (.new UnimplementedException)))

(method Stream .tell (:rest args)
  ; Returns the read position on the stream as a byte offset from the beginning.
  (throw (.new UnimplementedException)))

(class MemoryStream (Stream)
  ; A stream whose contents are held in memory.
  buf buf-size rdpos wrpos)

(method MemoryStream .init ()
  (let (buf-size 256)
    (&buf-size self buf-size)
    (&buf self (byte-array buf-size))
    (&rdpos self 0)
    (&wrpos self 0))
  self)

(method MemoryStream _extend (size)
  (ensure-argument (integer? size))
  (let (req (+ (&wrpos self) size) new-buf nil)
    (while (< (&buf-size self) req)
      (&buf-size self (* (&buf-size self) 2)))
    (<- new-buf (byte-array (&buf-size self)))
    (byte-array-copy (&buf self) 0 new-buf 0  (&wrpos self))
    (&buf self new-buf))
  self)

(method MemoryStream .writeByte (byte)
  (ensure-argument (byte? byte))
  (let (wrpos (&wrpos self))
    (unless (< wrpos (&buf-size self)) (_extend self 1))
    (nth! (&buf self) wrpos byte)
    (&wrpos self (++ wrpos))))

(method MemoryStream .readByte ()
  (let (rdpos (&rdpos self))
    (if (= rdpos (&wrpos self)) -1
        (begin0 (nth (&buf self) rdpos)
                (&rdpos self (++ rdpos))))))

(method MemoryStream .seek (offset)
  (ensure-argument (<= 0 offset (&wrpos self)))
  (&rdpos self offset))

(method MemoryStream .tell (offset)
  (&rdpos self))

(method MemoryStream .toString ()
  (let (pos (&wrpos self) str (byte-array pos))
    (if (= pos 0) ""
        (byte-array->string (byte-array-copy (&buf self) 0 str 0 pos)))))

(method MemoryStream .reset ()
  ; Empty the contents of the stream.
  (&rdpos self 0)
  (&wrpos self 0))

(class FileStream (Stream)
  ; Provides I/O functions for files.
  ; Construct with methods such as Path.openRead, Path.openWrite.
  ; It should not be construct by new.
  fp)

(method FileStream .init (:key fp)
  (ensure-argument fp)
  (&fp self fp))

(method FileStream .readByte ()
  (fgetc (&fp self)))

(method FileStream .readLine ()
  (fgets (&fp self)))

(method FileStream .writeByte (byte)
  (ensure-argument (byte? byte))
  (fputc byte (&fp self)))

(method FileStream .writeString(o)
  (ensure-argument (or (byte-array? o) (string? o)))
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
  (&files self (string->list (reduce files concat) "/"))
  self)

(method Path .parent ()
  ; Returns the parent path, or nil if this path does not have a parent.
  (&files (.init (.new Path)) (butlast (&files self))))

(method Path .resolve (:rest body)
  ; Resolve the given path against this path.
  (ensure-argument (all-satisfy? body string?))
  (&files (.new Path) (concat (&files self)
                              (string->list (reduce body concat) "/"))))

(method Path .fileName ()
  ; Resolve file name of receiver.
  (last (&files self)))

(method Path .toString ()
  (reduce (&files self) (lambda (acc rest) (concat acc "/" rest))))

(method Path _open (mode)
  (print (.toString self))
  (.init (.new FileStream) :fp (fopen (.toString self) mode)))

(method Path .openRead ()
  ; Returns a stream that reads the contents of the receiver.
  (_open self 0))

(method Path .openWrite ()
  ; Returns a stream to write to the contents of the receiver.
  (_open self 1))

(method Path .openAppend ()
  ; Returns a stream to append to the receiver's content.
  (_open self 2))

(method Path .openUpdate ()
  ; Returns a stream that updates the contents of the receiver.
  ; The read/write position is at the beginning of the file.
  ; The file size cannot be reduced.
  (_open self 3))

(class ByteAheadReader ()
  ; A one-character look-ahead reader.
  ; While prefetching one character at a time from a character string or Reader, if necessary, cut out a part as a token.
  ; Can be used as a syllable reader or lexical analyzer.
  stream next buf)

(method ByteAheadReader .init (:key string stream)
  (ensure-argument (or (nil? string) (string? string))
                   (or (nil? stream) (is-a? stream Stream)))
  (when string
    (<- stream (.writeString (.new MemoryStream) string)))
  (&stream self (or stream (dynamic $stdin)))
  (&next self (.readByte stream))
  (&buf self (.new MemoryStream))
  self)

(method ByteAheadReader .next ()
  ; Returns a pre-read Char type character.
  (&next self))

(method ByteAheadReader .eof? ()
  ; Returns true if eof reached.
  (= (&next self) -1))

(method ByteAheadReader .skip ()
  ; Skip next character and returns it.
  (if (.eof? self) (throw (.message (.new IllegalStateException) "EOF reached"))
      (begin0 (&next self)
              (&next self (.readByte (&stream self))))))

(method ByteAheadReader .get ()
  ; Append next character to token and returns it.
  (let (c (.skip self))
    (.put self c)
    c))

(method ByteAheadReader .put (b)
  ; Put the specified byte b to the end of the token regardless of the stream.
  (ensure-argument (byte? b))
  (.writeByte (&buf self) b))

(method ByteAheadReader .token ()
  ; Returns the token string currently cut out.
  (.toString (&buf self)))

(method ByteAheadReader .reset ()
  ; Reset token and returns self.
  (.reset (&buf self))
  self)

(method ByteAheadReader .skipSpace ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (and (not (.eof? self)) (ascii-space? (&next self)))
    (.skip self))
  self)

(class AheadReader (ByteAheadReader))

(method AheadReader .init (:key string stream)
  (ensure-argument (or (nil? string) (string? string))
                   (or (nil? stream) (is-a? stream Stream)))
  (when string
    (<- stream (.writeString (.new MemoryStream) string)))
  (&stream self (or stream (dynamic $stdin)))
  (&next self (.readChar stream))
  (&buf self (.new MemoryStream))
  self)

(method AheadReader .eof? ()
  (same? (&next self) :EOF))

(method AheadReader .skip ()
  (begin0 (&next self)
          (&next self (.readChar (&stream self)))))

(method AheadReader .put (s)
  (ensure-argument (string? s))
  (.writeString (&buf self) s))

(method AheadReader .skipSpace ()
  (while (and (not (.eof? self))
              (find '(" " "\r" "\n") (&next self) :test string=))
    (.skip self))
  self)

; Paren reader

(class ParenLexer (ByteAheadReader))

(method ParenLexer .lex ()
  ; Returns (token-type [token]).
  (let (space? (lambda (x) (and (byte? x) (ascii-space? x)))
        raise (lambda (message)
                (throw (.message (.new IllegalStateException) message)))
        identifierLead? (lambda ()
                          (let (c (&next self))
                            (or (find '(0x21 0x24 0x25 0x26 0x2A 0x2B 0x2D 0x2F
                                        0x3C 0x3D 0x3E 0x3F 0x5F 0x2E 0x5B 0x5D)
                                      c :test =)
                                (ascii-alpha? c))))
        identifierTrail? (lambda ()
                           (or (identifierLead?)
                               (ascii-digit? (&next self))))
        lex-string (lambda ()
                     (.skip self)
                     (while (/= 0x22 (&next self))
                       (if (.eof? self) (raise "string not closed")
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
                                                         (+
                                                           (* 16
                                                              (ascii->digit
                                                                (.skip self)
                                                                :radix 16))
                                                           (ascii->digit
                                                             (.skip self)
                                                             :radix 16)))
                                        (.put self c))))))
                     (.skip self)
                     (.token self))
        lex-symbol (lambda ()
                     (while (identifierTrail?) (.get self))
                     (string->symbol (.token self)))
        lex-keyword (lambda ()
                      (.skip self)
                      (while (identifierTrail?) (.get self))
                      (string->keyword (.token self)))
        lex-number (lambda (sign)
                     (let (radix 10 factor 0 val 0)
                       (while (ascii-digit? (&next self))
                         (<- val (+ (* val 10) (ascii->digit (.skip self)))))
                       (if (= (&next self) 0x78)
                           (begin (.skip self)
                                  (<- radix (if (= val 0) 16 val)
                                      val 0)
                                  (while (or (ascii-alpha? (&next self))
                                             (ascii-digit? (&next self)))
                                    (<- val (+ (* val radix)
                                               (ascii->digit (.skip self)
                                                            :radix radix)))))
                           (= (&next self) 0x2E)
                           (begin (.skip self)
                                  (<- factor 0.1)
                                  (while (ascii-digit? (&next self))
                                    (<- val (+ val (* factor (ascii->digit
                                                               (.skip self))))
                                        factor (/ factor 10)))))
                       (if (and (byte? sign) (= sign 0x2D)) (- val) val)))
        lex (lambda ()
              (.reset self)
              (let (sign nil next (&next self))
                (if (space? next) (begin
                                    (while (space? (&next self)) (.skip self))
                                    (lex))
                    (.eof? self) '(:EOF)
                    (= next 0x22) (list :string (lex-string))
                    (= next 0x27) (begin (.skip self) '(:quote))
                    (= next 0x28) (begin (.skip self) '(:open-paren))
                    (= next 0x29) (begin (.skip self) '(:close-paren))
                    (= next 0x3A) (list :keyword (lex-keyword))
                    (= next 0x3B) (begin
                                    (while (/= (&next self) 0x0A) (.skip self))
                                    (lex))
                    (begin (if (find '(0x2B 0x2D) next :test =)
                               (<- sign (.skip self)))
                           nil) :unreachable
                    (ascii-digit? (&next self)) (list :number (lex-number sign))
                    (or sign (identifierLead?)) (begin
                                                  (if sign (.put self sign))
                                                  (list :symbol (lex-symbol)))
                    (raise (concat (number->string (&next self))
                                    " illegal char"))))))
    (lex)))

(class ParenParser ()
  lexer)

(method ParenParser .init (:key string stream)
  (&lexer self (.init (.new ParenLexer) :string string :stream stream))
  self)

(method ParenParser .parse ()
  (let (token-type nil
        token nil
        scan (lambda ()
                (let (next (.lex (&lexer self)))
                  (<- token-type (car next)
                      token (cadr next))))
        parse (lambda () (scan) (parse-s))
        parse-s (lambda ()
                  (if (same? token-type :EOF) :EOF
                      (same? token-type :quote) (list quote (parse))
                      (same? token-type :open-paren) (parse-list)
                      (or (same? token-type :symbol)
                          (same? token-type :keyword)
                          (same? token-type :string)
                          (same? token-type :number)) token
                      (throw (.message (.new IllegalStateException)
                                       "syntax error"))))
        parse-list (lambda ()
                     (scan)
                     (if (same? token-type :close-paren) nil
                         (cons (parse-s) (parse-list)))))
    (parse)))

(function read-byte (:opt stream)
  ; Read 1byte from the specified stream.
  ; Returns -1 when the stream reaches the end.
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdin)))
    (.readByte stream)))

(function read-char (:opt stream)
  ; Read 1character from the specified stream.
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdin)))
    (.readChar stream)))

(function read-line (:opt stream)
  ; Read line from the specified stream.
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdin)))
    (.readLine stream)))

(function write-byte (byte :opt stream)
  ; Write 1byte to the specified stream.
  ; Returns byte.
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdout)))
    (.writeByte stream byte)
    byte))

(function write-new-line (:opt stream)
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdout)))
    (write-byte 0x0A stream)))

(function write-string (s :opt stream)
  ; Write the specified stirng s to the specified stream.
  (if stream (ensure-argument (is-a? stream Stream)))
  (let (stream (or stream (dynamic $stdout)))
    (.writeString stream s)))

(macro with-memory-stream ((ms :opt s) :rest body)
  ; Create memory stream context.
  ; If the specified string s supplied, memory stream initialize with s.
  ; (with-memory-stream (ms s)
  ;    expr1 expr2 ...)
  ; (let (ms (.new MemoryStream))
  ;    (if s (.writeString ms s))
  ;    expr1 expr2 ...
  ;    (.toString ms))
  (with-gensyms (g)
    (list let (list ms (list '.new 'MemoryStream) g s)
          (list if g (list '.writeString ms g))
          (cons begin body)
          (list '.toString ms))))

(function with-open-mode (sym gsym path mode body)
  (let (path (list if (list string? path) (list '.init '(.new Path) path)
                   (list and (list 'object? path)
                         (list 'is-a? path 'Path)) path
                   (list ensure-argument nil)))
    (list let (list gsym nil)
          (list unwind-protect
                (cons let (cons (list sym (list mode path))
                                (cons (list <- gsym sym)
                                      body)))
                (list if gsym (list '.close gsym))))))

(macro with-open-read ((in path) :rest body)
  (with-gensyms (stream)
    (with-open-mode in stream path '.openRead body)))

(macro with-open-write ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.openWrite body)))

(macro with-open-append ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.openAppend body)))

(macro with-open-update ((out path) :rest body)
  (with-gensyms (stream)
    (with-open-mode out stream path '.openUpdate body)))

(function read (:opt stream)
  ; Read expression from the specified stream.
  ; Returns :EOF if eof reached.
  (let (stream (or stream (dynamic $stdin)))
    (.parse (.init (.new ParenParser) :stream stream))))

(function simple-print (x :opt stream)
  ; Print the specified x as a simple format.
  ; Returns x;
  (let (stream (or stream (dynamic $stdout)))
    (let (print-s-expr (lambda (x)
                         (if (cons? x) (print-cons x)
                             (print-atom x)))
          print-cons (lambda (x)
                       (write-string "(" stream)
                       (for (i 0) x (<- i (++ i) x (cdr x))
                         (if (= i 3) (begin (write-string " ..." stream)
                                            (break))
                             (begin (if (/= i 0) (write-string " " stream))
                                    (print-s-expr (car x)))))
                       (write-string ")" stream))
          print-operator (lambda (x)
                           (write-string "(" stream)
                           (if (function? x) (write-string "lambda" stream)
                               (write-string "macro" stream))
                           (write-string " " stream)
                           (if (lambda-parameter x)
                               (print-cons (lambda-parameter x))
                               (write-string "()" stream))
                           (map (lambda-body x) (lambda (x)
                                                  (write-string " " stream)
                                                  (print-s-expr x)))
                           (write-string ")" stream))
          print-atom
              (lambda (x)
                (if (macro? x) (print-operator x)
                    (builtin? x) (print-atom (builtin-name x))
                    (function? x) (print-operator x)
                    (string? x) (write-string x stream)
                    (symbol? x) (write-string (symbol->string x) stream)
                    (keyword? x) (write-string (keyword->string x) stream)
                    (number? x) (write-string (number->string x) stream)
                    (assert nil))))
      (print-s-expr x)))
  x)

(function print (x :opt stream)
  ; Print the specified x as a readable format.
  (let (stream (or stream (dynamic $stdout)))
      (let (print-s-expr (lambda (x)
                           (if (cons? x) (print-cons x)
                               (print-atom x)))
            print-cons (lambda (x)
                         (write-string "(" stream)
                         (print-s-expr (car x))
                         (map (cdr x) (lambda (x) 
                                        (write-string " " stream)
                                        (print-s-expr x)))
                         (write-string ")" stream))
            print-operator (lambda (x)
                             (write-string "(")
                             (if (function? x) (write-string "lambda" stream)
                                 (write-string "macro" stream))
                             (write-string " " stream)
                             (if (lambda-parameter x)
                                 (print-cons (lambda-parameter x))
                                 (write-string "()" stream))
                             (map (lambda-body x) (lambda (x)
                                                    (write-string " " stream)
                                                    (print-s-expr x)))
                             (write-string ")" stream))
            print-atom
                (lambda (x)
                  (if (macro? x) (print-operator x)
                      (builtin? x) (print-atom (builtin-name x))
                      (function? x) (print-operator x)
                      (string? x) (begin (write-string "\"" stream)
                                         (write-string x stream)
                                         (write-string "\"" stream))
                      (symbol? x) (write-string (symbol->string x) stream)
                      (keyword? x) (write-string (keyword->string x) stream)
                      (number? x) (write-string (number->string x) stream)
                      (assert nil))))
        (print-s-expr x)
        (write-new-line stream)
        x)))

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
  (with-gensyms (g)
    (while true
      (catch ((QuitSignal (e) (break))
              (Exception (e) (.printSimpleStackTrace e)))
        (write-string ") ")
        (if (same? (<- g (read)) :EOF) (break))
        (print (eval g))))))

(function quit ()
  ; Quit the system.
  (throw (.new QuitSignal)))

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
      (begin0 (load (->string (keyword->symbol key) ".p"))
              (push! $import key))))

(function shell ()
  ; Start paren shell.
  (let (s nil)
    (while true
      (catch ((QuitSignal (e) (break))
              (Exception (e) (.printSimpleStackTrace e)))
        (write-string (.toString $paren-home))
        (write-string "> ")
        (let (expr nil)
          (with-memory-stream (out (with-memory-stream (in)
                                     (write-string (read-line) in)))
            (while (different? (<- s (read out)) :EOF)
              (push! expr s)))
          (print (eval (reverse! expr))))))))

; (let ($encoding :UTF-8)
;   (<- ar (.init (.new AheadReader) :string "あいう"))
;   (print (.get ar))
;   (print (.get ar))
;   (print (.get ar))
;   (print (.token ar)))

; (<- p (.init (.new Path) "." "test.wk"))
; (with-open-write (out p)
;   (write-string ":hello" out)
;   (write-string ":hello" out)
;   (write-string ":hello" out)
;   (write-string ":hello" out)
;   (write-string ":hello" out))
; (load "test.wk")

(function boot (args)
  (if (nil? args) (repl)
      (dolist (arg args)
        (load arg))))

; global symbol
(<- $import nil
    $paren-home (.init (.new Path) $paren-home)
    $stdin (.init (.new FileStream) :fp (fp 0))
    $stdout (.init (.new FileStream) :fp (fp 1))
    $encoding (if (same? $os :Windows) :CP932 :UTF-8)
    $support-encodings '(:UTF-8 :CP932))

(boot $args)
