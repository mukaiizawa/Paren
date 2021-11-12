; Paren core library.

;; special-operator(7).

(macro special-operator (name) name)
(special-operator <-)
(special-operator assert)
(special-operator begin)
(special-operator break)
(special-operator catch)
(special-operator continue)
(special-operator dynamic)
(special-operator f)
(special-operator if)
(special-operator let)
(special-operator loop)
(special-operator macro)
(special-operator quote)
(special-operator return)
(special-operator throw)
(special-operator unwind-protect)

;; fundamental macro.

(macro built-in-function (name args :rest body)
  (cons begin body))

(macro function! (name args :rest body)
  (list <- name (cons f (cons args body))))

(macro with-gensyms ((:rest syms) :rest body)
  (let (rec (f (syms) (if syms (cons (car syms) (cons '(symbol) (rec (cdr syms)))))))
    (cons let (cons (rec syms) body))))

(macro begin0 (:rest body)
  (with-gensyms (x)
    (list let (list x (car body))
          (cons begin (cdr body))
          x)))

(macro when (test :rest body)
  (list if test (cons begin body)))

(macro && (:rest args)
  (if (! args) true
      (! (cdr args)) (car args)
      (let (rec (f (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))

(macro || (:rest args)
  (if (! args) nil
      (! (cdr args)) (car args)
      (with-gensyms (g)
        (let (rec (f (l) (if l (cons (list <- g (car l)) (cons g (rec (cdr l)))))))
          (list let (list g nil)
                (cons if (rec args)))))))

(macro while (test :rest body)
  (list loop (list if test (cons begin body) '(break))))

(macro for (binding test update :rest body)
  (with-gensyms (gupdate?)
    (list let (cons gupdate? (cons nil binding))
          (list loop
                (list if gupdate? (cons <- update) (list <- gupdate? true))    ; for continue(3).
                (list if (list ! test) '(break))
                (cons begin body)))))

(macro dolist ((i l) :rest body)
  (with-gensyms (gl)
    (list for (list gl l i (list car gl)) gl (list gl (list cdr gl) i (list car gl))
          (cons begin body))))

(macro dotimes ((i n) :rest body)
  (with-gensyms (gn)
    (list for (list i 0 gn n) (list < i gn) (list i (list '++ i))
          (cons begin body))))

(macro dostring ((c s) :rest body)
  (list doarray (list c (list array s))
        (cons begin body)))

(macro doarray ((i a) :rest body)
  (with-gensyms (ga gi glen)
    (list for (list gi 0 ga a glen (list len ga)) (list < gi glen) (list gi (list '++ gi))
          (list let (list i (list [] ga gi))
                (cons begin body)))))

(macro timeit (:rest body)
  (with-gensyms (clock-offset cycle-offset)
    (list let (list clock-offset '(clock) cycle-offset '(cycle))
          (list 'begin0
                (cons begin body)
                (list 'write (list 'list
                                   :time (list '- '(clock) clock-offset)
                                   :cycle (list '- '(cycle) cycle-offset)))))))

(built-in-function macroexpand-1 (expr)
  (assert (== (car (macroexpand-1 '(begin0 1 2 3))) let)))

(function! macroexpand (expr :key ignore)
  (let (%find (f (x l) (if l (if (== x (car l)) true (%find x (cdr l)))))
              ignore? (f (x) (%find x ignore))
              add-ignore (f (x)
                           (if (cons? x) (begin (add-ignore (car x)) (add-ignore (cdr x)))
                               (<- ignore (cons x ignore)) x)
                           x)
              expand1 (f (x)
                        (if x (cons (macroexpand (car x) :ignore ignore)
                                    (expand1 (cdr x)))))
              expand2 (f (x) (if x (cons (add-ignore (car x))
                                         (cons (macroexpand (cadr x) :ignore ignore)
                                               (expand2 (cddr x)))))))
    (if (! (cons? expr)) expr
        (let ((ope :rest args) expr)
          (if (symbol? ope) (if (&& (! (ignore? ope)) (bound? ope)) (<- ope (eval ope))
                                (return (cons ope (expand1 args)))))    ; do not expand.
          (if (macro? ope) (macroexpand (macroexpand-1 expr) :ignore ignore)
              (cons ope
                    (if (== ope quote) args
                        (== ope <-) (expand2 args)
                        (== ope f) (cons (car args) (expand1 (cdr args)))
                        (== ope macro) (cons (car args) (cons (cadr args) (expand1 (cddr args))))
                        (|| (== ope let) (== ope catch)) (cons (expand2 (car args)) (expand1 (cdr args)))
                        (expand1 args))))))))

(macro function (name args :rest body)
  (with-gensyms (gname)
    (let (expand-body (f (x) (if x (cons (macroexpand (car x)) (expand-body (cdr x))))))
      (list let (list gname (list quote name))
            (list if (list bound? gname)
                  (list 'raise 'ArgumentError (list 'str "function name '" gname "` already bound"))
                  (list <- name (cons f (cons args (expand-body body)))))
            gname))))

;; fundamental function.

(built-in-function = (x y)
  (assert (= 1 1))
  (assert (= 1.0 1))
  (assert (= 1 1.0))
  (assert (! (= 10 20)))
  (assert (= 'x 'x))
  (assert (! (= 'x 'y))))

(built-in-function == (x y)
  (assert (== :x :x))
  (assert (! (== "x" "x"))))

(built-in-function ! (x)
  (assert (! (== 'x 'y)))
  (assert (! nil))
  (assert (== (! true) nil)))

(function != (x y)
  (! (= x y)))

(function !== (x y)
  (! (== x y)))

(built-in-function hash (x)
  (assert (= (hash 10.0) (hash 10)))
  (assert (= (hash "foo") (hash :foo)))
  (assert (= (hash :foo) (hash 'foo)))
  (assert (= (hash (array 1)) 0))
  (assert (= (hash (bytes 1)) 0))
  (assert (= (hash (dict)) 0))
  (assert (= (hash (cons nil nil)) 0)))

(built-in-function address (x)
  (assert (= (address 'x) (address 'x))))

;; function & macro.

(built-in-function function? (x)
  (assert (function? (f (x) x)))
  (assert (! (function? begin0))))

(built-in-function built-in? (x)
  (assert (built-in? f))
  (assert (built-in? +))
  (assert (! (built-in? built-in-function))))

(built-in-function special-operator? (x)
  (assert (special-operator? <-))
  (assert (! (special-operator? special-operator?))))

(built-in-function macro? (x)
  (assert (macro? begin0))
  (assert (! (macro? begin))))

(built-in-function params (proc)
  (assert (= (params (f (x y z) nil)) '(x y z)))
  (assert (= (params (f (x :opt y :rest z) nil)) '(x :opt y :rest z))))

(built-in-function body (proc)
  (assert (= (body (f () (list 1 2 3))) '((list 1 2 3)))))

;; list.

(function nil? (x)
  (! x))

(built-in-function cons? (x)
  (assert (cons? '(1)))
  (assert (! (cons? nil))))

(function atom? (x)
  (! (cons? x)))

(built-in-function cons (x y)
  (assert (= (cons 'x nil) '(x))))

(built-in-function car (x)
  (assert (= (car '(1 2 3)) 1))
  (assert (nil? (car '()))))

(built-in-function cdr (x)
  (assert (= (cdr '(1 2 3)) '(2 3)))
  (assert (nil? (cdr '()))))

(built-in-function car! (x v)
  (assert (let (x '(1 2 3)) (&& (== (car! x :one) :one) (= x '(:one 2 3))))))

(built-in-function cdr! (x v)
  (assert (let (x '(1 2 3)) (&& (= (cdr! x '(two)) '(two)) (= x '(1 two))))))

;;; cxr.
(function caar (x) (car (car x)))
(function cadr (x) (car (cdr x)))
(function cdar (x) (cdr (car x)))
(function cddr (x) (cdr (cdr x)))
(function caaar (x) (car (caar x)))
(function caadr (x) (car (cadr x)))
(function cadar (x) (car (cdar x)))
(function caddr (x) (car (cddr x)))
(function cdaar (x) (cdr (caar x)))
(function cdadr (x) (cdr (cadr x)))
(function cddar (x) (cdr (cdar x)))
(function cdddr (x) (cdr (cddr x)))
(function caaaar (x) (car (caaar x)))
(function caaadr (x) (car (caadr x)))
(function caadar (x) (car (cadar x)))
(function caaddr (x) (car (caddr x)))
(function cadaar (x) (car (cdaar x)))
(function cadadr (x) (car (cdadr x)))
(function caddar (x) (car (cddar x)))
(function cadddr (x) (car (cdddr x)))
(function cdaaar (x) (cdr (caaar x)))
(function cdaadr (x) (cdr (caadr x)))
(function cdadar (x) (cdr (cadar x)))
(function cdaddr (x) (cdr (caddr x)))
(function cddaar (x) (cdr (cdaar x)))
(function cddadr (x) (cdr (cdadr x)))
(function cdddar (x) (cdr (cddar x)))
(function cddddr (x) (cdr (cdddr x)))

(built-in-function list (:rest args)
  (assert (= (list 1 2 3) '(1 2 3)))
  (assert (nil? (list))))

(function list? (x)
  (if (cons? x) true
      (nil? x)))

(function ->list (x)
  (if (list? x) x
      (list x)))

(function join (l :opt separator)
  (if (nil? l) ""
      (nil? (cdr l)) (car l)
      (nil? separator) (apply memcat l)
      (with-memory-stream ($out)
        (write-bytes (car l))
        (dolist (x (cdr l)) (write-bytes separator) (write-bytes x)))))

(function split (s :opt separator)
  (if (empty? s) nil
      (nil? separator) (array->list (array s))
      (let (i 0 lis nil chars nil
              sa (array s) salen (len sa)
              da (array separator) dalen (len da) end (- salen dalen)
              match? (f ()
                       (dotimes (j dalen)
                         (if (!= ([] sa (+ i j)) ([] da j)) (return nil)))
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

(built-in-function last-cons (x)
  (assert (= (car (last-cons '(1 2 3))) 3))
  (assert (nil? (last-cons nil))))

(function assoc (alist key)
  (if (nil? alist) nil
      (= (car alist) key) (cadr alist)
      (assoc (cddr alist) key)))

(function .. (start :opt stop step)
  (let (rec (f (next stop step :opt acc)
              (if (< next stop) (rec (+ next step) stop step (cons next acc))
                  (reverse! acc))))
    (if (nil? stop) (<- stop start start 0 step 1)
        (nil? step) (<- step 1)
        (<= step 0) (raise IndexError "step must be positive integer"))
    (rec start stop step)))

(function group (l n)
  (let (rec (f (l acc)
              (if (nil? l) (reverse! acc)
                  (rec (slice l n) (cons (slice l 0 n) acc)))))
    (if (<= n 0) (raise IndexError "sublists length must be positive integer")
        (rec l nil))))

(function reverse (l)
  (let (rec (f (l acc)
              (if (nil? l) acc
                  (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))

(built-in-function reverse! (l)
  (assert (nil? (reverse! nil)))
  (assert (= (car (reverse! '(0 1))) 1)))

(macro push! (x l)
  (with-gensyms (gx)
    (list let (list gx x)
          (list <- l (list cons gx l))
          gx)))

(macro pop! (l)
  (list begin0
        (list car l)
        (list <- l (list cdr l))))

(function flatten (l)
  (let (acc nil rec (f (x)
                      (if (atom? x) (push! x acc)
                          (dolist (i x) (rec i)))))
    (rec l)
    (reverse! acc)))

;;; higher-order functions.

(function collect (fn)
  (let (rec (f (val :opt acc)
              (if (nil? val) (reverse! acc)
                  (rec (fn) (cons val acc)))))
    (rec (fn))))

(function map (fn args :rest more-args)
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
  (dolist (x args) (fn x)))

(function reduce (fn args)
  (if (cdr args) (reduce fn (cons (fn (car args) (cadr args)) (cddr args)))
      (car args)))

(function select (fn l)
  (let (rec (f (l acc)
              (if (nil? l) (reverse! acc)
                  (fn (car l)) (rec (cdr l) (cons (car l) acc))
                  (rec (cdr l) acc))))
    (rec l nil)))

(function reject (fn l)
  (let (rec (f (l acc)
              (if (nil? l) (reverse! acc)
                  (fn (car l)) (rec (cdr l) acc)
                  (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))

(function find (fn l)
  (&& l (|| (fn (car l)) (find fn (cdr l)))))

(function position (fn l)
  (let (rec (f (l n)
              (if (nil? l) nil
                  (fn (car l)) n
                  (rec (cdr l) (++ n)))))
    (rec l 0)))

(function every? (fn l)
  (if (nil? l) true
      (fn (car l)) (every? fn (cdr l))))

(function some? (fn l)
  (if (nil? l) nil
      (fn (car l)) true
      (some? fn (cdr l))))

(function none? (fn l)
  (if (nil? l) true
      (fn (car l)) nil
      (none? fn (cdr l))))

(function every-adjacent? (fn l)
  (if (cdr l) (&& (fn (car l) (cadr l)) (every-adjacent? fn (cdr l)))
      true))

;;; set operations.

(function union (X Y)
  (reduce (f (X y)
            (if (in? y X) X
                (cons y X)))
          (cons X Y)))

(function intersection (X Y)
  (select (f (x) (in? x Y))
          X))

(function difference (X Y)
  (reject (f (x) (in? x Y))
          X))

(function symmetric-difference (X Y)
  (union (difference X Y) (difference Y X)))

(function product (X Y)
  (apply concat
         (map (f (x) (map (f (y) (list x y)) Y))
              X)))

;; number.

(built-in-function number? (x)
  (assert (number? 1))
  (assert (number? 3.14))
  (assert (number? 0x20))
  (assert (! (number? 'x))))

(function byte? (x)
  (&& (int? x) (<= 0 x 255)))

(built-in-function int? (x)
  (assert (int? 1))
  (assert (! (int? 3.14)))
  (assert (! (int? 'x))))

(function int (x)
  (// (float x)))

(function int32 (x)
  (& 0xffffffff (int x)))

(function float (x)
  (if (nil? x) 0
      (number? x) x
      (string? x) (with-memory-stream ($in x)
                    (let (ar (.new AheadReader) val (.skip-unumber ar))
                      (if (.next ar) (raise ArgumentError "illegal string")
                          val)))
      (raise ArgumentError "expected number or string")))

(built-in-function + (x :rest args)
  (assert (= (+) 0))
  (assert (= (+ 1) 1))
  (assert (= (+ 1 2 3) 6))
  (assert (= (+ 1 2.0 3.0) 6)))

(function - (x :rest args)
  (if (nil? args) (* x -1)
      (+ x (- (apply + args)))))

(built-in-function * (x :rest args)
  (assert (= (*) 1))
  (assert (= (* 1 2 3) 6))
  (assert (= (* 1.0 2.0 3.0) 6))
  (assert (= (* 1 2.0 3.0) 6)))

(built-in-function / (x :rest args)
  (assert (= (/ 2) 0.5))
  (assert (= (/ 12 2 3) 2))
  (assert (= (/ 3 2 5) 0.3)))

(built-in-function // (x :opt y)
  (assert (= (// 3) 3))
  (assert (= (// 3.14) 3))
  (assert (= (// 2 1) 2))
  (assert (= (// 2 2) 1))
  (assert (= (// 2 3) 0)))

(built-in-function % (x y)
  (assert (= (% 4 5) 4))
  (assert (= (% 4 3) 1))
  (assert (= (% 4 2) 0)))

(function ++ (x)
  (+ x 1))

(function -- (x)
  (+ x -1))

;;;; bitwise operates.

(built-in-function ~ (x)
  (assert (= (~ (~ 2x1010)) 2x1010))
  (assert (= (& (~ 2x1010) 2x1111) 2x0101)))

(built-in-function & (x y)
  (assert (= (& 0x333333333 0x555555555) 0x111111111)))

(built-in-function | (x y)
  (assert (= (| 0x333333333 0x555555555) 0x777777777)))

(built-in-function ^ (x y)
  (assert (= (^ 3 0x500000000) 0x500000003))
  (assert (= (^ 0x500000000 0x500000003) 3)))

(built-in-function << (x y)
  (assert (= (<< 3 2) 12)))

(function >> (x y)
  (<< x (- y)))

(function abs (x)
  (if (< x 0) (- x)
      x))

(function average (:rest args)
  ; Returns the average value of the arguments.
  (/ (apply + args) (len args)))

;;;; mathematical functions.

(built-in-function sin (x)
  (assert (= (sin 0) 0)))

(built-in-function cos (x)
  (assert (= (cos 0) 1.0)))

(built-in-function tan (x)
  (assert (= (tan 0) (/ (sin 0) (cos 0)))))

(built-in-function asin (x)
  (assert (= (sin (asin 0.0)) 0)))

(built-in-function acos (x)
  (assert (= (cos (acos 0.0)) 0)))

(built-in-function atan (x)
  (assert (= (tan (atan 0.0)) 0.0)))

(built-in-function sinh (x)
  (assert (= (sinh 1) (/ (- (exp 1) (exp -1)) 2))))

(built-in-function cosh (x)
  (assert (= (cosh 1) (/ (+ (exp 1) (exp -1)) 2))))

(built-in-function tanh (x)
  (assert (= (tanh 1) (/ (sinh 1) (cosh 1)))))

(built-in-function exp (x)
  (assert (= (log (exp 10)) 10)))

(built-in-function log (x :opt y)
  (assert (= (log (pow 2 10)) (* 10 (log 2))))
  (assert (= (log 10 100) (/ (log 100) (log 10)))))

(built-in-function pow (x y)
  (assert (= (// (pow 2 10)) 1024)))

(built-in-function sqrt (x)
  (assert (= (sqrt (pow 25 2)) 25)))

;; symbol & keyword.

(built-in-function symbol (:opt x i size)
  ; If there are no arguments, returns numbered symbol starting with `$G-`.
  ; If x is supplied, Same as `(bytes x i size)` except returns symbol.
  (assert (== (symbol "foo") 'foo)))

(built-in-function keyword (x :opt i size)
  ; Same as `(bytes x i size)` except returns keyword.
  (assert (== (keyword "foo") :foo)))

(built-in-function symbol? (x)
  (assert (symbol? 'foo))
  (assert (! (symbol? :foo)))
  (assert (! (symbol? (bytes 3)))))

(built-in-function keyword? (x)
  (assert (keyword? :foo))
  (assert (! (keyword? 'foo)))
  (assert (! (keyword? (bytes 3)))))

(function symcmp (x y)
  ; If x is equals to y, returns 0.
  ; If the memory address of x is less than y, returns -1.
  ; If the memory address of x is greater than y, returns 1.
  (if (== x y) 0
      (- (address x) (address y))))

(built-in-function bound? (sym)
  ; Returns whether the x is bound.
  (assert (bound? 'bound?))
  (assert (bound? 'nil)))

;; string.

(built-in-function string (x :opt i size)
  ; Same as `(bytes x i size)` except returns string.
  (assert (= (string 'foo) "foo"))
  (assert (= (string 'foo 1) "oo"))
  (assert (= (string 'foo 1 1) "o")))

(built-in-function string! (x)
  ; Same as `(string x)` except that it destructively modifies the specified bytes x.
  ; Generally faster than mem->str.
  (assert (let (x (bytes 1))
            ([] x 0 0x01)
            (= (string! x) "\x01"))))

(function str (:rest args)
  ; Returns concatenated string which each of the specified args as string.
  ; Treat nil as an empty string.
  (with-memory-stream ($out)
    (dolist (arg args)
      (if (nil? arg) :continue
          (|| (symbol? arg) (keyword? arg) (string? arg) (bytes? arg)) (write-bytes arg)
          (write arg :end "")))))

(function bin (x)
  ; Returns a binary representation string of argument.
  (format "2x%b" x))

(function oct (x)
  ; Returns a octal representation string of argument.
  (format "8x%o" x))

(function hex (x)
  ; Returns a hexdecimal representation string of argument.
  ; If argument is bytes, returns hexdecimal dump.
  (if (bytes? x)
      (with-memory-stream ($out)
        (doarray (i x) (write-bytes (format "%02x" i))))
      (format "0x%x" x)))

(built-in-function string? (x)
  (assert (string? ""))
  (assert (string? "aaa"))
  (assert (! (string? (bytes 1)))))

(built-in-function chr (i)
  ; Returns an integer representing the Unicode code point of that character.
  (assert (= (chr 0x20) " "))
  (assert (= (chr 0x61) "a"))
  (assert (= (chr 0x376) "Ͷ"))
  (assert (= (chr 0x8056) "聖"))
  (assert (= (chr 0x611b) "愛"))
  (assert (= (chr 0x2123d) "𡈽")))

(built-in-function ord (ch)
  ; Returns the string representing a character whose Unicode code point is the integer i.
  (assert (= (ord " ") 0x20))
  (assert (= (ord "a") 0x61))
  (assert (= (ord "Ͷ") 0x376))
  (assert (= (ord "聖") 0x8056))
  (assert (= (ord "愛") 0x611b))
  (assert (= (ord "𡈽") 0x2123d)))

(function wcwidth (s)
  ; Returns the number of columns needed to represent the string.
  (let (width 0)
    (dostring (ch s)
      (<- width (+ width (if (print? ch) 1
                             (let (cp (ord ch))
                               (if (<= 0xff61 cp 0xffdf) 1    ; Halfwidth Katakana
                                   (<= 0x3000 cp 0xffe6) 2    ; Fullwidth characters
                                   0))))))
    width))

(built-in-function ascii? (s)
  ; Returns whether all characters in the string are ASCII.
  ; If s is empty, returns nil.
  (assert (ascii? "abc"))
  (assert (! (ascii? "あいう"))))

(built-in-function alnum? (s)
  ; Returns whether all characters in the string are alphanumeric.
  ; If s is empty, returns nil.
  (assert (alnum? "abc123"))
  (assert (! (alnum? " "))))

(built-in-function alpha? (s)
  ; Returns whether all characters in the string are alphabetic ASCII characters.
  ; If s is empty, returns nil.
  (assert (alpha? "abc"))
  (assert (! (alpha? "123"))))

(built-in-function digit? (s)
  ; Returns whether all characters in the string are ASCII decimal digits.
  ; If s is empty, returns nil.
  (assert (digit? "0123456789"))
  (assert (! (digit? "abc"))))

(built-in-function space? (s)
  ; Returns whether all characters in the string are whitespace.
  ; If s is empty, returns nil.
  (assert (space? " \t\r\n"))
  (assert (! (space? ""))))

(built-in-function print? (s)
  ; Returns whether all characters in the string are printable.
  ; If s is empty, returns nil.
  (assert (print? " "))
  (assert (! (print? "\e"))))

(built-in-function lower? (s)
  ; Returns whether all characters in the string are ASCII lowercase.
  ; If s is empty, returns nil.
  (assert (lower? "abc"))
  (assert (! (lower? "ABC"))))

(built-in-function upper? (s)
  ; Returns whether all characters in the string are ASCII uppercase.
  ; If s is empty, returns nil.
  (assert (upper? "ABC"))
  (assert (! (upper? "abc"))))

(built-in-function lower (s)
  ; Return a copy of the string with all the cased characters converted to lowercase.
  (assert (= (lower "ABC123") "abc123")))

(built-in-function upper (b)
  ; Return a copy of the string with all the cased characters converted to uppercase.
  (assert (= (upper "abc123") "ABC123")))

(function strstr (s pat :opt start)
  ; Returns the position where the substring pat appears first in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  ; If start is specified, search for substring pat from start-th of the string s.
  (let (start (|| start 0) sa (array s) slen (len sa) pa (array pat) plen (len pa))
    (if (< (- slen start) 0) (raise ArgumentError "illegal start")
        (= plen 0) (return 0))
    (for (i start end (- slen plen) p0 ([] pa 0)) (<= i end) (i (++ i))
      (when (= ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (= ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

(function strlstr (s pat)
  ; Returns the position where the substring pat appears last in the string s.
  ; If the string pat is not a substring of the string s, returns nil.
  (let (sa (array s) slen (len sa) pa (array pat) plen (len pa))
    (if (= plen 0) (return (-- slen)))
    (for (i (- slen plen) p0 ([] pa 0)) (>= i 0) (i (-- i))
      (when (= ([] sa i) p0)
        (if (= plen 1) (return i))
        (let (si (++ i) pi 1)
          (while (= ([] sa si) ([] pa pi))
            (<- si (++ si) pi (++ pi))
            (if (= pi plen) (return i))))))))

(function format (fmt :rest args)
  ; Returns formatted string whitch the string in the same way as C `sprintf`.
  ;     %[FLAG...][WIDTH][.[PRECISION]]CONV
  ;         FLAG -- an character, which modify the meaning of the conversion specification.
  ;             + -- always print a sign for numeric values
  ;             - -- pad with spaces on the right rather than the left (left-justify the field)
  ;             <space> -- leave a space for elided sign in numbers
  ;             0 -- pad with leading zeros rather than spaces
  ;         WIDTH -- an unsigned integer, which gives the minimum field width.
  ;         PRECISION -- an unsigned integer, which modifies the field width.
  ;             d, o, x -- gives the minimum number of digits.
  ;             e, f, g -- gives the number of digits to output after the radix.
  ;             v, s -- gives the maximum number of bytes to be printed from a string in the s conversion specifiers.
  ;         CONV -- the type of conversion
  ;             v -- converts any argument to readable format.
  ;             b -- converts an unsigned integers to unsigned binary format.
  ;             o -- converts an unsigned integers to unsigned octal format.
  ;             d -- converts a number to integer format.
  ;             x -- converts an unsigned integers to unsigned hexadecimal format.
  ;             f -- converts float to decimal notation.
  ;             c -- converts an unsigned integer to a character represented by the corresponding Unicode code point.
  ;             s -- converts bytes-like object to string.
  ;             % -- print a '%' character; no argument is converted.
  (with-memory-stream ($out)
    (with-memory-stream ($in fmt)
      (let (rd (.new AheadReader)
               write-times (f (ch n) (dotimes (_ n) (write-bytes ch)))
               substr (f (x n) (if (|| (nil? n) (> n (len x))) x (slice x 0 n)))
               format1 (f (flags width prefix val)
                         (let (padding-width (- width (wcwidth val) (wcwidth prefix)))
                           (if (in? "-" flags)
                               (begin
                                 (write-bytes prefix) (write-bytes val) (write-times " " padding-width))
                               (in? "0" flags)
                               (begin
                                 (write-bytes prefix) (write-times "0" padding-width) (write-bytes val))
                               (begin
                                 (write-times " " padding-width) (write-bytes prefix) (write-bytes val))))))
        (while (.next rd)
          (if (!= (.next rd) "%") (write-bytes (.skip rd))
              (begin
                (.skip rd)
                (if (= (.next rd) "%") (write-bytes (.skip rd))
                    (nil? args) (raise ArgumentError "too few arguments")
                    (let (flags (collect (f () (if (in? (.next rd) '("+" "-" " " "0")) (.skip rd))))
                                width (if (digit? (.next rd)) (.skip-uint rd) 0)
                                precision (if (= (.next rd) ".") (begin (.skip rd) (if (digit? (.next rd)) (.skip-uint rd) 0)))
                                conv (.skip rd)
                                x (car args))
                      (if (= conv "s") (format1 flags width "" (substr x precision))
                          (= conv "v") (format1 flags width "" (substr (with-memory-stream ($out) (write x :end "")) precision))
                          (= conv "c") (format1 flags width "" (chr x))
                          (in? conv '("b" "o" "d" "x" "f" "e" "g"))
                          (let (prefix (if (< x 0) (begin (<- x (- x)) "-")
                                           (in? "+" flags) "+"
                                           (in? " " flags) " "
                                           "")
                                       val (with-memory-stream ($out)
                                             (if (in? conv '("e" "f" "g")) (.write-float $out x :precision precision :style (keyword conv))
                                                 (.write-int $out (// x) :radix (assoc '("b" 2 "o" 8 "d" 10 "x" 16) conv) :padding precision))))
                            (format1 flags width prefix val))
                          (raise ArgumentError (str "unexpected conversion specifier " conv)))))
                (<- args (cdr args)))))))))

;; bytes & bytes-like.

(built-in-function memlen (x)
  ; Returns byte length of bytes-like object x.
  (assert (= (memlen "foo") 3)))

(function prefix? (x prefix)
  ; Returns whether the byte sequence x with the specified prefix.
  (&& (>= (memlen x) (memlen prefix))
      (memmem x prefix 0 (memlen prefix))))

(function suffix? (x suffix)
  ; Returns whether the byte sequence x with the specified suffix.
  (&& (>= (memlen x) (memlen suffix))
      (memmem x suffix (- (memlen x) (memlen suffix)))))

(built-in-function memmem (x b :opt start end)
  ; Returns the position where the byte b appears first in the byte sequence x.
  ; If the b is not appeared, returns nil.
  ; If b is byte sequence, returns the position where the partial byte sequence appears first in the byte sequence x.
  ; If start is specified, search from start-th of the byte sequence x.
  ; If end is specified, search untile end-th of the byte sequence x.
  (assert (= (memmem "012" 0x31 1) 1))
  (assert (= (memmem "012" 0x31 0 3) 1))
  (assert (= (memmem "012" 0x31 0 3) 1))
  (assert (= (memmem "012" "12" 0 3) 1)))

(built-in-function memcpy (src src-i dst dst-i size)
  ; Copy size elements from the `src-i`th element of the src byte sequence to the dst byte sequence `dst-i`th element and beyond.
  ; Even if the areas to be copied overlap, it operates correctly.
  ; Returns dst.
  (assert (let (s (bytes "foo") d (bytes "bar"))
            (= (string (memcpy s 1 d 1 2)) "boo"))))

(built-in-function memcat (x :rest args)
  ; Returns the result of combining each args with x.
  (assert (= (memcat "0" "1" "2") "012")))

(built-in-function memcmp (x y)
  ; If x is equals to y, returns 0.
  ; If x is lexicographically less than y, returns -1.
  ; If x is lexicographically greater than y, returns 1.
  (assert (= (memcmp "bar" "foo") -1))
  (assert (= (memcmp "foo" "bar") 1))
  (assert (= (memcmp "foo" "foo") 0))
  (assert (= (memcmp "fo" "foo") -1))
  (assert (= (memcmp "foo" "fo") 1)))

(built-in-function bytes (bytes/size :opt i size)
  ; If the first argument is an integer, returns a bytes of size the specified size.
  ; The element is cleared to 0.
  ; If the first argument is a byte sequence object, returns bytes corresponding to byte sequence x.
  ; If i is supplied, returns bytes of partial byte sequence from i of x.
  ; If size is supplied, returns string of partial byte sequence from i to (size -1) of x.
  (assert (= (len (bytes 1)) 1))
  (assert (= ([] (bytes 1) 0) 0)))

(built-in-function bytes? (x)
  (assert (bytes? (bytes 3)))
  (assert (! (bytes? 'foo)))
  (assert (! (bytes? :foo)))
  (assert (! (bytes? "foo")))
  (assert (! (bytes? (array 3)))))

;; array.

(built-in-function array (x)
  ; Returns an array.
  ; If the argument x is a number, returns an array initialized with nil of size x.
  ; If the argument x is a sequence, returns the corresponding array.
  (assert (= ([] (array 1) 0) nil))
  (assert (= ([] (array (array 1)) 0) nil))
  (assert (= ([] (array "foo") 1) "o"))
  (assert (= ([] (array '(foo bar buzz)) 1) 'bar)))

(built-in-function array? (x)
  (assert (array? (array 3)))
  (assert (! (array? (bytes 3)))))

(function array->list (x)
  ; Returns a list containing all of the elements in x.
  (let (rec (f (i acc)
              (if (< i 0) acc
                  (rec (-- i) (cons ([] x i) acc)))))
    (rec (-- (len x)) nil)))

;; dictionary.

(built-in-function dict ()
  ; Returns an empty dictionary.
  )

(built-in-function dict? (x)
  (assert (dict? (dict)))
  (assert (! (dict? (array 1)))))

(built-in-function keys (d)
  ; Returns a list of keys contained in this dictionary.
  (assert (let (d (dict))
            (&& (nil? (keys d))
                (= ([] d :foo 'foo) 'foo)
                (= (keys d) '(:foo))))))

;; sequence

(built-in-function concat (:rest args)
  ; Returns a sequence of concatenated arguments.
  (assert (nil? (concat)))
  (assert (nil? (concat nil)))
  (assert (nil? (concat nil nil)))
  (assert (= (concat '(1 2) '(3)) '(1 2 3)))
  (assert (= (concat '(1) '(2)) '(1 2)))
  (assert (= (concat nil '(1) '(2)) '(1 2)))
  (assert (= (concat "0" "1" "2") "012"))
  (assert (= (concat (bytes 1) (bytes 2)) (bytes 3)))
  (assert (= (concat (array 1) (array 2)) (array 3))))

(built-in-function slice (seq :opt start stop)
  ; Returns a subsequence of sequence x.
  ; If start is omitted, it defaults to 0.
  ; If stop is omitted, it defaults to `(len seq)`.
  (assert (= (slice nil) nil))
  (assert (= (slice nil 0) nil))
  (assert (= (slice nil 0 1) nil))
  (assert (let (lis '(0 1 2))
            (= (slice lis) lis)
            (!== (slice lis) lis) ; guarantee to be copied.
            (= (slice lis 0) lis)
            (= (slice lis 2) '(2))
            (= (slice lis 2 2) nil)
            (= (slice lis 4) nil)
            (= (slice lis 0 2) '(0 1))))
  (assert (let (s "abc")
            (= (slice s ) s)
            (= (slice s 0) "bc")
            (= (slice s 0 0) "")
            (= (slice s 0 2) "ab"))))

(function swap! (seq i j)
  ; Swap the i-th and j-th elements of the sequence seq.
  ; Argument sequence changes destructively.
  ; Returns seq.
  (let (t ([] seq i))
    ([] seq i ([] seq j))
    ([] seq j t)
    seq))

(function sort! (seq :key sorter key start end)
  ; Sort the sequence seq.
  ; Argument sequence changes destructively.
  ; If you want to keep the original sequence, copy it like `(sort! (slice seq))`.
  ; Returns sorted sequence.
  (let (sort-range!
         (f (seq start end)
           (let (i start j end x (key ([] seq start)))
             (loop
               (while (&& (< i end) (sorter (key ([] seq i)) x)) (<- i (++ i)))
               (while (&& (>= j start) (sorter x (key ([] seq j)))) (<- j (-- j)))
               (if (< i j)
                   (begin
                     (swap! seq i j)
                     (<- i (++ i) j (-- j)))
                   (break)))
             (if (< start (<- i (-- i))) (sort-range! seq start i))
             (if (< (<- j (++ j)) end) (sort-range! seq j end)))))
    (if (nil? sorter) (<- sorter <))
    (if (nil? key) (<- key (f (x) x)))
    (sort-range! seq (|| start 0) (-- (|| end (len seq))))
    seq))

(function first (x)
  ; Same as `(car (last-cons x))`.
  ([] x 0))

(function last (x)
  ; Same as `(car (last-cons x))`.
  (if (list? x) (car (last-cons x))
      ([] x (-- (len x)))))

(function butlast (x)
  ; Returns a list excluding the last element of the specified list l.
  (slice x 0 (-- (len x))))

;; collection

(built-in-function [] (collection key :opt val)
  ; Returns the value corresponding to key of the collection.
  ; If val is specified, update the value corresponding to key.
  ; If the argument collection is a list and the index key is is out of range, returns nil.
  ; Error if the argument collection is not a list and the index key is out of range.
  ; Error if collection is an immutable collection and val is specified.
  (assert (= ([] '(0 1 2) 0) 0))
  (assert (= ([] (array 1) 0) nil))
  (assert (= ([] "foo" 0) "f"))
  (assert (= ([] (bytes 1) 0) 0))
  (assert (let (a (array 1) b (bytes 1))
            (&& ([] a 0 true)
                ([] a 0)
                ([] b 0 0xff)
                (= ([] b 0) 0xff)))))

(built-in-function in? (x collection)
  ; Returns whether element x exists in the collection.
  (assert (in? 1 '(1 2 3)))
  (assert (! (in? 0 '(1 2 3))))
  (assert (in? 0x00 (bytes 1)))
  (assert (! (in? 0x01 (bytes 1))))
  (assert (in? "foo" "xfoox"))
  (assert (! (in? "foo" "xbarx")))
  (assert (in? nil (array 1)))
  (assert (! (in? true (array 1))))
  (assert (let (d (dict)) ([] d nil nil) (in? nil d)))
  (assert (! (in? nil (dict)))))

(function index (collection x)
  (if (dict? collection)
      (dolist (key (keys collection))
        (if (= ([] collection key) x) (return key)))
      (list? collection)
      (position (f (y) (= x y)) collection)
      (|| (string? collection) (array? collection))
      (dotimes (i (len collection))
        (if (= ([] collection i) x) (return i)))
      (bytes? collection) (memmem collection x)
      (raise ArgumentError "expected collection")))

(built-in-function len (collection)
  ; Returns the length of the collection.
  (assert (= (len nil) 0))
  (assert (= (len '(1)) 1))
  (assert (= (len (array 1)) 1))
  (assert (= (len (let (d (dict)) ([] d :x 1) d)) 1))
  (assert (= (len "αβγ") 3)))

(function empty? (collection)
  ; Returns whether the collection is zero-length or nil.
  (|| (nil? collection) (= (len collection) 0)))

;; comparable.

(built-in-function < (:rest args)
  ; Returns whether the each of the specified args are in monotonically decreasing order.
  (assert (< 0 1 2))
  (assert (< 0 1.0 2))
  (assert (! (< 0 0 1)))
  (assert (! (< :foo :foo)))
  (assert (< :f :fo :foo))
  (assert (! (< :foo :fo :f)))
  (assert (< "あ" "い" "う"))
  (assert (< "abcあ" "abcい" "abcいい"))
  (assert (! (< "あいう" "あい" "あ"))))

(function > (:rest args)
  ; Returns whether the each of the specified args are in monotonically increasing order.
  (every-adjacent? (f (x y) (< y x)) args))

(function <= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nondecreasing order.
  (every-adjacent? (f (x y) (! (< y x))) args))

(function >= (:rest args)
  ; Returns whether the each of the specified args are in monotonically nonincreasing order.
  (every-adjacent? (f (x y) (! (< x y))) args))

(function max (:rest args)
  ; Returns maximum value from argument.
  (reduce (f (x y) (if (> x y) x y)) args))

(function min (:rest args)
  ; Returns minimum value from argument.
  (reduce (f (x y) (if (< x y) x y)) args))

;; os.

(built-in-function fp (fd)
  ; Returns the file pointer associated with the file descriptor.
  ; The argument fd can specify bellow value.
  ;      0 -- stdin
  ;      1 -- stdout
  ;      2 -- stderr
  )

(built-in-function fopen (filename mode)
  ; Opens the file whose name is the string pointed to by filename and associates a stream with it.
  ; Returns file poiner for the opened file.
  ; The argument mode can specify bellow value.
  ;      0 -- Open file for reading.
  ;      1 -- Open file for writing.
  ;      2 -- Open file for appending
  ;      3 -- Open file for reading and writing.
  )

(built-in-function fgetc (fp)
  ; Read byte from the stream associated with the file pointer fp.
  ; Returns read byte.
  ; If stream reached EOF, returns -1.
  )

(built-in-function fputc (c fp)
  ; Write the byte specified by c to the output stream pointed to by fp.
  ; Returns written byte.
  )

(built-in-function fgets (fp)
  ; Read a line from the steream pointed to by fp and return it.
  ; Do not include newline characters.
  ; Returns nil if EOF.
  )

(built-in-function fread (buf from size fp)
  ; Reads size bytes of data from the stream pointed to by fp, storing them at the location given by bytes buf offset from.
  ; Returns size;
  )

(built-in-function fwrite (buf from size fp)
  ; Writes size bytes of data to the stream pointed to by fp, obtaining them at the location given by bytes buf offset from.
  ; Returns size;
  )

(built-in-function fseek (fp)
  ; Sets the file position indicator for the stream pointed to by fp
  ; Returns nil.
  )

(built-in-function ftell (fp)
  ; Returns the current value of the file position indicator for the stream pointed to by fp.
  )

(built-in-function fclose (fp)
  ; Flushes the stream pointed to by fp (writing any buffered output data) and closes the underlying file descriptor.
  ; Returns nil.
  )

(built-in-function stat (filename)
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

(built-in-function utime (filename unix-time)
  ; Change the access time and modification time of the file indicated filename to the specified unix-time in times.
  ; Returns nil.
  )

(built-in-function getcwd ()
  ; Returns a string containing the absolute filename of the current working directory.
  )

(built-in-function chdir (filename)
  ; Change the current working directory to the directory specified in filename
  ; Returns nil.
  )

(built-in-function readdir (filename)
  ; Return the contents of the directory indicated by filename as a character string delimited by a newline character.
  )

(built-in-function remove (filename)
  ; Attempts to remove a file whose name is pathname.
  ; Returns nil.
  )

(built-in-function mkdir (filename)
  ; Attempts to create a directory whose name is pathname.
  ; Error if filename already exists.
  ; Returns nil.
  )

(built-in-function rename (src dst)
  ; Rename the file and move between directories if necessary.
  ; Returns nil.
  )

(built-in-function time ()
  ; Returns the number of seconds relative to the unix epoch (January 1, 1970, 00:00:00 UTC).
  )

(built-in-function clock ()
  ; Returns the approximate processor time[sec] used by the program.
  )

(built-in-function cycle ()
  ; Returns the cycle of the internal virtual machine.
  )

(built-in-function utcoffset ()
  ; Returns the difference in hours and minutes from Coordinated Universal Time (UTC) for time zone set in the host system.
  )

(built-in-function sleep (sec)
  ; Sleep for a specified number of seconds.
  ; Returns nil.
  )

(built-in-function system (command)
  ; Execute host system commands.
  ; Returns the termination status of the child shell used to execute command.
  )

(built-in-function getenv (name)
  ; Looks up the environment variable named name in the environment list and returns value string.
  ; Returns nil if not found.
  )

(built-in-function putenv (name value)
  ; Add environment variables or change values.
  ; If name does not exist in the environment, name-value is added to the environment.
  ; If name exists in the environment, the value of name is changed to value.
  ; Returns nil.
  )

;; Paren object system.

(function object? (x)
  ; Returns whether x is an object in the Paren object system.
  ; Same as `(&& (dict? x) ([] x :class))`.
  (&& (dict? x) ([] x :class)))

(built-in-function is-a? (o cls)
  ; Returns whether the specified object o regarded as the specified class cls's instance.
  )

(built-in-function find-class (cls-sym)
  ; Returns the class corresponding to the specified symbol cls-sym.
  ; If cls-sym is not bound or cls-sym is not a class instance, returns nil.
  )

(built-in-function find-method (cls-sym method-sym)
  ; Returns the method by which an instance of the class name cls-sym is dispatched.
  ; Error If the class or method is undefined.
  )

(macro make-accessor (field)
  ; Returns an expression that binds getter and setter.
  ; If field name is 'xxx', bind a getter named `&xxx` and setter named `&xxx!`.
  ; Works faster than method which defined with the `method` macro.
  (with-gensyms (receiver val)
    (let (key (keyword field) getter (memcat '& field) setter (memcat getter '!))
      (list begin
            (list if (list ! (list bound? (list quote getter)))
                  (list function getter (list receiver)
                        (list [] receiver key)))
            (list if (list ! (list bound? (list quote setter)))
                  (list function setter (list receiver val)
                        (list [] receiver key val)
                        receiver))))))

(macro make-method-dispatcher (method-sym)
  (when (! (bound? method-sym))
    (with-gensyms (receiver args)
      (list function method-sym (list receiver :rest args)
            (list apply
                  (list find-method (list [] receiver :class) (list quote method-sym))
                  (list cons receiver args))))))

(macro class (cls-sym (:opt super :rest features) :rest fields)
  ; Define class.
  ; Returns class symbol.
  (with-gensyms (gcls-sym gsuper gfeatures gfields)
    (list let (list gcls-sym (list quote cls-sym)
                    gsuper (list quote super)
                    gfeatures (list quote features)
                    gfields (list quote fields))
          (list if
                (list bound? gcls-sym)
                (list 'raise 'ArgumentError (list str "class name '" gcls-sym "` already bound"))
                (list ! (list bound? gsuper))
                (list 'raise 'ArgumentError (list str "super class '" gsuper "` is not defined"))
                (list ! (list every? symbol? gfeatures))
                (list 'raise 'ArgumentError (list str "features classes " gfeatures " must be symbol"))
                (list ! (list every? bound? gfeatures))
                (list 'raise 'ArgumentError (list str "some of the features " gfeatures " is not defined"))
                (list ! (list every? symbol? gfields))
                (list 'raise (list str "instance variables " gfields " must be symbol")))
          (list <- cls-sym '(dict))
          (cons begin
                (map (f (k v) (list [] cls-sym k v))
                     '(:class :symbol :super :features :fields)
                     (list ''Class
                           gcls-sym
                           (list || gsuper (list if (list != gcls-sym ''Object) ''Object))
                           gfeatures
                           gfields)))
          (cons begin
                (map (f (field) (list make-accessor field))
                     fields))
          gcls-sym)))

(macro method (cls-sym method-sym args :rest body)
  ; Define method.
  ; Returns method symbol.
  (let (global-method-name (memcat cls-sym method-sym))
    (list begin
          (list if
                (list ! (list find-class (list quote cls-sym)))
                (list 'raise 'ArgumentError (list str "class '" (list quote cls-sym) "` is not defined"))
                (list bound? (list quote global-method-name))
                (list 'raise 'ArgumentError (list str "method '" (list quote global-method-name) "` already bound")))
          (list make-method-dispatcher method-sym)
          (cons function (cons global-method-name (cons (cons 'self args) body))))))

(macro overload (method-sym sym)
  ; Overload operator.
  (with-gensyms (gsym gargs garg)
    (list let (list gsym sym)
          (list function! sym (list :rest gargs)
                (list let (list garg (list car gargs))
                      (list if (list object? garg)
                            (list apply (list find-method (list [] garg :class) (list quote method-sym)) gargs)
                            (list apply gsym gargs)))))))

;;; fundamental class.

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
  (== self o))

(method Object .hash (o)
  ; Returns hash value of the receiver.
  ; Overwrite this method if there is class-specific comparisons.
  0)

(method Object .to-s ()
  ; Returns a String representing the receiver.
  (str "<" (&symbol (.class self)) ":0x" (address self)  ">"))

(class Class ()
  ; Class of class object.
  ; All class objects are instances of Class class.
  symbol super features fields methods)

(method Class .new ()
  ; Construct an instance.
  ; If .init method has argument, must invoke after create an instance.
  ; Otherwise automatically invoke .init method.
  (let (o (dict))
    (for (cls self) cls (cls (find-class ([] cls :super)))
      (foreach (f (x) ([] o (keyword x) nil))
               ([] cls :fields)))
    ([] o :class ([] self :symbol))
    (if (= (params (find-method ([] o :class) '.init)) '(self)) (.init o)
        o)))

(method Class .symbol ()
  ; Returns the class symbol.
  (&symbol self))

(method Class .super ()
  ; Returns the class representing the superclass of the receiver.
  (find-class (&super self)))

(method Class .features ()
  ; Returns the feature list representing the feature of the receiver.
  (map find-class (&features self)))

(class Exception ()
  ; The Exception class is the superclass of all exceptions.
  ; It is recommended that new exceptions derive from the Error class or one of its subclasses.
  ; Do not derive from Exception.
  message status-cd stack-trace)

(method Exception .init (message :key status-cd)
  (&message! self message)
  (&status-cd! self (|| status-cd 1)))

(method Exception .to-s ()
  ; Returns a String representing the receiver.
  (let (class-name (string (&class self)) msg (&message self))
    (if msg (memcat class-name " -- " msg)
        class-name)))

(method Exception .status-cd ()
  (&status-cd self))

(method Exception .stack-trace ()
  ; Returns stack trace.
  (&stack-trace self))

(method Exception .print-stack-trace ()
  ; Display stack trace.
  ; Returns nil.
  (let ($out $stderr)
    (write-bytes (.to-s self))
    (write-line)
    (dolist (x (.stack-trace self))
      (write-bytes "\tat: ") (write x))))

(class SystemExit (Exception)
  ; Dispatched to shut down the Paren system.
  ; In principle, this exception is not caught.
  )

(method SystemExit .init ()
  (&status-cd! self 0))

(class Error (Exception)
  ; All built-in, non-system-exiting exceptions are derived from this class.
  ; All user-defined exceptions should also be derived from this class.
  )

(class ArgumentError (Error)
  ; Raised when an operation or function is applied to an object of inappropriate argument.
  )

(class IndexError (ArgumentError)
  ; Raised when a sequence subscript is out of range.
  )

(class ArithmeticError (ArgumentError)
  ; Raised when an error occurs in an arithmetic operation.
  )

(class StateError (Error)
  ; Raised when a function has been invoked at an illegal or inappropriate time.
  )

(class NotImplementedError (StateError)
   ; In user defined base classes, abstract methods should raise this exception when they require derived classes to override the method, or while the class is being developed to indicate that the real implementation still needs to be added.
  )

(class SyntaxError (StateError)
  ; Raised when the parser encounters a syntax error.
  )

(class UnicodeError (StateError)
  ; Raised when a Unicode-related encoding or decoding error occurs.
  )

(method UnicodeError .init (args)
  (&message! self (str "illegal byte sequence " (map hex args))))

(class EOFError (StateError)
  ; Raised when reached EOF unexpectedly.
  )

(class OSError (StateError)
  ; Raised when a system function returns a system-related error, including I/O failures.
  )

;;; Path.

(class Path ()
  ; A class that handles a file path.
  ; Construct with path function.
  ; It should not be construct by new.
  ; The corresponding file does not have to exist.
  ; You can read and write files to the corresponding path as needed.
  path)

(function path (path-name)
  ; Constructs and returns the path object corresponding to path-name.
  ; Internally it just holds the string path-name as a list of filenames.
  ;     (path "foo/bar/buzz") -- ("foo" "bar" "buzz")
  ;     (path "/etc") -- ("/" "etc")
  ; The first `~` expands to the home directory using the environment variable.
  ; Any `~` other than the beginning is ignored.
  ;     (path "~/.vimrc") <=> ("home" "foo" ".vimrc")
  ;     (path "~/~.vimrc") <=> ("home" "foo" ".vimrc")
  ; All characters `\` in path-name are replaced with `/` for processing.
  ;     (path "C:\\foo") <=> ("C:" "foo")
  ; `.` and `..` included in path-name are not treated specially.
  ; Path class places the highest priority on keeping the implementation simple, and assumes that these features are implemente where necessary.
  ;     (path "foo/bar/../buzz") <=> ("foo" "bar" ".." "buzz")
  ; Two or more consecutive `/`s or trailing `/`s are ignored.
  ;     (path "foo//bar/") <=> ("foo" "bar")
  (if (is-a? path-name Path) path-name
      (let (c nil path nil root? nil)
        (if (prefix? path-name "/") (<- root? true)
            (prefix? path-name "~") (<- path-name (memcat (if (!= $hostname :windows) (getenv "HOME")
                                                              (memcat (getenv "HOMEDRIVE") (getenv "HOMEPATH")))
                                                          "/" (slice path-name 1))))
        (<- path (reject empty?
                         (split
                           (with-memory-stream ($out)
                             (with-memory-stream ($in path-name)
                               (while (<- c (read-char))
                                 (if (= c "\\") (write-bytes "/")
                                     (write-bytes c)))))
                           "/")))
        (if root? (<- path (cons "/"path)))
        (&path! (.new Path) path))))

(function path.getcwd ()
  ; Returns the path corresponding to the current directory.
  (path (getcwd)))

(method Path .name ()
  ; Returns file name.
  (last (&path self)))

(method Path .base-name ()
  ; Returns base name (the string up to the first dot).
  ; If not including dot, returns the entire name.
  (let (name (.name self) i (strstr name "."))
    (if i (slice name 0 i)
        name)))

(method Path .suffix ()
  ; Returns the suffix (the string after the last dot).
  ; If not including dot, returns nil.
  (let (name (.name self) i (strlstr name "."))
    (if i (slice name (++ i)))))

(method Path .but-suffix ()
  ; Returns the name without the suffix.
  (let (name (.name self) i (strlstr name "."))
    (if i (slice name 0 i)
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

(method Path .resolve (p)
  ; Resolve the given path against this path.
  ; If the argument is a character string, convert it to a path object before processing.
  ; If the path parameter is an absolute path then this method trivially returns path
  ; Otherwise this method concatenate this path and the speciifed path.
  ; `.` and `..` included in path-name are not treated specially.
  (if (string? p) (<- p (path p)))
  (if (.absolute? p) p
      (path (memcat (.to-s self) "/" (.to-s p)))))

(method Path .relativize (p)
  ; Returns a relative path between the receiver and a given path.
  (let (relative nil src (&path self) dst (&path p))
    (while src
      (if (= (pop! src) (car dst)) (pop! dst)
          (push! ".." relative)))
    (&path! (.new Path) (concat relative dst))))

(method Path .absolute? ()
  ; Returns whether this path regarded as the absolute path.
  (let (first (car (&path self)))
    (if (== $hostname :windows) (&& (= (len first) 2) (= ([] first 1) ":"))
        (= first "/"))))

(method Path .relative? ()
  ; Same as `(! (.absolute? self))`.
  (! (.absolute? self)))

(method Path .to-l ()
  ; Reads the contents of the file corresponding to the receiver.
  ; Returns it as a list.
  (with-open ($in self :read)
    (return (collect read-line))))

(method Path .to-s ()
  ; Returns a string representation of the receiver.
  (reduce (f (acc rest)
            (memcat (if (= acc "/") "" acc) "/" rest))
          (&path self)))

(method Path .open (mode)
  ; Returns a stream that reads the contents of the receiver.
  (.init (.new FileStream) (fopen (.to-s self) (if (= mode :read) 0
                                                   (= mode :write) 1
                                                   (= mode :append) 2
                                                   (= mode :update) 3
                                                   (assert nil)))))

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
  (!= (& (.mode self) 1) 0))

(method Path .file? ()
  ; Returns whether the receiver is a regular file.
  (!= (& (.mode self) 2) 0))

(method Path .dir? ()
  ; Returns whether the receiver is a directory.
  (!= (& (.mode self) 4) 0))

(method Path .other? ()
  ; Returns whether the receiver is neither a regular file nor a directory.
  (!= (& (.mode self) 8) 0))

(method Path .readable? ()
  ; Returns whether the receiver is readable.
  (!= (& (.mode self) 16) 0))

(method Path .writable? ()
  ; Returns whether the receiver is writable.
  (!= (& (.mode self) 32) 0))

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
       (sort! (split (readdir (.to-s self)) "\n"))))

;;; I/O.

(class Stream ()
  ; Abstract class for reading and writing streams.
  )

(method Stream .read-byte (:rest args)
  ; Read 1byte from the receiver.
  ; Returns -1 when the stream reaches the end.
  ; Must be implemented in the inherited class.
  (assert nil))

(method Stream .read-bytes (:opt buf from size)
  ; Reads size bytes of data from the receiver and saves it in the from position from the location specified by buf.
  ; Return the size of items read.
  ; If args is omitted, read until the stream reaches the end and returns it.
  ; Must be implemented in the inherited class.
  (assert nil))

(method Stream .read-char ()
  ; Read 1 character from the receiver
  ; Returns nil when the stream reaches the end.
  (let (b1 (.read-byte self) b2 nil b3 nil b4 nil size 0)
    (if (< b1 0) (return nil)
        (< b1 0x80) (<- size 1)
        (< b1 0xc2) (raise UnicodeError (list b1))
        (< b1 0xe0) (if (|| (= (& b1 0x3e) 0)
                            (!= (& (<- b2 (.read-byte self)) 0xc0) 0x80))
                        (raise UnicodeError (list b1 b2))
                        (<- size 2))
        (< b1 0xf0) (if (|| (!= (& (<- b2 (.read-byte self)) 0xc0) 0x80)
                            (&& (= b1 0xe0) (= (& b2 0x20) 0))
                            (!= (& (<- b3 (.read-byte self)) 0xc0) 0x80))
                        (raise UnicodeError (list b1 b2 b3))
                        (<- size 3))
        (< b1 0xf8) (if (|| (!= (& (<- b2 (.read-byte self)) 0xc0) 0x80)
                            (&& (= b1 0xf0) (= (& b2 0x30) 0))
                            (!= (& (<- b3 (.read-byte self)) 0xc0) 0x80)
                            (!= (& (<- b4 (.read-byte self)) 0xc0) 0x80))
                        (raise UnicodeError (list b1 b2 b3 b4))
                        (<- size 4))
        (raise UnicodeError (list b1)))
    (let (c (bytes size))
      (if (= size 1) ([] c 0 b1)
          (= size 2) (begin ([] c 0 b1) ([] c 1 b2))
          (= size 3) (begin ([] c 0 b1) ([] c 1 b2) ([] c 2 b3))
          (= size 4) (begin ([] c 0 b1) ([] c 1 b2) ([] c 2 b3) ([] c 3 b4)))
      (string! c))))

(method Stream .read ()
  ; Read expression from the receiver.
  ; Returns nil if eof reached.
  (let ($in self)
    (.read (.new ParenReader))))

(method Stream .read-line ()
  ; Input one line from the receiver.
  ; Returns read line.
  ; If stream reached eof, returns nil.
  (with-memory-stream (out)
    (let (c nil)
      (loop
        (if (= (<- c (.read-byte self)) -1) (return nil)
            (= c 0x0a) (break)
            (.write-byte out c))))))

(method Stream .write-byte (byte)
  ; Write byte to the receiver.
  ; Returns byte.
  ; Must be implemented in the inherited class.
  (raise NotImplementedError))

(method Stream .write-bytes (bytes :opt from size)
  ; Write size bytes to the receiver from the from position from the location specified by buf.
  ; Returns the size of bytes written.
  ; If from is omitted, write the entire argument bytes to the stream and returns bytes.
  ; Must be implemented in the inherited class.
  (raise NotImplementedError))

(method Stream .write-line (:opt string)
  ; Write string to the receiver.
  ; Returns bytes.
  (if string (.write-bytes self string))
  (.write-byte self 0x0a)
  string)

(method Stream .write-int (n :key radix padding)
  ; Write an integer with the specified padding and radix.
  ; Returns n.
  (let (x n radix (|| radix 10) padding (|| padding 0)
          ->byte (f (x)
                   (if (< x 10) (+ x 0x30)
                       (+ x 0x61 -10)))
          write1 (f (x padding)
                   (let (upper (// x radix))
                     (if (!= upper 0) (write1 upper (-- padding))
                         (dotimes (i padding) (.write-byte self 0x30)))
                     (.write-byte self (->byte (% x radix))))))
    (when (< x 0)
      (.write-byte self 0x2d)
      (<- x (- x) padding (-- padding)))
    (write1 x (-- padding)))
  n)

(method Stream .write-float (n :key style precision)
  ; Write float.
  ; Returns n.
  (let (x n exponent 0 precision (|| precision 6)
          fraction-writer (let (val nil max-val-len nil max-val nil zero-padding nil)
                            (f (ope :key init-val offset decimal-point remove-zero?)
                              (if (== ope :init)
                                  (begin
                                    (<- zero-padding (if (&& offset (< exponent 0)) (abs exponent) 0)
                                        max-val-len (++ (if offset (+ precision exponent) precision))
                                        max-val (// (pow 10 max-val-len))
                                        val (// (+ (/ (* init-val max-val) 10) 0.5)))
                                    (if (>= val max-val)
                                        (<- val (// val 10)
                                            exponent (++ exponent))))
                                  (== ope :write1)
                                  (begin
                                    (if (> zero-padding 0)
                                        (begin
                                          (.write-int self 0)
                                          (<- zero-padding (-- zero-padding)))
                                        (begin
                                          (<- val (* val 10))
                                          (.write-int self (// val max-val))
                                          (<- val (% val max-val)))))
                                  (== ope :write)
                                  (begin
                                    (dotimes (i (+ max-val-len zero-padding))
                                      (fraction-writer :write1)
                                      (if (&& (>= i decimal-point) (= val 0) remove-zero?) (break)
                                          (= i decimal-point) (.write-bytes self "."))))
                                  (assert nil))))
          style-g (f (:key upper?)
                    (if (<= -4 exponent 5) (style-f :remove-zero? true)
                        (style-e :remove-zero? true :upper? upper?)))
          style-f (f (:key remove-zero?)
                    (fraction-writer :init :init-val x :offset exponent)
                    (if (>= exponent 0) (fraction-writer :write :decimal-point exponent :remove-zero? remove-zero?)
                        (fraction-writer :write :decimal-point 0 :remove-zero? remove-zero?)))
          style-e (f (:key remove-zero? upper?)
                    (fraction-writer :init :init-val x)
                    (fraction-writer :write :decimal-point 0 :remove-zero? remove-zero?)
                    (.write-bytes self (if upper? "E" "e"))
                    (if (< exponent 0)
                        (begin
                          (.write-bytes self "-")
                          (.write-int self (- exponent) :padding 2))
                        (begin
                          (.write-bytes self "+")
                          (.write-int self exponent :padding 2)))))
    (when (< x 0.0)
      (.write-byte self 0x2d)
      (<- x (- x)))
    (if (&& (!= x 1.0) (< 0.0 x 1.0)) ; for x that satisfies `(= x 1.0)` and `(< x 1.0)`.
        (while (< x 1.0)
          (<- x (* x 10.0) exponent (-- exponent)))
        (while (>= x 10.0)
          (<- x (/ x 10.0) exponent (++ exponent))))
    (if (== style :f) (style-f)
        (in? style '(:e :E)) (style-e :upper? (== style :E))
        (in? style '(nil :g :G)) (style-g :upper? (== style :G))
        (raise ArgumentError "invalid style"))
    n))

(method Stream .write (x :key start end)
  ; Write the specified x as a readable format.
  ; Returns x.
  (if start (.write-bytes self start))
  (if (nil? x) (.write-bytes self :nil)
      (built-in? x) (.write-bytes self (built-in-name x))
      (symbol? x) (.write-bytes self x)
      (int? x) (.write-int self x)
      (number? x) (.write-float self x)
      (cons? x)
      (let (ope (car x))
        (if (&& (== ope 'quote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x27) (.write self (cadr x) :end ""))
            (&& (== ope 'quasiquote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x60) (.write self (cadr x) :end ""))
            (&& (== ope 'unquote) (nil? (cddr x)))
            (begin
              (.write-byte self 0x2c) (.write self (cadr x) :end ""))
            (&& (== ope 'unquote-splicing) (nil? (cddr x)))
            (begin
              (.write-bytes self ",@") (.write self (cadr x) :end ""))
            (begin
              (.write-byte self 0x28)
              (.write self (car x) :end "")
              (dolist (x (cdr x)) (.write self x :start " " :end ""))
              (.write-byte self 0x29))))
      (string? x)
      (begin
        (.write-byte self 0x22)
        (dostring (c x)
          (if (= c "\a") (.write-bytes self "\\a")
              (= c "\b") (.write-bytes self "\\b")
              (= c "\e") (.write-bytes self "\\e")
              (= c "\f") (.write-bytes self "\\f")
              (= c "\n") (.write-bytes self "\\n")
              (= c "\r") (.write-bytes self "\\r")
              (= c "\t") (.write-bytes self "\\t")
              (= c "\v") (.write-bytes self "\\v")
              (= c "\\") (.write-bytes self "\\\\")
              (= c "\"") (.write-bytes self "\\\"")
              (.write-bytes self c)))
        (.write-byte self 0x22))
      (keyword? x)
      (begin
        (.write-byte self 0x3a)
        (.write-bytes self x))
      (dict? x)
      (begin
        (.write-bytes self "#{ ")
        (dolist (key (keys x))
          (.write self key :end " ")
          (.write self ([] x key) :end " "))
        (.write-bytes self "}"))
      (bytes? x)
      (begin
        (.write-bytes self "#[ ")
        (dotimes (i (len x))
          (.write-bytes self "0x")
          (.write-int self ([] x i) :radix 16 :padding 2)
          (.write-byte self 0x20))
        (.write-byte self 0x5d))
      (array? x)
      (begin
        (.write-bytes self "#[ ")
        (dotimes (i (len x)) (.write self ([] x i) :end " "))
        (.write-byte self 0x5d))
      (function? x) (.write self (cons 'f (cons (params x) (body x))))
      (macro? x) (.write self (cons 'macro (cons (params x) (body x))))
      (assert nil))
  (.write-bytes self (|| end "\n"))
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

(method MemoryStream .buf ()
  ; Returns the buffer of the receiver.
  (slice (&buf self) (&rdpos self) (&wrpos self)))

(method MemoryStream .reserve (size)
  (let (req (+ (&wrpos self) size) buf-size (len (&buf self)))
    (when (< buf-size req)
      (while (< (<- buf-size (* buf-size 2)) req))
      (let (buf (bytes buf-size))
        (memcpy (&buf self) 0 buf 0 (&wrpos self))
        (&buf! self buf)))
    self))

(method MemoryStream .read-byte ()
  ; Implementation of the Stream.read-byte.
  (let (rdpos (&rdpos self))
    (if (= rdpos (&wrpos self)) -1
        (begin0 ([] (&buf self) rdpos)
                (&rdpos! self (++ rdpos))))))

(method MemoryStream .read-bytes (:opt buf from size)
  ; Implementation of the Stream.read-bytes.
  (if (nil? buf) (slice (&buf self) (&rdpos self) (&wrpos self))
      (let (rest (- (&wrpos self) (&rdpos self)))
        (if (< rest size) (<- size rest))
        (memcpy (&buf self) (&rdpos self) buf (&wrpos self) size)
        size)))

(method MemoryStream .write-byte (byte)
  ; Implementation of the Stream.write-byte.
  (let (wrpos (&wrpos self))
    (.reserve self 1)
    ([] (&buf self) wrpos byte)
    (&wrpos! self (++ wrpos))
    byte))

(method MemoryStream .write-bytes (x :opt from size)
  ; Implementation of the Stream.write-bytes.
  (.reserve self (|| size (<- size (memlen x))))
  (memcpy x (|| from 0) (&buf self) (&wrpos self) size)
  (&wrpos! self (+ (&wrpos self) size))
  (if from size x))

(method MemoryStream .seek (offset)
  ; Sets the file position indicator of the receiver.
  ; Returns the receiver.
  (if (! (<= 0 offset (&wrpos self))) (raise IndexError)
      (&rdpos! self offset)))

(method MemoryStream .tell ()
  ; Returns current byte position in stream.
  (&rdpos self))

(method MemoryStream .to-s ()
  ; Returns the contents written to the stream as a string.
  (let (size (&wrpos self))
    (if (= size 0) ""
        (string (&buf self) 0 size))))

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
  ; Implementation of the Stream.read-byte.
  (fgetc (&fp self)))

(method FileStream .read-bytes (:opt buf from size)
  ; Implementation of the Stream.read-bytes.
  (if (nil? buf)
      (let (out (.new MemoryStream) buf (bytes 1024))
        (while (> (<- size (fread buf 0 1024 (&fp self))) 0)
          (.write-bytes out buf 0 size))
        (.buf out))
      (fread buf from size (&fp self))))

(method FileStream .read-line ()
  ; Override of the Stream.read-line.
  (fgets (&fp self)))

(method FileStream .write-byte (byte)
  ; Implementation of the Stream.write-byte.
  (fputc byte (&fp self)))

(method FileStream .write-bytes (x :opt from size)
  ; Implementation of the Stream.write-bytes.
  (fwrite x (|| from 0) (|| size (memlen x)) (&fp self))
  (if from size x))

(method FileStream .seek (offset)
  ; Sets the file position indicator of the receiver.
  ; Returns the receiver.
  (fseek (&fp self) offset)
  self)

(method FileStream .tell ()
  ; Returns the current value of the file position indicator of the receiver.
  (ftell (&fp self)))

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

(method AheadReader .next? (predicate)
  ; Returns whether The end of the file has not been reached and the predicate is satisfied
  ; EOF is reached, returns nil.
  (&& (&next self) (predicate (&next self))))

(method AheadReader .skip (:opt expected)
  ; Skip next character and returns it.
  ; Error if expected is specified and the next character is not the same as the expected.
  (let (next (&next self))
    (if (nil? next) (raise EOFError "unexpected EOF")
        (&& expected (!= next expected)) (raise StateError (str "unexpected character '" next "`"))
        (= next "\n") (&lineno! self (++ (&lineno self))))
    (&next! self (.read-char (&stream self)))
    next))

(method AheadReader .skip-escape ()
  (let (c (.skip self))
    (if (!= c "\\") c
        (= (<- c (.skip self)) "n") "\n"
        (= c "r") "\r"
        (= c "t") "\t"
        (= c "a") "\a"
        (= c "b") "\b"
        (= c "c") (if (<= 0x40 (<- c (ord (upper (.skip self)))) 0x5f) (chr (& c 0x1f))
                      (raise StateError "illegal ctrl char"))
        (= c "e") "\e"
        (= c "f") "\f"
        (= c "v") "\v"
        (= c "x") (chr (+ (* 16 (.skip-digit self 16)) (.skip-digit self 16)))
        c)))

(method AheadReader .skip-line ()
  ; Skip line.
  ; Returns line.
  ; If stream reached eof, returns nil.
  (let (next (&next self))
    (if (nil? next) (.skip self)    ; raise error.
        (= next "\n") (begin (.skip self) "")
        (let (line (.read-line (&stream self)))
          (if (nil? line) (&next! self nil)
              (begin
                (<- line (memcat next line))
                (&lineno! self (++ (&lineno self)))
                (&next! self (.read-char (&stream self)))))
          line))))

(method AheadReader .skip-space ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (.next? self space?) (.skip self))
  self)

(method AheadReader .skip-sign ()
  (let (next (&next self))
    (if (= next "+") (begin (.skip self) nil)
        (= next "-") (begin (.skip self) true)
        nil)))

(method AheadReader .skip-digit (:opt radix)
  (let (ch (.skip self) val (if (digit? ch) (- (ord ch) 0x30)
                                (alpha? ch) (+ (- (ord (lower ch)) 0x61) 10)))
    (if (|| (nil? val) (>= val (|| radix 10)))
        (raise StateError "illegal digit")
        val)))

(method AheadReader .skip-uint ()
  (if (! (.next? self digit?)) (raise StateError "missing digits")
      (let (val 0)
        (while (.next? self digit?)
          (<- val (+ (* val 10) (.skip-digit self))))
        val)))

(method AheadReader .skip-int ()
  (let (minus? (.skip-sign self) val (.skip-uint self))
    (if minus? (- val)
        val)))

(method AheadReader .skip-unumber ()
  (let (val (.skip-uint self) next (&next self))
    (if (= next "x")
        (let (radix (if (= val 0) 16 val) val 0)
          (.skip self)
          (if (! (.next? self alnum?)) (raise StateError "missing lower or digits")
              (while (.next? self alnum?)
                (<- val (+ (* val radix) (.skip-digit self radix)))))
          val)
        (= next ".")
        (let (factor 0.1)
          (.skip self)
          (while (.next? self digit?)
            (<- val (+ val (* factor (.skip-digit self)))
                factor (/ factor 10)))
          (when (.next? self (f (x) (= (lower x) "e")))
            (.skip self)
            (<- val (* val (pow 10 (.skip-int self)))))
          val)
        ;; integer
        val)))

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
  (.write-bytes (&token self) o)
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
  (str "<" (&symbol (.class self)) ":0x" (address self) " "
       (list :next (&next self) :lineno (&lineno self)) ">"))

(class ParenLexer (AheadReader))

(method ParenLexer .identifier-symbol-alpha? ()
  (|| (memmem "!#$%&*./<=>?^[]_{|}~" (&next self))
      (.next? self alpha?)))

(method ParenLexer .identifier-sign? ()
  (memmem "+-" (&next self)))

(method ParenLexer .identifier-trail? ()
  (|| (.identifier-symbol-alpha? self)
      (.identifier-sign? self)
      (.next? self digit?)))

(method ParenLexer .get-identifier-sign ()
  (when (|| (.identifier-sign? self) (.identifier-symbol-alpha? self))
    (while (.identifier-trail? self) (.get self)))
  self)

(method ParenLexer .get-identifier ()
  (if (.identifier-sign? self)
      (begin
        (.get self)
        (.get-identifier-sign self))
      (.identifier-symbol-alpha? self)
      (begin
        (while (.identifier-trail? self)
          (.get self))
        self)
      (raise SyntaxError "illegal identifier")))

(method ParenLexer .lex-sign ()
  (let (sign (.skip self))
    (if (.next? self digit?)
        (let (val (.skip-number self))
          (if (= sign "-") (- val) val))
        (begin
          (.put self sign)
          (symbol (.token (.get-identifier-sign self)))))))

(method ParenLexer .lex-symbol ()
  (symbol (.token (.get-identifier self))))

(method ParenLexer .lex-keyword ()
  (.skip self)
  (keyword (.token (.get-identifier self))))

(method ParenLexer .lex-string ()
  (.skip self)
  (while (!= (&next self) "\"") (.get-escape self))
  (.skip self "\"")
  (.token self))

(method ParenLexer .lex ()
  (.skip-space self)
  (let (next (&next self))
    (if (nil? next) '(:EOF)
        (= next "(") (begin (.skip self) '(:open-paren))
        (= next ")") (begin (.skip self) '(:close-paren))
        (= next "'") (begin (.skip self) '(:quote))
        (= next "`") (begin (.skip self) '(:backquote))
        (= next ",") (begin (.skip self) (if (= (&next self) "@") (begin (.skip self) '(:unquote-splicing)) '(:unquote)))
        (= next "\"") (list :atom (.lex-string self))
        (= next ":") (list :atom (.lex-keyword self))
        (= next ";") (begin (.skip-line self) (.lex self))
        (= next "#") (begin (.skip self) (list :read-macro (symbol (&next self))))
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
                    (if (== (&token-type self) :close-paren) (reverse! acc)
                        (== (&token-type self) :EOF) (raise SyntaxError "missing close-paren")
                        (parse-cdr (cons (.parse self) acc)))))
    (parse-cdr nil)))

(method ParenReader .parse ()
  (let (type (&token-type self))
    (if (== type :EOF) nil
        (== type :atom) (&token self)
        (== type :open-paren) (.parse-list self)
        (== type :quote) (list 'quote (.parse (.scan self)))
        (== type :backquote) (list 'quasiquote (.parse (.scan self)))
        (== type :unquote) (list 'unquote (.parse (.scan self)))
        (== type :unquote-splicing) (list 'unquote-splicing (.parse (.scan self)))
        (== type :read-macro) (apply ([] $read-table (&token self)) (list self))
        (raise SyntaxError))))

(macro unquote (expr)
  (list 'raise 'SyntaxError (str "unexpected unquote -- ," expr)))

(macro unquote-splicing (expr)
  (list 'raise 'SyntaxError (str "unexpected unquote-splicing -- ,@" expr)))

(macro quasiquote (expr)
  (let (descend
         (f (x level)
           (if (atom? x) (list quote x)
               (let (ope (car x))
                 (if (= ope 'quasiquote) (list cons ''quasiquote (descend (cdr x) (++ level)))
                     (= ope 'unquote) (if (= level 0) (cadr x)
                                          (list cons ''unquote (descend (cdr x) (-- level))))
                     (= ope 'unquote-splicing) (if (= level 0)
                                                   (cadr x) (list cons ''unquote-splicing (descend (cdr x) (-- level))))
                     (list concat (descend-car (car x) level) (descend (cdr x) level))))))
         descend-car
         (f (x level)
           (if (atom? x) (list quote (list x))
               (let (ope (car x))
                 (if (= ope 'quasiquote) (list list (list cons ''quasiquote (descend (cdr x) (++ level))))
                     (= ope 'unquote) (if (= level 0) (cons list (cdr x))
                                          (list list (list cons ''unquote (descend (cdr x) (-- level)))))
                     (= ope 'unquote-splicing) (if (= level 0)
                                                   (cons concat (cdr x)) (list list (list cons ''unquote-splicing (descend (cdr x) (-- level)))))
                     (list list (list concat (descend-car (car x) level) (descend (cdr x) level))))))))
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
          (list [] $read-table (list quote next) g))))

(function read-byte ()
  ; Same as `(.read-byte (dynamic $in))`.
  (.read-byte (dynamic $in)))

(function read-bytes (:opt buf from size)
  ; Same as `(.read-bytes (dynamic $in))`.
  (.read-bytes (dynamic $in) buf from size))

(function read-char ()
  ; Same as `(.read-char (dynamic $in))`.
  (.read-char (dynamic $in)))

(function read-line ()
  ; Same as `(.read-line (dynamic $in))`.
  (.read-line (dynamic $in)))

(function read ()
  ; Same as `(.read (dynamic $in))`.
  (.read (dynamic $in)))

(function write-byte (x)
  ; Same as `(.write-byte (dynamic $out) x)`.
  (.write-byte (dynamic $out) x))

(function write-bytes (x :opt from size)
  ; Same as `(.write-bytes (dynamic $out) x)`.
  (.write-bytes (dynamic $out) x from size))

(function write-line (:opt x)
  ; Same as `(.write-line (dynamic $out) x)`.
  (.write-line (dynamic $out) x))

(function write (x :key start end)
  ; Same as `(.write (dynamic $out) x :start start :end end))`.
  (.write (dynamic $out) x :start start :end end))

(macro with-memory-stream ((ms :opt s) :rest body)
  ; Create memory stream context.
  ; If the string s is specified, construct an input stream with s as the source.
  ; Returns last evaluated value.
  ; Otherwise act as an output stream.
  ; Returns the string written to the output stream.
  ;     (with-memory-stream (ms s)
  ;        expr1 expr2 ...)
  ;     (let (ms (.new MemoryStream))
  ;        (if s (.write-bytes ms s))
  ;        expr1 expr2 ...
  ;        (if s (.to-s ms)))
  (with-gensyms (gs)
    (list let (list gs s ms '(.new MemoryStream))
          (list if gs
                (list begin
                      (list '.write-bytes ms gs)
                      (cons begin body))
                (list begin
                      (cons begin body)
                      (list '.to-s ms))))))

(macro with-open ((sym p mode) :rest body)
  ; Create file stream context.
  ; The file stream is guaranteed to be closed when exiting the context.
  ; Returns nil.
  (with-gensyms (gsym)
    (list let (list gsym nil)
          (list unwind-protect
                (cons let (cons (list sym (list <- gsym (list '.open (list path p) mode)))
                                body))
                (list if gsym (list '.close gsym))))))

;; execution.

(built-in-function eval (expr)
  ; Evaluates the specified expression and returns a value.
  (assert (nil? (eval 'nil))))

(built-in-function apply (fn args)
  ; Evaluates the specified expression and returns a value.
  ; Applies the function to the args.
  (assert (= (apply car '((1))) 1)))

(function repl ()
  ; Enter repl(read eval print loop) mode.
  ; Executed when there is no command line argument when paren starts.
  (<- (:opt $_ $1 $2 $3 $4 $5) nil)
  (loop
    (catch (Error (f (e) (.print-stack-trace e)))
      (write-bytes ") ")
      (if (<- $_ (read)) (<- $_ (write (eval $_)) $5 $4 $4 $3 $3 $2 $2 $1 $1 $_)
          (break)))))

(function raise (cls :rest args)
  ; Throw the cls Class instance which initialized with argument args.
  (throw (apply .init (cons (.new cls) args))))

(function quit ()
  (raise SystemExit))

(built-in-function exit (status-cd))

(function load (file)
  (with-open ($in file :read)
    (foreach eval (collect read)))
  true)

(function import (key :opt dir)
  (if (in? key $import) key
      (let ($G-module (.resolve (if dir (path dir) (.resolve $paren-home "modules"))
                                (memcat (string key) ".p")))
        (if (! (.file? $G-module)) (raise OSError (str "unreadable module " (.to-s $G-module)))
            (begin
              (load $G-module)
              (<- main nil)
              (push! key $import))))))

(function boot (args)
  ; Executed when paren is executed.
  ; Invoke repl if there are no command line arguments that bound to the symbol $args.
  ; If command line arguments are specified, read the first argument as the script file name and execute main.
  ; Can be omitted if the script file has a `p` extension.
  (catch (SystemExit (f (e) (exit 0))
                     Exception (f (e) (.print-stack-trace e) (exit (.status-cd e))))
    (if (.file? $parenrc) (load $parenrc))
    (if (nil? args) (repl)
        (let (file-name (car args)
                        script (find (f (x) (if (.file? x) x))
                                     (map (f (x) (apply .resolve x))
                                          (product (cons (path.getcwd) $runtime-path)
                                                   (list file-name (str file-name ".p"))))))
          (if (nil? script) (raise ArgumentError (str "unreadable file " file-name))
              (&& (load script) (bound? 'main) main) (main (cdr args)))))))

(<- $import '(:core)
    $read-table (dict)
    ($stdin $stdout $stderr) (map (f (x) (.init (.new FileStream) (fp x))) (.. 3))
    ($in $out) (list $stdin $stdout)
    $debug? (== (assert true) true)
    $paren-home (.parent (.parent (.resolve (path.getcwd) core.p)))
    $parenrc (path "~/.parenrc")
    $runtime-path (map (f (p) (.resolve $paren-home p))
                       '("tools/coreutils" "tools")))

(reader-macro [ (reader)
   ; Define array/bytes literal reader.
   (if (!= (.read reader) '[) (raise SyntaxError "missing space in array literal")
       (let ($G-l nil $G-v nil)
         (while (!= (<- $G-v (.read reader)) '])
             (push! (eval $G-v) $G-l))
         (array (reverse! $G-l)))))

(reader-macro { (reader)
  ; Define dictionary literal reader.
  (if (!= (.read reader) '{) (raise SyntaxError "missing space in dictionary literal")
      (let ($G-d (dict) $G-k nil)
        (while (!= (<- $G-k (.read reader)) '})
          ([] $G-d $G-k (eval (.read reader))))
        $G-d)))

(reader-macro p (reader)
  ; Define print reader macro.
  (.skip (&lexer reader))
  (list 'write (.read reader)))

(reader-macro . (reader)
  ; Define eval reader.
  (.skip (&lexer reader))
  (eval (.read reader)))

(boot $args)
