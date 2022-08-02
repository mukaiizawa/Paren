; Paren core library.

;; special-operator(7).

(macro special-operator (name) name)
(special-operator <-)
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

(macro built-in-function (name :opt args :rest body)
  (cons 'begin body))

(macro function! (name args :rest body)
  (list '<- name (cons 'f (cons args body))))

(macro with-gensyms ((:rest syms) :rest body)
  (let (rec (f (syms) (if syms (cons (car syms) (cons '(symbol) (rec (cdr syms)))))))
    (cons 'let (cons (rec syms) body))))

(macro begin0 (:rest body)
  (with-gensyms (gsym)
    (list 'let (list gsym (car body))
          (cons 'begin (cdr body))
          gsym)))

(macro when (test :rest body)
  (list 'if test (cons 'begin body)))

(macro && (:rest args)
  (if (cdr (cdr args)) (list 'if (car args) (cons '&& (cdr args)))
      (cdr args) (list 'if (car args) (car (cdr args)))
      (car args) (car args)
      true))

(macro || (:rest args)
  (if (cdr args) (with-gensyms (gsym)
                   (list 'let (list gsym (car args))
                         (list 'if gsym gsym
                               (cons '|| (cdr args)))))
      (car args)))

(macro while (test :rest body)
  (list 'loop (list 'if test (cons 'begin body) '(break))))

(macro for (binding test update :rest body)
  (with-gensyms (first-loop?)
    (list 'let (cons first-loop? (cons true binding))
          (list 'loop
                (list 'if first-loop? (list '<- first-loop? nil)
                      (cons '<- update))    ; for continue(3).
                (list 'if test (cons 'begin body)
                      '(break))))))

(macro dolist ((x lis) :rest body)
  (with-gensyms (gl)
    (list 'for (list gl lis x (list car gl)) gl (list gl (list cdr gl) x (list car gl))
          (cons 'begin body))))

(macro dotimes ((i n) :rest body)
  (with-gensyms (gend)
    (list 'for (list i 0 gend n) (list '< i gend) (list i (list '++ i))
          (cons 'begin body))))

(macro dostring ((c s) :rest body)
  (list 'doarray (list c (list 'array s))
        (cons 'begin body)))

(macro doarray ((x a) :rest body)
  (with-gensyms (ga gi)
    (list 'let (list ga a)
          (cons 'dotimes (cons (list gi (list 'len ga))
                               (cons (list '<- x (list '[] a gi))
                                     body))))))

(macro timeit (:rest body)
  (with-gensyms (clock-offset cycle-offset)
    (list 'let (list clock-offset '(clock) cycle-offset '(cycle))
          (list 'begin0
                (cons 'begin body)
                (list 'write (list 'list
                                   :time (list '- '(clock) clock-offset)
                                   :cycle (list '- '(cycle) cycle-offset)))))))

(macro assert (expr)
  (list if expr true
        (list 'raise 'AssertException (list 'str (list quote expr)))))

(built-in-function macroexpand-1)

(function! macroexpand (expr :key ignores)
  (let (ignore (f (x)
                 (if (cons? x) (begin (ignore (car x)) (ignore (cdr x)))
                     (<- ignores (cons x ignores)) x)
                 x)
               expand1 (f (x)
                         (if x (cons (macroexpand (car x) :ignores ignores)
                                     (expand1 (cdr x)))))
               expand2 (f (x)
                         (if (cdr x) (cons (ignore (car x))
                                           (cons (macroexpand (cadr x) :ignores ignores)
                                                 (expand2 (cddr x))))
                             x (raise SyntaxError (str "missing value for variable " expr)))))
    (if (! (cons? expr)) expr
        (let ((ope :rest args) expr)
          (if (in? ope ignores) (cons ope (expand1 args))
              (&& (symbol? ope) (bound? ope) (macro? (eval ope))) (macroexpand (macroexpand-1 expr) :ignores ignores)
              (cons ope
                    (if (== ope 'quote) args
                        (== ope '<-) (expand2 args)
                        (== ope 'f) (cons (car args) (expand1 (cdr args)))
                        (== ope 'macro) (cons (car args) (cons (cadr args) (expand1 (cddr args))))
                        (in? ope '(let catch)) (cons (expand2 (car args)) (expand1 (cdr args)))
                        (expand1 args))))))))

(macro function (name args :rest body)
  (let (expand-body (f (x) (if x (cons (macroexpand (car x)) (expand-body (cdr x))))))
    (list 'if
          (list 'bound? (list 'quote name)) '(raise ArgumentError "symbol already bound")
          (list '<- name (cons 'f (cons args (expand-body body)))) (list 'quote name))))

;; fundamental function.

(built-in-function hash)
(built-in-function address)
(built-in-function =)
(built-in-function ==)
(built-in-function !)

(function != (x y)
  (! (= x y)))

(function !== (x y)
  (! (== x y)))

;; function & macro.

(built-in-function function?)
(built-in-function built-in?)
(built-in-function built-in-name)
(built-in-function special-operator?)
(built-in-function macro?)
(built-in-function params)
(built-in-function body)
(function identity (x) x)

;; list.

(built-in-function [])
(built-in-function car!)
(built-in-function car)
(built-in-function cdr!)
(built-in-function cdr)
(built-in-function concat)
(built-in-function cons)
(built-in-function cons?)
(built-in-function in?)
(built-in-function index)
(built-in-function last-cons)
(built-in-function last-index)
(built-in-function len)
(built-in-function list)
(built-in-function list...)
(built-in-function slice)

(function nil? (x)
  (! x))

(function atom? (x)
  (! (cons? x)))

(function list? (x)
  (if (cons? x) true
      (nil? x)))

(function ->list (x)
  (if (list? x) x
      (list x)))

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

(function assoc (key alist)
  (keep1 (f (x) (if (= (car x) key) x))
         alist))

(function .. (start :opt stop step)
  (let (rec (f (next stop step :opt acc)
              (if (< next stop) (rec (+ next step) stop step (cons next acc))
                  (reverse! acc))))
    (if (nil? stop) (<- stop start start 0 step 1)
        (nil? step) (<- step 1)
        (<= step 0) (raise IndexError "step must be positive integer"))
    (rec start stop step)))

(function join (lis :opt separator)
  (if (nil? lis) ""
      (nil? (cdr lis)) (car lis)
      (nil? separator) (apply concat lis)
      (with-memory-stream ($out)
        (write-bytes (car lis))
        (dolist (x (cdr lis)) (write-bytes separator) (write-bytes x)))))

(function split (s :opt separator)
  (if (empty? s) nil
      (nil? separator) (list... s)
      (let (pos nil lis nil seplen (len separator))
        (while (<- pos (index separator s))
          (<- lis (cons (slice s 0 pos) lis)
              s (slice s (+ pos seplen))))
        (reverse! (cons s lis)))))

(function last (lis)
  (car (last-cons lis)))

(function butlast (lis)
  (drop-last lis 1))

(function take (lis n)
  (slice lis 0 n))

(function drop (lis n)
  (slice lis n))

(function take-last (lis n)
  (slice lis (- (max (len lis) n) n)))

(function drop-last (lis n)
  (slice lis 0 (- (max (len lis) n) n)))

(function take-while (fn lis)
  (let (rec (f (lis acc)
              (if (&& lis (fn (car lis))) (rec (cdr lis) (cons (car lis) acc))
                  (reverse! acc))))
    (rec lis nil)))

(function drop-while (fn lis)
  (if (nil? lis) nil
      (fn (car lis)) (drop-while fn (cdr lis))
      lis))

(function split-at (lis i)
  (apply (juxt take drop) (list lis i)))

(function split-with (fn lis)
  (apply (juxt take-while drop-while) (list fn lis)))

(function group (lis n)
  (let (rec (f (lis acc)
              (if (nil? lis) (reverse! acc)
                  (rec (slice lis n) (cons (slice lis 0 n) acc)))))
    (if (<= n 0) (raise IndexError "sublists length must be positive integer")
        (rec lis nil))))

(function group-by (fn lis)
  (let (d (dict) keys nil ret nil)
    (dolist (x lis)
      (let (key (fn x) val ([] d key))
        (if (! (in? key d)) (push! key keys))
        ([] d key (cons x val))))
    (reduce (f (acc key) (cons (cons key (reverse! ([] d key))) acc))
            (cons nil keys))))

(function chunk (fn lis)
  (if (nil? lis) nil
      (let (val (fn (car lis)) separator (compose (partial = val) fn))
        (cons (cons val (take-while separator lis))
              (chunk fn (drop-while separator lis))))))

(function reverse (lis)
  (let (rec (f (lis acc)
              (if (nil? lis) acc
                  (rec (cdr lis) (cons (car lis) acc)))))
    (rec lis nil)))

(built-in-function reverse!)

(macro push! (x lis)
  (with-gensyms (gx)
    (list 'let (list gx x)
          (list '<- lis (list 'cons gx lis))
          gx)))

(macro pop! (lis)
  (list 'begin0
        (list 'car lis)
        (list '<- lis (list 'cdr lis))))

(function flatten (lis)
  (let (acc nil rec (f (x)
                      (if (atom? x) (push! x acc)
                          (dolist (i x) (rec i)))))
    (rec lis)
    (reverse! acc)))

(function repeat (x n)
  (if (pos? n) (cons x (repeat x (-- n)))))

(function swap! (seq i j)
  (let (t ([] seq i))
    ([] seq i ([] seq j))
    ([] seq j t)
    seq))

(function sort! (seq :key sorter key start end)
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

;;; higher-order functions.

(function partial (fn :rest partial-args)
  (f (:rest args) (apply fn (concat partial-args args))))

(function complement (fn)
  (f (:rest args) (! (apply fn args)))) ; <=> (compose ! fn)

(function compose (:rest fns)
  (if (nil? fns) identity
      (let ((fn1 :rest fns) (reverse fns))
        (f (:rest args)
          (reduce (f (args fn) (fn args))
                  (cons (apply fn1 args) fns))))))

(function juxt (:rest fns)
  (f (:rest args)
    (map (f (fn) (apply fn args))
         fns)))

(function foreach (fn args)
  (dolist (x args) (fn x)))

(function collect (fn)
  (let (rec (f (val :opt acc)
              (if (nil? val) (reverse! acc)
                  (rec (fn) (cons val acc)))))
    (rec (fn))))

(function count (fn lis)
  (let (rec (f (lis sum)
              (if (nil? lis) sum
                  (fn (car lis)) (rec (cdr lis) (++ sum))
                  (rec (cdr lis) sum))))
    (rec lis 0)))

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

(function zip (:rest args)
  (apply (partial map list) args))

(function reduce (fn args)
  (if (cdr args) (reduce fn (cons (fn (car args) (cadr args)) (cddr args)))
      (car args)))

(function select (fn lis)
  (let (rec (f (lis acc)
              (if (nil? lis) (reverse! acc)
                  (fn (car lis)) (rec (cdr lis) (cons (car lis) acc))
                  (rec (cdr lis) acc))))
    (rec lis nil)))

(function select1 (fn lis)
  (if (nil? lis) nil
      (fn (car lis)) (car lis)
      (select1 fn (cdr lis))))

(function reject (fn lis)
  (select (complement fn) lis))

(function keep (fn lis)
  (let (rec (f (lis acc)
              (if (nil? lis) (reverse! acc)
                  (let (val (fn (car lis)))
                    (if (nil? val) (rec (cdr lis) acc)
                        (rec (cdr lis) (cons val acc)))))))
    (rec lis nil)))

(function keep1 (fn lis)
  (&& lis (|| (fn (car lis)) (keep1 fn (cdr lis)))))

(function uniq (lis)
  (let (rec (f (lis acc)
              (if (nil? lis) (reverse! acc)
                  (let (x (car lis))
                    (rec (drop-while (partial = x) lis) (cons x acc))))))
    (rec lis nil)))

(function member (x lis)
  (if (nil? lis) nil
      (= x (car lis)) lis
      (member x (cdr lis))))

(function position (fn lis)
  (let (rec (f (lis n)
              (if (nil? lis) nil
                  (fn (car lis)) n
                  (rec (cdr lis) (++ n)))))
    (rec lis 0)))

(function map-group (fn args chunk-size)
  (if args (cons (apply fn (slice args 0 chunk-size))
                 (map-group fn (cddr args) chunk-size))))

(function every? (fn lis)
  (if (nil? lis) true
      (fn (car lis)) (every? fn (cdr lis))))

(function some? (fn lis)
  (if (nil? lis) nil
      (fn (car lis)) true
      (some? fn (cdr lis))))

(function none? (fn lis)
  (if (nil? lis) true
      (fn (car lis)) nil
      (none? fn (cdr lis))))

(function every-adjacent? (fn lis)
  (if (cdr lis) (&& (fn (car lis) (cadr lis)) (every-adjacent? fn (cdr lis)))
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

(built-in-function number?)
(built-in-function int?)

(function byte? (x)
  (&& (int? x) (<= 0 x 255)))

(function zero? (x)
  (= x 0))

(function pos? (x)
  (> x 0))

(function neg? (x)
  (< x 0))

(function even? (x)
  (zero? (% x 2)))

(function odd? (x)
  (! (even? x)))

(function int (x)
  (// (float x)))

(function int32 (x)
  (& 0xffffffff (int x)))

(function float (x)
  (if (nil? x) 0
      (number? x) x
      (string? x) (with-memory-stream ($in x)
                    (let (ar (.new AheadReader) val (.skip-number ar))
                      (if (.next ar) (raise ArgumentError "illegal string")
                          val)))
      (raise ArgumentError "expected number or string")))

(built-in-function +)

(function - (x :rest args)
  (if (nil? args) (* x -1)
      (+ x (- (apply + args)))))

(built-in-function ++)
(built-in-function --)
(built-in-function *)
(built-in-function /)
(built-in-function //)
(built-in-function %)

(built-in-function <)

(function > (:rest args)
  (every-adjacent? (f (x y) (< y x)) args))

(function <= (:rest args)
  (every-adjacent? (f (x y) (! (< y x))) args))

(function >= (:rest args)
  (every-adjacent? (f (x y) (! (< x y))) args))

(function max (:rest args)
  (reduce (f (x y) (if (> x y) x y)) args))

(function min (:rest args)
  (reduce (f (x y) (if (< x y) x y)) args))

(function abs (x)
  (if (< x 0) (- x)
      x))

;;;; bitwise operates.

(built-in-function ~)
(built-in-function &)
(built-in-function |)
(built-in-function ^)
(built-in-function <<)

(function >> (x y)
  (<< x (- y)))

;;;; mathematical functions.

(built-in-function sin)
(built-in-function cos)
(built-in-function tan)
(built-in-function asin)
(built-in-function acos)
(built-in-function atan)
(built-in-function sinh)
(built-in-function cosh)
(built-in-function tanh)
(built-in-function exp)
(built-in-function log)
(built-in-function pow)
(built-in-function sqrt)

;; symbol & keyword.

(built-in-function symbol?)
(built-in-function keyword?)
(built-in-function bound?)
(built-in-function symbol)
(built-in-function keyword)

;; string.

(built-in-function string!)
(built-in-function string)
(built-in-function string?)

(built-in-function byte-len)
(built-in-function chr)
(built-in-function ord)

(built-in-function alnum?)
(built-in-function alpha?)
(built-in-function ascii?)
(built-in-function digit?)
(built-in-function lower)
(built-in-function lower?)
(built-in-function print?)
(built-in-function space?)
(built-in-function upper)
(built-in-function upper?)

(function str (:rest args)
  (with-memory-stream ($out)
    (dolist (arg args)
      (if (nil? arg) :continue
          (|| (symbol? arg) (keyword? arg) (string? arg) (bytes? arg)) (write-bytes arg)
          (write arg :end "")))))

(function bin (x)
  (format "2x%b" x))

(function oct (x)
  (format "8x%o" x))

(function hex (x)
  (if (bytes? x)
      (with-memory-stream ($out)
        (doarray (i x) (write-bytes (format "%02x" i))))
      (format "0x%x" x)))

(function empty? (x)
  (if (nil? x) true
      (= (len x) 0)))

(function prefix? (x prefix)
  (let (plen (len prefix))
    (|| (= plen 0) (= (index prefix x 0 plen) 0))))

(function suffix? (x suffix)
  (let (slen (len suffix) start (- (len x) slen))
    (|| (= slen 0) (&& (>= start 0) (= (last-index suffix x start) start)))))

(function title? (s)
  (if (empty? s) nil
      (= (len s) 1) (upper? s)
      (&& (upper? (slice s 0 1))
          (lower? (slice s 1)))))

(function title (s)
  (join (map (f (word)
               (if (empty? word) word
                   (concat (upper (slice word 0 1))
                           (lower (slice word 1)))))
             (split s " "))
        " "))

(function wcwidth (s)
  (apply + (map (f (ch)
                  (if (print? ch) 1
                      (let (cp (ord ch))
                        (if (<= 0xff61 cp 0xffdf) 1    ; Halfwidth Katakana
                            (<= 0x3000 cp 0xffe6) 2    ; Fullwidth characters
                            0))))
                (split s))))

(function format (fmt :rest args)
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
                          (in? conv '("b" "o" "d" "x" "X" "f" "e" "g"))
                          (let (prefix (if (< x 0) (begin (<- x (- x)) "-")
                                           (in? "+" flags) "+"
                                           (in? " " flags) " "
                                           "")
                                       val (with-memory-stream ($out)
                                             (if (in? conv '("e" "f" "g")) (.write-float $out x :precision precision :style (keyword conv))
                                                 (.write-int $out (// x)
                                                             :radix (cadr (member (lower conv) '("b" 2 "o" 8 "d" 10 "x" 16)))
                                                             :upper? (upper? conv)
                                                             :padding precision))))
                            (format1 flags width prefix val))
                          (raise ArgumentError (str "unexpected conversion specifier " conv)))
                      (<- args (cdr args)))))))))))

(function strip (s :opt fn)
  (rstrip (lstrip s fn) fn))

(function lstrip (s :opt fn)
  (let (i 0 a (array s) size (len a) fn (|| fn space?))
    (while (&& (< i size) (fn ([] a i))) (<- i (++ i)))
    (if (= i 0) s
        (slice s i))))

(function rstrip (s :opt fn)
  (let (a (array s) i (len a) fn (|| fn space?))
    (while (&& (>= i 0) (fn ([] a (-- i)))) (<- i (-- i)))
    (slice s 0 i)))

;; bytes

(built-in-function bytes)
(built-in-function bytes?)
(built-in-function memcpy)
(built-in-function memcmp)

;; array.

(built-in-function array)
(built-in-function array?)

(function fill! (seq x :opt start end)
  (let (s (|| start 0) e (|| end (len seq)))
    (dotimes (i (- e s))
      ([] seq (+ s i) x))
    seq))

;; dictionary.

(built-in-function dict)
(built-in-function dict?)
(built-in-function keys)

(function vals (d)
  (map (partial [] d) (keys d)))

;; os.

(built-in-function fp)
(built-in-function fopen)
(built-in-function fclose)
(built-in-function fgetc)
(built-in-function fgets)
(built-in-function fputc)
(built-in-function fread)
(built-in-function fwrite)
(built-in-function fseek)
(built-in-function ftell)
(built-in-function stat)
(built-in-function utime)
(built-in-function getcwd)
(built-in-function chdir)
(built-in-function readdir)
(built-in-function mkdir)
(built-in-function remove)
(built-in-function rename)
(built-in-function time)
(built-in-function clock)
(built-in-function cycle)

(built-in-function utcoffset ()
  ; Returns the difference in hours and minutes from Coordinated Universal Time (UTC) for time zone set in the host system.
  )

(built-in-function sleep (sec)
  ; Sleep for a specified number of seconds.
  ; Returns nil.
  )

(built-in-function popen)
(built-in-function pclose)

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

(macro with-arrow-syntax (:rest body)
  (let (compile0 (f (expr)
                   (if (! (symbol? expr)) expr
                       (let (name (string expr) pos (index "->" name))
                         (if (nil? pos) expr
                             (reduce (f (x y) (list '[] x (keyword y)))
                                     (map symbol (split name "->")))))))
                 compile (f (expr)
                           (if (atom? expr) (compile0 expr)
                               (let ((ope :rest args) expr)
                                 (if (== ope '<-) (cons 'begin
                                                        (map-group (f (left right)
                                                                     (let (x (compile0 left) y (compile right))
                                                                       (if (== x left) (list '<- x y)    ; (<- x y)
                                                                           (concat x (list y)))))        ; ([] x y)
                                                                   args 2))
                                     (== ope 'let) (cons 'let
                                                         (cons (apply concat (map-group (f (left right) (list left (compile right)))
                                                                                        (car args) 2))
                                                               (map compile (cdr args))))
                                     (== ope 'f) (cons 'f (cons (car args) (map compile (cdr args))))
                                     (== ope 'quote) expr
                                     (cons ope (map compile args)))))))
    (cons begin (map compile body))))

(macro class (cls-sym (:opt super :rest features) :rest fields)
  ; Define class.
  ; Returns class symbol.
  (with-gensyms (gcls-sym gsuper gfeatures gfields)
    (list 'let (list gcls-sym (list 'quote cls-sym)
                     gsuper (list 'quote super)
                     gfeatures (list 'quote features)
                     gfields (list 'quote fields))
          (list 'if
                (list 'bound? gcls-sym) '(raise ArgumentError "symbol already bound")
                (list '! (list 'bound? gsuper)) '(raise ArgumentError "undefined super class")
                (list '! (list 'every? 'bound? gfeatures)) '(raise ArgumentError "undefined feature class")
                (list '! (list 'every? 'symbol? gfields)) '(raise ArgumentError "invalid fields")
                (cons 'begin
                      (cons (list '<- cls-sym '(dict))
                            (map (f (k v) (list '[] cls-sym k v))
                                 '(:class :symbol :super :features :fields)
                                 (list ''Class
                                       gcls-sym
                                       (list '|| gsuper (list 'if (list '!= gcls-sym ''Object) ''Object))
                                       gfeatures
                                       gfields)))))
          gcls-sym)))

(macro method (cls-sym method-sym args :rest body)
  ; Define method.
  ; Returns method symbol.
  (let (global-method-name (concat cls-sym method-sym))
    (list 'if
          (list '! (list 'find-class (list 'quote cls-sym))) '(raise ArgumentError "class not found")
          (list 'bound? (list 'quote global-method-name)) '(raise ArgumentError "symbol already bound")
          (list 'with-arrow-syntax
                (list 'function! method-sym '(self :rest args)
                      (list 'apply
                            (list 'find-method 'self->class (list 'quote method-sym))
                            '(cons self args)))
                (cons 'function (cons global-method-name (cons (cons 'self args) body)))))))

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
  (find-class self->class))

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
  (str "<" (.symbol (.class self)) ":0x" (address self)  ">"))

(class Class ()
  ; Class of class object.
  ; All class objects are instances of Class class.
  symbol super features fields methods)

(method Class .new ()
  ; Construct an instance.
  ; If .init method has argument, must invoke after create an instance.
  ; Otherwise automatically invoke .init method.
  (let (o (dict))
    (for (cls self) cls (cls (find-class cls->super))
      (foreach (f (x) ([] o (keyword x) nil))
               cls->fields))
    (<- o->class self->symbol)
    (if (= (params (find-method o->class '.init)) '(self)) (.init o)
        o)))

(method Class .symbol ()
  ; Returns the class symbol.
  self->symbol)

(method Class .super ()
  ; Returns the class representing the superclass of the receiver.
  (find-class self->super))

(method Class .features ()
  ; Returns the feature list representing the feature of the receiver.
  (map find-class self->features))

(class Exception ()
  ; The Exception class is the superclass of all exceptions.
  ; It is recommended that new exceptions derive from the Error class or one of its subclasses.
  ; Do not derive from Exception.
  message status-cd stack-trace)

(method Exception .init (:opt message :key status-cd)
  (<- self->message message
      self->status-cd (|| status-cd 1))
  self)

(method Exception .to-s ()
  ; Returns a String representing the receiver.
  (let (class-name (string self->class) msg self->message)
    (if (nil? msg) class-name
        (concat class-name " -- " msg))))

(method Exception .status-cd ()
  self->status-cd)

(method Exception .stack-trace ()
  ; Returns stack trace.
  self->stack-trace)

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
  (<- self->status-cd 0)
  self)

(class AssertException (Exception))

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
  (<- self->message (str "illegal byte sequence " (map hex args)))
  self)

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
  path absolute?)

(method Path .init (path absolute?)
  (<- self->path path self->absolute? absolute?)
  self)

(function path (path-name)
  ; Constructs and returns the path object corresponding to path-name.
  ; Internally it just holds the string path-name as a list of filenapath functionmes.
  ;     (path "./foo/bar/buzz") -- ("." "foo" "bar" "buzz")
  ;     (path "/etc") -- ("etc")
  ; The first `~` expands to the home directory using the environment variable.
  ;     (path "~/.vimrc") <=> ("home" "foo" ".vimrc")
  ; Path class places the highest priority on keeping the implementation simple, and assumes that these features are implemente where necessary.
  ;     (path "foo/bar/../buzz") <=> ("foo" "buzz")
  ; Two or more consecutive `/`s or trailing `/`s are ignored.
  ;     (path "foo//bar/") <=> ("foo" "bar")
  (if (is-a? path-name Path) path-name
      (let (path nil absolute? nil windows? (== $hostname :windows)
                 home (if windows? (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH")) (getenv "HOME"))
                 sepr? (if windows? (f (x) (in? x '("/" "\\"))) (f (x) (= x "/")))
                 parse-file (f (x s e)
                              (if (< s e)
                                  (let (file (slice x s e))
                                    (if (= file ".") (if (nil? path) (push! file path))
                                        (= file "..") (if (nil? path) (push! file path) (pop! path))
                                        (push! file path))))))
        (if (prefix? path-name "~") (<- path-name (concat home (slice path-name 1)) absolute? true)
            (|| (&& windows? (> (len path-name) 1) (= ([] path-name 1) ":"))
                (&& (! windows?) (prefix? path-name "/"))) (<- absolute? true))
        (let (s 0 e (len path-name))
          (for (i 0) (< i e) (i (++ i))
            (when (sepr? ([] path-name i))
              (parse-file path-name s i)
              (<- s (++ i))))
          (parse-file path-name s e)
          (if (nil? path) nil
              (.init (.new Path) (reverse! path) absolute?))))))

(function path.getcwd ()
  ; Returns the path corresponding to the current directory.
  (path (getcwd)))

(method Path .name ()
  ; Returns file name.
  (last self->path))

(method Path .base-name ()
  ; Returns base name (the string up to the first dot).
  ; If not including dot, returns the entire name.
  (car (split (.name self) ".")))

(method Path .suffix (:opt new-suffix)
  ; Returns the suffix (the string after the last dot).
  ; If not including dot, returns nil.
  (if (nil? new-suffix) (let (name (.name self) pos (last-index "." name))
                          (if (nil? pos) ""
                              (slice name (++ pos))))
      (.resolve self (concat "../" (.but-suffix self) "." new-suffix))))

(method Path .but-suffix ()
  ; Returns the name without the suffix.
  (let (name (.name self) pos (last-index "." name))
    (if (nil? pos) name
        (slice name 0 pos))))

(method Path .parent ()
  ; Returns the parent path
  ; If the receiver is root directory, returns nil.
  ; However, the receiver is relative path, non-root directory may return nil.
  (.resolve self ".."))

(method Path .resolve (p)
  ; Resolve the given path against this path.
  ; If the argument is a character string, convert it to a path object before processing.
  ; If the path parameter is an absolute path then this method trivially returns path
  ; Otherwise this method concatenate this path and the speciifed path.
  (if (.absolute? (<- p (path p))) p
      (path (concat (.to-s self) "/" (.to-s p)))))

(method Path .relativize (p)
  ; Returns a relative path between the receiver and a given path.
  (let (relative nil src self->path dst p->path)
    (while src
      (if (= (pop! src) (car dst)) (pop! dst)
          (push! ".." relative)))
    (.init (.new Path) (concat relative dst) nil)))

(method Path .absolute? ()
  ; Returns whether this path regarded as the absolute path.
  ; Same as `(! (.relative? self))`.
  self->absolute?)

(method Path .relative? ()
  ; Same as `(! (.absolute? self))`.
  (! (.absolute? self)))

(method Path .contents ()
  ; Returns file contents of the receiver.
  (with-open ($in self :read)
    (read-bytes)))

(method Path .to-l ()
  ; Reads the contents of the file corresponding to the receiver.
  ; Returns it as a list.
  (with-open ($in self :read)
    (collect read-line)))

(method Path .to-s ()
  ; Returns a string representation of the receiver.
  (join (if (&& (!= $hostname :windows) (.absolute? self)) (cons "" self->path)
            self->path)
        "/"))

(method Path .open (mode)
  ; Returns a stream that reads the contents of the receiver.
  (if (&& (== mode :write) (.parent self)) (.mkdir (.parent self)))
  (.init (.new FileStream) (fopen (.to-s self) (index mode '(:read :write :append :update)))))

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
  (if (.dir? self) (foreach .remove (.children self)))
  (if (! (.none? self)) (remove (.to-s self)))
  self)

(method Path .move (to)
  (with-open ($out to :write)
    (write-bytes (.contents self)))
  (.remove self))

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

(method Path .walk (fn)
  (if (.dir? self) (foreach (f (x) (.walk x fn)) (.children self))
      (.file? self) (fn self)))

;;; I/O.

(class Stream ()
  ; Abstract class for reading and writing streams.
  )

(method Stream .read-byte (:rest args)
  ; Read 1byte from the receiver.
  ; Returns -1 when the stream reaches the end.
  ; Must be implemented in the inherited class.
  (raise NotImplementedError))

(method Stream .read-bytes (:opt buf from size)
  ; Reads size bytes of data from the receiver and saves it in the from position from the location specified by buf.
  ; Return the size of items read.
  ; If args is omitted, read until the stream reaches the end and returns it.
  ; Must be implemented in the inherited class.
  (raise NotImplementedError))

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

(method Stream .write-int (n :key radix padding upper?)
  ; Write an integer with the specified padding and radix.
  ; Returns n.
  (let (x n radix (|| radix 10) padding (|| padding 0)
          ->byte (f (x)
                   (if (< x 10) (+ x 0x30)
                       (! upper?) (+ x 0x61 -10)
                       (+ x 0x41 -10)))
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
        (.write-bytes self "#< ")
        (dotimes (i (len x))
          (.write-bytes self "0x")
          (.write-int self ([] x i) :radix 16 :padding 2)
          (.write-byte self 0x20))
        (.write-byte self 0x3e))
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

(method Stream .seek (offset)
  (raise NotImplementedError))

(method Stream .tell ()
  (raise NotImplementedError))

(method Stream .peek-byte ()
  (let (pos (.tell self) byte (.read-byte self))
    (.seek self pos)
    byte))

(method Stream .peek-char ()
  (let (pos (.tell self) ch (.read-char self))
    (.seek self pos)
    ch))

(class MemoryStream (Stream)
  ; A stream whose contents are held in memory.
  buf rdpos wrpos)

(method MemoryStream .init ()
  (<- self->buf (bytes 64)
      self->rdpos 0
      self->wrpos 0)
  self)

(method MemoryStream .size ()
  ; Returns the number of bytes written to the stream.
  self->wrpos)

(method MemoryStream .buf ()
  ; Returns the buffer of the receiver.
  (slice self->buf self->rdpos self->wrpos))

(method MemoryStream .reserve (size)
  (let (req (+ self->wrpos size) buf-size (len self->buf))
    (when (< buf-size req)
      (while (< (<- buf-size (* buf-size 2)) req))
      (let (buf (bytes buf-size))
        (memcpy self->buf 0 buf 0 self->wrpos)
        (<- self->buf buf)))
    self))

(method MemoryStream .read-byte ()
  ; Implementation of the Stream.read-byte.
  (let (rdpos self->rdpos)
    (if (= rdpos self->wrpos) -1
        (begin0 ([] self->buf rdpos)
                (<- self->rdpos (++ rdpos))))))

(method MemoryStream .read-bytes (:opt buf from size)
  ; Implementation of the Stream.read-bytes.
  (if (nil? buf) (begin0
                   (slice self->buf self->rdpos self->wrpos)
                   (<- self->rdpos self->wrpos))
      (let (rest (- self->wrpos self->rdpos))
        (if (< rest size) (<- size rest))
        (memcpy self->buf self->rdpos buf self->wrpos size)
        size)))

(method MemoryStream .write-byte (byte)
  ; Implementation of the Stream.write-byte.
  (let (wrpos self->wrpos)
    (.reserve self 1)
    ([] self->buf wrpos byte)
    (<- self->wrpos (++ wrpos))
    byte))

(method MemoryStream .write-bytes (x :opt from size)
  ; Implementation of the Stream.write-bytes.
  (.reserve self (|| size (<- size (byte-len x))))
  (memcpy x (|| from 0) self->buf self->wrpos size)
  (<- self->wrpos (+ self->wrpos size))
  (if from size x))

(method MemoryStream .seek (offset)
  ; Sets the file position indicator of the receiver.
  ; Returns the receiver.
  (if (! (<= 0 offset self->wrpos)) (raise IndexError)
      (<- self->rdpos offset))
  self)

(method MemoryStream .tell ()
  ; Returns current byte position in stream.
  self->rdpos)

(method MemoryStream .to-s ()
  ; Returns the contents written to the stream as a string.
  (let (size self->wrpos)
    (if (= size 0) ""
        (string self->buf 0 size))))

(method MemoryStream .reset ()
  ; Empty the contents of the stream.
  ; Returns the receiver.
  (<- self->rdpos 0
      self->wrpos 0)
  self)

(class FileStream (Stream)
  ; Provides I/O functions for files.
  ; Construct with methods such as File.open-read, File.open-write.
  ; It should not be construct by new.
  fp)

(method FileStream .init (fp)
  (<- self->fp fp)
  self)

(method FileStream .read-byte ()
  ; Implementation of the Stream.read-byte.
  (fgetc self->fp))

(method FileStream .read-bytes (:opt buf from size)
  ; Implementation of the Stream.read-bytes.
  (if (nil? buf) (let (out (.new MemoryStream) buf (bytes 1024))
                   (while (> (<- size (fread buf 0 1024 self->fp)) 0)
                     (.write-bytes out buf 0 size))
                   (.buf out))
      (fread buf from size self->fp)))

(method FileStream .read-line ()
  ; Override of the Stream.read-line.
  (fgets self->fp))

(method FileStream .write-byte (byte)
  ; Implementation of the Stream.write-byte.
  (fputc byte self->fp))

(method FileStream .write-bytes (x :opt from size)
  ; Implementation of the Stream.write-bytes.
  (fwrite x (|| from 0) (|| size (byte-len x)) self->fp)
  (if from size x))

(method FileStream .seek (offset)
  ; Sets the file position indicator of the receiver.
  ; Returns the receiver.
  (fseek self->fp offset)
  self)

(method FileStream .tell ()
  ; Returns the current value of the file position indicator of the receiver.
  (ftell self->fp))

(method FileStream .close ()
  (fclose self->fp))

(class AheadReader ()
  ; A one-character look-ahead reader.
  ; While prefetching one character at a time from a character string or Reader, if necessary, cut out a part as a token.
  ; Can be used as a syllable reader or lexical analyzer.
  stream next token lineno)

(method AheadReader .init ()
  ; initializing AheadReader with (dynamic $in).
  ; Returns the receiver.
  (<- self->stream (dynamic $in)
      self->next (.read-char self->stream)
      self->token (.new MemoryStream)
      self->lineno 1)
  self)

(method AheadReader .next ()
  ; Returns a pre-read character.
  self->next)

(method AheadReader .next? (predicate)
  ; Returns whether The end of the file has not been reached and the predicate is satisfied
  ; EOF is reached, returns nil.
  (&& self->next (predicate self->next)))

(method AheadReader .skip (:opt expected)
  ; Skip next character and returns it.
  ; Error if expected is specified and the next character is not the same as the expected.
  (let (next self->next)
    (if (nil? next) (raise EOFError "unexpected EOF")
        (&& expected (!= next expected)) (raise StateError (str "unexpected character '" next "`"))
        (= next "\n") (<- self->lineno (++ self->lineno)))
    (<- self->next (.read-char self->stream))
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
  (let (next self->next)
    (if (nil? next) (.skip self)    ; raise error.
        (= next "\n") (begin (.skip self) "")
        (let (line (.read-line self->stream))
          (if (nil? line) (<- self->next nil)
              (<- line (concat next line)
                  self->lineno (++ self->lineno)
                  self->next (.read-char self->stream)))
          line))))

(method AheadReader .skip-space ()
  ; Skip as long as a space character follows.
  ; Returns self.
  (while (.next? self space?) (.skip self))
  self)

(method AheadReader .skip-sign ()
  (let (next self->next)
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
  (let (val (.skip-uint self) next self->next)
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
  (.write-bytes self->token o)
  o)

(method AheadReader .token ()
  ; Returns the token string currently cut out.
  ; In the process of processing, token is initialized.
  (begin0 (.to-s self->token)
          (.reset self->token)))

(method AheadReader .stream ()
  ; Returns the stream held by the receiver.
  self->stream)

(method AheadReader .to-s ()
  (str "<" (.symbol (.class self)) ":0x" (address self) " "
       (list :next self->next :lineno self->lineno) ">"))

(class ParenLexer (AheadReader))

(method ParenLexer .identifier-symbol-alpha? ()
  (|| (in? self->next "!#$%&*./<=>?^[]_{|}~")
      (.next? self alpha?)))

(method ParenLexer .identifier-sign? ()
  (in? self->next "+-"))

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
  (while (!= self->next "\"") (.get-escape self))
  (.skip self "\"")
  (.token self))

(method ParenLexer .lex ()
  (.skip-space self)
  (let (next self->next)
    (if (nil? next) '(:EOF)
        (= next "(") (begin (.skip self) '(:open-paren))
        (= next ")") (begin (.skip self) '(:close-paren))
        (= next "'") (begin (.skip self) '(:quote))
        (= next "`") (begin (.skip self) '(:backquote))
        (= next ",") (begin (.skip self) (if (= self->next "@") (begin (.skip self) '(:unquote-splicing)) '(:unquote)))
        (= next "\"") (list :atom (.lex-string self))
        (= next ":") (list :atom (.lex-keyword self))
        (= next ";") (begin (.skip-line self) (.lex self))
        (= next "#") (begin (.skip self) (list :reader-macro (symbol (.skip self))))
        (in? next "+-") (list :atom (.lex-sign self))
        (digit? next) (list :atom (.skip-number self))
        (list :atom (.lex-symbol self)))))

(class ParenReader ()
  lexer token-type token)

(method ParenReader .init ()
  (<- self->lexer (.new ParenLexer))
  self)

(method ParenReader .scan ()
  (let ((token-type :opt token) (.lex self->lexer))
    (<- self->token-type token-type
        self->token token)
    self))

(method ParenReader .parse-list ()
  (let (parse-cdr (f (acc)
                    (.scan self)
                    (if (== self->token-type :close-paren) (reverse! acc)
                        (== self->token-type :EOF) (raise SyntaxError "missing close-paren")
                        (parse-cdr (cons (.parse self) acc)))))
    (parse-cdr nil)))

(method ParenReader .parse ()
  (let (type self->token-type)
    (if (== type :EOF) nil
        (== type :atom) self->token
        (== type :open-paren) (.parse-list self)
        (== type :quote) (list 'quote (.parse (.scan self)))
        (== type :backquote) (list 'quasiquote (.parse (.scan self)))
        (== type :unquote) (list 'unquote (.parse (.scan self)))
        (== type :unquote-splicing) (list 'unquote-splicing (.parse (.scan self)))
        (== type :reader-macro) (apply (|| ([] $read-table self->token) (raise ArgumentError "undefined reader macro")) (list self))
        (raise SyntaxError))))

(macro unquote (expr)
  (list 'raise 'SyntaxError (str "unexpected unquote -- ," expr)))

(macro unquote-splicing (expr)
  (list 'raise 'SyntaxError (str "unexpected unquote-splicing -- ,@" expr)))

(macro quasiquote (expr)
  (let (descend
         (f (x level)
           (if (atom? x) (list 'quote x)
               (let (ope (car x))
                 (if (= ope 'quasiquote) (list 'cons ''quasiquote (descend (cdr x) (++ level)))
                     (= ope 'unquote) (if (= level 0) (cadr x)
                                          (list 'cons ''unquote (descend (cdr x) (-- level))))
                     (= ope 'unquote-splicing) (if (= level 0)
                                                   (cadr x) (list 'cons ''unquote-splicing (descend (cdr x) (-- level))))
                     (list 'concat (descend-car (car x) level) (descend (cdr x) level))))))
         descend-car
         (f (x level)
           (if (atom? x) (list 'quote (list x))
               (let (ope (car x))
                 (if (= ope 'quasiquote) (list 'list (list 'cons ''quasiquote (descend (cdr x) (++ level))))
                     (= ope 'unquote) (if (= level 0) (cons 'list (cdr x))
                                          (list 'list (list 'cons ''unquote (descend (cdr x) (-- level)))))
                     (= ope 'unquote-splicing) (if (= level 0)
                                                   (cons 'concat (cdr x)) (list 'list (list 'cons ''unquote-splicing (descend (cdr x) (-- level)))))
                     (list 'list (list 'concat (descend-car (car x) level) (descend (cdr x) level))))))))
    (descend expr 0)))

(method ParenReader .read ()
  (.parse (.scan self)))

(macro reader-macro (next params :rest body)
  ; Define a reader macro starting with `# + next`.
  ; next must be a single character string.
  ; When the reserved character string is read, the processing moves to the specified function f and the evaluation result is expanded.
  ; Returns nil.
  (list '[] '$read-table (list 'quote (symbol next)) (cons 'f (cons params body))))

(function read-byte ()
  (.read-byte (dynamic $in)))

(function read-bytes (:opt buf from size)
  (.read-bytes (dynamic $in) buf from size))

(function read-char ()
  (.read-char (dynamic $in)))

(function read-line ()
  (.read-line (dynamic $in)))

(function read ()
  (.read (dynamic $in)))

(function write-byte (x)
  (.write-byte (dynamic $out) x))

(function write-bytes (x :opt from size)
  (.write-bytes (dynamic $out) x from size))

(function write-line (:opt x)
  (.write-line (dynamic $out) x))

(function write (x :key start end)
  (.write (dynamic $out) x :start start :end end))

(function print (:rest args)
  (foreach (compose write-bytes str) args))

(function println (:rest args)
  (begin0
    (apply print args)
    (write-line)))

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
  (cons 'let (cons (list ms '(.new MemoryStream))
                   (if (nil? s) (list (cons 'begin body) (list '.to-s ms))    ; output stream
                       (cons (list '.write-bytes ms s) body)))))              ; input stream

(macro with-open ((sym p mode) :rest body)
  ; Create file stream context.
  ; The file stream is guaranteed to be closed when exiting the context.
  ; Returns evaluation results for expression body.
  (with-gensyms (gsym)
    (list 'let (list gsym nil)
          (list 'unwind-protect
                (cons 'let (cons (list sym (list '<- gsym (list '.open (list 'path p) mode))) body))
                (list 'if gsym (list '.close gsym))))))

(macro with-process ((sym cmd mode) :rest body)
  (with-gensyms (gsym)
    (list 'let (list gsym (list 'popen cmd (list 'index mode ''(:read :write))))
          (list 'unwind-protect
                (cons 'let (cons (list sym (list '.init '(.new FileStream) gsym)) body))
                (list 'pclose gsym)))))

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
  (<- $0 nil $1 nil $2 nil $3 nil $4 nil $5 nil $6 nil $7 nil $8 nil $9 nil)
  (loop
    (catch (Error .print-stack-trace)
      (write-bytes ") ")
      (if (== (<- $$ (read)) :q) (break)
          (== $$ :h) (foreach write
                              '((:h "show this help")
                                (:r "show register")
                                (:q "quit repl")))
          (== $$ :r) (foreach (f (x) (write (list x (eval x))))
                              (map (compose symbol (partial str "$"))
                                   (.. 10)))
          (<- $9 $8 $8 $7 $7 $6 $6 $5 $5 $4 $4 $3 $3 $2 $2 $1 $1 $0 $0 (write (eval $$)))))))

(function raise (cls :rest args)
  ; Throw the cls Class instance which initialized with argument args.
  (throw (apply .init (cons (.new cls) args))))

(function quit ()
  (raise SystemExit))

(built-in-function exit (status-cd))

(function load (file)
  (if (keyword? file) (<- file (str file ".p")))
  (with-open ($in file :read)
    (foreach eval (collect read))
    true))

(function import (key :opt dir)
  (if (in? key $import) key
      (let ($G-module (.resolve (if dir (path dir) (.resolve $paren-home "modules"))
                                (concat (string key) ".p")))
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
        (let (file-name (path (car args)) script (select1 .file?
                                                          (map (f (x) (apply .resolve x))
                                                               (product (cons (path.getcwd) $runtime-path)
                                                                        (list file-name (.suffix file-name "p"))))))
          (if (nil? script) (raise ArgumentError (str "unreadable file " file-name))
              (&& (load script) (bound? 'main) main) (main (cdr args)))))))

(<- $import '(:core)
    $read-table (dict)
    ($stdin $stdout $stderr) (map (f (x) (.init (.new FileStream) (fp x))) (.. 3))
    ($in $out) (list $stdin $stdout)
    $paren-home (.parent (.parent (.resolve (path.getcwd) core.p)))
    $parenrc (path "~/.parenrc")
    $runtime-path (map (f (p) (.resolve $paren-home p)) '("scripts")))

(reader-macro "<" (reader)
  ; Define bytes literal reader.
  (let ($G-val nil $G-pos 0 $G-buf (bytes 8))
    (while (!= (<- $G-val (.read reader)) '>)
      (when (= $G-pos (len $G-buf))
        (let ($G-newbuf (bytes (* (len $G-buf) 2)))
          (memcpy $G-buf 0 $G-newbuf 0 $G-pos)
          (<- $G-buf $G-newbuf)))
      ([] $G-buf $G-pos $G-val)
      (<- $G-pos (++ $G-pos)))
    (slice $G-buf 0 $G-pos)))

(reader-macro "[" (reader)
  ; Define array literal reader.
  (let ($G-l nil $G-v nil)
    (while (!= (<- $G-v (.read reader)) ']) (push! $G-v $G-l))
  (array (reverse! $G-l))))

(reader-macro "{" (reader)
  ; Define dictionary literal reader.
  (let ($G-d (dict) $G-k nil)
    (while (!= (<- $G-k (.read reader)) '}) ([] $G-d $G-k (.read reader)))
    $G-d))

(reader-macro "p" (reader)
  ; Define print reader macro.
  (list 'write (.read reader)))

(reader-macro "." (reader)
  ; Define eval reader.
  (eval (.read reader)))

(reader-macro ";" (reader)
  ; Define comment reader.
  (.read reader)    ; skip
  (.read reader))

(boot $args)
