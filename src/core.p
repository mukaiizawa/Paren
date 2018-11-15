; paren core library.

; fundamental macro

(<- list (lambda (:rest args) args))
(assert (= (list 1 2 3) '(1 2 3)))

(macro function (name args :rest body)
  "仮引数がargs、本体がbodyであるような関数をシンボルnameに束縛する。
  argsの書式はspecial operatorのlambdaに準ずる。"
  (list <- name (cons lambda (cons args body))))

(macro begin0 (:rest body)
  "bodyを逐次評価し、最初に評価した結果を返す。"
  (let (sym (gensym))
    (cons let (cons (list sym (car body))
                    (add (cdr body) sym)))))
(assert (= (begin0 1 2 3) 1))

(macro when (test :rest body)
  "testを評価しnil以外の場合にbodyを逐次評価し、最後に評価した結果を返す。
  testがnil偽の場合はnilを返す。"
  (list if test (cons begin body)))
(assert (= (when true 1 2 3) 3))
(assert (= (when nil 1 2 3) nil))

(macro or (:rest args)
  "argsを逐次評価し、ある評価結果がnil以外だった場合にその値を返す。
  この場合、後続の評価は行わない。
  すべての評価結果がnilの場合はnilを返す。"
  (if args (list if (car args) (car args) (cons or (cdr args)))))
(assert !(or))
(assert !(or nil))
(assert (or nil true))

(macro and (:rest args)
  "argsを逐次評価し、すべてnil出ない場合にのみ最後に評価した結果を返す。
  逐次評価の過程でnilが得られた場合は後続の評価を中断し即nilを返す。
  ただし、argsがnilの場合はtrueを返す。"
  (if (nil? args) true
      (list if (car args) (cons and (cdr args)))))
(assert (and))
(assert (and true true true))
(assert !(and true nil true))

(macro while (test :rest body)
  "引数testがnilでない間bodyを逐次評価する。
  lambdaを含む式に展開されるため、returnオペレータを使用することによりwhileコンテキストを抜けることができる。"
  (list (list lambda nil
              (cons labels
                    (cons :while
                          (cons (list if (list not test) (list return nil))
                                (add body '(goto :while))))))))

; fundamental function

(function identity (x)
  "xを返す。恒等関数。"
  x)
(assert (same? (identity :a) :a))

(function not (x)
  "xがnilの場合はtrueを、そうでなければnilを返す。"
  (same? x nil))
(assert !nil)
(assert !!true)
(assert (same? !'x nil))
(assert (same? !nil true))

(macro nil? (x)
  "式(not x)に等価。"
  (list not x))
(assert (nil? (nil? true)))
(assert (nil? nil))
(assert !(nil? true))

(function cons? (x)
  "xがコンスの場合はtrueを、そうでなければnilを返す。"
  (same? ($$type x) :cons))
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

(function list? (x)
  "xがコンスまたはnilの場合はtrueを、そうでなければnilを返す。"
  (or (nil? x) (cons? x)))
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

(function atom? (x)
  "xがアトムの場合にtrueを、そうでなければnilを返す。"
  !(cons? x))
(assert (atom? 1))
(assert (atom? nil))
(assert !(atom? '(1)))

(function number? (x)
  "xが数値の場合にtrueを、そうでなければnilを返す。"
  (same? ($$type x) :number))
(assert !(number? nil))
(assert !(number? (lambda (x) x)))
(assert (number? 3.14))
(assert (number? 0x20))

(function symbol? (x)
  "xがシンボルの場合にtrueを、そうでなければnilを返す。"
  (same? ($$type x) :symbol))
(assert !(symbol? (lambda (x) x)))
(assert !(symbol? 3.14))
(assert (symbol? (gensym)))
(assert (symbol? nil))

(function function? (x)
  "xが関数の場合にtrueを、そうでなければnilを返す。"
  (same? ($$type x) :lambda))
(assert !(function? 3.14))
(assert !(function? (macro (x) x)))
; todo (assert (function? car))
(assert (function? function?))
(assert (function? (lambda (x) x)))

(<- caar (lambda (x) (car (car x)))
    cadr (lambda (x) (car (cdr x)))
    cdar (lambda (x) (cdr (car x)))
    cddr (lambda (x) (cdr (cdr x)))
    caaar (lambda (x) (car (caar x)))
    caadr (lambda (x) (car (cadr x)))
    cadar (lambda (x) (car (cdar x)))
    caddr (lambda (x) (car (cddr x)))
    cdaar (lambda (x) (cdr (caar x)))
    cdadr (lambda (x) (cdr (cadr x)))
    cddar (lambda (x) (cdr (cdar x)))
    cdddr (lambda (x) (cdr (cddr x)))
    caaaar (lambda (x) (car (caaar x)))
    caaadr (lambda (x) (car (caadr x)))
    caadar (lambda (x) (car (cadar x)))
    caaddr (lambda (x) (car (caddr x)))
    cadaar (lambda (x) (car (cdaar x)))
    cadadr (lambda (x) (car (cdadr x)))
    caddar (lambda (x) (car (cddar x)))
    cadddr (lambda (x) (car (cdddr x)))
    cdaaar (lambda (x) (cdr (caaar x)))
    cdaadr (lambda (x) (cdr (caadr x)))
    cdadar (lambda (x) (cdr (cadar x)))
    cdaddr (lambda (x) (cdr (caddr x)))
    cddaar (lambda (x) (cdr (cdaar x)))
    cddadr (lambda (x) (cdr (cdadr x)))
    cdddar (lambda (x) (cdr (cddar x)))
    cddddr (lambda (x) (cdr (cdddr x))))
(assert (same? (caar '((z))) 'z))
(assert (same? (cadr '(x z)) 'z))
(assert (= (cdar '((x z))) '(z)))
(assert (= (cddr '(x x z)) '(z)))
(assert (same? (caaar '(((z)))) 'z))
(assert (same? (caadr '(x (z))) 'z))
(assert (same? (cadar '((x z))) 'z))
(assert (same? (caddr '(x x z)) 'z))
(assert (= (cdaar '(((x z)))) '(z)))
(assert (= (cdadr '(x (x z))) '(z)))
(assert (= (cddar '((x x z))) '(z)))
(assert (= (cdddr '(x x x z)) '(z)))
(assert (same? (caaaar '((((z))))) 'z))
(assert (same? (caaadr '(x ((z)))) 'z))
(assert (same? (caadar '((x (z)))) 'z))
(assert (same? (caaddr '(x x (z))) 'z))
(assert (same? (cadaar '(((x z)))) 'z))
(assert (same? (cadadr '(x (x z))) 'z))
(assert (same? (caddar '((x x z))) 'z))
(assert (same? (cadddr '(x x x z)) 'z))
(assert (= (cdaaar '((((x z))))) '(z)))
(assert (= (cdaadr '(x ((x z)))) '(z)))
(assert (= (cdadar '((x (x z)))) '(z)))
(assert (= (cdaddr '(x x (x z))) '(z)))
(assert (= (cddaar '(((x x z)))) '(z)))
(assert (= (cddadr '(x (x x z))) '(z)))
(assert (= (cdddar '((x x x z))) '(z)))
(assert (= (cddddr '(x x x x z)) '(z)))

(function ->list (x)
  "xがリストの場合にxを、そうでなければxをリストにして返す。"
  (if (list? x) x (list x)))

(function nth (lis n)
  "リストlisのn番目の要素を返す。
  ただし、nは零から数える。
  nがlisの長さよりも大きい場合はnilを返す。"
  (precondition (list? lis))
  (car (nthcdr lis n)))
(assert (= (nth '(1 2 3) 0) 1))
(assert (= (nth '(1 2 3) 10) nil))

(function nthcdr (lis n)
  "リストlisを構成するn番目のコンスを取得する。
  nがlisの長さよりも大きい場合はnilを返す。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (= n 0) lis
      :default (nthcdr (cdr lis) (-- n))))
(assert (= (nthcdr '(1 2 3) 1) '(2 3)))

(function sublist (lis s :opt e)
  "リストlisのs番目からe - 1番目までを要素に持つ部分リストを返す。
  sが零未満、eがリストの長さ以上、sがeより大きい場合はエラーと見做す。
  部分リストはlisとは別に作成される。"
  (let (len (length lis)
        e (or e len)
        rec (lambda (lis n)
              (if (= n 0) nil
                  (cons (car lis) (rec (cdr lis) (-- n))))))
    (precondition (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr lis s) (- e s))))
(assert (= (sublist nil 0) nil))
(assert (= (sublist '(1 2 3) 1) '(2 3)))
(assert (= (sublist '(1 2 3) 1 2) '(2)))

(function copy-list (lis)
  "リストlisの複製を作成して返す。
  ただし、要素は複製されない。"
  (precondition (list? lis))
  (sublist lis 0 (length lis)))
(assert (= (copy-list '(1 2 3)) '(1 2 3)))

(function last-cons (lis)
  "リストlisを構成する最後のコンスを返す。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (let (rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis)))
        (rec lis))))
(assert (= (last-cons '(1 2 3)) '(3)))

(function last (lis)
  "リストlisの最後の要素を返す。"
  (precondition (list? lis))
  (car (last-cons lis)))
(assert (= (last '(1 2 3)) 3))

(function length (lis)
  "リストの要素数を返す。"
  (precondition (list? lis))
  (let (rec (lambda (lis n) (if (nil? lis) n (rec (cdr lis) (++ n)))))
    (rec lis 0)))
(assert (= (length nil) 0))
(assert (= (length '(1 2 3)) 3))

(function adds (lis l)
  "リストlisの末尾にリストargsのすべての要素を追加する。
  lisは破壊的に変更される。"
  (precondition (and (list? lis) (list? l)))
  (if (nil? lis) (<- lis l)
      (cdr (last-cons lis) l))
  lis)
(assert (= (adds '(1) '(2 3 4)) '(1 2 3 4)))
(assert (= (adds nil '(1 2 3)) '(1 2 3)))

(function add (lis o)
  "リストlisの末尾に引数oを破壊的に追加する。"
  (precondition (list? lis))
  (adds lis (list o)))
(assert (= (add nil 1) '(1)))
(assert (= (add '(1) '(2 3 4)) '(1 (2 3 4))))

(macro push (sym x)
  "シンボルsymを束縛しているリストの先頭に破壊的にxを追加する。"
  (precondition (symbol? sym))
  (list begin
        (list precondition (list list? sym))
        (list <- sym (list cons x sym))
        :SideEffects))
(assert (= (begin (<- l nil) (push l 1) (push l 2) l) '(2 1)))

(macro pop (sym)
  "シンボルsymを束縛しているリストの先頭を返し、symをリストのcdrで再束縛する。"
  (precondition (symbol? sym))
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))
(assert (= (begin (<- l '(1 2 3)) (pop l)) 1))

(function flatten (lis)
  "リストlisを構成するすべてのコンスのcar部が要素であるような新しいリストを返す。
  作成されるリストの要素の順は、元のリストのcar優先探索となる。"
  (precondition (list? lis))
  (let (acc nil rec (lambda (x)
                      (if (nil? x) (reverse acc)
                          (atom? x) (push acc x)
                          (nil? (car x)) (push acc nil)
                          (begin (rec (car x))) (rec (cdr x)))))
    (rec lis)))
(assert (= (flatten '(1 (2) (3 4))) '(1 2 3 4)))
(assert (= (flatten '(1 (nil) 2)) '(1 nil 2)))

(function map (args f)
  "リストargsの各々の要素を関数fで写像した結果をリストにして返す。"
  (precondition (list? args))
  (if args (cons (f (car args)) (map (cdr args) f))))
(assert (= (map '(1 2 3) (lambda (x) (+ x 10))) '(11 12 13)))

(function reverse (lis)
  "リストlisの要素を逆の順で持つリストを新たに作成して返す。"
  (precondition (list? lis))
  (let (rec (lambda (lis acc)
              (if (nil? lis) acc (rec (cdr lis) (cons (car lis) acc)))))
    (rec lis nil)))
(assert (= (reverse nil) nil))
(assert (= (reverse '(1 2 3)) '(3 2 1)))

(function reduce (lis f :key (identity nil identity?))
  "リストlisを二変数関数fで畳み込んだ結果を返す。
  キーワードパラメターidentityが指定された場合は単位元として使用する。"
  (precondition (list? lis))
  (let (rec (lambda (lis)
              (if (nil? (cdr lis)) (car lis)
                  (rec (cons (f (car lis) (cadr lis)) (cddr lis))))))
    (rec (if identity? (cons identity lis) lis))))

(function find (lis e :key (test =) (key identity))
  "リストlisの先頭からeに等しい要素を返す。
  eが存在しない場合はnilを返す。
  比較は=で行われ、testで指定された場合はそれを用いる。
  keyが指定された場合は要素をkey関数で評価した後に比較を行う。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (test (key (car lis)) e) (car lis)
      (find (cdr lis) e :test test :key key)))
(assert (= (find nil true) nil))
(assert (= (find '(true nil true) nil) nil))
(assert (= (find '(1 2 3) 2) 2))
(assert (= (find '(1 (2 3) 4) '(2 3)) '(2 3)))
(assert (= (find '(1 (2 3) 4) '(2 3) :test same?) nil))
(assert (= (find '((1 :a) (2 :b) (3 :c)) :b :key cadr) '(2 :b)))

(function find-if (lis f :key (key identity))
  "リストlisの先頭から関数fがnilを返さない最初の要素を返す。
  該当する要素が存在しない場合はnilを返す。
  keyが指定された場合は要素をkey関数で評価した後に比較を行う。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (f (key (car lis))) (car lis)
      (find-if (cdr lis) f :key key)))
(assert (= (find-if nil identity) nil))
(assert (= (find-if '(1 2 3) (lambda (x) (> x 2))) 3))
(assert (= (find-if '((:a 1) (:b 2)) (lambda (x) (= x :b)) :key car) '(:b 2)))

(function all-satisfy? (lis f)
  "リストlisのすべての要素が関数fの引数として評価したときに、nilでない値を返す場合にtrueを返す。
  そうでなければnilを返す。"
  (precondition (and (list? lis) (function? f)))
  (if (nil? lis) true
      (and (f (car lis)) (all-satisfy? (cdr lis) f))))
(assert (all-satisfy? nil cons?))
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

(function any-satisfy? (lis f)
  "リストlisのいずれかの要素が関数fの引数として評価したときにnil以外の値を返す場合はtrueを返す。
  そうでなければnilを返す。
  なお、lisが空の場合はnilを返す。"
  ; (precondition (and (list? lis) (function? f)))
  (if lis (or (f (car lis)) (any-satisfy? (cdr lis) f))))
(assert !(any-satisfy? nil number?))
(assert (any-satisfy? '(1 2 3 4 5) number?))
(assert !(any-satisfy? '(:a :b :c) number?))

(function each-pair-satisfy? (lis f)
  "リストの隣接するすべての二要素に対して二変数関数fの評価が真か否か返す。"
  ; (precondition (and (list? lis) (function? f)))
  (if (nil? (cdr lis)) true
      (and (f (car lis) (cadr lis)) (each-pair-satisfy? (cdr lis) f))))
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

; numeric
(function - (x :rest args)
  "xからargsの合計を引いた値を返す。
  argsがnilの場合はxを負にした値を返す。"
  (precondition (and (number? x) (all-satisfy? args number?)))
  (if (nil? args) (negated x)
      (+ x (negated (reduce args +)))))
(assert (= (- 1) -1))
(assert (= (- 3 2 1) 0))

(function negated (x)
  "xの符号を反転させた値を返す。"
  (* x -1))
(assert (= (negated 1) -1))
(assert (= (negated -1) 1))

(function > (:rest args)
  (each-pair-satisfy? args (lambda (x y) (< y x))))

(function <= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< y x))))

(function >= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< x y))))

(function ++ (x)
  "xに1を加えた結果を返す。"
  (precondition (number? x))
  (+ x 1))
(assert (= (++ 0) 1))

(function -- (x)
  "xから1を引いた結果を返す。"
  (precondition (number? x))
  (+ x -1))

(function even? (x)
  (= (mod x 2) 0))

(function odd? (x)
  !(even? x))

; error and exception

(macro precondition (test)
  "引数testがnilの場合に、例外を発生させる。
  関数、マクロの事前条件を定義するために使用する。"
  (list if (list not test)
        (list basic-throw :PreconditionException (list quote test))))

(macro postcondition (test)
  "引数testがnilの場合に、例外を発生させる。
  関数、マクロの事後条件を定義するために使用する。"
  (list if (list not test)
        (list basic-throw  :PostconditionException (list quote test))))

(macro assert (test)
  "testがnilの場合に例外を発生させる。
  状態異常の早期検知のために使用する。"
  (list if (list = test nil)
        (list basic-throw :AssertionFailed (list quote test))))

; pos
; (<- Object '((:super nil)
;              (:type :Object)))
; (function . (object property :opt (val nil val?))
;   (let ((pair (find object property :key car)))
;     (if (nil? pair) (assert (list :NotFountProperty property)))
;     (if val? (cdr pair val) (cdr pair))))
; (print (. Object :type)) ; :Object
; (print (. Object :type)) ; :Object
