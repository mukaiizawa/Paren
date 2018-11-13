; paren core library.

; fundamental operators

(<- list (lambda (:rest args) args))
(assert (= (list 1 2 3) '(1 2 3)))

(macro function (name args :rest body)
  "argsを引数にbodyを実行する関数をシンボルnameに束縛する。
  argsに指定できる書式はspecial-operator lambdaを参照のこと。"
  (list <- name (cons lambda (cons args body))))

(macro begin0 (:rest body)
  "最初に評価した結果を返す点を除いてbeginと等価。"
  (let (sym (gensym))
    (cons let (cons (list sym (car body))
                    (add (cdr body) sym)))))

(macro when (test :rest body)
  "testを評価結果がnil以外の場合にbodyを逐次評価する。"
  (list if test (cons begin body)))

(macro or (:rest expr)
  "リストexprの要素を逐次評価し、評価結果がnilでない場合にその値を返す。この場合、後続の評価は行わない。
  すべての評価結果がnilの場合はnilを返す。"
  (if expr (list if (car expr) (car expr) (cons or (cdr expr)))))
(assert !(or))
(assert (or nil nil true))
(assert !(or nil nil nil))

(macro and (:rest expr)
  "リストexprの要素を逐次評価し、最後に評価した値を返す。ただし、逐次評価の過程でnilが得られた場合はnilを返す。
  ただし、exprがnilの場合はtrueを返す。"
  (if (nil? expr) true
      (list if (car expr) (cons and (cdr expr)))))
(assert (and))
(assert (and true true true))
(assert !(and true nil true))

(macro while (test :rest body)
  "testがnilでない間bodyを逐次評価する。
  whileはlambdaを含む式に展開される。そのためreturnオペレータを使用することによりwhileコンテキストを抜けることができる。"
  (list (list lambda nil
              (cons labels
                    (cons :while
                          (cons (list if (list not test) (list return nil))
                                (add body '(goto :while))))))))

; error and exception

(macro precondition (test)
  "testがnilの場合に、システムを終了する。
  関数、マクロが評価される事前条件を定義するために使用する。"
  (list if (list not test)
        (list basic-throw :PreconditionException (list quote test))))

(macro postcondition (test)
  "testがnilの場合に、システムを終了する。
  関数、マクロ評価後の事後条件を定義するために使用する。"
  (list if (list not test)
        (list basic-throw  :PostconditionException (list quote test))))

(macro assert (test)
  "testがnilの場合にその旨を通知してParenを強制終了する。
  状態異常の早期検知のために使用する。"
  (list if (list = test nil)
        (list begin
              (list basic-throw :AssertionFailed (list quote test)))))

; fundamental function

(function identity (x)
  "引数xを返す、恒等関数。"
  x)
(assert (same? (identity :a) :a))

(function not (x)
  "引数xの否定値を返す。即ち、xがnilの場合はtrueを、そうでなければnilを返す。"
  (same? x nil))
(assert !nil)
(assert !!true)
(assert (same? !'x nil))
(assert (same? !nil true))

(macro nil? (arg)
  "関数notのエイリアス。"
  (list not arg))
(assert (nil? nil))
(assert (nil? (nil? true)))
(assert (nil? nil))
(assert !(nil? true))

(function /= (x y)
  "式!(= x y)に等価。"
  !(= x y))
(assert (/= 1 2))
(assert !(/= 1 1))

(function cons? (x)
  "引数xがコンスか否か返す。"
  (same? ($$type x) :cons))
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

(function list? (x)
  "引数xがリストか否か返す。
  即ち、(or (nil? x) (cons? x)))と等価。"
(or (nil? x) (cons? x)))
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

(function atom? (x)
  "引数xがアトムか否か返す。
  即ち、式(atom? x)と等価。"
  !(cons? x))
(assert (atom? 1))
(assert (atom? nil))
(assert !(atom? '(1)))

(function number? (x)
  "引数xが数値か否か返す。"
  (same? ($$type x) :number))
(assert !(number? nil))
(assert !(number? (lambda (x) x)))
(assert (number? 3.14))
(assert (number? 0x20))

(function symbol? (x)
  "引数xがシンボルか否か返す。"
  (same? ($$type x) :symbol))
(assert !(symbol? (lambda (x) x)))
(assert !(symbol? 3.14))
(assert (symbol? (gensym)))
(assert (symbol? nil))

(function function? (x)
  "引数xが関数か否か返す。"
  (same? ($$type x) :lambda))
(assert !(function? 3.14))
(assert !(function? (macro (x) x)))
; todo (assert (function? car))
(assert (function? function?))
(assert (function? (lambda (x) x)))

(function all-satisfy? (lis f)
  "リストlisの任意の要素に対して関数fの評価結果が真であるか返す。"
  (precondition (and (list? lis) (function? f)))
  (if (nil? lis) true
      (and (f (car lis)) (all-satisfy? (cdr lis) f))))
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

(function any-satisfy? (lis f)
  "リストlisの要素に関数fの評価結果が真であるかものが存在するか否か返す。"
  (if lis (or (f (car lis)) (any-satisfy? (cdr lis) f))))
(assert (any-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert (any-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

(function each-pair-satisfy? (lis f)
  "リストの隣接するすべての要素に対して関数fの評価が真か否か返す。"
  (if (nil? (cdr lis)) true
      (and (f (car lis) (cadr lis)) (each-pair-satisfy? (cdr lis) f))))
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

; list processor
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
  "リストlisをなすn番目のコンスを取得する。
  nがlisの長さよりも大きい場合はnilを返す。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (= n 0) lis
      :default (nthcdr (cdr lis) (-- n))))
(assert (= (nthcdr '(1 2 3) 1) '(2 3)))

(function sublist (lis s :opt e)
  "リストlisのs番目からe - 1番目までを要素に持つ部分リストを返す。
  sが零未満、eがリストの長さ以上、sがeより大きい場合はエラーと見做す。
  部分リストは元のリストとは別に作成される。"
  (let (len (length lis)
        e (or e len)
        rec (lambda (lis n)
              (if (= n 0) nil
                  (cons (car lis) (rec (cdr lis) (-- n))))))
    (precondition (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr lis s) (- e s))))
(assert (= (sublist '(1 2 3) 1) '(2 3)))
(assert (= (sublist '(1 2 3) 1 2) '(2)))

(function copy-list (lis)
  "リストの複製を作成して返す。
  ただし、リストの要素は複製されない。"
  (sublist lis 0 (length lis)))
(assert (= (copy-list '(1 2 3)) '(1 2 3)))

(function last-cons (lis)
  "リストの複製を作成して返す。
  ただし、リストの要素は複製されない。"
  (precondition (list? lis))
  (if (nil? lis) nil
      (let (rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis)))
        (rec lis))))
(assert (= (last-cons '(1 2 3)) '(3)))

(function last (lis)
  "リストの最後の要素を返す。"
  (precondition (list? lis))
  (car (last-cons lis)))
(assert (= (last '(1 2 3)) 3))

(function length (lis)
  "リストの要素数を返す。"
  (precondition (list? lis))
  (let (rec (lambda (lis n) (if (nil? lis) n (rec (cdr lis) (++ n)))))
    (rec lis 0)))
(assert (= (length '(1 2 3)) 3))

(function append (lis :rest args)
  "リストlisの末尾にリストargsのすべての要素を追加する。"
  (precondition (and (list? lis) (all-satisfy? args list?)))
  (reduce args (lambda (x y)
                 (if (nil? x) (return y))
                 (cdr (last-cons x) (copy-list y))
                 x)
          :identity (copy-list lis)))
(assert (= (append '(1) '(2) '(3 4)) '(1 2 3 4)))
(assert (= (append nil '(1) '(2 3)) '(1 2 3)))

(function add (lis o)
  "リストlisの末尾に引数oを破壊的に追加する。"
  (precondition (list? lis))
  (cdr (last-cons lis) (cons o nil))
  lis)

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

(function reduce (args f :key (identity nil identity?))
  "リストargsを二変数関数fで畳み込んだ結果を返す。
  キーワードパラメターidentityが指定された場合は単位元として使用する。"
  (precondition (list? args))
  (let (rec (lambda (args)
              (if (nil? (cdr args)) (car args)
                  (rec (cons (f (car args) (cadr args)) (cddr args))))))
    (rec (if identity? (cons identity args) args))))

(function find (lis e :key (test =) (key identity))
  "リストlisの先頭から要素eが存在する場合にeを返す。
  eが存在しない場合はnilを返す。
  比較は=関数で行われ、パラメーターtestで指定された場合はそれを用いる。
  パラメーターkeyが指定された場合は要素をkey関数で評価した後に比較を行う。"
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
  ""
  (if (nil? lis) nil
      (f (key (car lis))) (car lis)
      (find-if (cdr lis) f :key key)))
(assert (= (find-if nil identity) nil))
(assert (= (find-if '(1 2 3) (lambda (x) (> x 2))) 3))
(assert (= (find-if '((:a 1) (:b 2)) (lambda (x) (= x :b)) :key car) '(:b 2)))

; numeric
(macro inc (x :opt (y 1))
  (list <- x (list + x y)))

(macro dec (x :opt (y 1))
  (list <- x (list - x y)))

(function ++ (x)
  (+ x 1))

(function - (:rest args)
  (reduce (map (cdr args) negated) + :identity (car args)))

(function -- (x)
  (+ x -1))

(function negated (x)
  (* x -1))

(function > (:rest args)
  (each-pair-satisfy? args (lambda (x y) (< y x))))

(function <= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< y x))))

(function >= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< x y))))

(function even? (x)
  (= (mod x 2) 0))

(function odd? (x)
  !(even? x))

; pos
; (<- Object '((:super nil)
;              (:type :Object)))
; (function . (object property :opt (val nil val?))
;   (let ((pair (find object property :key car)))
;     (if (nil? pair) (assert (list :NotFountProperty property)))
;     (if val? (cdr pair val) (cdr pair))))
; (print (. Object :type)) ; :Object
; (print (. Object :type)) ; :Object
