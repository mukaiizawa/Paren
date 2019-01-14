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
(assert (= (or 1 2) 1))

(macro and (:rest args)
  "argsを逐次評価し、すべてnil出ない場合にのみ最後に評価した結果を返す。
  逐次評価の過程でnilが得られた場合は後続の評価を中断し即nilを返す。
  ただし、argsがnilの場合はtrueを返す。"
  (if (nil? args) true
      (let (rec (lambda (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))
(assert (same? (and) true))
(assert (= (and 1 2 3) 3))
(assert !(and true nil true))

(macro while (test :rest body)
  "引数testがnilでない間bodyを逐次評価し、nilを返す。
  lambdaを含む式に展開されるため、returnオペレータを使用することによりwhileコンテキストを抜けることができる。"
  (list (list lambda ()
              (cons labels
                    (cons :while
                          (cons (list if (list not test) '(return nil))
                                (add body '(goto :while))))))))

(macro dolist ((i l) :rest body)
  "リストlをインデックスiを用いてイテレーションする。nilを返す。
  returnオペレータを使用するとdolistを指定した値を返却して処理を終了する。"
  (precondition (symbol? i))
  (let (gl (gensym))
    (list (list lambda ()
                (list <- gl l)
                (cons labels
                      (cons :dolist
                            (cons (list if (list not gl) '(return nil))
                                  (cons (list <- i (list car gl))
                                        (adds body
                                              (list (list <- gl (list cdr gl))
                                                    '(goto :dolist)))))))))))

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

(function keyword? (x)
  "xがキーワードの場合にtrueを、そうでなければnilを返す。"
  (same? ($$type x) :keyword))
(assert (keyword? :a))
(assert !(keyword? nil))

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

(function nth (l n)
  "リストlのn番目の要素を返す。
  ただし、nは零から数える。
  nがlの長さよりも大きい場合はnilを返す。"
  (precondition (list? l))
  (car (nthcdr l n)))
(assert (= (nth '(1 2 3) 0) 1))
(assert (= (nth '(1 2 3) 10) nil))

(function nthcdr (l n)
  "リストlを構成するn番目のコンスを取得する。
  nがlの長さよりも大きい場合はnilを返す。"
  (precondition (list? l))
  (if (nil? l) nil
      (= n 0) l
      :default (nthcdr (cdr l) (-- n))))
(assert (= (nthcdr '(1 2 3) 1) '(2 3)))
(assert (= (nthcdr '(1 2 3) 3) nil))

(function sublist (l s :opt e)
  "リストlのs番目からe - 1番目までを要素に持つ部分リストを返す。
  sが零未満、eがリストの長さ以上、sがeより大きい場合はエラーと見做す。
  部分リストはlとは別に作成される。"
  (let (len (length l)
        e (or e len)
        rec (lambda (l n)
              (if (= n 0) nil
                  (cons (car l) (rec (cdr l) (-- n))))))
    (precondition (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr l s) (- e s))))
(assert (= (sublist nil 0) nil))
(assert (= (sublist '(1 2 3) 1) '(2 3)))
(assert (= (sublist '(1 2 3) 1 2) '(2)))

(function copy-list (l)
  "リストlの複製を作成して返す。
  ただし、要素は複製されない。"
  (precondition (list? l))
  (sublist l 0 (length l)))
(assert (= (copy-list '(1 2 3)) '(1 2 3)))

(function last-cons (l)
  "リストlを構成する最後のコンスを返す。"
  (precondition (list? l))
  (if (nil? l) nil
      (let (rec (lambda (l) (if (cdr l) (rec (cdr l)) l)))
        (rec l))))
(assert (= (last-cons nil) nil))
(assert (= (last-cons '(1 2 3)) '(3)))

(function last (l)
  "リストlの最後の要素を返す。"
  (precondition (list? l))
  (car (last-cons l)))
(assert (= (last nil) nil))
(assert (= (last '(1 2 3)) 3))

(function length (l)
  "リストの要素数を返す。"
  (precondition (list? l))
  (let (rec (lambda (l n) (if (nil? l) n (rec (cdr l) (++ n)))))
    (rec l 0)))
(assert (= (length nil) 0))
(assert (= (length '(1 2 3)) 3))

(function .. (s e :opt (step 1))
  "整数sから整数eまでstep刻みの要素を持つリストを返す。"
  (precondition (and (number? s) (number? e) (number? step) !(= step 0)
                     (or (and (< step 0) (>= s e))
                         (and (> step 0) (<= s e)))))
  (let (acc nil test (if (> step 0) <= >=))
    (while (test s e)
      (push acc s)
      (<- s (+ s step)))
    (reverse acc)))
(assert (= (.. 0 0) '(0)))
(assert (= (.. 0 2) '(0 1 2)))
(assert (= (.. 0 2 0.5) '(0 0.5 1 1.5 2)))
(assert (= (.. 0 2 10) '(0)))
(assert (= (.. -1 1 0.5) '(-1 -0.5 0 0.5 1)))

(function adds (l args)
  "リストlの末尾にリストargsのすべての要素を追加したようなリストを返す。"
  (precondition (and (list? l) (list? args)))
  (if (nil? l) args
      (let (result (copy-list l))
        (cdr (last-cons result) args)
        result)))
(assert (= (adds '(1) '(2 3 4)) '(1 2 3 4)))
(assert (= (adds nil '(1 2 3)) '(1 2 3)))

(function add (l x)
  "リストlの末尾に引数xが追加されたようなリストを返す。"
  (precondition (list? l))
  (adds l (list x)))
(assert (= (add nil 1) '(1)))
(assert (= (add '(1) '(2 3 4)) '(1 (2 3 4))))

(macro push (sym x)
  "シンボルsymを束縛しているリストの先頭に破壊的にxを追加する。
  式としてxを返す。"
  (precondition (symbol? sym))
  (list begin
        (list precondition (list list? sym))
        (list <- sym (list cons x sym))
        x))
(assert (= (begin (<- l nil) (push l 1) (push l 2) l) '(2 1)))

(macro pop (sym)
  "シンボルsymを束縛しているリストの先頭を返し、symをリストのcdrで再束縛する。"
  (precondition (symbol? sym))
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))
(assert (= (begin (<- l '(1 2 3)) (pop l)) 1))

(function flatten (l)
  "リストlisを構成するすべてのコンスのcar部が要素であるような新しいリストを返す。
  作成されるリストの要素の順は、元のリストのcar優先探索となる。"
  (precondition (list? l))
  (let (acc nil rec (lambda (x)
                      (if (nil? x) (reverse acc)
                          (atom? x) (begin (push acc x) acc)
                          (nil? (car x)) (begin (push acc nil) acc)
                          (begin (rec (car x))) (rec (cdr x)))))
    (rec l)))
(assert (= (flatten '(1 (2) (3 4))) '(1 2 3 4)))
(assert (= (flatten '(1 (nil) 2)) '(1 nil 2)))

(function map (args f)
  "リストargsの各々の要素を関数fで写像した結果をリストにして返す。"
  (precondition (list? args))
  (if args (cons (f (car args)) (map (cdr args) f))))
(assert (= (map '(1 2 3) (lambda (x) (+ x 10))) '(11 12 13)))

(function reverse (l)
  "リストlの要素を逆の順で持つリストを新たに作成して返す。"
  (precondition (list? l))
  (let (rec (lambda (l acc)
              (if (nil? l) acc (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))
(assert (= (reverse nil) nil))
(assert (= (reverse '(1 2 3)) '(3 2 1)))

(function reduce (l f :key (identity nil identity?))
  "リストlを二変数関数fで畳み込んだ結果を返す。
  キーワードパラメターidentityが指定された場合は単位元として使用する。"
  (precondition (list? l))
  (let (rec (lambda (l)
              (if (nil? (cdr l)) (car l)
                  (rec (cons (f (car l) (cadr l)) (cddr l))))))
    (rec (if identity? (cons identity l) l))))

(function find-cons (l e :key (test =) (key identity))
  "リストlをなすコンスのうち、car部がeに等しいコンスを返す。
  探索はリストの先頭から順に行われる。
  該当するコンスが存在しない場合はnilを返す。
  比較は=で行われ、testで指定された場合はそれを用いる。
  keyが指定された場合は要素をkey関数で評価した後に比較を行う。"
  (precondition (list? l))
  (if (nil? l) nil
      (test (key (car l)) e) l
      (find-cons (cdr l) e :test test :key key)))
(assert (= (find-cons nil true) nil))
(assert (= (find-cons '(true nil true) nil) '(nil true)))
(assert (= (find-cons '(1 2 3) 2) '(2 3)))
(assert (= (find-cons '(1 (2 3) 4) '(2 3)) '((2 3) 4)))
(assert (= (find-cons '(1 (2 3) 4) '(2 3) :test same?) nil))
(assert (= (find-cons '((1 :a) (2 :b) (3 :c)) :b :key cadr) '((2 :b) (3 :c))))

(function find-cons-if (l f :key (key identity))
  "リストlをなすコンスのうち、car部が関数fの引数として評価されたときにnilとならないものを返す。
  探索はリストの先頭から順に行われる。
  該当するコンスが存在しない場合はnilを返す。
  keyが指定された場合はcar部をkey関数で評価した後に比較を行う。"
  (precondition (list? l))
  (if (nil? l) nil
      (f (key (car l))) l
      (find-cons-if (cdr l) f :key key)))
(assert (= (find-cons-if nil identity) nil))
(assert (= (find-cons-if '(1 2 3) (lambda (x) (> x 2))) '(3)))
(assert (= (find-cons-if '((:a 1)) (lambda (x) (= x :a)) :key car) '((:a 1))))

(function find (l e :key (test =) (key identity))
  "リストlの先頭からeに等しい要素を返す。
  eが存在しない場合はnilを返す。
  比較は=で行われ、testで指定された場合はそれを用いる。
  keyが指定された場合は要素をkey関数で評価した後に比較を行う。"
  (precondition (list? l))
  (car (find-cons l e :test test :key key)))
(assert (= (find nil true) nil))
(assert (= (find '(true nil true) nil) nil))
(assert (= (find '(1 2 3) 2) 2))
(assert (= (find '(1 (2 3) 4) '(2 3)) '(2 3)))
(assert (= (find '(1 (2 3) 4) '(2 3) :test same?) nil))
(assert (= (find '((1 :a) (2 :b) (3 :c)) :b :key cadr) '(2 :b)))

(function find-if (l f :key (key identity))
  "リストlの先頭から関数fがnilを返さない最初の要素を返す。
  該当する要素が存在しない場合はnilを返す。
  keyが指定された場合は要素をkey関数で評価した後に比較を行う。"
  (precondition (list? l))
  (car (find-cons-if l f :key key)))
(assert (= (find-if nil identity) nil))
(assert (= (find-if '(1 2 3) (lambda (x) (> x 2))) 3))
(assert (= (find-if '((:a 1) (:b 2)) (lambda (x) (= x :b)) :key car) '(:b 2)))

(function all-satisfy? (l f)
  "リストlのすべての要素が関数fの引数として評価したときに、nilでない値を返す場合にtrueを返す。
  そうでなければnilを返す。"
  (precondition (and (list? l) (function? f)))
  (if (nil? l) true
      (and (f (car l)) (all-satisfy? (cdr l) f))))
(assert (all-satisfy? nil cons?))
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

(function any-satisfy? (l f)
  "リストlのいずれかの要素が関数fの引数として評価したときにnil以外の値を返す場合はtrueを返す。
  そうでなければnilを返す。
  なお、lが空の場合はnilを返す。"
  ; (precondition (and (list? l) (function? f)))
  (if l (or (f (car l)) (any-satisfy? (cdr l) f))))
(assert !(any-satisfy? nil number?))
(assert (any-satisfy? '(1 2 3 4 5) number?))
(assert !(any-satisfy? '(:a :b :c) number?))

(function each-pair-satisfy? (l f)
  "リストの隣接するすべての二要素に対して二変数関数fの評価がnil以外の場合は"
  ; (precondition (and (list? l) (function? f)))
  (if (nil? (cdr l)) true
      (and (f (car l) (cadr l)) (each-pair-satisfy? (cdr l) f))))
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

; association list
;; parenでは、キーワードと任意のS式の対を保持するリストを連想リストという。
;; 探索は線形時間必要になるが、比較はアドレスで行われるため高速。
;; 任意のオブジェクトの対応を保持する場合はMapクラスを利用する。

(function . (al k :opt (v nil v?))
  "連想リストalのキー値kに対応する値を返す。
  値がない場合は例外を発生させる。
  vが指定された場合はkに対応する値をvで上書きする。"
  (precondition (list? al))
  (let (rec (lambda (al)
              (if (nil? al) (basic-throw :MissingPropertyException)
                  (same? (car al) k) al
                  (rec (cddr al))))
        pair (rec al))
    (if (nil? v?) (cadr pair)
        (car (cdr pair) v))))
(assert (= (. '(:a 1 :b 2 :c 3) :a) 1))
(assert (= (begin (<- al '(:a 1 :b 2)) (. al :b -2) al) '(:a 1 :b -2)))


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
(assert (> 1 0))
(assert !(> 0 0))
(assert !(> 0 1))
(assert (> 2 1 0))
(assert !(> 2 0 1))

(function <= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< y x))))
(assert (<= 0 0))
(assert (<= 0 1))
(assert !(<= 1 0))
(assert (<= 0 1 2))
(assert !(<= 2 0 1))

(function >= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< x y))))
(assert (>= 0 0))
(assert (>= 1 0))
(assert !(>= 0 1))
(assert (>= 0 0 0))
(assert (>= 2 1 0))
(assert !(>= 2 0 1))

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
  "xが偶数の場合にtrueを、そうでなければnilを返す。"
  (= (mod x 2) 0))
(assert (even? 0))
(assert (even? 2))
(assert !(even? 3))

(function odd? (x)
  "xが奇数の場合にtrueを、そうでなければnilを返す。"
  !(even? x))
(assert !(odd? 0))
(assert (odd? 1))
(assert !(odd? 2))

; fixed byte array

(function symbol->keyword (s)
  "シンボルsをキーワードに変換する。"
  (precondition (symbol? s))
  (let (sba (symbol->byte-array s)
        copy-len (array-size sba)
        kba (byte-array (++ copy-len)))
    ([] kba 0 0x3a)
    (byte-array->symbol/keyword (array-copy sba 0 kba 1 copy-len))))
(assert (same? (symbol->keyword 'a) :a))

(function keyword->symbol (k)
  "キーワードkをシンボルに変換する。"
  (precondition (keyword? k))
  (let (kba (keyword->byte-array k)
        copy-len (-- (array-size kba))
        sba (byte-array copy-len))
    (byte-array->symbol/keyword (array-copy kba 1 sba 0 copy-len))))
(assert (same? (keyword->symbol :a) 'a))

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
        (list basic-throw :PostconditionException (list quote test))))

(macro assert (test)
  "testがnilの場合に例外を発生させる。
  状態異常の早期検知のために使用する。"
  (list if (list = test nil)
        (list basic-throw :AssertionFailedException (list quote test))))

; paren object system
(<- POS._class nil)

(function POS._registered? (cls-sym)
  (find POS._class cls-sym))

(function POS._find-class (cls-sym)
  (precondition (and (symbol? cls-sym) (POS._registered? cls-sym)))
  (. POS._class cls-sym))

(function POS._find-method (cls-sym method-sym)
  (precondition (and (symbol? cls-sym) (symbol? method-sym)))
  (let (find-class-method
            (lambda (cls)
              (cadr (find-cons (. cls :methods) method-sym)))
        find-feature-method
            (lambda (features)
              (and features
                  (or (find-class-method (POS._find-class (car features)))
                      (find-feature-method (cdr features)))))
        rec
            (lambda (cls)
                (or (find-class-method cls)
                    (find-feature-method (. cls :features))
                    (let (super (. cls :super))
                      (and super (rec (POS._find-class super)))))))
    (or (rec (POS._find-class cls-sym)) (basic-throw :MissingMethodException))))

(macro POS._make-accesser (cls-sym var)
  (let (sba (keyword->byte-array var)
        copy-len (-- (array-size sba))
        ba (byte-array (++ copy-len))
        val (gensym)
        val? (gensym))
    ([] ba 0 0x2e)
    (list method cls-sym (byte-array->symbol/keyword
                           (array-copy sba 1 ba 1 copy-len))
          (list :opt (list val nil val?))
          (list if val? (list '. 'self var val)
              (list '. 'self var)))))

(macro POS._make-method-dispatcher (method-sym)
  (let (args (gensym))
    (list macro method-sym (list :rest args)
          (list cons (list 'list 'POS._find-method
                           (list 'list '. (list car args) :class)
                           (list quote (list quote method-sym)))
                args))))

(macro class (cls-sym (:opt (super 'Object) :rest features) :rest fields)
  (precondition (and (all-satisfy? fields keyword?)
                     !(POS._registered? cls-sym)))
  (let (Object? (= cls-sym 'Object))
    (adds
      (list begin0
            (list quote cls-sym)
            (list <- cls-sym (list quote (list :class 'Class
                                               :symbol cls-sym
                                               :super (if (not Object?) super)
                                               :features features
                                               :fields fields
                                               :methods nil)))
            (list 'push 'POS._class cls-sym)
            (list 'push 'POS._class (list quote cls-sym)))
      (map fields (lambda (var)
                  (list 'POS._make-accesser cls-sym var))))))

(macro method (cls-sym method-sym args :rest body)
  (precondition (POS._registered? cls-sym))
  (list begin
        (list POS._make-method-dispatcher method-sym)
        (list . cls-sym :methods
              (list cons (list quote method-sym)
                    (list cons (cons lambda (cons (cons 'self args) body))
                          (list '. cls-sym :methods))))))

(macro feature (f-sym)
  "フィーチャーはクラスを横断して共通のメソッドを定義する仕組みである。
  フィーチャーを割り当てられたクラスはクラスで定義されたメソッドの他に、フィーチャーで定義されたメソッドを実行できる。
  クラスに同名のメソッドがあればクラスのメソッドが優先して実行される。
  スーパークラスに同名のメソッドがあった場合はフィーチャーのメソッドが優先される。"
  (precondition (symbol? f-sym))
  (list class f-sym (list 'Feature)))

(function object? (x)
  "xがオブジェクトの場合trueを、そうでなければnilを返す。
  paren object systemでは先頭要素がキーワード:classで始まるような連想リストをオブジェクトと見做す。"
  (and (list? x) (= (car x) :class)))

(function is-a? (o cls)
  "xがclsクラスのインスタンスの場合trueを、そうでなければnilを返す。"
  (precondition (and (object? o) (object? cls)))
  (let (rec (lambda (o-cls cls)
              (or (same? o-cls (. cls :symbol))
                  (let (super (. cls :super))
                    (and super (rec o-cls (POS._find-class super)))))))
    (rec (. o :class) cls)))


(class Object ()
  "唯一スーパークラスを持たない、クラス階層の最上位クラス。
  スーパークラスを指定しない場合は暗黙的にObjectクラスを継承する。"
  :class)

(method Object .equal? (o)
  "レシーバとoが同一オブジェクトの場合にtrueを、そうでなければnilを返す。
  サブクラスで同等性を定義する場合はこのメソッドをオーバーロードする。"
  (same? self o))

(class Class ()
  "レシーバとoが同一オブジェクトの場合にtrueを、"
  :symbol :super :features :fields :methods)

(method Class .new ()
  (let (o nil cls self fields nil)
    (while cls
      (<- fields (reverse (copy-list (. cls :fields))))
      (while fields
        (push o (if (same? (car fields) :class) (. self :symbol)))
        (push o (car fields))
        (<- fields (cdr fields)))
      (<- cls (and (. cls :super) (POS._find-class (. cls :super)))))
    o))

(class Feature (Class)
  "フィーチャーの基底クラス。
  すべてのフィーチャーはこのクラスを継承しなければならない。")

(class OS ()
  "OSクラス。
  ファイルのオープンなどオペレーティングシステムに関する情報を扱う。
  基本的にインスタンスを生成することはせずに、グローバルシンボルのosを参照する。")

(<- os (.new OS)
    OS._fp-list nil
    OS.stdin nil
    OS.stdout nil)

(method OS .fp (fd)
  (OS._fp fd))

(method OS .fopen (file-name mode)
  (push OS._fp-list (OS._fopen fn mode)))

(method OS .fgetc (fp)
  (OS._fgetc fp))

(method OS .fputc (byte fp)
  (OS._fputc byte fp))

(<- OS.stdin (.fp os 0)
    OS.stdout (.fp os 1))

(<- i 40)
(while (< (<- i (++ i)) 0x7f)
  (.fputc os i OS.stdout)
  (.fputc os 0x0a OS.stdout))
