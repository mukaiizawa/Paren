; Paren core library.

; fundamental macro

(macro function (name args :rest body)
  ; 仮引数がargs、本体がbodyであるような関数をシンボルnameに束縛する。
  ; argsの書式はspecial operatorのlambdaに準ずる。
  (if (bound? name) (throw "symbol already bound")
      (cons <- (cons name (cons (cons lambda (cons args body)) nil)))))

(macro begin0 (:rest body)
  ; bodyを逐次評価し、最初に評価した結果を返す。
  (let (sym (gensym))
    (cons let (cons (list sym (car body))
                    (append-atom (cdr body) sym)))))

(macro when (test :rest body)
  ; testを評価しnil以外の場合にbodyを逐次評価し、最後に評価した結果を返す。
  ; testがnil偽の場合はnilを返す。
  (list if test (cons begin body)))

(macro unless (test :rest body)
  ; testを評価しnilの場合にbodyを逐次評価し、最後に評価した結果を返す。
  ; testがnil偽の場合はnilを返す。
  (cons when (cons (list not test)
                   body)))

(macro or (:rest args)
  ; argsを逐次評価し、ある評価結果がnil以外だった場合にその値を返す。
  ; この場合、後続の評価は行わない。
  ; すべての評価結果がnilの場合はnilを返す。
  (if args (list if (car args) (car args) (cons or (cdr args)))))

(macro and (:rest args)
  ; argsを逐次評価し、すべてnil出ない場合にのみ最後に評価した結果を返す。
  ; 逐次評価の過程でnilが得られた場合は後続の評価を中断し即nilを返す。
  ; ただし、argsがnilの場合はtrueを返す。
  (if (nil? args) true
      (let (rec (lambda (l)
                  (if (cdr l) (list if (car l) (rec (cdr l)))
                      (car l))))
        (rec args))))

(macro continue ()
  ; :continueというラベルにジャンプする。
  ; 反復系のマクロはコンテキスト内でcontinueを使用したときに
  ; 後続の処理をスキップし反復処理を再開するように実装することが望ましい。
  '(goto :continue))

(macro break ()
  ; :breakというラベルにジャンプする。
  ; 反復系のマクロはコンテキスト内でbreakを使用したときに
  ; 反復処理を終了するように実装することが望ましい。
  '(goto :break))

(macro for (binding test update :rest body)
  ; bindingをletで束縛し、testが真の場合にbody、updateを反復評価する。
  ; continueマクロとbreakマクロをサポートしている。
  ; nilを返す。
  (list let binding
     (cons labels
           (cons :start
           (cons (list if (list not test) '(goto :break))
           (append body
           (cons :continue
           (cons update
           (list '(goto :start)
                 :break)))))))
     nil))

(macro while (test :rest body)
  ; 引数testがnilでない間bodyを逐次評価し、nilを返す。
  ; continueマクロとbreakマクロをサポートしている。
  (cons 'for (cons nil (cons test (cons nil body)))))

(macro dolist ((i l) :rest body)
  ; リストlをインデックスiを用いてイテレーションする。
  ; nilを返す。
  (assert (symbol? i))
  (let (gl (gensym))
    (cons for (cons (list gl l i (list car gl))
                    (cons gl
                    (cons (list <- gl (list cdr gl) i (list car gl))
                          body))))))

(macro dotimes ((i n) :rest body)
  ; シンボルiを0から順にn - 1まで束縛しながらbodyを反復評価する。
  ; nilを返す。
  (assert (symbol? i))
  (let (gn (gensym))
    (cons for (cons (list i 0 gn n)
              (cons (list < i gn)
              (cons (list inc! i)
                    body))))))

; fundamental function

(function list (:rest args)
  ; 引数をリストにして返す。
  args)

(function caar (x)
  ; (car (car x))に等価
  (car (car x)))

(function cadr (x)
  ; (car (cdr x))に等価
  (car (cdr x)))

(function cdar (x)
  ; (cdr (car x))に等価
  (cdr (car x)))

(function cddr (x)
  ; (cdr (cdr x))に等価
  (cdr (cdr x)))

(function caaar (x)
  ; (car (caar x))に等価
  (car (caar x)))

(function caadr (x)
  ; (car (cadr x))に等価
  (car (cadr x)))

(function cadar (x)
  ; (car (cdar x))に等価
  (car (cdar x)))

(function caddr (x)
  ; (car (cddr x))に等価
  (car (cddr x)))

(function cdaar (x)
  ; (cdr (caar x))に等価
  (cdr (caar x)))

(function cdadr (x)
  ; (cdr (cadr x))に等価
  (cdr (cadr x)))

(function cddar (x)
  ; (cdr (cdar x))に等価
  (cdr (cdar x)))

(function cdddr (x)
  ; (cdr (cddr x))に等価
  (cdr (cddr x)))

(function caaaar (x)
  ; (car (caaar x))に等価
  (car (caaar x)))

(function caaadr (x)
  ; (car (caadr x))に等価
  (car (caadr x)))

(function caadar (x)
  ; (car (cadar x))に等価
  (car (cadar x)))

(function caaddr (x)
  ; (car (caddr x))に等価
  (car (caddr x)))

(function cadaar (x)
  ; (car (cdaar x))に等価
  (car (cdaar x)))

(function cadadr (x)
  ; (car (cdadr x))に等価
  (car (cdadr x)))

(function caddar (x)
  ; (car (cddar x))に等価
  (car (cddar x)))

(function cadddr (x)
  ; (car (cdddr x))に等価
  (car (cdddr x)))

(function cdaaar (x)
  ; (cdr (caaar x))に等価
  (cdr (caaar x)))

(function cdaadr (x)
  ; (cdr (caadr x))に等価
  (cdr (caadr x)))

(function cdadar (x)
  ; (cdr (cadar x))に等価
  (cdr (cadar x)))

(function cdaddr (x)
  ; (cdr (caddr x))に等価
  (cdr (caddr x)))

(function cddaar (x)
  ; (cdr (cdaar x))に等価
  (cdr (cdaar x)))

(function cddadr (x)
  ; (cdr (cdadr x))に等価
  (cdr (cdadr x)))

(function cdddar (x)
  ; (cdr (cddar x))に等価
  (cdr (cddar x)))

(function cddddr (x)
  ; (cdr (cdddr x))に等価
  (cdr (cdddr x)))

(function identity (x)
  ; xを返す。恒等関数。
  x)

(function different? (x y)
  ; 式(not (same? x y))に等価。
  (not (same? x y)))

(function nil? (x)
  ; 式(not x)に等価。
  (not x))

(function atom? (x)
  ; xがアトムの場合はtrueを、そうでなければnilを返す。
  (not (cons? x)))

(function list? (x)
  ; xがコンスまたはnilの場合はtrueを、そうでなければnilを返す。
  (or (nil? x) (cons? x)))

(function byte? (x)
  ; xが0から255の整数の場合はtrueを、そうでなければnilを返す。
  (and (integer? x ) (<= 0 x 255)))

(function ->list (x)
  ; xがリストの場合にxを、そうでなければxをリストにして返す。
  (if (list? x) x (list x)))

(function nth (l n)
  ; リストlのn番目の要素を返す。
  ; ただし、nは零から数える。
  ; nがlの長さよりも大きい場合はnilを返す。
  (assert (list? l))
  (car (nthcdr l n)))

(function nthcdr (l n)
  ; リストlを構成するn番目のコンスを取得する。
  ; nがlの長さよりも大きい場合はnilを返す。
  (assert (and (list? l) (not (minus? n))))
  (if (nil? l) nil
      (= n 0) l
      :default (nthcdr (cdr l) (-- n))))

(function list= (x y :key (test same?))
  ; listの要素がすべて関数testでtrueを返す場合はtrueを、そうでなければnilを返す。
  (assert (and (list? x) (list? y)))
  (let (rec (lambda (x y)
              (if (and (nil? x) (nil? y)) true
                  (or (nil? x) (nil? y)) nil
                  (and (test (car x) (car y))
                       (rec (cdr x) (cdr y))))))
    (rec x y)))

(function length (l)
  ; リストlの要素数を返す。
  (assert (list? l))
  (let (rec (lambda (l)
              (if (nil? l) 0
                  (+ 1 (rec (cdr l))))))
    (rec l)))

(function sublist (l s :opt e)
  ; リストlのs番目からe - 1番目までを要素に持つ部分リストを返す。
  ; sが零未満、eがリストの長さ以上、sがeより大きい場合はエラーと見做す。
  ; 部分リストはlとは別に作成される。
  (let (len (length l)
        e (or e len)
        rec (lambda (l n)
              (if (= n 0) nil
                  (cons (car l) (rec (cdr l) (-- n))))))
    (assert (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr l s) (- e s))))

(function copy-list (l)
  ; リストlの複製を作成して返す。
  ; ただし、要素は複製されない。
  (assert (list? l))
  (if (nil? l) nil
      (sublist l 0 (length l))))

(function last-cons (l)
  ; リストlを構成する最後のコンスを返す。
  (assert (list? l))
  (if (nil? l) nil
      (let (rec (lambda (l) (if (cdr l) (rec (cdr l)) l)))
        (rec l))))

(function last (l)
  ; リストlの最後の要素を返す。
  (assert (list? l))
  (car (last-cons l)))

(function .. (s e :opt (step 1))
  ; 整数sから整数eまでstep刻みの要素を持つリストを返す。
  (assert (and (number? s) (number? e) (number? step) (/= step 0)
               (or (and (< step 0) (>= s e))
                   (and (> step 0) (<= s e)))))
  (let (acc nil test (if (> step 0) <= >=))
    (while (test s e)
      (push! acc s)
      (<- s (+ s step)))
    (reverse acc)))

(function append-atom (l x)
  ; リストlの末尾にxを追加したような新たなリストを返す。
  (assert (list? l))
  (let (rec (lambda (l)
              (if l (cons (car l) (rec (cdr l)))
                  (list x))))
    (rec l)))

(function append (l :rest args)
  ; リストlの要素としてargsの各要素を追加する。
  ; argsの任意の要素はリストでなければならない。
  (assert (and (list? l) (all-satisfy? args list?)))
  (reduce args (lambda (acc rest)
                 (reduce rest append-atom :identity acc))
          :identity l))

(macro push! (sym x)
  ; シンボルsymを束縛しているリストの先頭に破壊的にxを追加する。
  ; 式としてxを返す。
  (assert (symbol? sym))
  (list begin
        (list assert (list list? sym))
        (list <- sym (list cons x sym))
        x))

(macro pop! (sym)
  ; シンボルsymを束縛しているリストの先頭を返し、symをリストのcdrで再束縛する。
  (assert (symbol? sym))
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))

(function flatten (l)
  ; リストlisを構成するすべてのコンスのcar部が要素であるような新しいリストを返す。
  ; 作成されるリストの要素の順は、元のリストのcar優先探索となる。
  (assert (list? l))
  (let (acc nil
        rec (lambda (x)
               (if (nil? x) (reverse acc)
                   (atom? x) (begin (push! acc x) acc)
                   (nil? (car x)) (begin (push! acc nil) acc)
                   (begin (rec (car x))) (rec (cdr x)))))
    (rec l)))

(function map (args f)
  ; リストargsの各々の要素を関数fで写像した結果をリストにして返す。
  (assert (list? args))
  (if args (cons (f (car args)) (map (cdr args) f))))

(function reverse (l)
  ; リストlの要素を逆の順で持つリストを新たに作成して返す。
  (assert (list? l))
  (let (rec (lambda (l acc)
              (if (nil? l) acc
                  (rec (cdr l) (cons (car l) acc)))))
    (rec l nil)))

(function reduce (l f :key (identity nil identity?))
  ; リストlを二変数関数fで畳み込んだ結果を返す。
  ; キーワードパラメターidentityが指定された場合は単位元として使用する。
  (assert (list? l))
  (let (rec (lambda (l)
              (if (nil? (cdr l)) (car l)
                  (rec (cons (f (car l) (cadr l)) (cddr l))))))
    (rec (if identity? (cons identity l) l))))

(function find-cons (l e :key (test same?) (key identity))
  ; リストlをなすコンスのうち、car部がeに等しいコンスを返す。
  ; 探索はリストの先頭から順に行われる。
  ; 該当するコンスが存在しない場合はnilを返す。
  ; 比較は=で行われ、testで指定された場合はそれを用いる。
  ; keyが指定された場合は要素をkey関数で評価した後に比較を行う。
  (assert (list? l))
  (if (nil? l) nil
      (test (key (car l)) e) l
      (find-cons (cdr l) e :test test :key key)))

(function find-cons-if (l f :key (key identity))
  ; リストlをなすコンスのうち、car部が関数fの引数として評価されたときにnilとならないものを返す。
  ; 探索はリストの先頭から順に行われる。
  ; 該当するコンスが存在しない場合はnilを返す。
  ; keyが指定された場合はcar部をkey関数で評価した後に比較を行う。
  (assert (list? l))
  (if (nil? l) nil
      (f (key (car l))) l
      (find-cons-if (cdr l) f :key key)))

(function find (l e :key (test same?) (key identity))
  ; リストlの先頭からeに等しい要素を返す。
  ; eが存在しない場合はnilを返す。
  ; 比較は=で行われ、testで指定された場合はそれを用いる。
  ; keyが指定された場合は要素をkey関数で評価した後に比較を行う。
  (assert (list? l))
  (car (find-cons l e :test test :key key)))

(function find-if (l f :key (key identity))
  ; リストlの先頭から関数fがnilを返さない最初の要素を返す。
  ; 該当する要素が存在しない場合はnilを返す。
  ; keyが指定された場合は要素をkey関数で評価した後に比較を行う。
  (assert (list? l))
  (car (find-cons-if l f :key key)))

(function all-satisfy? (l f)
  ; リストlのすべての要素が関数fの引数として評価したときに、nilでない値を返す場合にtrueを返す。
  ; そうでなければnilを返す。
  (assert (and (list? l) (operator? f)))
  (if (nil? l) true
      (and (f (car l)) (all-satisfy? (cdr l) f))))

(function any-satisfy? (l f)
  ; リストlのいずれかの要素が関数fの引数として評価したときにnil以外の値を返す場合はtrueを返す。
  ; そうでなければnilを返す。
  ; なお、lが空の場合はnilを返す。
  (assert (and (list? l) (operator? f)))
  (if l (or (f (car l)) (any-satisfy? (cdr l) f))))

(function adjacent-satisfy? (l f)
  ; リストの隣接するすべての二要素に対して二変数関数fの評価がnil以外の場合は
  (assert (and (list? l) (operator? f)))
  (if (nil? (cdr l)) true
      (and (f (car l) (cadr l)) (adjacent-satisfy? (cdr l) f))))

; associated list
;; Parenでは、キーワードと任意のS式の対を保持するリストを連想リストという。
;; 探索は線形時間必要になるが、比較はアドレスで行われるため高速。
;; 任意のオブジェクトの対応を保持する場合はMapクラスを利用する。

(function . (al k :opt (v nil v?))
  ; 連想リストalのキー値kに対応する値を返す。
  ; 値がない場合は例外を発生させる。
  ; vが指定された場合はkに対応する値をvで上書きする。
  (assert (list? al))
  (let (rec (lambda (rest)
              (if (nil? rest) (throw "property not found")
                  (same? (car rest) k) rest
                  (rec (cddr rest))))
        pair (rec al))
    (if (nil? v?) (cadr pair)
        (car! (cdr pair) v))))

; char

(function char-space? (c)
  (assert (number? c))
  (find '(0x09 0x0A 0x0D 0x20) c :test =))

(function char-alpha? (c)
  (assert (number? c))
  (or (<= 0x41 c 0x5A) (<= 0x61 c 0x7A)))

(function char-digit? (c)
  (assert (number? c))
  (<= 0x30 c 0x39))

(function char-lower (c)
  (assert (byte? c))
  (if (<= 0x41 c 0x5A) (+ c 0x20)
      c))

(function char-upper (c)
  (assert (byte? c))
  (if (<= 0x61 c 0x7A) (- c 0x20)
      c))

(function char->digit (c :key (radix 10))
  (assert (byte? c))
  (let (n (if (char-digit? c) (- c 0x30)
              (char-alpha? c) (+ (- (char-lower c) 0x61) 10)))
    (if (and n (< n radix)) n
        (throw (.message (.new Exception) "illegal number")))))

; number

(function - (x :rest args)
  ; xからargsの合計を引いた値を返す。
  ; argsがnilの場合はxを負にした値を返す。
  (assert (and (number? x) (all-satisfy? args number?)))
  (if (nil? args) (negated x)
      (+ x (negated (reduce args +)))))

(function negated (x)
  ; xの符号を反転させた値を返す。
  (* x -1))

(function /= (x y)
  ; 数値x、yを比較した結果を返す。
  (not (= x y)))

(function > (:rest args)
  (adjacent-satisfy? args (lambda (x y) (< y x))))

(function <= (:rest args)
  (adjacent-satisfy? args (lambda (x y) (not (< y x)))))

(function >= (:rest args)
  (adjacent-satisfy? args (lambda (x y) (not (< x y)))))

(function ++ (x)
  ; xに1を加えた結果を返す。
  (assert (number? x))
  (+ x 1))

(function -- (x)
  ; xから1を引いた結果を返す。
  (assert (number? x))
  (- x 1))

(macro inc! (s :opt (v 1))
  ; sの値にvを加えた値をsに束縛する式に展開する。
  (assert (symbol? s))
  (list <- s (list '+ s v)))

(macro dec! (s :opt (v 1))
  ; sの値からvを引いた値をsに束縛する式に展開する。
  (assert (symbol? s))
  (list <- s (list '- s v)))

(function even? (x)
  ; xが偶数の場合にtrueを、そうでなければnilを返す。
  (= (mod x 2) 0))

(function odd? (x)
  ; xが奇数の場合にtrueを、そうでなければnilを返す。
  (not (even? x)))

(function plus? (x)
  ; xが正の数の場合にtrueを、そうでなければnilを返す。
  (assert (number? x))
  (> x 0))

(function zero? (x)
  ; xが0の場合はtrueを、そうでなければnilを返す。
  (assert (number? x))
  (= x 0))

(function minus? (x)
  ; xが負の数の場合はtrueを、そうでなければnilを返す。
  (assert (number? x))
  (< x 0))

; Paren object system

(<- $class nil)

(function class-exists? (cls-sym)
  (find $class cls-sym))

(function find-class (cls-sym)
  (assert (and (symbol? cls-sym) (class-exists? cls-sym)))
  (. $class cls-sym))

(function find-method (cls-sym method-sym)
  (assert (and (symbol? cls-sym) (symbol? method-sym)))
  (let (find-class-method
            (lambda (cls)
              (cadr (find-cons (. cls :methods) method-sym)))
        find-feature-method
            (lambda (features)
              (and features
                  (or (find-class-method (find-class (car features)))
                      (find-feature-method (cdr features)))))
        rec
            (lambda (cls)
                (or (find-class-method cls)
                    (find-feature-method (. cls :features))
                    (let (super (. cls :super))
                      (and super (rec (find-class super)))))))
    (let (m (rec (find-class cls-sym)))
      (if m m (throw (.message (.new Error) "method not found"))))))

(macro make-accessor (cls-sym var)
  (let (val (gensym) val? (gensym))
    (list method cls-sym (string->symbol (string+ "." (symbol->string var)))
          (list :opt (list val nil val?))
          (list if val?
                (list begin
                      (list '. 'self (symbol->keyword var) val)
                      'self)
                (list '. 'self (symbol->keyword var))))))

(macro make-method-dispatcher (method-sym)
  (let (receiver (gensym) args (gensym))
    (list function method-sym (list receiver :rest args)
          :method
          (list assert (list object? receiver))
          (list 'apply
                (list 'find-method
                      (list '. receiver :class)
                      (list quote method-sym))
                (list 'cons receiver args)))))

(function method? (o)
  (and (lambda? o)
       (same? (car (lambda-body o)) :method)))

(macro class (cls-sym (:opt (super 'Object) :rest features) :rest fields)
  (let (Object? (same? cls-sym 'Object))
    (assert (and (all-satisfy? fields symbol?) (not (class-exists? cls-sym))))
    (append
      (list begin0
            (list quote cls-sym)
            (list <- cls-sym (list quote (list :class 'Class
                                               :symbol cls-sym
                                               :super (if (not Object?) super)
                                               :features features
                                               :fields fields
                                               :methods nil)))
            (list 'push! '$class cls-sym)
            (list 'push! '$class (list quote cls-sym)))
      (map fields (lambda (var) (list 'make-accessor cls-sym var))))))

(macro method (cls-sym method-sym args :rest body)
  (assert (class-exists? cls-sym))
  (if (and (bound? method-sym)
           (not (method? (eval method-sym))))
      (throw (.message (.new Error) "symbol already bound"))
      (list begin
            (if (not (bound? method-sym))
                (list make-method-dispatcher method-sym))
            (list . cls-sym :methods
                  (list cons (list quote method-sym)
                        (list cons (cons lambda (cons (cons 'self args) body))
                              (list '. cls-sym :methods)))))))

(macro catch ((:rest handlers) :rest body)
  ; (catch ((Exception1 (e) ...)
  ;         (Exception2 (e) ...)
  ;         (Exception3 (e) ...))
  ;   ...)
  ; (basic-catch (lambda (gsym)
  ;                (if (is-a gsym Exception1) (apply (lambda (e) ...) gsym)
  ;                    (is-a gsym Exception2) (apply (lambda (e) ...) gsym)
  ;                    (is-a gsym Exception3) (apply (lambda (e) ...) gsym)
  ;                    (throw gsym)))
  ;              ...)
  (let (gargs (gensym) if-clause nil)
    (push! if-clause if)
    (push! if-clause (list 'not (list 'object? gargs)))
    (push! if-clause (list 'throw gargs))
    (dolist (h handlers)
      (push! if-clause (list 'is-a? gargs (car h)))
      (push! if-clause (list apply (cons lambda (cons (cadr h) (cddr h)))
                             (list list gargs))))
    (push! if-clause (list 'throw gargs))
    (cons 'basic-catch (cons (cons lambda (list (list gargs)
                                                (reverse if-clause)))
                             body))))

(function object? (x)
  ; xがオブジェクトの場合trueを、そうでなければnilを返す。
  ; Paren object systemでは先頭要素がキーワード:classで始まるような連想リストをオブジェクトと見做す。
  (and (list? x) (same? (car x) :class)))

(function is-a? (o cls)
  ; oがclsクラスのインスタンスの場合trueを、そうでなければnilを返す。
  (assert (and (object? o) (object? cls) (same? (cadr cls) 'Class)))
  (let (cls-sym (. cls :symbol)
                rec (lambda (o-cls-sym)
                      (and o-cls-sym
                           (or (same? o-cls-sym cls-sym)
                               (rec (. (find-class o-cls-sym) :super))))))
    (rec (. o :class))))

(class Object ()
  ; 唯一スーパークラスを持たない、クラス階層の最上位クラス。
  ; スーパークラスを指定しない場合は暗黙的にObjectクラスを継承する。
  class)

(method Object .init ()
  ; オブジェクトの初期化メソッド。
  ; 引数なしの場合は、オブジェクト生成時に自動で初期化される。
  ; クラスごとに必要に応じて固有の初期化処理を上書きする。
  self)

(method Object .equal? (o)
  ; レシーバとoが同一オブジェクトの場合にtrueを、そうでなければnilを返す。
  ; サブクラスで同等性を定義する場合はこのメソッドをオーバーロードする。
  (same? self o))

(class Class ()
  symbol super features fields methods)

(method Class .new ()
  (let (o nil cls self fields nil)
    (while cls
      (<- fields (reverse (copy-list (map (. cls :fields) symbol->keyword))))
      (while fields
        (push! o (if (same? (car fields) :class) (. self :symbol)))
        (push! o (car fields))
        (<- fields (cdr fields)))
      (<- cls (and (. cls :super) (find-class (. cls :super)))))
    (if (list= (lambda-parameter (find-method (. o :class) '.init)) '(self))
        (.init o)
        o)))

;; error, exception

(class Error ()
  ; エラークラス。
  ; 継続が困難な状態や、到達すべきでない状態を表す。
  ; throwされた場合は、原則としてcatchオペレーターで補足すべきではない。
  message)

(function Error.shouldBeImplemented ()
  (throw (.message (.new Error) "should be implemented")))

(class Exception ()
  ; 例外クラス。
  ; すべての例外クラスはこのクラスを継承する。
  ; 補足すべきでない例外を表す場合はErrorクラスを継承すること。
  message)

(method Exception .addMessage (msg)
  (assert (string? msg))
  (.message self (+ (.message self) msg)))

(method Exception .toString ()
  (let (class-name (symbol->string (.class self)) msg (.message self))
    (if msg (+ class-name " -- " msg)
        class-name)))

;; stream I/O

(class Stream ()
  ;ストリームクラス。入出力の基本的なメソッドを持つ。
  )

(method Stream .readByte (:rest args)
  (Error.shouldBeImplemented))

(method Stream .writeByte (:rest args)
  (Error.shouldBeImplemented))

(method Stream .readChar ()
  (if (same? (dynamic $encoding) :UTF-8)
          (let (utf8-exception
                   (lambda ()
                     (throw (.message (.new Exception) "illegal UTF-8")))
                trail? (lambda (b) (= (bit-and b 0xc0) 0x80))
                ms (.new MemoryStream)
                b1 (.readByte self) b2 nil b3 nil b4 nil)
            (if (< b1 0) (return :EOF)
                (< b1 0x80) (.writeByte ms b1)
                (< b1 0xc2) (utf8-exception)
                (not (trail? (<- b2 (.readByte self)))) (utf8-exception)
                (< b1 0xe0)    ; 2-byte character
                    (begin (if (= (bit-and b1 0x3e) 0) (utf8-exception))
                           (.writeByte (.writeByte ms b1) b2))
                (< b1 0xf0)    ; 3-byte character
                    (begin (<- b3 (.readByte self))
                           (if (or (and (= b1 0xe0) (= (bit-and b2 0x20) 0))
                                   (not (trail? b3)))
                               (utf8-exception))
                           (.writeByte (.writeByte (.writeByte ms b1) b2) b3))
                (< b1 0xf8)    ; 4-byte character
                    (begin (<- b3 (.readByte self) b4 (.readByte self))
                           (if (or (not (trail? b3)) (not (trail? b4))
                                   (and (= b1 0xf0) (= (bit-and b2 0x30) 0)))
                               (utf8-exception))
                           (.writeByte
                             (.writeByte
                               (.writeByte
                                 (.writeByte ms b1) b2) b3) b4))
                (utf8-exception))
            (.toString ms))
          (throw (.message (.new Exception) "unsupport encoding"))))

(method Stream .readLine (:rest args)
  (Error.shouldBeImplemented))

(method Stream .writeString (s)
  (assert (string? s))
  (let (ba (string->byte-array s))
    (dotimes (i (byte-array-length ba))
      (.writeByte self ([] ba i))))
  self)

(class MemoryStream (Stream)
  ; メモリ上に内容を保持するストリームクラス。
  buf buf-size rd-pos wr-pos)

(method MemoryStream .init ()
  (let (buf-size 256)
    (.buf-size self buf-size)
    (.buf self (byte-array buf-size))
    (.rd-pos self 0)
    (.wr-pos self 0))
  self)

(method MemoryStream .extend (size)
  (assert (integer? size))
  (let (req (+ (.wr-pos self) size) new-buf nil)
    (while (< (.buf-size self) req)
      (.buf-size self (* (.buf-size self) 2)))
    (<- new-buf (byte-array (.buf-size self)))
    (array-copy (.buf self) 0 new-buf 0  (.wr-pos self))
    (.buf self new-buf))
  self)

(method MemoryStream .writeByte (byte)
  (assert (byte? byte))
  (let (pos (.wr-pos self))
    (if (< pos (.buf-size self))
        (begin ([] (.buf self) pos byte)
               (.wr-pos self (++ pos)))
        (.writeByte (.extend self 1) byte)))
  self)

(method MemoryStream .readByte ()
  (let (pos (.rd-pos self))
    (if (>= pos (.wr-pos self)) -1
        (begin0 ([] (.buf self) pos)
                (.rd-pos self (++ pos))))))

(method MemoryStream .toString ()
  (let (pos (.wr-pos self) str (byte-array pos))
    (if (= pos 0) ""
        (byte-array->string (array-copy (.buf self) 0 str 0 pos)))))

(method MemoryStream .reset ()
  (.rd-pos self 0)
  (.wr-pos self 0))

(class FileStream (Stream)
  ; ファイルストリームクラス
  fp)

(method FileStream .init (:key fp)
  (assert fp)
  (.fp self fp))

(method FileStream .readByte ()
  (fgetc (.fp self)))

(method FileStream .writeByte (byte)
  (assert (byte? byte))
  (fputc byte (.fp self)))

(class ByteAheadReader ()
  ; 先読みリーダー。
  ; 文字列やストリームから1byte先読みを行う機能を提供するクラス。
  stream next buf)

(method ByteAheadReader .init (:key string (stream $stdin))
  ; 文字列または、ストリームのいずれかを用いてレシーバを初期化する。
  (assert (and (or (nil? string) (string? string))
               (object? stream) (is-a? stream Stream)))
  (when string
    (<- stream (.new MemoryStream))
    (.writeString stream string))
  (.stream self stream)
  (.next self (.readByte stream))
  (.buf self (.new MemoryStream))
  self)

(method ByteAheadReader .eof? ()
  ; ストリームが終端に達している場合にtrueを、そうでなければnilを返す。
  (= (.next self) -1))

(method ByteAheadReader .ensureNotEOFReached ()
  ; ストリームが終端に達していた場合は例外をスローする。
  (if (.eof? self) (throw (.message (.new Error) "EOF reached"))))

(method ByteAheadReader .skip ()
  ; 次の1byte読み飛ばし、返す。
  (.ensureNotEOFReached self)
  (begin0 (.next self)
          (.next self (.readByte (.stream self)))))

(method ByteAheadReader .get ()
  ; 次の1byteをトークンの末尾に追加し、返す。
  (let (c (.skip self))
    (.put self c)
    c))

(method ByteAheadReader .put (b)
  ; ストリームとは無関係にトークンの末尾に1byte追加する。
  (assert (byte? b))
  (.writeByte (.buf self) b))

(method ByteAheadReader .token ()
  ; 現在切り出しているbyte列を文字列にして返す。
  (.toString (.buf self)))

(method ByteAheadReader .reset ()
  ; 現在切り出しているトークン文字列を返す。
  (.reset (.buf self))
  self)

(method ByteAheadReader .skipSpace ()
  ; スペース、改行文字を読み飛ばし、レシーバを返す。
  (while (and (not (.eof? self)) (char-space? (.next self)))
    (.skip self))
  self)

(class AheadReader (ByteAheadReader)
  ; 先読みリーダー。
  ; 文字列やストリームから1文字先読みを行う機能を提供するクラス。
  )

(method AheadReader .init (:key string stream)
  ; 文字列または、ストリームのいずれかを用いてレシーバを初期化する。
  (assert (or (and string (string? string))
              (and (object? stream) (is-a? stream Stream))))
  (when string
    (<- stream (.new MemoryStream))
    (.writeString stream string))
  (.stream self stream)
  (.next self (.readChar stream))
  (.buf self (.new MemoryStream))
  self)

(method AheadReader .eof? ()
  ; ストリームが終端に達している場合にtrueを、そうでなければnilを返す。
  (same? (.next self) :EOF))

(method AheadReader .skip ()
  ; 次の1文字を読み飛ばし、返す。
  (.ensureNotEOFReached self)
  (begin0 (.next self)
          (.next self (.readChar (.stream self)))))

(method AheadReader .put (s)
  ; ストリームとは無関係にトークンの末尾に文字列sを追加する。
  (assert (string? s))
  (.writeString (.buf self) s))

(method AheadReader .skipSpace ()
  ; スペース、改行文字を読み飛ばし、レシーバを返す。
  (while (and (not (.eof? self))
              (find '(" " "\r" "\n") (.next self) :test string=))
    (.skip self))
  self)

; Paren reader

(class ParenLexer (ByteAheadReader)
  ; Paren字句解析機
  )

(method ParenLexer .raise (message)
  (throw (.message (.new Error) message)))

(method ParenLexer .identifierLead? ()
  (let (c (.next self))
    (or (find '(0x21 0x24 0x25 0x26 0x2A 0x2B 0x2D 0x2F 0x3C 0x3D 0x3E 0x3F
                0x5F 0x2E 0x5B 0x5D)
              c :test =)
        (char-alpha? c))))

(method ParenLexer .identifierTrail? ()
  (let (c (.next self))
    (or (.identifierLead? self) (char-digit? c))))

(method ParenLexer .getString ()
  (.skip self)
  (while (/= 0x22 (.next self))
    (if (.eof? self) (.raise self "quote not closed")
        (/= (.next self) 0x5C) (.get self)
        (begin (.skip self)
               (throw "todo/implement not yet"))))
  (.skip self)
  (.token self))

(method ParenLexer .getNumber (sign)
  (let (radix 10 factor 0 val 0)
    (while (char-digit? (.next self))
      (<- val (+ (* val 10) (char->digit (.skip self)))))
    (if (= (.next self) 0x78)
        (begin (.skip self)
               (<- radix (if (= val 0) 16 val)
                   val 0)
               (while (or (char-alpha? (.next self))
                          (char-digit? (.next self)))
                 (<- val (+ (* val radix)
                            (char->digit (.skip self) :radix radix)))))
        (= (.next self) 0x2E)
        (begin (.skip self)
               (<- factor 0.1)
               (while (char-digit? (.next self))
                 (<- val (+ val (* (char->digit (.skip self)) factor))
                     factor (/ factor 10)))))
    (if (and (byte? sign) (= sign 0x2D)) (negated val)
        val)))

(method ParenLexer .getKeyword ()
  (.skip self)
  (while (.identifierTrail? self) (.get self))
  (symbol->keyword (string->symbol (.token self))))

(method ParenLexer .getSymbol ()
  (while (.identifierTrail? self) (.get self))
  (string->symbol (.token self)))

(method ParenLexer .getToken ()
  (let (sign nil)
    (.reset self)
    (while (char-space? (.next self)) (.skip self))
    (if (= (.next self) 0x3B) (begin
                                (while (/= (.next self) 0x0A) (.skip self))
                                (.getToken self))
        (.eof? self) :EOF
        (find '(0x27 0x28 0x29) (.next self) :test =) (.skip self)
        (= (.next self) 0x22) (.getString self)
        (= (.next self) 0x3A) (.getKeyword self)
        (begin (if (find '(0x2B 0x2D) (.next self) :test =)
                   (<- sign (.skip self)))
               nil) :unreachable
        (char-digit? (.next self)) (.getNumber self sign)
        (or sign (.identifierLead? self)) (begin (if sign (.put self sign))
                                                 (.getSymbol self))
        (.raise self (list (.next self) "illegal char")))))

(class ParenParser ()
  ; Paren構文解析機
  lexer next)

(method ParenParser .init (:key string (stream $stdin))
  (.lexer self (.init (.new ParenLexer) :string string :stream stream))
  (.scan self)
  self)

(method ParenParser .raise (:opt message)
  (throw (.message (.new Exception) (or message "illegal token"))))

(method ParenParser .scan (:opt f)
  (if (and f (not (f (.next self)))) (.raise self "missing token")
      (let (token (.getToken (.lexer self)))
        (.next self token)
        token)))

(method ParenParser .parseList ()
  (let (close-paren? (lambda (x) (and (byte? x) (= x 0x29))))
    (if (close-paren? (.scan self)) nil
        (begin0 (cons (.parse self) (.parseCdr self))
                (.scan self close-paren?)))))

(method ParenParser .parseCdr ()
  (let (next (.next self))
    (if (and (byte? next) (= next 0x29)) nil
        (cons (.parse self) (.parseCdr self)))))

(method ParenParser .parseAtom ()
  (let (atom (.next self))
    (if (or (symbol? atom) (keyword? atom) (string? atom) (number? atom))
        (begin0 atom (.scan self))
        (.raise self))))

(method ParenParser .parse ()
  (let (next (.next self))
    (if (and (byte? next) (= next 0x27)) (begin (.scan self)
                                                (list quote (.parse self)))
        (and (byte? next) (= next 0x28)) (.parseList self)
        (.parseAtom self))))

;; I/O
(<- $stdin (.init (.new FileStream) :fp (fp 0))
    $stdout (.init (.new FileStream) :fp (fp 1))
    $in $stdin
    $out $stdout
    $encoding (if (same? $os :Windows) :CP932 :UTF-8)
    $support-encodings '(:UTF-8 :CP932))

(function read-byte (:opt (stream $stdin))
  (assert (is-a? stream Stream))
  (.readByte stream))

(function read-char (:opt (stream $stdin))
  ; streamから1byte読み込み返す。
  (assert (is-a? stream Stream))
  (.readChar stream))

(function write-byte (byte :opt (stream $stdout))
  ; streamに1byte書き込みbyteを返す。
  (assert (and (byte? byte) (is-a? stream Stream)))
  (.writeByte stream byte)
  byte)

(function write-string (s :opt (stream $stdout))
  ; streamに文字列を書き込みsを返す。
  (assert (and (string? s) (is-a? stream Stream)))
  (.writeString stream s)
  s)

(function read (:opt (stream (dynamic $stdin)))
  (.parse (.init (.new ParenParser) :stream stream)))

(function repl ()
  (while true
    (write-string ") ")
    (print (eval (read)))))

; ------------------------------------------------------------------------------
; testing for development.
(print (os_clock))

; (let ($encoding :UTF-8)
;   (<- ar (.init (.new AheadReader) :string "あいう"))
;   (print :init)
;   (print (.get ar))
;   (print (.get ar))
;   (print (.get ar))
;   (print (.token ar)))

(function fib (x)
  (if (> x 1) (+ (fib (-- x)) (fib (- x 2)))
      1))
(print (map (.. 0 15) fib))

(repl)

; (assert (= (read) 3))
; (assert (= (read) nil))
; (assert (= (read) :aaa))
; (assert (string= (read) "str"))
; (assert (string= (read) "文字列"))
; (assert (list= (read) '(list)))
; (assert (list= (read) '(list :1 :2 :3)))

(print (os_clock))
; ------------------------------------------------------------------------------

; ./paren
; )
;
;  /paren xxx.p
;      load xxx.p
;
;  /paren
;      invoke repl
;
;
; (if $args (loop (print (eval (read))))
;     (load xxx.p))
