; paren core library.

;: # 概要
;: コアライブラリはParenの基盤となるライブラリ群で、必要最低限の機能を提供する。

;: # global symbol
;: ## symbol nil
;: nilは次の特徴をもつシンボルである。
;: - 空のリストとして扱われる
;: - 唯一、真偽値の偽として扱われる
;:
;: 評価されると自信を返す。
;:
;: ## symbol true
;: trueは真偽値の真を代表するシンボルである。
;:
;: 評価されると自信を返す。

;: # special operator
;: スペシャルオペレーターはParenに組み込まれた特殊なオペレーターであり、ユーザが定義することはできない。また、引数の扱いはスペシャルオペレーターごとに異なる。
;:
;: ## special-operator <- :rest pairs
;: pairsをn番目のシンボルをn+1番目の式を評価した結果で束縛し、最後にシンボルを束縛した結果を返す。ただし、pairsがnilの場合にはnilを返す。
;:
;: 評価は先頭から行われることが保証されている。即ち、次の二つの式は等価である。
;:     (<- s1 v1 s2 v2 ... sn vn)
;:     (begin (<- s1 v1)
;:            (<- s2 v2)
;:            ...
;:            (<- sn vn))
;:
;: シンボルは、自身が束縛されている最も近い環境に束縛される。ただし、そのシンボルが現在の環境から辿れる環境に束縛されていない場合は大域環境に束縛する。
;:
;: pairsの長さが偶数でない場合や、奇数番目の要素がシンボルでない場合はエラーと見做す。
;:
;: ## special-operator begin :rest body
;: bodyを逐次評価し、最後に評価した式を返す。ただし、bodyがnilの場合はnilを返す。
;:
;: ## special-operator lambda (:rest args) :rest body
;: argsを引数に、bodyを評価する無名関数を返す。
;:
;: argsに指定できる書式は次の通り。
;:     <args> ::= [<required_params>] [:opt <xparams>] [:rest <symbol>] [:key <xparams>]
;:     <required_params> ::= <symbol> <symbol> ...
;:     <xparams> ::= <xparam> <xparam> ...
;:     <xparam> ::= { <symbol> | (<symbol> <initial_value> [<supplyp>]) }
;:     <symbol> -- シンボル
;:     <initial_value> -- 実引数が省略された場合の初期値
;:     <supplyp> -- 関数呼び出し時に実引数が指定されたかを保持するシンボル
;:
;: ## special-operator macro macro-name (:rest args) :rest body
;: argsを引数に、bodyを評価するマクロを生成し、シンボルmacro-nameに束縛する。
;:
;: macro-nameは省略可能で、その場合は生成したマクロはどこにも束縛しない。
;:
;: argsの書式は<required_params>に<args>を指定できる点を除いてlambdaと同じである。
;:
;: 即ち、上のBNFは次のように訂正される。
;:     <required_params> ::= { <symbol> <symbol> ... | <args> }
;:
;: ## special-operator let (:rest syms) :rest body
;: 現在の環境を親に持つ環境を新たに作成し、作成した環境にシンボルのリストsymsを順にnilで束縛する。その後、作成した環境下でbodyを逐次評価する。
;:
;: シンボルを束縛する値があらかじめわかっている場合は、次のようにシンボルと束縛する値をリストで指定することもできる。
;:     (let ((s1 v1) (s2 v2) ... (sn vn))
;:       expr1
;:       epxr2
;:       ...
;:       exprn)
;:
;: これは次の式と等価である。
;:     (let (s1 s2 ... sn)
;:       (<- s1 v1 s2 v2 ... sn vn)
;:       expr1
;:       epxr2
;:       ...
;:       exprn)
;:
;: varには第一要素がシンボルであるような長さが二のリストを渡すこともでき、その場合は第二要素の評価結果で第一要素を束縛する。
;:
;: ## special-operator if test then :opt else
;: testがnil以外の場合はthenを、そうでなければelseを評価する。
;:
;: thenがnilの場合に、elseが省略された場合はnilを返す。
;:
;: ## special-operator quote expr
;: exprを評価せずに返す。
;:
;: Parenではその使用頻度からリードマクロ'が定義されており、次の二つの式は等価である。
;:     'expr
;:     (quote expr)
;:
;: # fundamental list processer
;: Parenの最も基本的なリスト操作ユーティリティを定義する。
;:
;: ## function list :rest args
;: argsを要素とするようなリストを返す。
;:
;: ## function car lis :opt val
;: optが省略された場合、リストlisのcar部を取得する。ただし、lisがnilの場合はnilを返す。
;:
;: optが指定された場合は、lisのcar部の参照をvalに破壊的に変更する。
;:
;: ## function cdr lis :opt val
;: optが省略された場合、リストlisのcdr部を取得する。ただし、lisがnilの場合はnilを返す。
;:
;: optが指定された場合は、lisのcdr部の参照をvalに破壊的に変更する。
;:
;: ## function caar lis
;: 式(car (car lis))と等価。
;:
;: ## function cadr lis
;: 式(car (cdr lis))と等価。
;:
;: ## function cdar lis
;: 式(cdr (car lis))と等価。
;:
;: ## function cddr lis
;: 式(cdr (cdr lis))と等価。
;:
;: ## function caaar lis
;: 式(car (car (car lis)))と等価。
;:
;: ## function caadr lis
;: 式(car (car (cdr lis)))と等価。
;:
;: ## function cadar lis
;: 式(car (cdr (car lis)))と等価。
;:
;: ## function caddr lis
;: 式(car (cdr (cdr lis)))と等価。
;:
;: ## function cdaar lis
;: 式(cdr (car (car lis)))と等価。
;:
;: ## function cdadr lis
;: 式(cdr (car (cdr lis)))と等価。
;:
;: ## function cddar lis
;: 式(cdr (cdr (car lis)))と等価。
;:
;: ## function cdddr lis
;: 式(cdr (cdr (cdr lis)))と等価。
;:
;: ## function caaaar lis
;: 式(car (car (car (car lis))))と等価。
;:
;: ## function caaadr lis
;: 式(car (car (car (cdr lis))))と等価。
;:
;: ## function caadar lis
;: 式(car (car (cdr (car lis))))と等価。
;:
;: ## function caaddr lis
;: 式(car (car (cdr (cdr lis))))と等価。
;:
;: ## function cadaar lis
;: 式(car (cdr (car (car lis))))と等価。
;:
;: ## function cadadr lis
;: 式(car (cdr (car (cdr lis))))と等価。
;:
;: ## function caddar lis
;: 式(car (cdr (cdr (car lis))))と等価。
;:
;: ## function cadddr lis
;: 式(car (cdr (cdr (cdr lis))))と等価。
;:
;: ## function cdaaar lis
;: 式(cdr (car (car (car lis))))と等価。
;:
;: ## function cdaadr lis
;: 式(cdr (car (car (cdr lis))))と等価。
;:
;: ## function cdadar lis
;: 式(cdr (car (cdr (car lis))))と等価。
;:
;: ## function cdaddr lis
;: 式(cdr (car (cdr (cdr lis))))と等価。
;:
;: ## function cddaar lis
;: 式(cdr (cdr (car (car lis))))と等価。
;:
;: ## function cddadr lis
;: 式(cdr (cdr (car (cdr lis))))と等価。
;:
;: ## function cdddar lis
;: 式(cdr (cdr (cdr (car lis))))と等価。
;:
;: ## function cddddr lis
;: 式(cdr (cdr (cdr (cdr lis))))と等価。
(<- list (lambda (:rest args) args)
    caar (lambda (x) (car (car x)))
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

; (assert (= (list 1 2 3) '(1 2 3)))
; (assert (same? (caar '((z))) 'z))
; (assert (same? (cadr '(x z)) 'z))
; (assert (= (cdar '((x z))) '(z)))
; (assert (= (cddr '(x x z)) '(z)))
; (assert (same? (caaar '(((z)))) 'z))
; (assert (same? (caadr '(x (z))) 'z))
; (assert (same? (cadar '((x z))) 'z))
; (assert (same? (caddr '(x x z)) 'z))
; (assert (= (cdaar '(((x z)))) '(z)))
; (assert (= (cdadr '(x (x z))) '(z)))
; (assert (= (cddar '((x x z))) '(z)))
; (assert (= (cdddr '(x x x z)) '(z)))
; (assert (same? (caaaar '((((z))))) 'z))
; (assert (same? (caaadr '(x ((z)))) 'z))
; (assert (same? (caadar '((x (z)))) 'z))
; (assert (same? (caaddr '(x x (z))) 'z))
; (assert (same? (cadaar '(((x z)))) 'z))
; (assert (same? (cadadr '(x (x z))) 'z))
; (assert (same? (caddar '((x x z))) 'z))
; (assert (same? (cadddr '(x x x z)) 'z))
; (assert (= (cdaaar '((((x z))))) '(z)))
; (assert (= (cdaadr '(x ((x z)))) '(z)))
; (assert (= (cdadar '((x (x z)))) '(z)))
; (assert (= (cdaddr '(x x (x z))) '(z)))
; (assert (= (cddaar '(((x x z)))) '(z)))
; (assert (= (cddadr '(x (x x z))) '(z)))
; (assert (= (cdddar '((x x x z))) '(z)))
; (assert (= (cddddr '(x x x x z)) '(z)))

;: # fundamental operator
;: 関数定義、制御構造等Parenの基本的なマクロを定義する。

;: ## macro function name args :rest body
;: argsを引数にbodyを実行する関数をシンボルnameに束縛する。
;:
;: argsに指定できる書式はspecial-operator lambdaを参照のこと。
(macro function (name args :rest body)
  (list <- name (cons lambda (cons args body))))

;: ## macro cond :rest expr
;: 連想リストexprの先頭要素からキー値を逐次評価し、nilでない値を返したキー値のcdr部を逐次評価して最後に評価した結果を返す。
;:
;: キー値がnilでない要素がない場合や、exprがnilの場合はnilを返す。
;:
;: exprが連想リストでない場合はエラーと見做す。
(macro cond (:rest expr)
  (precondition (alist? expr))
  (if (nil? expr) nil
    (list if (caar expr) (cons begin (cdar expr)) (cons cond (cdr expr)))))

;: ## macro begin0 :rest body
;: 最初に評価した結果を返す点を除いてbeginと等価。
(macro begin0 (:rest body)
  (let ((sym (gensym)))
    (cons let (cons (list (list sym (car body)))
                    (add (cdr body) sym)))))

;: ## macro begin-if test :rest body
;: testを評価結果がnil以外の場合にbodyを逐次評価する。
(macro begin-if (test :rest body)
  (list if test (cons begin body)))

;: ## macro or :rest expr
;: リストexprの要素を逐次評価し、評価結果がnilでない場合にその値を返す。この場合、後続の評価は行わない。
;:
;: すべての評価結果がnilの場合はnilを返す。
(macro or (:rest expr)
  (if expr (list if (car expr) (car expr) (cons or (cdr expr)))))

;: ## macro and :rest expr
;: リストexprの要素を逐次評価し、最後に評価した値を返す。ただし、逐次評価の過程でnilが得られた場合はnilを返す。
;:
;: ただし、exprがnilの場合はtrueを返す。
(macro and (:rest expr)
  (if (nil? expr) true
    (list if (car expr) (cons and (cdr expr)))))

;: ## macro while test :rest body
;: testがnilでない間bodyを逐次評価する。
;:
;: whileはlambdaを含む式に展開される。そのためreturnオペレータを使用することによりwhileコンテキストを抜けることができる。
(macro while (test :rest body)
  (list (list lambda nil
              (cons labels
                    (cons :while
                          (cons (list if (list not test) (list return nil))
                                (add body '(goto :while))))))))

;: # error and exception
;: ## function error :rest msg
;: メッセージmsgを表示し、システムを終了する。
(function error (:rest msg)
  (print (cons :Error msg))
  (quit))

;: ## macro precondition test
;: testがnilの場合に、システムを終了する。
;:
;: 関数、マクロが評価される事前条件を定義するために使用する。
(macro precondition (test)
  (list if (list not test) (list error :PreconditionError (list quote test))))

;: ## macro postcondition test
;: testがnilの場合に、システムを終了する。
;:
;: 関数、マクロ評価後の事後条件を定義するために使用する。
(macro postcondition (test)
  (list if (list not test) (list error  :PostconditionError (list quote test))))

;: ## macro assert test
;: testがnilの場合にその旨を通知してParenを強制終了する。
;:
;: 状態異常の早期検知のために使用する。
(macro assert (test)
  (list if (list = test nil)
        (list begin
              (list print (list list :AssertionFailed (list quote test)))
              '(quit))))

;: # fundamental function
;: ## function identity x
;: 引数xを返す、恒等関数。
(function identity (x) x)

;: ## function not x
;: 引数xの否定値を返す。即ち、xがnilの場合はtrueを、そうでなければnilを返す。
(function not (x) (same? x nil))
(assert !nil)
(assert !!true)

;: ## function nil? x
;: 関数notのエイリアス。
(<- nil? not)
(assert (nil? nil))
(assert (nil? (nil? true)))

;: ## function /= x y
;: 式!(= x y)に等価。
(function /= (x y) !(= x y))
(assert (/= 1 2))
(assert !(/= 1 1))

;: ## function cons? x
;: 引数xがコンスか否か返す。
(function cons? (x)
  (same? ($$type x) :cons))
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

;: ## function list? x
;: 引数xがリストか否か返す。
;:
;: 即ち、(or (nil? x) (cons? x)))と等価。
(function list? (x)
  (or (nil? x) (cons? x)))
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

;: ## function atom? x
;: 引数xがアトムか否か返す。
;:
;: 即ち、式(atom? x)と等価。
(function atom? (x)
  !(cons? x))
(assert (atom? 1))
(assert (atom? nil))
(assert !(atom? '(1)))

;: ## function number? x
;: 引数xが数値か否か返す。
(function number? (x)
  (same? ($$type x) :number))
(assert !(number? nil))
(assert !(number? (lambda (x) x)))
(assert (number? 3.14))
(assert (number? 0x20))

;: ## function symbol? x
;: 引数xがシンボルか否か返す。
(function symbol? (x)
  (same? ($$type x) :symbol))
(assert !(symbol? (lambda (x) x)))
(assert !(symbol? 3.14))
(assert (symbol? (gensym)))
(assert (symbol? nil))

;: ## function function? x
;: 引数xが関数か否か返す。
(function function? (x)
  (same? ($$type x) :lambda))
(assert !(function? 3.14))
(assert !(function? (macro (x) x)))
; todo (assert (function? car))
(assert (function? function?))
(assert (function? (lambda (x) x)))

;: ## function alist? x
;: 引数xが連想リストか否か返す。
;:
;: 連想リストとは、すべての要素がリストであるようなリストのことをいう。
(function alist? (x)
  (and (list? x) (all-satisfy? x list?)))
; (assert !(alist? '(1)))
; (assert !(alist? '(1 ())))
; (assert (alist? nil))
; (assert (alist? '((a b) (c d))))

(function all-satisfy? (lis f)
  (precondition (and (list? lis) (function? f)))
  (if (nil? lis) true
    (and (f (car lis)) (all-satisfy? (cdr lis) f))))

(function any-satisfy? (lis f)
  (if lis (or (f (car lis)) (any-satisfy? (cdr lis) f))))

(function each-pair-satisfy? (lis f)
  (if (nil? (cdr lis)) true
    (and (f (car lis) (cadr lis)) (each-pair-satisfy? (cdr lis) f))))

; list processor
(function ->list (x)
  (if (list? x) x (list x)))

;: ## function list->alist lis
;: リストlisを連想リストにして返す。
(function list->alist (lis)
  (precondition (even? (length lis)))
  (let ((acc nil)
        (rec (lambda (lis)
               (if (nil? lis) (reverse acc)
                 (begin (push acc (list (car lis) (cadr lis)))
                        (rec (cddr lis)))))))
    (rec lis)))
; (assert (= (list->alist '(1 2 3 4)) '((1 2) (3 4))))
; (assert !(list->alist nil))

;: ## function nth lis n
;: リストlisのn番目の要素を返す。
;:
;: ただし、nは零から数える。
;:
;: nがlisの長さよりも大きい場合はnilを返す。
(function nth (lis n)
  (precondition (list? lis))
  (car (nthcdr lis n)))
; (assert (= (nth '(1 2 3) 0) 1))
; (assert (= (nth '(1 2 3) 10) nil))

;: ## function nthcdr lis n
;: リストlisをなすn番目のコンスを取得する。
;:
;: nがlisの長さよりも大きい場合はnilを返す。
(function nthcdr (lis n)
  (precondition (list? lis))
  (cond ((nil? lis) nil)
        ((= n 0) lis)
        (:default (nthcdr (cdr lis) (-- n)))))
; (assert (= (nthcdr '(1 2 3) 1) '(2 3)))

(function sublist (lis s :opt e)
  (let ((len (length lis))
        (e (or e len))
        (rec (lambda (lis n)
               (if (= n 0) nil
                 (cons (car lis) (rec (cdr lis) (-- n)))))))
    (precondition (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr lis s) (- e s))))

(function copy-list (lis)
  (sublist lis 0 (length lis)))

(function last-cons (lis)
  (precondition (list? lis))
  (if (nil? lis) nil
    (let ((rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis))))
      (rec lis))))

(function last (lis)
  (precondition (list? lis))
  (car (last-cons lis)))

(function length (lis)
  (precondition (list? lis))
  (let ((rec (lambda (lis n)
               (if (nil? lis) n (rec (cdr lis) (++ n))))))
    (rec lis 0)))

(function append (lis :rest args)
  (precondition (and (list? lis) (all-satisfy? args list?)))
  (reduce args (lambda (x y)
                 (cdr (last-cons x)
                      (if (list? y) (copy-list y) (->list y)))
                 x)
          :identity (copy-list lis)))

(function add (lis o)
  (precondition (list? lis))
  (cdr (last-cons lis) (cons o nil))
  lis)

(macro push (sym x)
  (precondition (symbol? sym))
  (list begin
        (list precondition (list list? sym))
        (list <- sym (list cons x sym))
        :SideEffects))

(macro pop (sym)
  (precondition (symbol? sym))
  (list begin0
        (list car sym)
        (list <- sym (list cdr sym))))

(macro queue (sym x)
  (precondition (symbol? sym))
  (list push sym x))

(macro dequeue (sym)
  (precondition (symbol? sym))
  (let ((len (gensym)) (top-lc (gensym)))
    (list let (list (list len (list length sym))
                    (list top-lc (list nthcdr sym (list - len 2))))
          (list if (list < len 2)
                (list begin0
                      (list car sym)
                      (list <- sym nil))
                (list begin0
                      (list cadr top-lc)
                      (list cdr top-lc nil))))))

(function flatten (lis)
  (precondition (list? lis))
  (let ((acc nil)
        (rec (lambda (x)
               (cond ((nil? x) (reverse acc))
                     ((atom? x) (push acc x))
                     (true (if (nil? (car x)) (push acc nil)
                             (rec (car x)))
                           (rec (cdr x)))))))
    (rec lis)))

(function map (args f)
  (if args (cons (f (car args)) (map (cdr args) f))))

(function reverse (lis)
  (let ((rec (lambda (lis acc)
               (if (nil? lis) acc (rec (cdr lis) (cons (car lis) acc))))))
    (rec lis nil)))

(function reduce (args f :key (identity nil identity?))
  (let ((rec (lambda (args)
               (if (nil? (cdr args)) (car args)
                 (rec (cons (f (car args) (cadr args)) (cddr args)))))))
    (rec (if identity? (cons identity args) args))))

(function find (lis e :key (test =) (key identity))
  (if (nil? lis) nil
    (if (test (key (car lis)) e) (car lis)
      (find (cdr lis) e :test test :key key))))

(function find-if (lis f :key (key identity))
  (if (nil? lis) nil
    (if (f (key (car lis))) (car lis)
      (find-if (cdr lis) f :key key))))

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

(macro unquote (:rest forms)
  (error :comma-not-inside-backquote))

(macro splice (:rest forms)
  (unquote))

; quote n-times
(macro nquote (expr n)
  (let ((rec (lambda (expr n)
               (if (<= n 0) expr
                 (list quote (rec expr (-- n)))))))
    (rec expr n)))

(function bqexpander (expr level)
  (if (atom? expr) (nquote expr level)
    (let ((first (car expr)))
      (cond ((same? first unquote) (bqexpander (cdr expr) (-- level)))
            ((same? first backquote) (bqexpander (cdr expr) (++ level)))
            (true (cons (bqexpander first level)
                        (bqexpander (cdr expr level))))))))

(macro backquote (expr)
  (bqexpander expr 0))

; pos
; {{{
; (<- Object '((:super nil)
;              (:type :Object)))
; (function . (object property :opt (val nil val?))
;   (let ((pair (find object property :key car)))
;     (if (nil? pair) (assert (list :NotFountProperty property)))
;     (if val? (cdr pair val) (cdr pair))))
; (print (. Object :type)) ; :Object
; (print (. Object :type)) ; :Object
; }}}

; test
; {{{

;;; identity
(assert (same? (identity :a) :a))

;;; not
(assert (same? !'x nil))
(assert (same? !nil true))

;;; /=
(assert (/= 1 2))
(assert !(/= 1 1))

;;; nil?
(assert (nil? nil))
(assert !(nil? true))

;;; cons?
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

;;; list?
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

;;; all-satisfy?
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

;;; any-satisfy?
(assert (any-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert (any-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

;;; each-pair-satisfy?
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

;;; sublist
; (assert (= (sublist '(1 2 3) 1) '(2 3)))
; (assert (= (sublist '(1 2 3) 1 2) '(2)))

;;; copy-list
(assert (= (copy-list '(1 2 3)) '(1 2 3)))

;;; last-cons
(assert (= (last-cons '(1 2 3)) '(3)))

;;; last
(assert (= (last '(1 2 3)) 3))

;;; length
(assert (= (length '(1 2 3)) 3))

;;; append
(assert (= (append '(1) '(2) '(3)) '(1 2 3)))

;;; push/pop
(let ((lis '(1)))
  (push lis 2)
  (push lis 3)
  (assert (= lis '(3 2 1)))
  (assert (= (pop lis) 3))
  (assert (= lis '(2 1))))

;;; queue/dequeue
(let ((lis '(1)))
  (queue lis 2)
  (queue lis 3)
  (assert (= lis '(3 2 1)))
  (assert (= (dequeue lis) 1))
  (assert (= lis '(3 2)))
  (assert (= (dequeue lis) 2))
  (assert (= (dequeue lis) 3))
  (assert (nil? lis)))

;;; flatten
(assert (= (flatten '(1 (2) (3 4))) '(1 2 3 4)))
(assert (= (flatten '(1 (nil) 2)) '(1 nil 2)))

; }}}

(assert !(or))
(assert (or nil nil true))
(assert !(or nil nil nil))
(assert (and))
(assert (and true true true))
(assert !(and true nil true))

(<- a 0)
(while !(= a 8)
       (if (= a 4) (return))
       (print (<- a (++ a))))

(let ())

(print :finish)
