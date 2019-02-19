; paren core library.

; fundamental macro
(<- list (lambda (:rest args) args))
(assert (= (list 1 2 3) '(1 2 3)))

(<- gensym (let (c 0)
             (lambda ()
               (->symbol (+ ":G" (<- c (+ c 1)))))))
"システム内で重複することのないシンボルを生成して返す。"
(assert !(same? (gensym) (gensym)))

(macro function (name args :rest body)
  "仮引数がargs、本体がbodyであるような関数をシンボルnameに束縛する。
  argsの書式はspecial operatorのlambdaに準ずる。"
  (list <- name (cons lambda (cons args body))))

(macro begin0 (:rest body)
  "bodyを逐次評価し、最初に評価した結果を返す。"
  (let (sym (gensym))
    (cons let (cons (list sym (car body))
                    (+ (copy-list (cdr body)) sym)))))
(assert (= (begin0 1 2 3) 1))

(macro when (test :rest body)
  "testを評価しnil以外の場合にbodyを逐次評価し、最後に評価した結果を返す。
  testがnil偽の場合はnilを返す。"
  (list if test (cons begin body)))
(assert (= (when true 1 2 3) 3))
(assert (= (when nil 1 2 3) nil))

(macro unless (test :rest body)
  "testを評価しnilの場合にbodyを逐次評価し、最後に評価した結果を返す。
  testがnil偽の場合はnilを返す。"
  (cons when (cons (list not test)
                   body)))
(assert (= (unless true 1 2 3) nil))
(assert (= (unless nil 1 2 3) 3))

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

(macro continue ()
  ":continueというラベルにジャンプする。
  反復系のマクロはコンテキスト内でcontinueを使用したときに
  後続の処理をスキップし反復処理を再開するように実装することが望ましい。"
  '(goto :continue))

(macro break ()
  ":breakというラベルにジャンプする。
  反復系のマクロはコンテキスト内でbreakを使用したときに
  反復処理を終了するように実装することが望ましい。"
  '(goto :break))

(macro for (binding test update :rest body)
  "bindingをletで束縛し、testが真の場合にbody、updateを反復評価する。
  continueマクロとbreakマクロをサポートしている。
  nilを返す。"
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
(assert (= (let (sum 0) (for (i 0) (< i 3) (<-+ i 1) (<-+ sum i)) sum) 3))
(assert (= (let (sum 0) (for (i 0) (< i 3) (<-+ i 1) (if (= i 1) (break)) (<-+ sum i)) sum) 0))
(assert (= (let (sum 0) (for (i 0) (< i 3) (<-+ i 1) (if (= i 1) (continue)) (<-+ sum i)) sum) 2))

(macro while (test :rest body)
  "引数testがnilでない間bodyを逐次評価し、nilを返す。
  continueマクロとbreakマクロをサポートしている。"
  (cons 'for (cons nil (cons test (cons nil body)))))
(assert (= (let (a 0) (while (< a 5) (<-+ a 1)) a) 5))
(assert (= (let (a 0) (while true (<- a 1) (break) (<- a 2)) a) 1))
(assert (= (let (a 0) (while (< (<-+ a 1) 5) (if (< a 5) (continue)) (<- a 9)) a) 5))

(macro dolist ((i l) :rest body)
  "リストlをインデックスiを用いてイテレーションする。
  nilを返す。"
  (precondition (symbol? i))
  (let (gl (gensym))
    (cons for (cons (list gl l i (list car gl))
                    (cons gl
                          (cons (list <- gl (list cdr gl) i (list car gl))
                                body))))))
(assert (= (let (l '(1 2 3) acc nil) (dolist (i l) (push acc i)) acc) '(3 2 1)))
(assert (= (let (l '(1 2 3) x nil) (dolist (i l) (if (= (<- x i) 2) (break))) x) 2))
(assert (= (let (l '(1 2 3) sum 0) (dolist (i l) (if (= i 2) (continue)) (<-+ sum i)) sum) 4))

(macro dotimes ((i n) :rest body)
  "シンボルiを0から順にn - 1まで束縛しながらbodyを反復評価する。
  nilを返す。"
  (precondition (symbol? i))
  (let (gn (gensym))
    (cons for (cons (list i 0 gn n)
              (cons (list < i gn)
              (cons (list <-+ i 1)
                    body))))))
(assert (= (let (acc nil) (dotimes (i 3) (push acc i)) acc) '(2 1 0)))
(assert (= (let (x nil) (dotimes (i 3) (if (= (<- x i) 1) (break))) x) 1))
(assert (= (let (sum 0) (dotimes (i 3) (if (= i 1) (continue)) (<-+ sum i)) sum) 2))

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
  (list 'not x))
(assert (nil? (nil? true)))
(assert (nil? nil))
(assert !(nil? true))

(function cons? (x)
  "xがコンスの場合はtrueを、そうでなければnilを返す。"
  !(atom? x))
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

(function list? (x)
  "xがコンスまたはnilの場合はtrueを、そうでなければnilを返す。"
  (or (nil? x) (cons? x)))
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

(function byte? (x)
  "xが0から255の整数の場合はtrueを、そうでなければnilを返す。"
  (<= 0 x 255))
(assert (byte? 0))
(assert (byte? 255))
(assert !(byte? 256))

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

(function append (l :rest args)
  "リストlの要素としてargsの各要素を追加する。
  argsの任意の要素はリストでなければならない。"
  (precondition (all-satisfy? args list?))
  (reduce args (lambda (acc rest)
                 (reduce (copy-list rest) + :identity (copy-list acc)))
          :identity l))
(assert (= (append '(1 2) '((3 4) (5)) '(6)) '(1 2 (3 4) (5) 6)))

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
  (precondition (and (list? l) (operator? f)))
  (if (nil? l) true
      (and (f (car l)) (all-satisfy? (cdr l) f))))
(assert (all-satisfy? nil cons?))
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (number? x))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (number? x))))

(function any-satisfy? (l f)
  "リストlのいずれかの要素が関数fの引数として評価したときにnil以外の値を返す場合はtrueを返す。
  そうでなければnilを返す。
  なお、lが空の場合はnilを返す。"
  (precondition (and (list? l) (operator? f)))
  (if l (or (f (car l)) (any-satisfy? (cdr l) f))))
(assert !(any-satisfy? nil number?))
(assert (any-satisfy? '(1 2 3 4 5) number?))
(assert !(any-satisfy? '(:a :b :c) number?))

(function each-pair-satisfy? (l f)
  "リストの隣接するすべての二要素に対して二変数関数fの評価がnil以外の場合は"
  (precondition (and (list? l) (operator? f)))
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

(function push-pair (al k v)
  "連想リストに対を追加する。"
  (precondition (list? al))
  (push al v)
  (push al k))

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

(macro <-+ (s v)
  "sの値にvを加えた値をsに束縛する式に展開する。"
  (precondition (symbol? s))
  (list <- s (list + s v)))

(macro <-- (s v)
  "sの値からvを引いた値をsに束縛する式に展開する。"
  (precondition (symbol? s))
  (list <- s (list - s v)))

(macro <-* (s v)
  "sの値にvをかけた値をsに束縛する式に展開する。"
  (precondition (symbol? s))
  (list <- s (list * s v)))

(macro <-/ (s v)
  "sの値をvをで割った値をsに束縛する式に展開する。"
  (precondition (symbol? s))
  (list <- s (list * s v)))

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

(<- $class nil)

(function class-exists? (cls-sym)
  (find $class cls-sym))

(function find-class (cls-sym)
  (precondition (and (symbol? cls-sym) (class-exists? cls-sym)))
  (. $class cls-sym))

(function find-method (cls-sym method-sym)
  (precondition (and (symbol? cls-sym) (symbol? method-sym)))
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
    (or (rec (find-class cls-sym)) (basic-throw :MissingMethodException ))))

(macro make-accessor (cls-sym var)
  (let (val (gensym) val? (gensym))
    (list method cls-sym (->symbol (+ "." var))
          (list :opt (list val nil val?))
          (list if val? (list '. 'self (->keyword var) val)
                (list '. 'self (->keyword var))))))

(macro make-method-dispatcher (method-sym)
  (let (args (gensym))
    (list macro method-sym (list :rest args)
          (list cons (list 'list 'find-method
                           (list 'list '. (list car args) :class)
                           (list quote (list quote method-sym)))
                args))))

(macro class (cls-sym (:opt (super 'Object) :rest features) :rest fields)
  (let (Object? (= cls-sym 'Object)
        has-desc? (string? (car fields))
        desc (and has-desc? (car fields))
        fields (if has-desc? (cdr fields) fields))
    (precondition (and (all-satisfy? fields symbol?) !(class-exists? cls-sym)))
    (append
      (list begin0
            (list quote cls-sym)
            (list <- cls-sym (list quote (list :class 'Class
                                               :desc desc
                                               :symbol cls-sym
                                               :super (if (not Object?) super)
                                               :features features
                                               :fields fields
                                               :methods nil)))
            (list 'push '$class cls-sym)
            (list 'push '$class (list quote cls-sym)))
      (map fields (lambda (var) (list 'make-accessor cls-sym var))))))

(macro method (cls-sym method-sym args :rest body)
  (precondition (class-exists? cls-sym))
  (list begin
        (list make-method-dispatcher method-sym)
        (list . cls-sym :methods
              (list cons (list quote method-sym)
                    (list cons (cons lambda (cons (cons 'self args) body))
                          (list '. cls-sym :methods))))))

(function object? (x)
  "xがオブジェクトの場合trueを、そうでなければnilを返す。
  paren object systemでは先頭要素がキーワード:classで始まるような連想リストをオブジェクトと見做す。"
  (and (list? x) (= (car x) :class)))

(function is-a? (o cls)
  "oがclsクラスのインスタンスの場合trueを、そうでなければnilを返す。"
  (precondition (and (object? o) (object? cls) (same? (cadr cls) 'Class)))
  (let (cls-sym (. cls :symbol)
        rec (lambda (o-cls-sym)
              (and o-cls-sym
                   (or (same? o-cls-sym cls-sym)
                       (rec (. (find-class o-cls-sym) :super))))))
    (rec (. o :class))))

(class Object ()
  "唯一スーパークラスを持たない、クラス階層の最上位クラス。
  スーパークラスを指定しない場合は暗黙的にObjectクラスを継承する。"
  class)

(method Object .init ()
  "オブジェクトの初期化メソッド。
  クラスごとに必要に応じて固有の初期化処理を上書きする。"
  self)

(method Object .equal? (o)
  "レシーバとoが同一オブジェクトの場合にtrueを、そうでなければnilを返す。
  サブクラスで同等性を定義する場合はこのメソッドをオーバーロードする。"
  (same? self o))

(class Class () symbol desc super features fields methods)

(method Class .new ()
  (let (o nil cls self fields nil)
    (while cls
      (<- fields (reverse (copy-list (map (. cls :fields) ->keyword))))
      (while fields
        (push o (if (same? (car fields) :class) (. self :symbol)))
        (push o (car fields))
        (<- fields (cdr fields)))
      (<- cls (and (. cls :super) (find-class (. cls :super)))))
    (.init o)))

(class Stream (Object)
  "ストリームクラス。
  入出力の基本的なメソッドを持つ。")

(method Stream .readByte (:rest args)
  (basic-throw :NotImplementedException))

(method Stream .writeByte (:rest args)
  (basic-throw :NotImplementedException))

(method Stream .readChar (:opt (encoding $encoding))
  (if (same? encoding :UTF-8)
          (let (throw (lambda () (basic-throw :IllegalUTF-8Exception))
                trail? (lambda (b) (= (bit-and b 0xc0) 0x80))
                mem (.new MemoryStream)
                b1 (.readByte self) b2 nil b3 nil b4 nil)
            (if (< b1 0) (return :EOF)
                (< b1 0x80) (.writeByte mem b1)
                (< b1 0xc2) (throw)
                !(trail? (<- b2 (.readByte self))) (throw)
                (< b1 0xe0)    ; 2-byte character
                    (begin (if (= (bit-and b1 0x3e) 0) (throw))
                           (.writeByte mem b1)
                           (.writeByte mem b2))
                (< b1 0xf0)    ; 3-byte character
                    (begin (<- b3 (.readByte self))
                           (if (or (and (= b1 0xe0) (= (bit-and b2 0x20) 0))
                                   !(trail? b3))
                               (throw))
                           (.writeByte mem b1)
                           (.writeByte mem b2)
                           (.writeByte mem b3))
                (< b1 0xf8)    ; 4-byte character
                    (begin (<- b3 (.readByte self) b4 (.readByte self))
                           (if (or !(trail? b3) !(trail? b4)
                                   (and (= b1 0xf0) (= (bit-and b2 0x30) 0)))
                               (throw))
                           (.writeByte mem b1)
                           (.writeByte mem b2)
                           (.writeByte mem b3)
                           (.writeByte mem b4))
                (throw))
            (.toString mem))
          (same? encoding :CP932)
              (assert nil)    ; not implemented yet.
          (basic-throw :UnsupportEncodingException)))

(class FileStream (Stream)
  "ファイルストリームクラス"
  fp)

(method FileStream .initWith (:key fp)
  (precondition fp)
  (.fp self fp)
  self)

(method FileStream .readByte ()
  (fgetc (.fp self)))

(method FileStream .writeByte (byte)
  (precondition (byte? byte))
  (fputc byte (.fp self)))

(class MemoryStream (Stream)
  "メモリ上に内容を保持するストリームクラス。"
  buf buf-size rd-pos wr-pos)

(method MemoryStream .init ()
  (let (buf-size 256)
    (.buf-size self buf-size)
    (.buf self (byte-array buf-size))
    (.rd-pos self 0)
    (.wr-pos self 0))
  self)

(method MemoryStream .extend (size)
  (precondition (integer? size))
  (let (req (+ (.wr-pos self) size) new-buf nil)
    (while (< (.buf-size self) req)
      (.buf-size self (* (.buf-size self) 2)))
    (<- new-buf (byte-array (.buf-size self)))
    (array-copy (.buf self) 0 new-buf 0  (.wr-pos self))
    (.buf self new-buf)
    self))

(method MemoryStream .writeByte (byte)
  (precondition (byte? byte))
  (let (pos (.wr-pos self))
    (if (< pos (.buf-size self))
        (begin ([] (.buf self) pos byte)
               (.wr-pos self (++ pos)))
        (.writeByte (.extend self 1) byte))))

(method MemoryStream .writeString (s)
  (precondition (string? s))
  (let (ba (->byte-array s))
    (dotimes (i (length ba))
             (.writeByte self ([] ba i)))))

(method MemoryStream .readByte ()
  (let (pos (.rd-pos self))
    (if (>= pos (.wr-pos self)) -1
        (begin0 ([] (.buf self) pos)
                (.rd-pos self (++ pos))))))

(method MemoryStream .toString ()
  (let (pos (.wr-pos self) str (byte-array pos))
    (->string (array-copy (.buf self) 0 str 0 pos))))

(method MemoryStream .reset ()
  (.rd-pos self 0)
  (.wr-pos self 0)
  self)

(class AheadReader ()
  "先読みリーダー"
  stream nextChar buf)

(method AheadReader .initWith (:key string stream)
  (precondition (or (and string (string? string))
                    (and (object? stream) (is-a? stream Stream))))
  (when string
    (<- stream (.new MemoryStream))
    (.writeString stream string))
  (.stream self stream)
  (.nextChar self (.readChar (.stream self)))
  (.buf self (.new MemoryStream))
  self)

(method AheadReader .checkEOF ()
  (if (same? (.nextChar self) :EOF) (basic-throw :EOFReachedException)))

(method AheadReader .skipChar ()
  (.checkEOF self)
  (begin0 (.nextChar self)
          (.nextChar self (.readChar (.stream self)))))

(method AheadReader .addString (s)
  (precondition (string? s))
  (.writeString (.buf self) s)
  s)

; I/O
(<- $stdin (.initWith (.new FileStream) :fp (fp 0))
    $stdout (.initWith (.new FileStream) :fp (fp 1))
    $in $stdin
    $out $stdout
    ; $encoding (if (same? $os :Windows) :CP932 :UTF-8)
    $encoding :UTF-8
    $support-encodings '(:UTF-8 :CP932))

(function read-byte (:opt (stream $stdin))
  (precondition (is-a? stream Stream))
  (.readByte stream))

(function read-char (:opt (stream $stdin))
  (precondition (is-a? stream Stream))
  (.readChar stream))

(function write-byte (byte :opt (stream $stdout))
  (precondition (and (byte? byte) (is-a? stream Stream)))
  (.writeByte stream byte))

(<- mem (.new MemoryStream))
; (.writeByte mem 0x20)
; (.readChar mem :UTF-8)

(<- ar (.initWith (.new AheadReader) :string "abcd"))
(print (.nextChar ar))
(print (.skipChar ar))
(print (.addString ar "abc"))

; ./paren
; )
;
; ./paren xxx.p
; load xxx.p
;
;
; (if $args (loop (print (eval (read))))
;     (load xxx.p))
