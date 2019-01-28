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
        copy-len (-- (length sba))
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
