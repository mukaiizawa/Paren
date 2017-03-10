イミュータブルなポイントクラス

# クラス
defclassによりクラスを定義することができる。
(defclass className typeList &rest instansVars)
ポイントクラスでの例を示す。
(defclass point () x y)

# 関数定義
Parenの関数は
;; クラスのメソッドはdefmethodで定義する．
;; 第一引数は習慣により，レシーバにする．
;; クラスのフィールドにはドット記法によりアクセスできる．
(defmethod size ((p point))
  (* (x p) (y p)))

(defmethod add ((p1 point) (p2 point))
  (new 'point
       :x (+ (. x p1) (. x p2))
       :y (+ (. y p1) (. y p2))))

;; 代入は<-マクロを用いる
;; 方針として、引数の組が固定で決まっているフォームは括弧の数を極力書かないシンタックスとする。
(<- a 1
    b 2
    c 3)

;; letはclosureのようなシンタックスとする。
(let (a 1 b 2 c 3)
  body)

;; 型の階層構造
'(t (atom ((number (int)
            (double))
    (nil))

;; Type atom
;; Type cahr
;; Type base-string
;; Type bignum
;; Type bit
;; Type boolean
;; Type compiled-function
;; Type extended-char
;; Type fixnum
;; Type keyword
;; Type nil
;; Type short-float, single-float, double-float...
;; Type signed-byte
;; Type simple-array
;; Type simple-base-string
;; Type simple-bit-vector
;; Type simple-string
;; Type simple-vector
;; Type standard-char
;; Type unsigned-byte
