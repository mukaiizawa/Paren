#|
paren core routine.
|#

#|
;; 宣言
(def! a b c)

;; 代入
(<- a 1 b 2 c 3)

;; 変数束縛
(let (a 1 b 2 c 3)
  forms)

;; 条件分岐
(ifElse (< n 0) -1
        (= n 0) 0
        1)

;; 繰り返し
(for (<- i 0) (< i 10) (++ i)
     forms)

;; 関数定義
(def! square (Number n)
  (* n n))
;;;; コンテナーの型宣言例
(def! sum (List<Number> n)
  (ifElse n (+ (car n) (cdr n))
          0))

;; 無名関数
((fn (Object o)
     (asString o))
 1)

;; スペシャルフォーム
;; ifElse
;; progn
;; def!
;; <-
|#
