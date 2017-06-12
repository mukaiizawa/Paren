# todo
parenにおけるtodoを羅列する。

# GC
- GCの実装

# Error
- フォーマット対応及び、可変長引数化

# Special Operator
## fn
- 定義時に未定義のシンボルチェックを行う。
- 型staticなメソッドの仕様の策定

# Function
- importメソッドの実装

# バグ対応
- generic functionのディスパッチの不具合
    (def double)
    (<- double (fn (:Number x) (* x 2)))
    (=? (double 2) (* (double 1) 2)) => nil ;!?
