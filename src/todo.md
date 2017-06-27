# todo
parenにおけるtodoを羅列する。

# 未実装項目
- GC
- importメソッドの実装
- チルダリードマクロ
- 一般化代入
- マクロ

# 検討項目
- プリミティブ`new`
- プリミティブ`struct` => プリミティブにしない?
- 末尾再帰最適化
- goto機構

# バグ対応

# 覚書
## オートインデントについて
オートインデントは(すくなくともvimのlispindentを使なら)あきらめる必要がある。
次のような場合にCライクなインデントが必要になる。
    (if test trueClause
        longLongPredicate
            thenClause
        elseClause)
これは、一行に欠ける場合は次のように書くべきである。
    (if test trueClause
        longLongPredicate thenClause
        elseClause)
これは、構文解析上不要な括弧は省く思想で言語を作成していることによる。
`let`も同じ思想で設計されているため、当該懸案が起こりうる。
    (let (va1 val1
          var2 val2)
      body)
一行で書ける場合はそうするのが望ましい。
    (let (va1 val1 var2 val2)
      body)
