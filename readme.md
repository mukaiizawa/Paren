# What is Paren?
Paren is a programmable programming languages.
Paren(パレン)はプログラミング言語です．
Common Lisp、arcとその他のオブジェクト指向言語に影響を受けて作られました．

# Tutorial
## Evaluation
ParenはListまたはAtomで構成され、次のようなルールで評価されます。

### Atomの評価例
Atomには次のよく使う型があります。
- Symbol
- Keyword
- String
- Character
- Number
- Function
 Symbol以外の評価
    2
    ;; => 2
    "abc"
    ;; => abc

### Listの評価例
    (+ 2 3)
    ;; => 5
    (- (+ 2 3) 4)
    ;; => 1

Listは`(<function> <arg1> <arg2> ... <argN>)`の形式をとります。
