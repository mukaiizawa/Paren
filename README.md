# What is Paren?
Paren is a programmable programming languages.
Paren(パレン)はプログラミング言語です．
Common Lispとその他のオブジェクト指向言語に影響を受けて作られました．

# Tutorial
## Evaluation
ParenはListまたはAtomで構成され、次のようなルールで評価されます。

#### Atomの評価例
    2
    ;; => 2
    "abc"
    ;; => abc

#### Listの評価例
    (+ 2 3)
    ;; => 5
    (- (+ 2 3) 4)
    ;; => 1

Listは`(<function> <arg1> <arg2> ... <argN>)`の形式をとります。
