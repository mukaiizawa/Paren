# NAME
paren-tutorial - a tutorial introduction to Paren.

# DESCRIPTION
## 概要
このチュートリアルはParenの基本的な概念や文法を早巡りすることを目的とする。読者に計算機科学の初等的な知識があることを想定し、いくつかの専門用語は定義することなしに使用する。

ParenはS式によって記述されるプログラミング言語LISPの方言である。

## Parenの実行
引数なしでParenを実行するとREPLが起動し、対話的に式を評価できる。

    $ paren
    ) 

`)`がプロンプトである。プロンプトに続けて式を入力すると評価され結果が印字される。

    ) 1
    1
    ) (+ 1 2 3 4)
    10
    ) (* (+ 1 2) 3 4)
    36

Parenのプログラムは演算子を前置し、被演算子と共に括弧で括る。このように演算子を前置する記法を前置記法という。

あるプログラミング言語を習得する唯一の方法は、その言語でプログラムを書くことである。そしてそれは、慣習により次のプログラムを書くことから始まる。

    文字列`hello world`を印字せよ。

文字列を印字するには`write`関数を使用する。

    ) (write "hello world")
    "hello world"
    "hello world"

上記のプログラムを評価すると二行出力される。一行目が`write`関数が印字した文字列であり、二行が`write`の返り値をREPLが印字したものである。

コード片を評価するにはREPLは便利だが、一般に大きなプログラムを作る場合にはファイルに記述する。

引数を与えてParenを実行すると、第一引数をファイル名と見做して読み込み、評価する。

その後、`main`関数が定義されていた場合は`main`関数を実行する。このとき、第二引数以降が`main`関数の引数として渡される。

    $ cat test.p
    (function main (args)
      (write args))
    
    $ paren test.p hello world
    ("hello" "world")

## コメント
`;`から行末まではコメントと見做される。

    ; 一行コメント

Parenには複数行に跨がれるコメントは言語仕様として存在しない。ただし、そのような機能が必要になった場合は後述するリードマクロで容易に定義できる。

## 数値
すべての実数は数値型として扱われる。

数値の前に`radix x`と前置することにより後続する数値の基数を`radix`に指定することができる。

    ) 2x1010
    10
    ) 8x12
    10

例外的に0を前置した場合、16進数と見做す。

    ) 0x30
    48

代表的な算術関数の使用例を以下に示す。

    ) (= 1 1)
    true
    ) (+ 3 4 5)
    12
    ) (- 10 4 5)
    1
    ) (* 3 4 5)
    60
    ) (/ 3 4 5)
    0.15
    ) (/ 3)
    0.3333333333333332
    ) (// 5 4)
    1
    ) (% 6 4)
    2

## 文字列
文字列はリテラルや文字列操作関数から生成される、イミュータブルな組み込み型である。

文字列リテラルはダブルクォートで囲まれた文字の列である。

    ) "hello world"
    "hello world"

代表的な文字列操作関数の使用例を以下に示す。

    ) (= "foo" "foo")
    true
    ) (memcat "hello" " " "world")
    "hello world"
    ) (slice "hello world" 6)
    "world"
    ) (slice "hello world" 0 5)
    "hello"
    ) (strstr "foo" "foo")
    0
    ) (strstr "foo" "oo" 1)
    1
    ) (str "one:" 1)
    "one:1"

正規表現は`regex`モジュールを参照のこと。

## シンボル
シンボルは主にシンボルリテラルから生成される、イミュータブルな組み込み型である。

同名のシンボルはシステム上でただ一つしか存在しないことが保証されているため、同一性はアドレス比較で高速に行える。

シンボルは任意の値への参照を一つ保持でき、評価されると保持している値を返す。

シンボルリテラルは数字以外の英文字と一部の記号から始まり、その後、英数字と一部の記号が続く。

    var
    x1

複数の単語で構成される場合は、慣習としてチェインケースを利用する。

    user-name
    neighbor-node

シンボルに値を対応付ける行為を束縛といい、スペシャルオペレーター`<-`を使用する。

    ) (<- pi 3.14)
    3.14
    ) pi
    3.14

シンボルと値の対応を記録しておくために、内部的には何らかの記憶域を専有する必要がある。

この記憶域のことを環境といい、シンボルに対応付けられた値がどのように解決されるかを理解するには環境の理解が不可欠である。

## 環境
環境は前述したシンボルと値の対応のほかに、外側の環境への参照を零または一つ持つ。

    +-------------+
    |E1           |
    +-------------+
    |foo:3.14     |
    |bar:10       |
    +-------------+
         |  +-------------+
         |- |E2           |
         |  +-------------+
         |  |foo:3        |
         |  |buzz:0       |
         |  +-------------+
         |       |  +-------------+
         |       |- |E3           |
         |          +-------------+
         |          |bar:1        |
         |          +-------------+
         |      ...
         |  +-------------+
         |- |E4           |
            +-------------+
            |foo:4        |
            |buzz:1       |
            +-------------+
    
         ...
    
    凡例
    - 環境E1
        - 外側の環境への参照を持たない
        - シンボルと値の対(foo, 3.14)、(bar, 10)が存在する
    - 環境E2
        - 外側の環境への参照E1を持つ
        - シンボルと値の対(foo, 3)、(buzz, 0)が存在する

環境E1のように、外側の環境への参照を持たない環境のことを大域環境といいシステム内にただ一つだけ存在する。

すべての環境は外側の環境をたどると大域環境に到達する。

あらゆる式が評価されるとき、必ずどこかの環境の下で評価される。Parenを起動した直後の環境は大域環境である。

前述したスペシャルオペレータ`<-`による束縛は次の手順で実行される。

    現在の環境にシンボルが既に束縛されている場合、その値を更新する。
    そうでない場合は外側の環境を再帰的に辿る。
    大域環境にもシンボルが束縛されていない場合は、大域環境に新たにシンボルを束縛する。

対照的に、シンボルの値の参照は次の手順で行われる。

    現在の環境にシンボルが既に束縛されている場合、その値を返す。
    そうでない場合、再帰的に外側の環境を辿る。
    大域環境にも束縛されていない場合はエラーとなる。

スペシャルオペレーター`let`は現在の環境を外側の環境に持つような新たな環境を作り、その環境下で評価を行う。

第一引数には新たな環境に束縛するシンボルと値の対のリストを、第二引数以降には新たな環境下で評価する式を指定する。

    ; 大域環境に対{ (a, 1), (b, 2) (c, 3) }を束縛
    ) (<- a 1 b 2 c 3)
    3
    ) (list a b c)
    (1 2 3)
    
    ; シンボルと値の対{ (a, 2), (b, 4) (c, 6), (d, 8) }を持つ環境を作り、
    ; その環境下で式(list a b c d)を評価。
    ) (let (a 2 b 4 c 6 d 8)
        (list a b c d))
    (2 4 6 8)
    ) (list a b c)
    (1 2 3)
    
    ; 大域環境にはdは束縛されていないため、エラー。
    ) d
    Error -- unbound symbol
            at: d
            at: (repl)
            at: (boot nil)

このように、あるシンボルの保持する値は、どの環境下でシンボルが評価されたのかに依存する。

大域環境に束縛されたシンボルのことをグローバルシンボルという。

慣習として、グローバルシンボルは目立つように`$`を先頭につける。

    $global-var

## キーワード
キーワードは主にキーワードリテラルから生成される、イミュータブルな組み込み型である。

評価されると常に自身を返す点を除いてシンボルと概ね同じである。

その特性から、列挙型の要素や、辞書のキーなどに利用する。

キーワードリテラルは`:`から始まる英数字と一部の記号の列である。

    :foo
    :bar

## コンス
コンスとは、二つのデータへの参照`car`及び`cdr`を持つデータ型である。

`car`は任意のデータへの参照を保持し、`cdr`はコンスまたは`nil`への参照を保持する。

リストとは、あるコンスの`cdr`を辿ったコンス全体のことをいう。

このとき、辿ったコンスの`car`全体をリストの要素という。

要素がないリストを空のリストといい、シンボル`nil`で表す。

あるコンスの`cdr`を辿っていった終端が`nil`を指している場合、そのコンス全体は純リストであるという。定義により、Parenの任意のリストは純リストである。純リスト以外のリストが作れないという制約はほかのLISP方言と異なる特徴の一つである。

コンスは組み込み関数`cons`で作ることができる。`cons`の第一引数、第二引数が、それぞれ作成されるコンスの`car`と`cdr`に対応する。

    ) (cons 1 nil)
    (1)
    ) (cons 1 (cons 2 nil))
    (1 2)
    ) (cons (cons 1 nil) (cons 2 nil))
    ((1) 2)

コンスは次の規則で印字される。

- 左括弧を印字する
- `car`を印字する
- `cdr`が`nil`でない間`car`を印字する
- 右括弧を印字する

    ) (cons 1 (cons 2 (cons 3 nil)))
    (1 2 3)

    +-----+-----+
    | car | cdr |
    +-----+-----+
       |     |    +-----+-----+
       |     +--->| car | cdr |
       |          +-----+-----+
       |             |     |    +-----+-----+
       |             |     +--->| car | cdr |
       |             |          +-----+-----+
       |             |             |     |
       |             |             |     +---> nil
       1             2             3

あるコンスの`car`がコンスを指している場合でもそのルールは再帰的に適用される。

    ) (cons (cons 1 nil) (cons 2 (cons 3 nil)))
    ((1) 2 3)

    +-----+-----+
    | car | cdr |
    +-----+-----+
       |     |    +-----+-----+
       |     +--->| car | cdr |
       |          +-----+-----+
       |             |     |    +-----+-----+
       |             |     +--->| car | cdr |
       |             2          +-----+-----+
       |                           |     |
       |    +-----+-----+          |     +---> nil
       +--->| car | cdr |          3
            +-----+-----+
               |     |
               |     +---> nil
               1

任意のリストは`cons`関数で作ることができるが、複数の要素を持つようなリストを作ろうとすると、すぐに困ったことになる。

    ) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))
    (1 2 3 4 5)

そのため、引数を要素に持つようなリストを作成するための`list`関数が存在する。

次のように、要素を引数にしてリストを作成できる。

    ) (list 1 2 3 4 5)
    (1 2 3 4 5)
    ) (list (list 1 (list 2 3) 4 5))
    ((1 (2 3)) 4 5)

コンスの`car`及び`cdr`の指す場所を得る関数があり、それぞれ`car`と`cdr`という。

    ) (<- lis (list 1 2))
    (1 2)
    ) (car lis)
    1
    ) (cdr lis)
    (2)
    ) (car (cdr lis))
    2

`car`と`cdr`は引数が空のリストである場合は`nil`を返す。

    ) (car nil)
    nil
    ) (cdr nil)
    nil
    ) (car (cdr (cdr (list 1))))
    nil

リストの要素を参照するときに`car`と`cdr`を組み合わせることは頻繁にあるため、計四回までリストを辿るすべての組み合わせが定義されている。

    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
    caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
    cdaddr cddaar cddadr cdddar cddddr

これらの関数`(cx1x2...xnr y)`は`(cx1r (cx2r ... (cxnr y) ...))`と等価である。

コンスの`car`と`cdr`が指す場所を変更するには、それぞれ`car!`、`cdr!`関数を使用する。

    ) (car! (<- lis (list 0 0 0)) 1)
    1
    ) lis
    (1 0 0)
    ) (cdr! lis nil)
    nil
    ) lis
    (1)

`cdr`を変更する際、純リストでなくなるような値は指定できない。

### リストの評価
リストを式として評価する場合、先頭要素のことを演算子、それ以外の要素を被演算子、または、引数という。

演算子が次のいずれかの場合は、評価可能なS式であるという。

- シンボル
- 関数
- スペシャルオペレーター
- マクロ

評価可能なS式でなかった場合はエラーとなる。

演算子がシンボルの場合、対応する値の評価規則に従う。

演算子が関数の場合、すべての引数を評価し、その結果を関数に渡して評価する。

実引数の評価は左から順に行われることが保証されている。

    ) (+ 1 (* 2 3))
    7

上記の例では式`(+ 1 (* 2 3))`を評価するために引数`1`と`(* 2 3)`を、式`(* 2 3)`を評価するために引数`2`と`3`を再帰的に評価している。

演算子がスペシャルオペレーターの場合、スペシャルオペレーターの規則に従って評価を行う。

演算子がマクロの場合、引数を評価せずにマクロ展開を行う。ただし、マクロの定義のされ方によっては引数が評価されているようにも見える。詳しくはマクロの章で述べる。

### プログラムとデータ
既に述べたようにParenのプログラム、つまりS式はParenのデータである。

一方で、Parenのデータのうち、評価可能なS式はParenのプログラムである。

データとプログラムが相互に変換可能であるという事実は、プログラムを書くプログラムを書きやすくする。実際、マクロはこの事実を上手く利用した機能であり、Parenを強力な言語にしている。

例えば、次のS式は評価結果が評価可能なS式である。

    ) (list + 1 2 3)
    (+ 1 2 3)

ここで垣間見た可能性についてマクロの章で述べる。

## 配列
配列は数値と任意の値を対応付けるデータデータ構造である。この数値のことを添え字という。

大きさを指定して配列を生成する。生成後は大きさの変更はできない。

生成時、すべての値は`nil`に初期化される。

大きさ`N`の配列には、`0`から`N - 1`の添え字で値を参照、更新できる。

連続したメモリを利用するように実装されているため、任意の値の参照及び更新は定数時間で行われる。

配列にはリテラル表記のためのリーダーマクロ`#[`が定義されている。

`read`関数でシンボル`]`が現れるまでS式を読みこみ評価する。

    ) #[ 1 2 (+ 1 2) ]
    #[ 1 2 3 ]

`read`関数によりS式の読み込みを行うため、`]`と要素の間に空白を入れないと意図した結果にならない。

    ) #[ foo bar]
    Error -- unbound symbol
            at: bar]
            ...

`read`関数はリーダーマクロの展開を行う。このことは　多次元配列のリテラル表記が保証されていることを意味する。

    ) #[
        #[ 1 2 3 ]
        #[ 4 5 6 ]
        #[ 7 8 9 ]
      ]
    #[ #[ 1 2 3 ] #[ 4 5 6 ] #[ 7 8 9 ] ]

配列の使用例を以下に示す。

    ) (<- a (array 3))
    #[ nil nil nil ]
    ) (array? a)
    true
    ) ([] a 0 :zero)
    :zero
    ) a
    #[ :zero nil nil ]
    ) ([] a 0)
    :zero
    ) (len a)
    3
    ) (arr->list a)
    (:zero nil nil)

速度が問題になる場合を除き、リストを使用すべきである。

## バイト列
バイト列はバイナリ列を扱うことに特化した配列である。

次にあげる点を除いて、配列と同等である。

- 添え字に対応付けられる値は`0`から`255`までの数値のみ
- リテラル表記はない

バイト列の使用例を以下に示す。

    ) (<- b (bytes 3))
    #[ 0x00 0x00 0x00 ]
    ) ([] b 0 0x22)
    34
    ) b
    #[ 0x22 0x00 0x00 ]
    ) ([] b 0)
    34
    ) (len b)
    3

いくつかの関数は、バイト列と見做せるシンボル、文字列、キーワードにも適用できる。

    ) (len "foo")
    3
    ) (memcat "foo" "bar")
    "foobar"
    ) (slice "012" 0)
    "012"
    ) (slice "012" 1)
    "12"
    ) (slice "012" 1 2)
    "1"

## 辞書
辞書はシンボルまたは、キーワードと任意の値を対応付けるデータデータ構造である。シンボル、またはキーワードのことをキーという。

概念上は添え字がシンボルまたは、キーワードであるような可変サイズの配列と同等である。

ハッシュテーブルとして実装されているため、任意の値の参照及び更新は定数時間で行われる。

辞書にはリテラル表記のためのリーダーマクロ`#{`が定義されている。

`read`関数でシンボル`}`現れるまでキーと値を順に読み込む。このとき、値のみ評価される。

    ) #{ :zero 0 :one 1 :two (++ 1) }
    #{ :two 2 :one 1 :zero 0 }

配列リテラル同様、適切に空白を入れないと末尾の`}`が検出されないことに注意が必要である。

辞書の使用例を以下に示す。

    ) (<- d (dict))
    #{ }
    ) ({} d :zero 0)
    0
    ) ({} d :one 1)
    1
    ) ({} d :two)
    nil
    ) ({} d :two 2)
    2
    ) ({} d :two)
    2
    ) d
    #{ :two 2 :one 1 :zero 0 }
    ) (keys d)
    (:two :one :zero)

一般的な辞書に比べ、次のような制約がある。

- 一度登録したキーは削除できない
- キーに指定できるのはシンボルまたは、キーワードのみ
- 登録順は保持しない

上記の制約が問題となる場合は以下のモジュールを使用すること。

- splay
- hashtable

## 関数
関数は複数の式をひとつの手続きとしたものである。

再利用性のある一連の式を関数化することで、可読性や保守性を高く保つことができる。

関数を定義するにはマクロ`function`を用いる。

    (function name ([required_param] ...
                    [:opt optional_param ...]
                    [{ :rest rest_param | :key keyword_param ... }] )
        body ...)
    name -- 定義する関数名
    required_param -- 必須パラメーター
    optional_param -- オプショナルパラメーター
    keyword_param -- キーワードパラメーター
    rest_param -- レストパラメーター
    body -- 関数本体

    ) (function double (x) (* 2 x))
    double
    ) (double 4)
    8

仮引数は以下に示すパラメーターからなる。

- 必須パラメーター
- オプショナルパラメーター
- レストパラメーター
- キーワードパラメーター

複数のパラメーターを同時に組み合わせることができるが、その場合は上の順番で指定しなければならない。

ただし、レストパラメーターとキーワードパラメーターは同時に指定できない。

### 必須パラメーター
必須パラメーターは関数呼び出し時に必ず与えなければならない引数である。

関数呼び出し時に必須パラメーターが足りない場合はエラーとなる。

    ) (function avg2 (x y)
        (/ (+ x y) 2))
    ) (avg2 2 4)
    3

### オプショナルパラメーター
オプショナルパラメーターは関数呼び出し時に省略可能な引数である。省略された場合nilが束縛される。

    ) (function inc (x :opt y)
        (+ x (|| y 1)))
    x-add
    ) (inc 3)
    4
    ) (inc 3 2)
    5

### レストパラメーター
レストパラメーターは束縛されなかった実引数に対応する可変長引数である。対応する実引数がない場合はnilが束縛される。

    ) (function first-rest (first :rest rest)
        (list first rest))
    ) (first-rest 1 2 3)
    (1 (2 3))
    ) (first-rest 1)
    (1 nil)

### キーワードパラメーター
キーワードパラメーターは省略可能な順序を問わない名前付きの引数である。省略された場合はnilが束縛される。

関数呼び出し時は対応するキーワードを指定する。

    ) (function k1-k2-k3 (:key k1 k2 k3)
        (list k1 k2 k3))
    k1-k2-k3
    ) (k1-k2-k3 :k1 1 :k2 2)
    (1 2 nil)
    ) (k1-k2-k3 :k1 1)
    (1 2 nil)
    ) (k1-k2-k3 :k3 3 :k1 1)
    (1 nil 3)

### 汎関数
関数は他のデータ型である数値や文字列と同様に、シンボルを束縛したり、関数に引数として渡したり、関数の返り値として返したりすることができる。

関数全体の集合のうち関数を引数に受け取る、または、返り値が関数であるように定義された関数を汎関数という。

代表的な汎関数の使用例を以下に示す。

    ; 写像
    ) (function double (* 2 x))
    double
    ) (map double '(1 2 3))
    (2 4 6)

    ; 述語
    ) (function zero? (x) (= x 0))
    zero?
    ) (<- lis '(0 1 2))
    (0 1 2)
    ) (every? zero? lis)
    nil
    ) (some? zero? lis)
    true
    ) (none? zero? lis)
    nil

    ; 選択
    ) (select zero? lis)
    (0)
    ) (except zero? lis)
    (1 2)

### 再帰関数
関数全体の集合のうち、関数の本体で自分自身を呼ぶような関数を再帰関数と呼ぶ。

再帰関数を用いると直感的にプログラムできることがある。典型的な例として整数nの階乗を求める関数`factorial`を示す。

    (function factorial (n)
      (if (= n 1) 1 
          (* n (factorial (-- n)))))

これはwhile文を用いるより、直感的である。

    (function factorial (n)
      (let (result n)
        (while (!= n 1)
          (<- result (* result (<- n (-- n)))))
        result))

また、再帰関数全体の集合のうち自身の呼び出しごとにスタックが積まれないような関数を末尾再帰関数という。

上記のfactorialは呼び出しごとにスタックが積まれていくため末尾再帰関数ではない。

factorialは次のような末尾再起関数に変換できる。

    (function factorial (n)
      (let (rec (f (n acc)
                  (if (<= n 0) acc
                      (rec (-- n) (* acc n)))))
        (rec n 1)))

Parenでは末尾再帰の最適化が行われているため、スタックオーバーフローを気にすることなしに再帰を利用できる。

## 条件分岐
条件分岐はスペシャルオペレータ`if`を用いる。

Parenでは`nil`を偽として扱い、それ以外の値は真と見做される。

    ) (if true 1)
    1
    ) (if nil 1)
    nil
    ) (if true 1 2)
    1
    ) (if nil 1 2)
    2
    ) (if nil 1
          nil 2
          3)
    3

真偽値に関連する演算子の使用例を以下に示す。

    ) (&& 1 2 3)
    true
    ) (&& 1 nil (/ 1 0))    ; 短絡評価
    nil
    ) (|| 1 2 (/ 1 0))    ; 短絡評価
    1
    ) (|| nil 2 3)
    2
    ) (! nil)
    true
    ) (! true)
    nil

## 反復
Parenには反復のための演算子が複数用意されている。また、必要であればマクロで反復のための演算子を追加することもできる。

- loop
- while
- for
- dolist
- dotimes

### loop
`loop`は引数を順に繰り返し評価スペシャルオペレータである。

    (loop expr ...)
    expr -- 評価する式

スペシャルオペレータ`break`を利用することにより繰り返し処理を抜ける。

スペシャルオペレーター`continue`を利用すると、繰り返しの先頭にジャンプする。

### while
`while`は条件を満たす間反復するマクロである。

    (while end-test-form
        body-form ...)
    end-test-form -- 反復終了判定式
    body-form -- 反復処理

次の規則で反復処理が行われる。

    1. end-test-formを評価した結果が真なら2へ偽なら反復終了
    2. body-formを逐次評価し、1へ戻る

### for
forはwhileよりも細かく反復条件を指定することができるマクロである。

    (for binding-form end-test-form step-form
        [body-form] ...)
    binding-form ::= (sym-val ...)
    sym-val ::= sym val
    end-test-form -- 反復判定式
    step-form -- 再束縛式
    body-form -- 反復処理

forは次の手順で評価される。

    1. binding-formのシンボルと値の組を環境に束縛する。
    2. end-test-formを評価した結果が真の場合3へ、偽なら反復終了
    3. body-formを逐次評価
    4. step-formを評価して1へ戻る

単純なforの使用例として1から10までの和を順次出力するプログラムを示す。

    ) (for (i 1 sum 0) (<= i 10) (i (++ i))
        (write (<- sum (+ sum i))))
    1
    3
    6
    10
    15
    21
    28
    36
    45
    55
    nil

## スペシャルオペレーター
スペシャルオペレーターはParenの他の評価規則に従わない特殊な演算子である。

スペシャルオペレーターには次の種類が存在する。

- let
- dynamic
- <-
- begin
- macro
- f
- quote
- if
- loop/break/continue
- throw/catch
- return
- unwind-protect
- assert

### <-
`<-`はシンボルの章で説明した。

### begin

    (begin body-form ...) => result
    body-form -- 逐次評価する式
    result -- 最後に評価した式
              ただし、実行する式がなかった場合はnil

`begin`は左から順に式を評価し、引数がある場合は最後の式の評価結果を返し、そうでなければ`nil`を返す。

`begin`は主に`if`の`then`式やマクロ定義時に使用される。

組み込みでは、`for`や`while`など複数の式を評価するマクロの展開結果に含まれる。

### f

    (f ([required_param] ...
        [:opt optional_param ...]
        [{ :rest rest_param | :key keyword_param ... }] )
        body ...)
    required_param -- 必須パラメーター
    optional_param -- オプショナルパラメーター
    keyword_param -- キーワードパラメーター
    rest_param -- レストパラメーター
    body -- 関数本体

`f`は関数を作成するスペシャルオペレーターである。

`f`に与える引数は、名前を指定しないという点を除いて`function`と同等である。

`f`が作る関数のことをその名前がないことにちなみ、無名関数という。無名関数は汎関数を使うときにしばしば用いられる。

関数の章で述べた`map`の使用例を再喝する。

    ) (function double (* 2 x))
    double
    ) (map double '(1 2 3))
    (2 4 6)

これは、しばしば次のように書かかれる。

    ) (map (f (x) (* 2 x)) '(1 2 3))
    (2 4 6)

### if
`if`は条件分岐の章で述べた。

### loop/break/continue
反復の章で述べた。

### throw/catch
これらのスペシャルオペレーターは例外処理の章で述べる。

### return
`return`は現在の関数コンテキストから大域脱出するためのスペシャルオペレーターである。

    ) (function ret () 1 (return 2) 3)
    ret
    ) (ret)
    2

マクロにより、暗黙の関数で包まれている場合、想定した結果にならないことがあるため注意が必要である。

### unwind-protect
`unwind-protect`は`unwind`される前に必ず保護された式を評価することを保証するスペシャルオペレーターである。

ファイルをオープンする際に、クローズを保証する`with-open`マクロがある。

### macro
マクロの章で述べる。

### quote
quoteは引数をそのまま返す演算子である。

    (quote expr)
    expr -- 評価しない式

次のように引数の値がそのまま返される。

    ) (<- a 3)
    3
    ) a
    3
    ) (quote a)
    a

このように、評価を見送ることをクォートするという。Parenでは、クォートすることが頻繁にあるため、そのための構文糖が定義されている。

クォートする対象に`'`を前置するとその対象がクォートされる。

    ) 'a    ; <=> (quote a)
    a

クォートは、評価するタイミングをずらすためにマクロ定義にて頻繁に利用される。

## マクロ
マクロは、プログラムが評価される前に評価されるプログラムである。このことは、同一ファイル内に評価するタイミングの異なるソースコードが混在していることを意味する。

Parenのマクロが他の言語のそれと大きく異なるのは、マクロが言語と調和していることである。これにより、マクロが言語の拡張を容易にする。

例えば、C言語のマクロはC言語とは全く関係ない一つの言語に等しく、本質的には単に文字列の置換処理を行っているに過ぎない。

### マクロ定義
マクロはスペシャルオペレーターmacroを使って定義する。

    (macro name params body ...) => result
    params ::= param ...
    param ::= '('
                  [{ param | required_param } ...  ]
                  [:opt optional_param ...]
                  [{ :rest rest_param | :key keyword_param ... }]
              ')'
    name -- マクロを束縛するシンボル
    required_param -- 必須パラメーター
    optional_param -- オプショナルパラメーター
    keyword_param -- キーワードパラメーター
    rest_param -- レストパラメーター
    body -- マクロ本体
    result -- nil

関数と類似しているが、マクロの方が引数をより柔軟に指定できる。

### マクロの評価
マクロが評価されることをマクロ展開という。

マクロ展開は展開結果にマクロが含まれなくなるまで再帰的に行われる。マクロ展開後に、展開結果が評価される。

### 組み込みのマクロ
ここではいくつかの組み込みマクロの定義を述べる。

    ) (for (i 0) (< i 5) (<- i (++ i))
        (write i))
    0
    1
    2
    3
    4
    nil

マクロは、しばしば他のマクロを用いて定義される。一つの例として組み込みマクロwhileを示す。

    (macro while (test :rest body)
      (cons 'for (cons nil (cons test (cons nil body)))))

    ) (let (i 0)
        (while (< i 5)
           (write i)
           (<- i (++ i))))
    0
    1
    2
    3
    4
    nil

チュートリアルでマクロの有用性について述べるのは限界がある。興味のある方は、On Lisp[^1]をお勧めする。

## クラス
Parenは関数型言語として設計されているが、ここではマクロを用いてParenの上に構築されたオブジェクト指向言語について述べる。以後、このドメイン特化言語をPOS(Paren Object System)と呼ぶ。

### クラスの作成
クラスはマクロclassにより作成する。

    (class name ([super [feature] ...]) [field] ...)
    name -- クラスの名前
    super -- スーパークラス
    feature -- フィーチャークラス
    field -- インスタンス変数

クラスは名前、インスタンス変数を指定して作成する。

例として二次元実数空間全体の集合の元を表すクラスPointを示す。クラス名は慣習としてパスカルケースで命名する。

    (class Point () x y)

### スーパークラス
クラス定義時にスーパークラスを指定した場合スーパークラスのインスタンス変数とメソッドが継承される。

    (class A () a)
    (class B (A) b)    ; A <- B
    (class C (B) c)    ; A <- B <- C

クラスは、`Object`をルートとしたツリー構造をもつ。スーパークラスを指定しない場合には、暗に`Object`クラスを継承する。

    (class X ()) <=> (class X (Object))

`is-a?`関数はオブジェクトが、あるクラスのインスタンスか調べるための関数である。継承している場合も次のように真を返す。

    ) (is-a? (.new B) A)
    true
    ) (is-a? (.new C) D)
    nil
    ) (is-a? (.new E) A)
    true

### フィーチャー
フィーチャーはクラスを横断して共通のメソッドを定義する仕組みである。

    (class X (Object A B C))

上の例では、`Object`クラスを継承し、`A`、`B`、`C`クラスのメソッドを呼び出し可能であるようなクラス`X`を定義できる。

フィーチャーを指定しても、そのクラスのインスタンスとは見做されない。

    ) (is-a? (.new X) A)
    nil

### インスタンスの生成
インスタンスの生成には`.new`メソッドを使用する。

    ) (.new Object)
    #{ :class Object }

生成したオブジェクトのインスタンス変数はすべてnilで初期化される。

### インスタンス変数の参照と代入
クラス定義時にインスタンス変数へのアクセサが、`'&' + インスタンス変数名`、`'&' + インスタンス変数名 + '!'`として自動で生成される。

Pointクラスには`x`及び`y`というインスタンス変数があったため、それぞれ`&x, &x!`、`&y, &y!`というアクセサが自動で生成されている。

セッターの返り値は自身となる。そのため、メソッドチェーンによる記述も可能。

    ) (&x (&x! (<- p (.new Point)) 10))
    10

POSでは、他のクラスのメソッド内でアクセサを直接呼び出すことはマナー違反となる。

外に公開する関数は後述するメソッドを用いて明示的に宣言を行う。

### メソッドの定義
メソッドは、レシーバにより振る舞いを変えるような関数を定義するための仕組みである。

メソッドの定義は`method`マクロを使用する。

    (method class name ([required_param] ...
                        [:opt optional_param ...]
                        [{ :rest rest_param | :key keyword_param ... }] )
        body ...)
    name -- メソッド名
    class -- メソッドを決定するクラス
    required_param -- 必須パラメーター
    optional_param -- オプショナルパラメーター
    keyword_param -- キーワードパラメーター
    rest_param -- レストパラメーター
    body -- メソッド本体

同一クラスには同名メソッドは一つまでしか定義できない。

`method`マクロは`class`を指定することを除き、`function`マクロと同じである。

メソッド本体のコンテキストではシンボル`self`が呼び出しオブジェクトとして暗に束縛されている。

慣習として、メソッド名は`.`から始める。例外として、外部のクラスに公開したくないメソッドは`_`から始める。

メソッドは呼び出し時に、呼び出し可能なメソッドが動的にディスパッチされて実行される。例えば、次のクラスとメソッドが定義されている場合を考える。

    (class Duck ())
    (class Cat ())
    (method Duck .sound () "quack")
    (method Cat .sound () "myaa")

この場合、呼び出される引数の型により動的にメソッドがディスパッチされる。

    ) (.sound (new Duck))
    "quack"
    ) (.sound (new Cat))
    "myaa"

### メソッドの完全修飾名
`method`マクロでマクロを定義すると、シンボル`クラス名 + メソッド名`にメソッド本体が束縛される。

これをメソッドの完全修飾名という。

スーパークラスのメソッドをオーバーライドした場合などに、明示的にスーパークラスのメソッドを呼び出す場合は完全修飾名を使用する。

以下に、簡単な例を示す。

    (class Duck ())
    (class XDuck (Duck))
    (method Duck .sound () "quack")
    (method XDuck .sound () "xquack")

    (.sound (.new XDuck)) ; xquack
    (Duck.sound (.new XDuck)) ; quack

## 初期化メソッド
POSでは、インスタンスを初期化するメソッドを`.init`という名称で作成する。

`.init`メソッドが引数不要な場合、インスタンス生成時に自動で`.init`も呼ばれる。

`.init`に引数が必要な場合はプログラマが明示的に呼ばなければならない。

### メソッドのディスパッチ
メソッドは次の優先順位で探索される。

    - そのクラスのメソッド
    - フィーチャーのメソッド(フィーチャーのリストの先頭から探索)
    - スーパークラスのメソッド

## 例外処理機構
### 階層構造
Parenのすべてのエラー/例外はExceptionクラスを継承している。

    Object
        Exception
                Error

一般に、新たに例外クラスを作成する場合は`Error`クラスを継承すべきである。

### 例外のスロー
例外をスローさせるにはthrowを使用する。

    (throw (.new Error))

`throw`の引数は`Exception`オブジェクトのサブクラスでなければならない。

### 例外の補足
例外を補足させるには`catch`マクロを使用する。

    (catch (handler-list) body)
    handler-list ::= (throwable-class (args) body) ...
    throwable-class -- Throwableクラスのサブクラス
    args -- 例外補足時にハンドラーの本体で参照される仮引数
    body -- 本体処理

`body`内で例外がスローされた場合、`catch`に登録されたハンドラーを左から順に探索し補足可能なクラスの場合にハンドラーの本体が実行される。

補足可能なハンドラーがなかった場合は、この`catch`よりも上位に再度スローされる。

例えば、次のコードは`catch`の本体にて例外がスローされた場合に、`Exception2`のインスタンスである場合は、`Exception2`のハンドラーの本体処理が実行される。

    (catch (Exception1 (f (e) body ...)
            Exception2 (f (e) body ...)    ; catch!
            Exception3 (f (e) body ...))
    ...
    (throw (.new Exception2))
    ...)

## 入出力
入出力はその接続先によらず以下の関数で透過的に操作できるようになっている。

- input
    - read-byte
    - read-bytes
    - read-char
    - read-line
    - read
- output
    - write-byte
    - write-bytes
    - write-line
    - write

これらの関数は、グローバルシンボル`$in`又は`$out`をダイナミックに参照して入出力を行う。

### 標準入出力
標準入出力は、グローバルシンボル`$stdin`又は`$stdout`に保持されている。

システム起動直後は、`$in`及び、`$out`は標準入出力に束縛される。

### ファイル入出力
ファイルの入出力は`with-open`マクロで行う。

    $ cat test.txt
    hello world
    
    $ paren
    ) (with-open ($in "test.txt" :read)
        (write-line (read-line)))
    hello world
    nil
    ) (with-open ($out "test.txt" :write)
        (write-line "hello paren"))
    nil
    ) (quit)
    
    $ cat test.txt
    hello paren

`with-open`マクロはコンテキストを抜ける際にファイルのクローズを行うため、明示的にクローズする必要がない。

### 文字列ストリーム
文字列をストリームとして扱うために`with-memory-stream`マクロがある。

    $ paren
    ) (with-memory-stream ($in "hello world\n")
        (write-line (read-line)))
    hello world
    ) (with-memory-stream ($out)
        (write-line "hello paren"))
    "hello paren\n"

# NOTES
## Common Lispとの違い
LISP経験者のために代表的なCommon Lispとの違いを述べる。

## シンボルの変換
両者ともシンボルの大文字小文字を区別するが、Common Lispはデフォルトで大文字に変換するのに対して、Parenでは一切変換を行わない。

## 多値を返す関数
Parenは多値を返す関数がない代わりにシンボルの束縛は構造化されている。

    ) (<- (foo bar) (list 1 2) buzz 3)
    3
    ) foo
    1
    ) bar
    2
    ) buzz
    3
    ) (let ((foo bar :opt buzz) '(1 2)) (list foo bar buzz))
    (1 2 nil)

## 純リスト
Parenの任意のリストは純リストである。ドット対に対応するデータ構造はない。

[^1]: On Lisp http://www.paulgraham.com/onlisp.html