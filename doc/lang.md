Paren言語仕様書

# 概要
この資料はParenの仕様を定義する。
Parenのチュートリアルは別紙を参照のこと。

# メタ言語
Parenの文法を以下のEBNF表記で定義する。

    x? -- xは省略可能。
    . -- 任意の一文字
    x* -- xの零回以上の繰り返し。
    x+ -- xの一回以上の繰り返し。
    (x | y) -- xまたはy。
    'x' -- 固定字句。文字の並びxを示す。xは複数の場合もある。
    [...] -- 文字グループ。[]内で指定された文字の何れか。
             x-yと表記された場合はxとyの間の何れかを表す。
             先頭に~を指定した場合は[]内で指定された文字以外の何れかを示す。
    ::= -- 定義。左辺で示される構文要素を右辺で定義される。

## 注意事項
メタ言語内で可読性のため空白を挿入することがあるが、その空白はメタ言語において無視されるものとする。

固定字句及び文字グループ中で`\`を記述した場合、後続する文字そのものを意味する。ただし、`\t`及び`\n`はそれぞれタブ文字、改行文字を意味するものとする。
量指定子は特に指定がなければ最長一致とする。

# 字句規則
ここで述べる字句規則の実装は、リードマクロによる実装も含む。

## 字句の区切り（separator）

    separator ::= space | comment

プログラムは空白又はコメントにより区切られる。

これらは要素の区切りとして使用される以外は無視される。

### 空白（space）

    space ::= [\t\n ]

空白はタブ文字、改行文字、半角スペースである。

### コメント（comment）

    comment ::= ';' [^\n]*

コメントは`;`から行末までである。

## S式（s_expr）

    s_expr ::= list | atom

S式はリストまたはアトムである。

### リスト（list）

    list ::= '(' (s_expr (separator s_expr)* )? ')'

リストは零以上のS式を括弧で括ったものである。

要素がないリストは空のリストと呼び、セマンティックス上は後述するキーワードnilと等価である。

### アトム（atom）

    atom ::= (symbol | keyword | number | string)

atomは次のリテラルがある。

- シンボル
- キーワード
- 数値
- 文字列

#### シンボル（symbol）

    symbol ::= identifier
    identifier ::= (identifier_first identifier_rest* | identifier_special)
    identifier_first ::= [!$%&*/<=>?a-zA-Z^_|]
    identifier_rest ::= (identifier_first | [0-9+\-])*
    identifier_special ::= ('+' | '-')

シンボルは一部の記号及び数字を除く文字から始まり、一部を除くascii文字が任意の数続く。

#### キーワード（keyword）

    keyword ::= ':' identifier

キーワードは`:`から始まるシンボルのことをいう。

#### 数値（number）

    number ::= (integer | float)

数値には整数と、浮動小数点数がある。

#### 整数（integer）

    integer ::= (digit+ 'x')? [0-9a-z]+
    digit ::= [0-9]

整数は数値と'x'を前置する事で基数を指定出来る。

基数は36まで設定可能だが、例外として0を指定すると16が指定されたものと見做す。

#### 浮動小数点数（float）

    float ::= digit+ '.' digit+

#### 文字列（string）

    string ::= '"' ([^"\\] | esc)* '"'
    esc ::= '\\' .

文字列はダブルクォートで囲まれた文字またはエスケープシーケンスの列である。

エスケープシーケンスは'\'から始まり次の一文字によりその意味が異なる。

    \a -- 0x07(bell)
    \b -- 0x08(back space)
    \e -- 0x1B(escape)
    \f -- 0x0c(form feed)
    \n -- 0x0a(line feed)
    \r -- 0x0d(carriage return)
    \t -- 0x09(horizontal tab)
    \v -- 0x0b(vertical tab)
    \x -- interpret the following two letters as a hexadecimal number.
    \ -- interpret the following letter as a 
