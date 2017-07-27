Parenドキュメント

# 概要
このドキュメントはParenドキュメントについて述べたものである。

# Parenドキュメント仕様
Parenドキュメントは次のフォーマットである。

# Parenドキュメントの作成
ParenのソースコードからParenドキュメントを作成するには、
pdocコマンドを使用する。
    paren pdoc filename
マークダウン形式で出力する場合はフラグ`-m`を使用する。
    paren pdoc -m filename

# Parenドキュメント作成対象
次の対象にはParenドキュメントを明示的に作成すべきである。
- モジュールファイルそのもの
- 変数
- 関数
- マクロ

# Parenドキュメントの記述
例として関数のParenドキュメントを示す。
    #||
     | # Function list
     | ## Syntax
     |     list . objects => list
     | ## Description
     | list returns a list containing the supplied objects.
     | ## Arguments
     |     objects --- a object list. 
     | ## Value
     |     list --- a list containing the supplied object.
     | ## Examples
     | ## Side Effects
     | ## Exceptional Situations
     | ## See Also
     |     [cons](core.html#cons).
     |#
    (def list (. args) args)
一行目は見出しレベル最大で記述対象を明記する。
Parenドキュメントを記載する場合は必要に応じて次の項目を使用する。
- Syntax
- Description
- Arguments
- Value
- Examples
- Side Effects
- Exceptional Situations
- See Also
## Syntax
構文を記述する。
また、評価結果を`=>`の後に記載する。
## Description
ドキュメント対象についての情報をできるだけ詳細に記載する。
## Arguments
仮引数の仕様を記述する。
## Value
評価結果について記述する。
## Examples
使用例を記載する。
## Side Effects
副作用がある場合に明記する。
## Side Effects
副作用がある場合は明記する。
## Exceptional Situations
例外が発生しうる場合は個々に明記する。
## See Also
参照ページがある場合はここにリンクを記述する。
