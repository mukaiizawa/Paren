# Paren

    プログラムを生成するプログラムだって?いつそんなものが必要なんだ?そんなこと滅多にないよ、とCobolプログラマは言うだろう。
    いつでもさ、とLispプログラマは言うだろう。
    -- Paul Graham, Beating the Averages

Paren(パレン)は少量の記述で処理が実現できるように設計されたプログラミング言語である。

また、単純な構文によりほとんど学習することなしに使用することができる。

# Building
Parenのビルド例を示す。

    $ git clone https://github.com/mukaiizawa/Paren
    $ cd src 
    $ make os=windows
    $ ../paren
    ) (write "hello world")
    "hello world"

コマンドライン引数なしで実行すると、インタープリタが起動する。コマンドライン引数がある場合、第一引数をスクリプトファイルとして読み込み、第二引数移行をそのスクリプトファイルに定義されているmain関数のargsに束縛してmainを実行する。

    $ cat scrypt.p
    (function main (args)
      (write args))
    $ paren scrypt.p hello world
    ("scrypt.p" "hello" "world")

# Structure

    + coreutils -- Paren implementation of like the GNU coreutils programs
    + game -- games and toy-like programs
    + misc -- miscellaneous programs
    + module -- importable Paren module files
    + src -- implementation of Paren itself
    + tool -- Implementation of tool by Paren
    lang.md  -- Paren language specification
    license.txt -- Paren license
    tutorial.md -- tutorial of Paren

# Special Thanks

- Ken'ichi Tokuoka
