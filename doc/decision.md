決断履歴

# 概要
Parenの仕様策定にあたり、いくつかの候補があった場合はのちに振り返れるように記録する。

# 入出力関数群の命名
次の命名とした。

    関数       処理
    -----------------------------------------
    write      入力可能な形式で出力
    read       Parenオブジェクトとして入力
    read-char  一文字読み込み
    read-byte  一バイト書き込み
    read-line  一行読み込み

# 例外のクラス
Parenでは例外系のクラス階層は次のように定義する。

    Error
    Exception
        EOFReachedException
        IOException
        ...

Errorクラスは継続が困難な状態や、到達すべきでない状態を表すクラスである。

throwされた場合はcatchオペレーターでは補足できず、即座に終了する。

原則として継承は許さず、どのようなエラーかはメッセージフィールドを用いて表現する。

    Error out of memory.
    Error illegal arguments.
    Error precondition not satisfied.
    ...

Exceptionクラスは大域脱出機構を提供するためのクラスである。

throwされた場合はcatchオペレーターを使用して補足することができる。

必要に応じて継承することができる。
