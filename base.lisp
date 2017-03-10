#|
base system.
ベースシステムはParenの基盤となるライブラリで、プログラムを実行するための最小限の機能を提供する。
|#

(defmacro defun (fname args &body body)
  "関数を定義する。"
  `(defvar fname
     (lambda (,@args)
       ,@body)))
