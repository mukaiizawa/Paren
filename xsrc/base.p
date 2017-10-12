"""
Base system.
ベースシステムはParenの基礎となるプログラムでプログラムを読み込み実行するための最低限度の機能を提供する。
"""

(function caar (x) (car (car x)))
(function cadr (x) (car (cdr x)))
(function cdar (x) (cdr (car x)))
(function cddr (x) (cdr (cdr x)))
(function caaar (x) (car (car (car x))))
(function caadr (x) (car (car (cdr x))))
(function cadar (x) (car (cdr (car x))))
(function caddr (x) (car (cdr (cdr x))))
(function cdaar (x) (cdr (car (car x))))
(function cdadr (x) (cdr (car (cdr x))))
(function cddar (x) (cdr (cdr (car x))))
(function cdddr (x) (cdr (cdr (cdr x))))
(function caaaar (x) (car (car (car (car x)))))
(function caaadr (x) (car (car (car (cdr x)))))
(function caadar (x) (car (car (cdr (car x)))))
(function caaddr (x) (car (car (cdr (cdr x)))))
(function cadaar (x) (car (cdr (car (car x)))))
(function cadadr (x) (car (cdr (car (cdr x)))))
(function caddar (x) (car (cdr (cdr (car x)))))
(function cadddr (x) (car (cdr (cdr (cdr x)))))
(function cdaaar (x) (cdr (car (car (car x)))))
(function cdaadr (x) (cdr (car (car (cdr x)))))
(function cdadar (x) (cdr (car (cdr (car x)))))
(function cdaddr (x) (cdr (car (cdr (cdr x)))))
(function cddaar (x) (cdr (cdr (car (car x)))))
(function cddadr (x) (cdr (cdr (car (cdr x)))))
(function cdddar (x) (cdr (cdr (cdr (car x)))))
(function cddddr (x) (cdr (cdr (cdr (cdr x))))))

(function list (:rest args) args)

(function println (:rest args)
  (apply print args))
