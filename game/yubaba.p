; yubaba.

(import :rand)

(function say (:rest args)
  (write-line (apply str (cons "湯婆婆：" args))))

(function new-name (name)
  ([] name (rand.int (len name))))

(function! main (args)
  (let (name nil)
    (say "契約書だよ。そこに名前を書きな。")
    (write-bytes "> ")
    (say "フン、" (<- name (read-line)) " というのかい。贅沢な名だねえ。")
    (say "今からおまえの名は " (<- name (new-name name)) " だ。いいかい、" name " だよ。")
    (say "分かったら返事をするんだ、" name " ！！")))
