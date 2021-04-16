; yubaba

(import :random)

(function say (:rest args)
  (write-line (apply str (cons "湯婆婆：" args))))

(function new-name (name)
  (let (i (randint (strlen name)))
    (substr name i (++ i))))

(function! main (args)
  (let (name nil)
    (say "契約書だよ。そこに名前を書きな。")
    (write-mem "> ")
    (say "フン、" (<- name (read-line)) " というのかい。贅沢な名だねえ。")
    (say "今からおまえの名は " (<- name (new-name name)) " だ。いいかい、" name " だよ。")
    (say "分かったら返事をするんだ、" name " ！！")))
