; zundoko kiyoshi.

(import :random)

(function zundoko-kiyoshi (n)
  (if (randbool)
      (begin
        (write-bytes "ズン")
        (zundoko-kiyoshi (++ n)))
      (begin
        (write-bytes "ドコ")
        (if (< n 4) (zundoko-kiyoshi 0)
            (write-line "キヨシ")))))

(function! main (args)
  (zundoko-kiyoshi 0))
