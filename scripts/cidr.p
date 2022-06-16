; cidr

(import :optparse)

(function x.x.x.x->i32 (x.x.x.x)
  (reduce (f (x y) (| (<< x 8) y))
          (map int (split x.x.x.x "."))))

(function i32->x.x.x.x (x)
  (format "%d.%d.%d.%d"
          (& (>> x 24) 0xff)
          (& (>> x 16) 0xff)
          (& (>> x 8) 0xff)
          (& x 0xff)))

(function parse-cidr (cidr)
  (let ((x.x.x.x prefix-length) (split cidr "/")
                                prefix-length (int prefix-length)
                                addr (x.x.x.x->i32 x.x.x.x)
                                mask (~ (>> 0xffffffff prefix-length))
                                address-count (int (pow 2 (- 32 prefix-length)))
                                network-addr (& addr mask)
                                broadcast-addr (+ network-addr address-count -1)
                                address-range (map i32->x.x.x.x (list network-addr broadcast-addr)))
    (list
      :address x.x.x.x
      :mask (i32->x.x.x.x mask)
      :addresses address-count
      :address-range address-range
      :network-address (car address-range)
      :broadcast-address (cadr address-range))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l") args)
                  result (parse-cidr (car args)))
    (if (nil? (.get op "l")) (foreach write (group result 2))
        (for ((s e) (map x.x.x.x->i32 (cadr (member :address-range result)))) (<= s e) (s (++ s))
          (write-line (i32->x.x.x.x s))))))
