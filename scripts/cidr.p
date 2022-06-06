; cidr

(import :optparse)

(function x.x.x.x->i32 (x.x.x.x)
  (let (x (map int (split x.x.x.x ".")))
    (| (<< ([] x 0) 24)
       (<< ([] x 1) 16)
       (<< ([] x 2) 8)
       ([] x 3))))

(function i32->x.x.x.x (x)
  (format "%d.%d.%d.%d"
          (>> (& x 0xff000000) 24)
          (>> (& x 0x00ff0000) 16)
          (>> (& x 0x0000ff00) 8)
          (& x 0x000000ff)))

(function parse-cidr (cidr)
  (let ((x.x.x.x prefix-length) (split cidr "/")
                                prefix-length (int prefix-length)
                                addr (x.x.x.x->i32 x.x.x.x)
                                mask (~ (>> 0xffffffff prefix-length))
                                address-count (int (pow 2 (- 32 prefix-length)))
                                network-addr (& addr mask)
                                broadcast-addr (+ network-addr address-count -1)
                                address-range (map i32->x.x.x.x (list network-addr broadcast-addr)))
    (group (list
             :address x.x.x.x
             :mask (i32->x.x.x.x mask)
             :addresses address-count
             :address-range address-range
             :network-address (car address-range)
             :broadcast-address (cadr address-range))
           2)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l") args)
                  result (parse-cidr (car args)))
    (if (nil? (.get op "l")) (foreach write result)
        (for ((s e) (map x.x.x.x->i32 (cadr (assoc :address-range result)))) (<= s e) (s (++ s))
          (write-line (i32->x.x.x.x s))))))
