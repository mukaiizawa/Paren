; Adler-32 checksum module.

(function adler32.sum (buf)
  (let (a 1 b 0 mod 65521)
    (<- buf (bytes buf))
    (for (i 0) (< i (len buf)) (i (++ i))
      (<- a (% (+ a ([] buf i)) mod)
          b (% (+ b a) mod)))
    (| (<< b 16) a)))

(function! main (args)
  (assert (= (adler32.sum "The quick brown fox jumps over the lazy dog") 0x5bdc0fda)))
