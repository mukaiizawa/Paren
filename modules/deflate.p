; deflate.

(class DeflateError (Error))

;; Reader

(class Deflate.Reader ()
  ;; Bit stream reader
  pos curr-byte)

(method Deflate.Reader .init ()
  (<- self->pos 0)
  self)

(method Deflate.Reader .align ()
  (.init self))

(method Deflate.Reader .read-bit ()
  (let (byte self->curr-byte pos self->pos)
    (if (= pos 0) (<- self->curr-byte (<- byte (read-byte))))
    (if (< byte 0) -1
        (begin
          (<- self->pos (% (++ pos) 8))
          (& (>> byte pos) 1)))))

(method Deflate.Reader .read (size)
  (let (val 0)
    (dotimes (i size)
      (let (bit (.read-bit self))
        (if (= bit -1) (raise EOFError)
            (<- val (| val (<< bit i))))))
    val))

;; Writer

(class Deflate.Writer (MemoryStream))

(method Deflate.Writer .copy (length distance)
  (if (|| (nil? distance) (< distance 1) (> distance 32768)) (raise DeflateError "invalid distance")
      (|| (nil? length) (< length 0)) (raise DeflateError "invalid length")
      (let (buf (.buf (.reserve self length)) pos self->wrpos)
        (dotimes (i length)
          (.write-byte self ([] buf (+ (- pos distance) i)))))))

(method Deflate.Writer .flush ()
  (slice self->buf 0 self->wrpos))

;; Haffman code

(function deflate.parse-code-lengths (code-lengths)
  ;; Canonical Huffman Code lengths to Canonical Huffman Code.
  ;; #[ 1 0 3 2 3 ] -> #[ (2x0 0) (2x10 2) (2x110 3)  (2x111 4) ]
  (let (size (len code-lengths) code-symbol-pairs (array size) n 0 code-len 0 MSB 0 LSBs 0)
    (dotimes (i 15)
      (<- code-len (++ code-len)
          MSB (<< 1 code-len)
          LSBs (<< LSBs 1))
      (dotimes (j size)
        (when (= ([] code-lengths j) code-len)
          ([] code-symbol-pairs n (list (| MSB LSBs) j))
          (<- n (++ n)
              LSBs (++ LSBs)))))
    (slice code-symbol-pairs 0 n)))

(function deflate.decode-huffman-code (rd code-symbol-pairs)
  (let (code 1)
    (loop
      (<- code (| (<< code 1) (.read rd 1)))
      (let (lo 0 hi (-- (len code-symbol-pairs)))
        (while (<= lo hi)
          (let (mi (>> (+ lo hi) 1) pair ([] code-symbol-pairs mi) val (car pair))
            (if (= code val) (return (cadr pair))
                (< code val) (<- hi (-- mi))
                (<- lo (++ mi)))))))))

;; inflate

(<- $deflate.fixed-huffman-literal-codes
    (deflate.parse-code-lengths
      (reduce (f (a args) (apply fill! (cons a args)))
              (cons (array 288) '((8 0 144)
                                  (9 144 256)
                                  (7 256 280)
                                  (8 280 288)))))
    $deflate.fixed-huffman-distance-codes
    (deflate.parse-code-lengths (fill! (array 32) 5)))

(function deflate.decode-length (rd sym)
  (if (<= sym 256) nil    ; invalid
      (<= sym 264) (- sym 254)
      (<= sym 284) (let (n (// (- sym 261) 4))
                     (+ (<< (+ (% (- sym 265) 4) 4) n) 3 (.read rd n)))
      (= sym 285) 258))

(function deflate.decode-distance (rd sym)
  (if (< sym 0) nil    ; invalid
      (<= sym 3) (++ sym)
      (<= sym 29) (let (n (-- (// sym 2)))
                    (+ (<< (+ (% sym 2) 2) n) 1 (.read rd n)))))

(function deflate.decode-huffman-block (rd wr literal-codes distance-codes)
  (loop
    (let (sym (deflate.decode-huffman-code rd literal-codes))
      (if (< sym 256) (.write-byte wr sym)
          (= sym 256) (return :end-of-block)
          (< sym 286) (.copy wr
                             (deflate.decode-length rd sym)
                             (deflate.decode-distance rd (deflate.decode-huffman-code rd distance-codes)))
          (raise DeflateError "invalid Huffman block")))))

(function deflate.no-compression-block (rd wr)
  (.align rd)    ; Any bits of input up to the next byte boundary are ignored.
  (let (len (.read rd 16))
    (if (!= (^ len 0xffff) (.read rd 16)) (raise DeflateError "invalid Non-compressed block")
        (dotimes (i len) (.write-byte wr (.read rd 8))))))

(function deflate.fixed-huffman-block (rd wr)
  (deflate.decode-huffman-block rd wr $deflate.fixed-huffman-literal-codes $deflate.fixed-huffman-distance-codes))

(<- $deflate-code-lengths-order '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(function deflate.dynamic-huffman-block (rd wr)
  (let (HLIT (.read rd 5) HDIST (.read rd 5) HCLEN (.read rd 4)
             code-lengths (fill! (array 19) 0) lit/dis-codes (fill! (array (+ HLIT 257 HDIST 1)) 0))
    (dotimes (i (+ HCLEN 4))
      ([] code-lengths ([] $deflate-code-lengths-order i) (.read rd 3)))
    (let (i 0 code-symbol-pairs (deflate.parse-code-lengths code-lengths))
      (while (< i (len lit/dis-codes))
        (let (sym (deflate.decode-huffman-code rd code-symbol-pairs))
          (if (<= 0 sym 15) (begin ([] lit/dis-codes i sym) (<- i (++ i)))
              (<= 16 sym 18) (let (len nil val 0)
                               (if (= sym 16) (<- len (+ (.read rd 2) 3) val ([] lit/dis-codes (-- i)))
                                   (= sym 17) (<- len (+ (.read rd 3) 3))
                                   (<- len (+ (.read rd 7) 11)))
                               (fill! lit/dis-codes val i (<- i  (+ i len))))
              (raise DeflateError)))))
    (deflate.decode-huffman-block rd wr
                                  (deflate.parse-code-lengths (slice lit/dis-codes 0 (+ HLIT 257)))
                                  (deflate.parse-code-lengths (slice lit/dis-codes (+ HLIT 257))))))

(function deflate.uncompress (data)
  (with-memory-stream ($in data)
    (let (rd (.new Deflate.Reader) wr (.new Deflate.Writer) last-block? nil type nil)
      (while (nil? last-block?)
        (<- last-block? (= (.read rd 1) 1) type (.read rd 2))
        (if (= type 0) (deflate.no-compression-block rd wr)
            (= type 1) (deflate.fixed-huffman-block rd wr)
            (= type 2) (deflate.dynamic-huffman-block rd wr)
            (raise DeflateError "unsupported type")))
      (.flush wr))))

(function! main (args)
  ;                                     type=0                 len=0                  nlen
  (assert (= (deflate.uncompress #< 2x00000001 2x00000000 2x00000000 2x11111111 2x11111111 >)
             #< >))
  ;                                     type=0                 len=3                  nlen          5         20         35
  (assert (= (deflate.uncompress #< 2x00000001 2x00000011 2x00000000 2x11111100 2x11111111 2x00000101 2x00010100 2x00100011 >)
             #< 0x05 0x14 0x23 >))
  ;                                     type=0                 len=1                  nlen          5
  (assert (= (deflate.uncompress #< 2x00000000 2x00000001 2x00000000 2x11111110 2x11111111 2x00000101
  ;                                     type=0                 len=2                  nlen         20         35
                                    2x00000001 2x00000010 2x00000000 2x11111101 2x11111111 2x00010100 2x00100011>)
             #< 0x05 0x14 0x23 >)))
