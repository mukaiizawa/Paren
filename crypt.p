; crypt.

(<- $usage
"
Usage: paren crypt.p [KEY]
	Encrypt the input with a periodic substitution formula.
	Since the character code of the key is acquired periodically and the XOR is taken with the input, it can be decrypted by re-encrypting the encrypted file with the same key.
	There is no practical strength.
	If KEY is omitted, it is considered that the 'nil' is specified.
")

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (let (key (cadr args) len (memlen key) pos 0 b nil)
      (while (/= (<- b (read-byte)) -1)
        (write-byte (^ b ([] key (<- pos (mod (++ pos) len)))))))))
