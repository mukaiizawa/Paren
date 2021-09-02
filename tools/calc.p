; arbitrary precision calculator.

(function parse-factor (rd)
  (if (!= (.next (.skip-space rd)) "(") (.skip-number rd)
      (begin
        (.skip rd)
        (let (val (parse-expr rd))
          (.skip rd ")")
          val))))

(function parse-term (rd)
  (let (next nil val (parse-factor rd))
    (while (in? (<- next (.next (.skip-space rd))) '("*" "/"))
      (.skip rd)
      (<- val (apply (eval (symbol next)) (list val (parse-factor rd)))))
    val))

(function parse-expr (rd)
  (let (next nil val (parse-term rd))
    (while (in? (<- next (.next (.skip-space rd))) '("+" "-"))
      (.skip rd)
      (<- val (apply (eval (symbol next)) (list val (parse-term rd)))))
    val))

(function calc (expr)
  ; calc [EXPR]
  ; Evaluate the EXPR and display the value.
  ; If the EXPR is omitted, read standard input.
  (with-memory-stream ($in expr)
    (let (rd (.new AheadReader))
      (parse-expr rd))))

(function! main (args)
  (if (nil? args)
      (let (expr (read-line))
        (when expr
          (write (calc expr))
          (main nil)))
      (write (calc (join args " ")))))
