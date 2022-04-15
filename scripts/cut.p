; cut.

(import :optparse)

(function parse-lists (lists)
  (map (f (x)
         (let ((s :opt e) (split x "-"))
           (if (nil? e) (<- s (int s) e s)
               (empty? s) (<- s 1 e (int e))
               (empty? e) (<- s (int s) e nil)
               (<- s (int s) e (int e)))
           (list (-- s) e)))    ; counted from 1.
       (split lists ",")))

(function xslice (seq range)
  (let ((s e) range size (len seq))
    (if (> s size) ""
        (|| (nil? e) (> e size)) (slice seq s)
        (slice seq s e))))

(function cutb (ranges :opt _)
  (f (line)
    (let (seq (bytes line))
      (foreach (f (r) (write-bytes (xslice seq r)))
               ranges)
      (write-line))))

(function cutc (ranges :opt _)
  (f (line)
    (foreach (f (r) (write-bytes (xslice line r)))
             ranges)
    (write-line)))

(function cutf (ranges :opt delim)
  (f (line)
    (let (seqs (split line delim) acc nil)
      (foreach (f (r) (push! (join (xslice seqs r) delim) acc))
               ranges)
      (write-line (join (reverse! acc) delim)))))

(function cut (fn lists :opt delim)
  (let (cut1 (fn (parse-lists lists) delim))
    (foreach cut1 (collect read-line))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "b:c:d:f:") args) lists nil)
    (if (<- lists (.get op "b")) (cut cutb lists)
        (<- lists (.get op "c")) (cut cutc lists)
        (<- lists (.get op "f")) (cut cutf lists (|| (.get op "d") "\t"))
        (raise ArgumentError "requires OPTION"))))
