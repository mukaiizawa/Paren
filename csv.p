; csv module.

(class CSVReader (AheadReader))

(method CSVReader .get-quoted ()
  (.skip self)
  (while true
    (if (memneq? (.next self) "\"") (.get self)
        (begin
          (.skip self)
          (if (memeq? (.next self) "\"") (.get self)
              (return nil))))))

(method CSVReader .parse-field ()
  (if (memeq? (.next self) "\"") (.get-quoted self)
      (while (none? (f (x) (memeq? (.next self) x)) '("," "\n"))
        (.get self)))
  (.token self))

(method CSVReader .read ()
  ; Read csv as specified by RFC 4180.
  ; Returns a list representation of read csv.
  ; Rules that are not compliant are as follows.
  ; - The last record in the file must have an ending line break.
  (if (nil? (.next self)) nil
      (let (fields nil)
        (push! fields (.parse-field self))
        (while (memeq? (.next self) ",")
          (.skip self)
          (push! fields (.parse-field self)))
        (.skip self "\n")
        (reverse! fields))))

(function! main (args)
  (with-memory-stream ($in
"foo,bar,buzz
\"foo\",\"bar\",\"buzz\"
")
    (let (rd (.new CSVReader)
             (r1 r2) (collect (f () (.read rd)))
             (r11 r12 r13) r1
             (r21 r22 r23) r2)
      (assert (memeq? r11 r21))
      (assert (memeq? r12 r22))
      (assert (memeq? r13 r23)))))
