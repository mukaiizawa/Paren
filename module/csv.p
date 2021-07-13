; csv module.

(class CSVReader (AheadReader))

(method CSVReader .get-quoted ()
  (.skip self)
  (loop
    (if (!= (.next self) "\"") (.get self)
        (begin
          (.skip self)
          (if (= (.next self) "\"") (.get self)
              (return nil))))))

(method CSVReader .parse-field ()
  (if (= (.next self) "\"") (.get-quoted self)
      (while (none? (f (x) (= (.next self) x)) '("," "\n"))
        (.get self)))
  (.token self))

(method CSVReader .read ()
  ; Read csv as specified by RFC 4180.
  ; Returns a list representation of read csv.
  ; Rules that are not compliant are as follows.
  ; - The last record in the file must have an ending line break.
  (if (nil? (.next self)) nil
      (let (fields nil)
        (push! (.parse-field self) fields)
        (while (= (.next self) ",")
          (.skip self)
          (push! (.parse-field self) fields))
        (.skip self "\n")
        (reverse! fields))))

(function! main (args)
  (let (csv (join '("foo,\"bar\",buzz\n" "\"foo\",bar,\"buzz\"\n")))
    (with-memory-stream ($in csv)
      (let (rd (.new CSVReader) (r1 r2) (collect (f () (.read rd))))
        (assert (= r1 r2))))))
