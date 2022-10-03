; http module.

(import :sock)

;; Stream.
(class HTTPStream (SocketStream))

(method HTTPStream .write-line (:opt args)
  (if args (.write-bytes self args))
  (.write-bytes self "\r\n"))

(method HTTPStream .write-header (key val)
  (.write-line self (format "%s: %s" key (str val))))

(method HTTPStream .write-status-line (status-code)
  (.write-line self (format "HTTP/1.0 %d %s" status-code (cadr (assoc status-code $status-code-value)))))

;; Request.
(class HTTPRequest ()
  request-line method resource-path headers timestamp)

;; Response.
(class HTTPResponse ()
  status-code headers body)

(method HTTPResponse .common-log-format ()
  (format "- - - [%02d/%3.3s/%04d:%02d:%02d:%02d +0000] %v %d %d\n"
          (.day dt) (.to-s.month dt) (.year dt) (.hour dt) (.minute dt) (.second dt)
          request-line status-code (byte-len contents))

;; API.

(function http.encode-url (url)
  ; Encode the given string according to the percent-encode described in RFC3986.
  ; The caller must determine whether it should be encoded or not.
  (with-memory-stream ($out)
    (doarray (x (bytes url))
      (write-bytes (format "%%%02X" x)))))

(function http.decode-url (url)
  ; The given string is assumed to be encoded with percent-encode described in RFC3986 and decoded.
  ; The caller must determine whether it should be decoded or not.
  (let (trail? nil val nil digit-val (f (x) (index (upper x) "0123456789ABCDEF")))
    (with-memory-stream ($out)
      (dostring (ch url)
        (if (= ch "%") (<- trail? true)
            (nil? trail?) (write-bytes ch)
            (nil? val) (<- val (digit-val ch))
            (begin
              (write-byte (+ (* val 16) (digit-val ch)))
              (<- trail? nil val nil)))))))

(function! main (args)
  (assert (= (http.encode-url "012あ") "%30%31%32%E3%81%82"))
  (assert (= (http.decode-url "012%E3%81%82") "012あ"))
  (assert (= (http.decode-url "%30%31%32%E3%81%82") "012あ")))
