; http module.

(<- $http.status-codes
    '((100 "Continue")
      (101 "Switching Protocols")
      (102 "Processing")
      (103 "Early Hints")
      (200 "OK")
      (201 "Created")
      (202 "Accepted")
      (203 "Non-Authoritative Information")
      (204 "No Content")
      (205 "Reset Content")
      (206 "Partial Content")
      (207 "Multi-Status")
      (208 "Already Reported")
      (226 "IM Used")
      (300 "Multiple Choices")
      (301 "Moved Permanently")
      (302 "Found")
      (303 "See Other")
      (304 "Not Modified")
      (305 "Use Proxy")
      (307 "Temporary Redirect")
      (308 "Permanent Redirect")
      (400 "Bad Request")
      (401 "Unauthorized")
      (402 "Payment Required")
      (403 "Forbidden")
      (404 "Not Found")
      (405 "Method Not Allowed")
      (406 "Not Acceptable")
      (407 "Proxy Authentication Required")
      (408 "Request Timeout")
      (409 "Conflict")
      (410 "Gone")
      (411 "Length Required")
      (412 "Precondition Failed")
      (413 "Content Too Large")
      (414 "URI Too Long")
      (415 "Unsupported Media Type")
      (416 "Range Not Satisfiable")
      (417 "Expectation Failed")
      (418 "I'm a teapot")
      (421 "Misdirected Request")
      (422 "Unprocessable Content")
      (423 "Locked")
      (424 "Failed Dependency")
      (425 "Too Early")
      (426 "Upgrade Required")
      (427 "Unassigned")
      (428 "Precondition Required")
      (429 "Too Many Requests")
      (430 "Unassigned")
      (431 "Request Header Fields Too Large")
      (451 "Unavailable For Legal Reasons")
      (500 "Internal Server Error")
      (501 "Not Implemented")
      (502 "Bad Gateway")
      (503 "Service Unavailable")
      (504 "Gateway Timeout")
      (505 "HTTP Version Not Supported")
      (506 "Variant Also Negotiates")
      (507 "Insufficient Storage")
      (508 "Loop Detected")
      (509 "Unassigned")
      (510 "Not Extended (OBSOLETED)")
      (511 "Network Authentication Required")))

(class HTTPError (Error))

;; Stream.
(class HTTPStream (SocketStream))

(method HTTPStream .write-line (:opt args)
  (if args (.write-bytes self args))
  (.write-bytes self "\r\n"))

(method HTTPStream .write-header (key val)
  (.write-line self (format "%s: %s" key (str val))))

(method HTTPStream .write-status-line (status-code)
  (.write-line self (format "HTTP/1.0 %d %s" status-code (cadr (assoc status-code $http.status-codes)))))

;; Request.
(class HTTPRequest ()
  method request-target version headers)

(method HTTPRequest .parse-header (line)
  (if (empty? line) nil
      (with-memory-stream ($in line)
        (let (rd (.new AheadReader))
          (while (!= (.next rd) ":")
            (if (nil? (.next rd)) (throw 400)
                (.get rd)))
          (.skip rd ":")
          (let (name (.token rd))
            (.skip-space rd)
            (while (.next rd) (.get rd))
            (list name (.token rd)))))))

(method HTTPRequest .parse (stream)
  (let (vals (split (.read-line stream) " "))
    (if (!= (len vals) 3) (throw 400)
        (<- self->method (car vals)
            self->request-target (http.decode-url (cadr vals))
            self->version (caddr vals)
            self->headers (collect (f () (.parse-header self (.read-line stream))))))
    self))

(method HTTPRequest .method ()
  self->method)

(method HTTPRequest .request-line ()
  (format "%s %s %s" self->method self->request-target self->version))

(method HTTPRequest .request-target ()
  self->request-target)

(method HTTPRequest .headers ()
  self->headers)

;; Response.
(class HTTPResponse ()
  status-code headers body)

(method HTTPResponse .init (status-code headers body)
  (<- self->status-code status-code
      self->headers headers
      self->body body)
  self)

(method HTTPResponse .status-line ()
  (format "HTTP/1.0 %d %s" self->status-code (cadr (assoc self->status-code $http.status-codes))))

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
