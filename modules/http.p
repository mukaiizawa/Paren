; http module.

(import :sock)

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
      (511 "Network Authentication Required"))
    $http.mime-types    ; based on suffix
    '(("aac" "audio/aac")
      ("abw" "application/x-abiword")
      ("arc" "application/x-freearc")
      ("avi" "video/x-msvideo")
      ("azw" "application/vnd.amazon.ebook")
      ("bin" "application/octet-stream")
      ("bmp" "image/bmp")
      ("bz" "application/x-bzip")
      ("bz2" "application/x-bzip2")
      ("csh" "application/x-csh")
      ("css" "text/css")
      ("csv" "text/csv")
      ("doc" "application/msword")
      ("docx" "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
      ("eot" "application/vnd.ms-fontobject")
      ("epub" "application/epub+zip")
      ("gz" "application/gzip")
      ("gif" "image/gif")
      ("htm" "text/html")
      ("html" "text/html")
      ("ico" "image/vnd.microsoft.icon")
      ("ics" "text/calendar")
      ("jar" "application/java-archive")
      ("jpeg" "image/jpeg")
      ("jpg" "image/jpeg")
      ("js" "text/javascript")
      ("json" "application/json")
      ("jsonld" "application/ld+json")
      ("mid" "audio/midi")
      ("midi" "audio/midi")
      ("mjs" "text/javascript")
      ("mp3" "audio/mpeg")
      ("mpeg" "video/mpeg")
      ("mpkg" "application/vnd.apple.installer+xml")
      ("odp" "application/vnd.oasis.opendocument.presentation")
      ("ods" "application/vnd.oasis.opendocument.spreadsheet")
      ("odt" "application/vnd.oasis.opendocument.text")
      ("oga" "audio/ogg")
      ("ogv" "video/ogg")
      ("ogx" "application/ogg")
      ("opus" "audio/opus")
      ("otf" "font/otf")
      ("png" "image/png")
      ("pdf" "application/pdf")
      ("php" "application/x-httpd-php")
      ("ppt" "application/vnd.ms-powerpoint")
      ("pptx" "application/vnd.openxmlformats-officedocument.presentationml.presentation")
      ("rar" "application/vnd.rar")
      ("rtf" "application/rtf")
      ("sh" "application/x-sh")
      ("svg" "image/svg+xml")
      ("swf" "application/x-shockwave-flash")
      ("tar" "application/x-tar")
      ("tif" "image/tiff")
      ("tiff" "image/tiff")
      ("ts" "video/mp2t")
      ("ttf" "font/ttf")
      ("txt" "text/plain")
      ("vsd" "application/vnd.visio")
      ("wav" "audio/wav")
      ("weba" "audio/webm")
      ("webm" "video/webm")
      ("webp" "image/webp")
      ("woff" "font/woff")
      ("woff2" "font/woff2")
      ("xhtml" "application/xhtml+xml")
      ("xls" "application/vnd.ms-excel")
      ("xlsx" "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      ("xml" "application/xml")
      ("xul" "application/vnd.mozilla.xul+xml")
      ("zip" "application/zip")
      ("3gp" "video/3gpp")
      ("3g2" "video/3gpp2")
      ("7z" "application/x-7z-compressed")))

;; Stream.
(class HTTPStream (SocketStream))

(method HTTPStream .write-line (:opt args)
  (if args (.write-bytes self args))
  (.write-bytes self "\r\n"))

(method HTTPStream .write-start-line (message)
  (if (is-a? message HTTPRequest) (.write-line self (.request-line message))
      (is-a? message HTTPResponse) (.write-line self (.status-line message))
      (raise ArgumentError "`%v` is not a HTTPMessage" message)))

(method HTTPStream .write-headers (message)
  (dolist (header (.headers message))
    (.write-line self (format "%s: %s" (car header) (str (cadr header)))))
  (.write-line self))

(method HTTPStream .write-body (message)
  (if (.body message) (.write-bytes self (.body message))))

(method HTTPStream .write (message)
  (.write-start-line self message)
  (.write-headers self message)
  (.write-body self message))

;; HTTPMessage
(class HTTPMessage ()
  version headers body)

(method HTTPMessage .version ()
  self->version)

(method HTTPMessage .version! (val)
  (<- self->version val))

(method HTTPMessage .headers ()
  self->headers)

(method HTTPMessage .header (name)
  (keep1 (f (x) (if (= (lower name) (lower (car x))) (cadr x)))
         (.headers self)))

(method HTTPMessage .parse ()
  (raise NotImplementedError))

(method HTTPMessage .parse-header (line)
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

(method HTTPMessage .set-header (name value)
  (<- self->headers (concat self->headers (list (list name value))))
  self)

(method HTTPMessage .set-content-type-from-file (file)
  (.set-header self "Content-Type"
               (|| (cadr (assoc (.suffix file) $http.mime-types))
                   (if (< (.size file) 10000) "text/plain"
                       "application/octet-stream"))))

(method HTTPMessage .body ()
  self->body)

(method HTTPMessage .set-body (val)
  (<- self->body val)
  self)

;; Request.
(class HTTPRequest (HTTPMessage)
  method uri)

(method HTTPRequest .method ()
  self->method)

(method HTTPRequest .uri ()
  self->uri)

(method HTTPRequest .request-line ()
  (format "%s %s %s" self->method self->uri self->version))

(method HTTPRequest .parse (stream)
  (let (vals (split (.read-line stream) " "))
    (if (!= (len vals) 3) (throw 400)
        (<- self->method (car vals)
            self->uri (cadr vals)
            self->version (caddr vals)
            self->headers (collect (f () (.parse-header self (.read-line stream))))))
    self))

;; Response.
(class HTTPResponse (HTTPMessage)
  status-code reason)

(method HTTPResponse .status-code ()
  self->status-code)

(method HTTPResponse .status-code! (val)
  (<- self->status-code val)
  self)

(method HTTPResponse .reason ()
  (|| self->reason (cadr (assoc self->status-code $http.status-codes))))

(method HTTPResponse .reason! (message)
  (<- self->reason message)
  self)

(method HTTPResponse .status-line ()
  (format "%s %d %s" (.version self) (.status-code self) (.reason self)))

(method HTTPResponse .set-date-header (date)
  (.set-header self "Date" (format "%s, %02d %d %d %02d:%02d:%02d GMT"
                                   (.to-s.day-week date) (.day date) (.month date) (.year date)
                                   (.hour date) (.minute date) (.second date))))

;; API.

(function http.encode-url (url)
  ; Encode the given string according to the percent-encode described in RFC3986.
  ; The caller must determine whether it should be encoded or not.
  (with-memory-stream ($out)
    (dostring (ch url)
      (if (ascii? ch) (write-bytes ch)
          (doarray (byte (bytes ch))
            (write-bytes (format "%%%02X" byte)))))))

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
  (assert (= (http.encode-url "012あ") "012%E3%81%82"))
  (assert (= (http.decode-url "012%E3%81%82") "012あ"))
  (assert (= (http.decode-url "%30%31%32%E3%81%82") "012あ")))
