; curl.

(import :sock)

(<- $usage
"
Usage: paren curl.p URL
	Get http request in URL.
")

(function default-port (proto)
  (if (= proto "http") 80
      (error "unsupported protocol. " proto)))

(function http-request (host port method uri version)
  ;; RFC 2616
  ;; Request-Line = Method SP Request-URI SP HTTP-Version CRLF
  (let (buf (bytes 4096) size nil)
    (with-client-socket (sock host port)
      (sendall (string method " " uri " " version "\r\n") sock)
      (sendall "\r\n" sock)
      (while (> (<- size (recv buf 0 (memlen buf) sock)) 0)
        (write-mem buf 0 size)))))

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (if (nil? args) (error "too few arguments.")
        (with-memory-stream ($in (car args))
          (let (ar (.new AheadReader) proto nil host nil port nil)
            (while (!= (.next ar) ":") (.get ar))
            (<- proto (.token ar))
            (.skip ar) (.skip ar "/") (.skip ar "/")    ; skip ://
            (while (&& (.next ar) (!= (.next ar) "/") (!= (.next ar) ":")) (.get ar))
            (<- host (.token ar))
            (when (= (.next ar) ":")
              (.skip ar)
              (<- port (.skip-uint ar)))
            (if (= (.next ar) "/") (.get ar)
                (.put ar "/"))
            (while (.next ar) (.get ar))
            (http-request host (|| port (default-port proto)) :GET (.token ar) "HTTP/1.0"))))))
