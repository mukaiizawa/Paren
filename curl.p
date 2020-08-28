; curl

(function usage ()
  (error "Usage: curl url"))

(function ->port (proto)
  (if (string= proto "http") "80"
      (string= proto "https") "443"
      (error "unexpected protocol " proto)))

(function! main (args)
  (if (nil? (cdr args)) (usage)
      (let (url (cadr args) pos// nil pos/ nil proto nil host nil port nil
                fd nil out nil in nil line nil)
        (unwind-protect
          (begin
            (<- pos// (bytes-index url "//")
                proto (bytes-slice url 0 (-- pos//))
                pos/ (bytes-index url "/" (+ pos// 2))
                host (bytes-slice url (+ pos// 2) pos/)
                fd (client_socket host (->port proto))
                out (.init (.new FileStream) (fdopen fd 1))
                in (.init (.new FileStream) (fdopen fd 0)))
            (write-line (string "GET " (bytes-slice url pos/) " HTTP/1.0\n") out)
            (flush out)
            (while (<- line (read-line in))
              (write-line line)))
          (begin
            (closesocket fd)
            (.close in)
            (.close out))))))
