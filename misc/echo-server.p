; echo server.

(import :sock)

(function! main (args)
  (with-server-socket (sock 8080)
    (while true
      (let (client-sock (accept sock) buf (bytes 4096) size nil)
        (while (!= (<- size (recvall buf client-sock)) 0)
          (write-bytes buf 0 size)
          (send buf 0 size client-sock))))))
