; echo client

(import :sock)

(function! main (args)
  (let (msg "hello world" buf (bytes 4096))
    (with-client-socket (sock "localhost" 8080)
      (sendall msg sock)
      (<- size (recvall buf sock))
      (write-bytes buf 0 size))))
