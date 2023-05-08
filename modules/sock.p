; socket module.

(<- $sock.SHUT_RD 0
    $sock.SHUT_WR 1
    $sock.SHUT_RDWR 2)

(if (! (bound? 'gethostname)) (raise StateError "Requires sock option at compile time"))

(built-in-function gethostname ()
  ; Returns the local host name.
  )

(built-in-function client-socket (host port)
  ; Create a new socket and connect it to terminal corresponding to host and port.
  ; host must be a string that getaddrinfo(3) can resolve.
  ; Returns the file descriptor corresponding to the created socket.
  )

(built-in-function server-socket (port)
  ; Create a new socket and bind it to port.
  ; Returns the file descriptor corresponding to the created socket.
  )

(built-in-function accept (sock)
  ; Extracts the first connection request on the queue of pending connections for the listening socket sock and creates a new connected socket.
  ; Only one can be connected at the same time.
  ; Returns a socket.
  )

(built-in-function recv (buf from size sock)
  ; Reads size bytes of data from socket stream, storing them at the location given by bytes buf offset from.
  ; Returns size;
  )

(built-in-function send (buf from size sock)
  ; Writes size bytes of data to the the socket stream, obtaining them at the location given by bytes buf offset from.
  ; Returns size;
  )

(function recvall (buf sock)
  ; Same as (recv buf 0 (len buf) sock)).
  (recv buf 0 (len buf) sock))

(function sendall (buf sock)
  ; Same as (send buf 0 (len buf) sock)).
  (send buf 0 (len buf) sock))

(built-in-function shutdown (sock how)
  ; Shut down one or both halves of the connection.
  ; Returns nil.
  )

(built-in-function close (sock)
  ; Closes the socket.
  ; Returns nil.
  )

(macro with-server-socket ((sock port) :rest body)
  ; Create a server socket context with the specified port number.
  ; This socket is guaranteed to be closed when exiting the context.
  ; Returns nil.
  (with-gensyms (gsock)
    `(let (,gsock nil)
       (unwind-protect
         (let (,sock (<- ,gsock (server-socket ,port)))
           ,@body)
         (if ,gsock (close ,gsock))))))

(macro with-client-socket ((sock host port) :rest body)
  ; Create a client socket context with the specified host name and port number.
  ; This socket is guaranteed to be closed when exiting the context.
  ; Returns nil.
  (with-gensyms (gsock)
    `(let (,gsock nil)
       (unwind-protect
         (let (,sock (<- ,gsock (client-socket ,host ,port)))
           ,@body)
         (if ,gsock (close ,gsock))))))

(class SocketStream (Object Stream)
  sock pos size buf)

(method SocketStream .init (sock :key buf-size)
  (<- self->sock sock
      self->pos 0
      self->size 0
      self->buf (bytes (|| buf-size 1024)))
  self)

(method SocketStream .fetch-bytes ()
  ;; Returns remain bytes.
  (let (remain (- self->size self->pos))
    (if (> remain 0) remain
        (<- self->pos 0
            self->size (recvall self->buf self->sock)))))

(method SocketStream .peek-byte ()
  (if (= (.fetch-bytes self) 0) -1
      ([] self->buf self->pos)))

(method SocketStream .read-byte ()
  (let (next (.peek-byte self))
    (if (!= next -1) (<- self->pos (++ self->pos)))
    next))

(method SocketStream .read-bytes (:opt buf from size)
  (if (nil? buf) (with-memory-stream ($out)
                   (while (> (<- size (.fetch-bytes self)) 0)
                     (write-bytes self->buf self->pos size)))
      (fread buf from size self->fp)))

(method SocketStream .write-byte (byte)
  (let (buf (bytes 1))
    ([] buf 0 byte)
    (.write-bytes self buf)))

(method SocketStream .write-bytes (x :opt from size)
  (send x (|| from 0) (|| size (byte-len x)) self->sock)
  (if from size x))

(method SocketStream .close ()
  (close self->sock))

(function! main (args)
  true)
