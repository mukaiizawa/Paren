; socket module.

(if (! (bound? 'client-socket))
    (raise StateError "Requires sock option at compile time"))

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

(built-in-function closesocket (sock)
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
         (if ,gsock (closesocket ,gsock))))))

(macro with-client-socket ((sock host port) :rest body)
  ; Create a client socket context with the specified host name and port number.
  ; This socket is guaranteed to be closed when exiting the context.
  ; Returns nil.
  (with-gensyms (gsock)
    `(let (,gsock nil)
       (unwind-protect
         (let (,sock (<- ,gsock (client-socket ,host ,port)))
           ,@body)
         (if ,gsock (closesocket ,gsock))))))

(function! main (args)
  true)