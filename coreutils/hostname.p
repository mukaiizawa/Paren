; show the system's host name.

(import :sock)

(function hostname ()
  ; hostname
  ; Displays the current host.
  (write-line (gethostname)))

(function! main (args)
  (hostname))
