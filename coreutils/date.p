; print the system date and time.

(import :datetime)

(function date ()
  ; date
  ; Display the current time.
  (write-line (.to-s (datetime.now))))

(function! main (args)
  (date))
