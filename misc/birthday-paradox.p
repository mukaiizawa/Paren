; birthday paradox.

(import :rand)

(function dates->str (dates)
  (map (f (x) (join (map str x) "/"))
       dates))

(function rand.date ()
  (let (month (++ (rand.int 12)))
    (list month (++ (rand.int (if (= month 2) 28
                                  (in? month '(2 4 6 9 11)) 30
                                  31))))))

(function rand.dates (n)
  (let (acc nil)
    (dotimes (i n)
      (push! (rand.date) acc))
    (sort! acc :key (f (x) (| (<< (car x) 6) (cadr x))))))

(function! main (args)
  (let (n (int (|| (car args) 30))
          birthdays (rand.dates n)
          shared-birthdays (uniq (select (f (x) (> (count (partial = x) birthdays) 1))
                                         birthdays)))
    (write (list :number-of-people n
                 :birthdays (dates->str birthdays)
                 :shared-birthdays (dates->str shared-birthdays)))))
