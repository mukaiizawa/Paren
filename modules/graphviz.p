; graphviz module.

(function graphviz.parse-option (option)
  ;; ((T png) (o "foo.png")) -> "-Tpng -ofoo.png"
  (join (map (f (x) (apply str (cons "-" x)))
             option)
        " "))

(function graphviz.dot (edges :opt option)
  (with-process ($out (str "dot " (graphviz.parse-option option)) :write)
    (write-line "digraph {")
    (dolist (edge edges)
      (write-line (join edge " -> ")))
    (write-line "}")))
