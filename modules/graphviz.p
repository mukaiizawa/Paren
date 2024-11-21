; graphviz module.

(function graphviz.parse-opt (expr)
  ;; ((T png) (o "foo.png")) -> "-Tpng -ofoo.png"
  (join (map (f (x) (apply str (cons "-" x)))
             expr)
        " "))

(function graphviz.write-option (expr)
  (let ((key val) expr)
    (printf "%s=%v;\n" (string key) val)))

(function graphviz.write-options (expr)
  (when expr
    (write-line " [")
    (foreach graphviz.write-option expr)
    (write-bytes "]")))

(function graphviz.write-node (expr)
  (let ((id :rest options) expr)
    (write-bytes id)
    (graphviz.write-options options)
    (write-line ";")))

(function graphviz.write-edge (expr)
  (if (atom? (car expr)) (graphviz.write-edge (cons (list (car expr) (cadr expr)) (cddr expr)))
      (let ((nodes :rest options) expr)
        (write-bytes (join nodes "->"))
        (graphviz.write-options options)
        (write-line ";"))))

(function graphviz.write-subgraph (expr)
  (let ((:key id nodes edges subgraphs) expr)
    (graphviz.write-graph 'subgraph (list :id id :nodes nodes :edges edges :subgraphs subgraphs))))

(function graphviz.write-graph (kind expr)
  (let ((:key id nodes edges subgraphs) expr)
    (if (! (in? kind '(graph digraph subgraph))) (raise SyntaxError "invalid graph kind")
        (begin
          (write-bytes kind)
          (if id (print " " id))
          (write-line " {")
          (foreach graphviz.write-node nodes)
          (foreach graphviz.write-edge edges)
          (foreach graphviz.write-subgraph subgraphs)
          (write-line "}")))))

(function graphviz.dot (:key id configures nodes edges subgraphs options)
  (let (dot (with-memory-stream ($out)
              (graphviz.write-graph 'digraph (list :id id :nodes nodes :edges edges :subgraphs subgraphs))))
    (write-bytes dot)
    (with-process ($out (str "dot " (graphviz.parse-opt options)) :write)
      (write-bytes dot))))
