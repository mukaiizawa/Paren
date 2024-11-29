# NAME
graphviz - domain-specific language for dot language.

# DESCRIPTION
Abstract grammar for defining Graphviz nodes, edges, graphs, subgraphs, and clusters.

DOT language.

    graph = [strict] (graph | digraph) [ ID ] '{' stmt_list '}'
    stmt_list = [ stmt [ ';' ] stmt_list ]
    stmt = node_stmt
            | edge_stmt
            | attr_stmt
            | ID '=' ID
            | subgraph
    attr_stmt = (graph | node | edge) attr_list
    attr_list = '[' [ a_list ] ']' [ attr_list ]
    a_list = ID '=' ID [ (';' | ',') ] [ a_list ]
    edge_stmt = (node_id | subgraph) edgeRHS [ attr_list ]
    edgeRHS = edgeop (node_id | subgraph) [ edgeRHS ]
    node_stmt = node_id [ attr_list ]
    node_id = ID [ ':' port ]
    port = ID [ ':' compass_pt ] | compass_pt
    subgraph = [ subgraph [ ID ] ] '{' stmt_list '}'
    compass_pt = n | ne | e | se | s | sw | w | nw | c | _
    edgeop = '--' | '->'
    ID -- Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or digits([0-9]), not beginning with a digit
            | a numeral [-]?(.[0-9]⁺ | [0-9]⁺(.[0-9]*)? )
            | any double-quoted string ("...") possibly containing escaped quotes (\")¹
            | an HTML string (<...>)

DNS language for DOT language.

    (graphviz.dot :nodes [node] ...
                  :edges [edge] ...
                  :subgraphs [subgraph] ...
                  :option [option] ...)
    node = (node-id [attr-list])
    edge = (src-node-id dst-node-id [attr-list])
            | ((node-id1 node-id2 ...) [attr-list])
    subgraph = ([:nodes node ...]
                [:edges edge ...]
                [:subgraphs subgraph ...])
    attr-list = (attr-key attr-val) ...
    option = option-name [option-value]
    option-name -- command line option for dot.


# EXAMPLES
A "Hello World" example.
> https://graphviz.org/Gallery/directed/hello.html

    (graphviz.dot :options '((T png) (o hello-world.png))
                  :edges '((hello world)))

This small example illustrates dot's feature to draw nodes and edges in clusters or separate rectangular layout regions.
> https://graphviz.org/Gallery/directed/cluster.html

    (graphviz.dot :options '((T svg) (o "clusters.svg"))
                  :nodes '((graph (fontname "Helvetica,Arial,sans-serif"))
                           (node (fontname "Helvetica,Arial,sans-serif"))
                           (edge (fontname "Helvetica,Arial,sans-serif"))
                           (start (shape Mdiamond))
                           (end (shape Msquare)))
                  :edges '((start a0)
                           (start b0)
                           (a1 b3)
                           (b2 a3)
                           (a3 a0)
                           (a3 end)
                           (b3 end))
                  :subgraphs '((:id cluster_0
                                    :nodes ((graph (label "process #1") (style filled) (color lightgrey))
                                            (node (style filled) (color white)))
                                    :edges (((a0 a1 a2 a3))))
                               (:id cluster_1
                                    :nodes ((graph (label "process #2") (color blue))
                                            (node (style filled)))
                                    :edges (((b0 b1 b2 b3))))))


# SEE ALSO
- `graphviz(3)`
