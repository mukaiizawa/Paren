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

# SEE ALSO
- `graphviz(3)`
