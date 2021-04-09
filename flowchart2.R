

# EMF file can imported natively as vector graphics in both Libre/MS Office
library(devEMF)  # install.packages("devEMF") 
# emf(file = "Rplot.emf", width = 7, height = 7,  # hights / width of plot in inches.
# bg = "transparent", fg = "black", pointsize = 12,  # font point size 
# family = "Helvetica", coordDPI = 300, custom.lty=emfPlus, # EMF is vector but use discrete coord systems
# emfPlus=TRUE, emfPlusFont = FALSE, emfPlusRaster = FALSE)
tmp_file <- "EMF_plot.emf"
emf(file= tmp_file, pointsize= 10, width= 5, height= 3) # Opens a device
temp_margins <- c(1.5, 2.3, 0.5, 0.2)                   # Adjust margins
par(mfrow = c(1,1), mar = temp_margins, mgp=c(0.6, 0.3, 0),  tcl = -0.15, las = 1)
plot(rnorm(10), ylab="", xlab="") # create your plot
grid(col= "grey")
dev.off()  # Close the device





grViz("
digraph nicegraph {
  graph [compound = true, nodesep = .5, ranksep = .25, color = crimson]
  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 1, color = darkslategray]
  edge [color = grey, arrowhead = none, arrowtail = none]

  subgraph cluster0 {
    node [fixedsize = true, width = 3]
    '@@1-1' -> '@@1-2' -> '@@1-3' -> '@@1-4'
    '@@1-4' -> '@@1-5' -> '@@1-6' -> '@@1-7'
  }

subgraph cluster1 {  node [fixedsize = true, width = 3] '@@2' -> '@@3'         }

  Information             [width = 1.5]
  Information -> R
  Information -> RStudio
  R -> '@@1-1'            [lhead = cluster0]
  RStudio -> '@@2'        [lhead = cluster1]

}

[1]: paste0(names(R.Version())[1:7], ':\\n ', R.Version()[1:7])
[2]: paste0('RStudio version:\\n ', rstudioapi::versionInfo()[[1]])
[3]: paste0('Current program mode:\\n ', rstudioapi::versionInfo()[[2]])

")


# wget --mirror --convert-links --adjust-extension --page-requisites  --no-parent http://example.org
# same as wget -mkEpnp http://example.org


# flow charts
#  pass into grViz() is a valid graph specification in the DOT language direct of .gv file 
# cannot contain any unescaped double-quote characters, no but single quote ok
# directive stating whether a directed graph (digraph) or an undirected graph (graph) is desired
# with optional graph ID.  Strict for no multi edge.
# basic structure:   [strict] (graph | digraph) [ID] '{' stmt_list '}'
# the three most common statements are : statement (graph_stmt), the node statement (node_stmt), and the edge statement (edge_stmt) 
# Graph statements allow for attributes to be set for all components
# Node statements define and provide attributes for graph nodes
# Edge statements specify the edge operations between nodes and supply edges attributes. directed graph must specify by  -> and undirected by -- .
# these statements follow statement lists. For node statement, a list of nodes, for edge statement, a list of edge operations.
# Comments within statement list by // or a /* */ structure and Comment lines by a # character.

DiagrammeR::grViz("
digraph nicegraph {
          graph [compound = true, nodesep = .5, ranksep = .25,   color = crimson]
          node [fontname = Helvetica, fontcolor = darkslategray,
          shape = rectangle, fixedsize = true, width = 1, color = darkslategray]

          edge [color = grey, arrowhead = none, arrowtail = none]

        subgraph cluster0 {
            node [fixedsize = true, width = 3]
            '@@1-1' -> '@@1-2' -> '@@1-3' -> '@@1-4'
            '@@1-4' -> '@@1-5' -> '@@1-6' -> '@@1-7'
            }

  # subgraph for RStudio information
  subgraph cluster1 {
    node [fixedsize = true, width = 3]
    '@@2' -> '@@3'
  }

  Information             [width = 1.5]
  Information -> R
  Information -> RStudio
  R -> '@@1-1'[lhead = cluster0]
  RStudio -> '@@2'[lhead = cluster1]
}

[1]: paste0(names(R.Version())[1:7], ':\\n ', R.Version()[1:7])
[2]: paste0('RStudio version:\\n ', rstudioapi::versionInfo()[[1]])
[3]: paste0('Current program mode:\\n ', rstudioapi::versionInfo()[[2]])

")


DiagrammeR::grViz("

digraph boxes_and_circles {

graph [overlap = true, fontsize = 10]       # a 'graph' statement

node [shape = box, fontname = Helvetica]    # several 'node' statements
  A; B; C; D; E; F

node [shape = circle, fixedsize = true, width = 0.9] // sets as circles
  1; 2; 3; 4; 5; 6; 7; 8

# several 'edge' statements
  A->1 
  B->2 
  B->3 
  B->4 
  C->A
  1->D E->A 2->4 1->5 1->F
  E->6 4->6 5->7 6->7 3->8
}
")

### Subgraphs and Clusters

# a subgraph can represent graph structure, indicating that certain nodes and edges should be grouped together. 
# This is the usual role for subgraphs and typically specifies semantic information about the graph components. 
# It can also provide a convenient shorthand for edges. 
# When this occurs, an edge is created from every node on the left to every node on the right. 
# eg    A -> {B C}     is equivalent to     A -> B  and then  A -> C

# a subgraph can provide a context for setting attributes. 
# eg it can specify that blue is the default color for all nodes defined in it. eg 
# subgraph { rank = same; A; B; C; }   # This anonymous subgraph specifies that the nodes A, B and C should all be placed on the same rank.

# The third role for subgraphs directly involves how the graph will be laid out by certain layout types. 
# If the name of the subgraph begins with cluster, Graphviz notes the subgraph as a special cluster subgraph. 
# If supported, the layout tires to cluster  drawn together  contained within a bounding rectangle.

# Node attributes  : key value pairs  eg [fillcolor = red]
# shape: [oval, diamond, egg, plaintext, point, square, triangle ]

# Edge Attributes: Similar way with  abc -> def, one would use  abc -> def [arrowhead = diamond]
# color X11 name space
# Value for arrowhead: [normal, box, crow, curve, diamond, dot, inv, none, tee, vee ]
# Graphviz Substitution using @@variable  but then must ensure a valid expression defined in footnote
# and their evaluations must result in an R vector object (i.e., not a data frame, list, or matrix)
# The substitution construction is: "@@" + [footnote number]
# layout equal to either neato, twopi, or circo in a Graphviz graph statement. graph [layout = dot|neato|twopi|circo]

DiagrammeR::grViz("
digraph neato {

#graph [layout = neato]  # neato, circo, twopi
graph [layout = circo ] 

node [shape = circle, style = filled, color = grey, label = '']  # only initial default

node [ shape = circle, fillcolor = red]
a

node [shape = box, fillcolor = green]
b c d

node [shape = circle, fillcolor = orange]

edge [color = red]
a -> {b c d}
b -> {e f g h i j}
c -> {k l m n o p}
d -> {q r s t u v}
}")



####  
# mermaid library is part of DiagrammeR. 
# With it, you can describe graphs and sequence diagrams.
# file .mmd file

# LR left to right; RL right to left;  TB top to bottom ;  BT bottom to top ; TD top down (= TB)
# --> arrow connection  ;  --- line connection

# 

DiagrammeR::mermaid("graph TD; A(Start this)-->B; A-->D; A-->C; C-->E; B-->D; C-->D; D-->F; E-->F")
# or
DiagrammeR::mermaid("
graph TB
  A-->B
  A-->C(testing  this lable)
  C-->E{A Rhombus}
  B-->D
  C-->D
  D-->F[Rectangle One]
  E-->F
")

# or DiagrammeR::mermaid("graph.mmd")

DiagrammeR::mermaid("
graph TD
A(Rounded)-->B[Rectangular]
B-->C{A Rhombus}
B-->F(branch)
C-->D[Rectangle One]
C-->E[Rectangle Two]
")


#######

DiagrammeR::mermaid("
graph LR
A(Total RDSO items = 661 )-->B[Less then 3 Vendors = 221]
B-->C{ 1 Crores = 145 }
B-->D( 1 Crores = 76)
C-->E[Diminishing , No vendor interest = 118]
C-->F(Increasing, Develop Vendor  = 27)
D-->G[Obsolete, diminishing  = 35]
D-->H[New tech, increasing  = 41]
C-->G[Total for vendor development = 68]
")

###############




# marmaid sequence diagram

DiagrammeR::mermaid("
sequenceDiagram
  customer->>ticket seller: ask ticket
  ticket seller->>database: seats
  alt tickets available
    database->>ticket seller: ok
    ticket seller->>customer: confirm
    customer->>ticket seller: ok
    ticket seller->>database: book a seat
    ticket seller->>printer: print ticket
    feedback taker-->>formfilling:get feedback
    
  else sold out
    database->>ticket seller: none left
    ticket seller->>customer: sorry
  end
")



DiagrammeR::mermaid("
sequenceDiagram
  Vendor->>Web portal: ask appointment ticket
  Software ->>officer: check up schedule
  alt tickets available
    database->>ticket seller: ok
    ticket seller->>customer: confirm
    customer->>ticket seller: ok
    ticket seller->>database: book a seat
    ticket seller->>printer: print ticket
    feedback taker-->>formfilling:get feedback
  else sold out
    database->>ticket seller: none left
    ticket seller->>customer: sorry
  end
")

###############################
# rdso fresh vendor registration
# marmaid sequence diagram

DiagrammeR::mermaid("
sequenceDiagram
  customer->>ticket seller: ask ticket
  ticket seller->>database: seats
  alt tickets available
    database->>ticket seller: ok
    ticket seller->>customer: confirm
    customer->>ticket seller: ok
    ticket seller->>database: book a seat
    ticket seller->>printer: print ticket
    feedback taker-->>formfilling:get feedback
  else sold out
    database->>ticket seller: none left
    ticket seller->>customer: sorry
  end
")




#################################
DiagrammeR::grViz(

#  "digraph graph2 {  ???
  "digraph  {
    graph [layout = dot, rankdir = LR]   # LR, RL, TD etc
    node [shape = rectangle, style = filled, fillcolor = Linen] # define the global default node styles
   
    data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
    data2 [label = '@@1', shape = folder, fillcolor = Beige]

    process [label =  'Process \n Data']
    statistical [label = 'Statistical \n Analysis']
    results [label= 'Results']

    {data1 data2}  -> process -> statistical -> results   # edge definitions with the node IDs
       }
[1]: paste0('Identify Potential Customers (n = ', 77, ')')   #footnote  or variable = db$item
                    " )


# DOT language using the GraphViz and `mermaid styles
# .dot, .mmd or .gv files
# directed graphs (diagraph)
#   graph: this sets the overall layout, themes of the plot
#   node: the boxes in the flowchart
#   edges: the lines used to connect each of the boxes

# # A minimal plot
DiagrammeR::grViz("digraph {
                    graph[layout = dot, rankdir = LR]
                    a
                    b
                    c
                    a -> b -> c
                            }"    )

# define as many styles as we wish within square brackets following the object using name-value pairs.


DiagrammeR::grViz("digraph {
    graph [layout = dot, rankdir = LR]   # LR, RL, TD etc
    node [shape = rectangle, style = filled, fillcolor = Linen] # define the global default node styles

    data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
    data2 [label = 'Dataset @@3', shape = folder, fillcolor = Beige]

    process [label =  'Process \n Data']
    statistical [label = 'Statistical \n Analysis']
    results [label= 'Results']

    {data1 data2}  -> process -> statistical -> results   # edge definitions with the node IDs
    
    [3]: paste0('Identify Potential Customers (n = ', , ')')   #footnote
                            }" )

# reading R values directly into our flowcharts.  
# use the @@X symbol then footnote [X]:, where X is the a unique numeric index.


###############
###
# Perform traversals with conditions
# based on node `label` regex matches
###

library(DiagrammeR)
library(magrittr)

# Create a graph with fruit, vegetables,
# and nuts

nodes <- DiagrammeR::create_node_df(
     nodes = 1:9,  # is it required
     n = 9, 
    type = c("fruit", "fruit", "fruit","veg", "veg", "veg", "nut", "nut", "nut"),
    label = c("pineapple", "apple","apricot", "cucumber","celery", "endive","hazelnut", "almond", "chestnut"),
    # style = "filled",
    # color = "aqua",
    # shape = c("circle", "circle", "rectangle", "rectangle"),
     value = c(1:9) #c(3.5, 2.6, 9.4, 2.7)  
        )

edges <-  create_edge_df( n = 9,
                         from = c(9, 3, 6, 2, 6, 2, 8, 2, 5, 5),
                         to   = c(1, 1, 4, 3, 7, 8, 1, 5, 3, 6) 
                       )

g <- create_graph(  nodes_df = nodes, edges_df = edges)
render_graph(g)   # View the graph



mermaid("
graph LR
  A[node text]
")

mermaid("
graph LR
  A((node text))
")




grViz("
digraph nicegraph {

  # graph, node, and edge definitions
  graph [compound = true, nodesep = .5, ranksep = .25,   color = crimson]

  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 1,
        color = darkslategray]

  edge [color = grey, arrowhead = none, arrowtail = none]

  # subgraph for R information
  subgraph cluster0 {
    node [fixedsize = true, width = 3]
    '@@1-1' -> '@@1-2' -> '@@1-3' -> '@@1-4'
    '@@1-4' -> '@@1-5' -> '@@1-6' -> '@@1-7'
  }

  # subgraph for RStudio information
  subgraph cluster1 {
    node [fixedsize = true, width = 3]
    '@@2' -> '@@3'
  }

  Information             [width = 1.5]
  Information -> R
  Information -> RStudio
  R -> '@@1-1'            [lhead = cluster0]
  RStudio -> '@@2'        [lhead = cluster1]

}

[1]: paste0(names(R.Version())[1:7], ':\\n ', R.Version()[1:7])
[2]: paste0('RStudio version:\\n ', rstudioapi::versionInfo()[[1]])
[3]: paste0('Current program mode:\\n ', rstudioapi::versionInfo()[[2]])

")

