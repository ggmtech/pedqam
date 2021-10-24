pacman::p_load(
  DiagrammeR,     # for flow diagrams
  networkD3,      # For alluvial/Sankey diagrams
  tidyverse)      # data management and visualization

# import the linelist
file.exists("linelist_cleaned.rds")  # list.files()

linelist <- import("linelist_cleaned.rds")   # pre downloaded in pedqam
linelist <- readRDS("linelist_cleaned.rds")

??import()
# can do
# filename <- file.choose()
# Canteen_clean <- readRDS(filename)
# 
# githubURL <- ("https://raw.githubusercontent.com/derek/master/best2.My.Lu2.rds")
# download.file(githubURL,"best2.My.Lu2.rds", method="curl")
# BestMyyu <- readRDS("best2.My.Lu2.rds")
# 

# Flow Diagram Basic structure

# Open  grViz("  .....  Close  }")
# Specify directionality and name of the graph, and open brackets, e.g. digraph my_flow_chart {
# Graph statement (layout, rank direction)
# Nodes statements (create nodes)
# Edges statements (gives links between nodes)

#####################
#####################
# from https://epirhandbook.com/diagrams-and-charts.html
dev.off()
grViz("                           # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 

graph [  layout = dot,            # dot , neato, twopi, circo
         rankdir = TB,            # dot (set rankdir to either TB, LR, RL, BT, 
         overlap = true,
         fontsize = 10 ]          # graph statement

# Nodes - editable attributes
node [shape = circle, #shape= circle, ellipse, oval, diamond, egg, plaintext, point, square, triangle
       # label = 'mynadenameforall',  # (text, in single quotes if multi-word)
       fixedsize = true,
       # alpha = 0.5,
       penwidth = 02.8, #(width of shape border)
       color = red,
       x = 1, y = 2,   # (displacement left/right, up/down)
       fillcolor = green,# ?? 
       height = 2, #?? # height # width # 
       width = 1.3,  # width of circles
       distortion = 2 
       fontcolor = green,  fontsize = 22, fontname = 'helvetica',
       # icon # style, # sides # peripheries # fixedsize (h x w)
       ]               

Primary                           # names of nodes
Secondary
Tertiary



# Edges - editable attributes
# arrowsize # arrowtail
# arrowhead (normal, box, crow, curve, diamond, dot, inv, none, tee, vee)
# dir (direction, ), # style (dashed, …)
# color # alpha 
# headport (text in front of arrowhead), # tailport (text in behind arrowtail)
# fontname # fontsize, # fontcolor
# penwidth (width of arrow) # minlen (minimum length)

Primary   -> Secondary [label = ' case transfer', 
                        fontname = helvetica, fontsize = 16,  fontcolor  = magenta,
                        color = blue, alpha = 0.5,
                        penwidth = 4 , arrowsize = 2 ,
                                                   ]    # edges
Secondary -> Tertiary  [label = ' case transfer'
                        fontname = helvetica, fontsize = 16,  fontcolor  = magenta,
                        color = blue, alpha = 0.5,
                        penwidth = 4 , arrowsize = 2 ,
                            ]
Primary   -> Tertiary  [label = ' forward transfer'] 
Tertiary  -> Primary   [label = ' back transfer'] 
}

")


#############################
DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
data2 [label = 'Dataset 2', shape = folder, fillcolor = Beige]
process [label =  'Process \n Data']
statistical [label = 'Statistical \n Analysis']
results [label= 'Results']

# edge definitions with the node IDs
{data1 data2}  -> process -> statistical -> results
}")










###########################################
###########################################
###########################################
#  Alluvial/Sankey Diagrams
pacman::p_load(  networkD3,   tidyverse)

# counts by hospital and age category
links <- linelist %>% 
  drop_na(age_cat) %>% 
  select(hospital, age_cat) %>%
  count(hospital, age_cat) %>% 
  rename(source = hospital,
         target = age_cat)

# The unique node names
nodes <- data.frame(
           name=c(as.character(links$source), as.character(links$target)) %>% 
           unique()
          )

nodes  # print
# match to numbers, not names
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


###### # plot
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "n",
  NodeID = "name",
  units = "TWh",
  fontSize = 12,
  nodeWidth = 30,
  iterations = 0)        # ensure node order is as in data
p

# counts by hospital and age category
age_hosp_links <- linelist %>% 
  drop_na(age_cat) %>% 
  select(hospital, age_cat) %>%
  count(hospital, age_cat) %>% 
  rename(source = age_cat,          # re-name
         target = hospital)

hosp_out_links <- linelist %>% 
  drop_na(age_cat) %>% 
  select(hospital, outcome) %>% 
  count(hospital, outcome) %>% 
  rename(source = hospital,       # re-name
         target = outcome)

# combine links
links <- bind_rows(age_hosp_links, hosp_out_links)

# The unique node names
nodes <- data.frame(   name=c(as.character(links$source), as.character(links$target)) %>% 
                       unique()
                    )

# Create id numbers
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# plot
######
p <- sankeyNetwork(Links = links,
                   Nodes = nodes,
                   Source = "IDsource",
                   Target = "IDtarget",
                   Value = "n",
                   NodeID = "name",
                   units = "TWh",
                   fontSize = 12,
                   nodeWidth = 30,
                   iterations = 0)
p

######################################
######################################
# Event timelines

# load package
pacman::p_load(vistime,  # make the timeline
               plotly    # for interactive visualization
               )
data
p <- vistime(data)    # apply vistime

library(plotly)

# step 1: transform into a list
pp <- plotly_build(p)

# step 2: Marker size
for(i in 1:length(pp$x$data)){
  if(pp$x$data[[i]]$mode == "markers") pp$x$data[[i]]$marker$size <- 10
}

# step 3: text size
for(i in 1:length(pp$x$data)){
  if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textfont$size <- 10
}


# step 4: text position
for(i in 1:length(pp$x$data)){
  if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textposition <- "right"
}




