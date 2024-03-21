
# https://r-graph-gallery.com/335-custom-ggraph-dendrogram.html

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
#theme_set(theme_void())

# data: edge list
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges <- rbind(d1, d2)
bspedges <- edges

mygraph <- graph_from_data_frame( edges )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() 

# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() 

# ..............................................................................
library(tidygraph)
graph <- as_tbl_graph(highschool)

# Not specifying the layout - defaults to "auto"
ggraph(graph) + 
  geom_edge_link(aes(colour = factor(year))) + 
  geom_node_point()
# ..............................................................................
graph <- tbl_graph(flare$vertices, flare$edges)
set.seed(1)
ggraph(graph, 'circlepack', weight = size) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = depth)) +
  coord_fixed()
# ..............................................................................
# hier für uns:

# data: edge list
edges=data.frame(
  from=as.character(direkt$Kind),
  to=as.character(direkt$KekuleNr))

# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))

vertices <- data.frame(name = as.numeric(name)) |> 
  left_join(direkt |> 
  select(KekuleNr, VORNAME, NACHNAME), 
  by = join_by(name == KekuleNr))

vertices[1, 2] <- "Ulrike + Juliane"  
vertices[1, 3] <- "NIEMANN" 

mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_link() +
  geom_node_text(aes(label = name, vjust = 1)) +
  geom_node_point()

ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_link() +
  geom_node_text(aes(label = NACHNAME, angle = -90, hjust = -0.5), size = 3)
