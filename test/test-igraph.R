library(igraph)

g <- make_graph("Zachary")
plot(g)


# ..............................................................................

g <- make_empty_graph()
g <- make_graph(edges = c(1, 2, 1, 5), n = 10, directed = FALSE)
plot(g)
g <- make_graph(~ 1--2, 1--5, 3, 4, 5, 6, 7, 8, 9, 10)
plot(g)


# ..............................................................................

direkt <- data |> 
  filter(KekuleNr > 1 & KekuleNr <= 50) |> 
  select(KekuleNr, VORNAME, NACHNAME, Kind)

direkt <- direkt |> 
  mutate(edges = str_c(KekuleNr, "--", Kind))

str <- str_c(direkt$edges, collapse = ", ")
str <- str_c("~ ", str)
str
g <- make_graph(~ 2--1, 3--1, 4--2, 5--2, 6--3, 7--3, 8--4, 9--4, 10--5, 11--5, 12--6, 13--6, 14--7, 15--7, 16--8, 17--8, 18--9, 19--9, 20--10, 21--10, 22--11, 23--11, 24--12, 25--12, 26--13, 27--13, 28--14, 29--14, 30--15, 31--15, 32--16, 33--16, 34--17, 35--17, 36--18, 37--18, 38--19, 39--19, 40--20, 41--20, 42--21, 43--21, 44--22, 45--22, 47--23, 48--24, 49--24, 50--25)
plot(g)

### besser:
direkt <- direkt |> 
  mutate(edges2 = str_c(KekuleNr, ", ", Kind))
str <- str_c(direkt$edges2, collapse = ", ")
str

d <- make_graph(edges = c(2, 1, 3, 1, 4, 2, 5, 2, 6, 3, 7, 3, 8, 4, 9, 4, 10, 5, 11, 5, 12, 6, 13, 6, 14, 7, 15, 7, 16, 8, 17, 8, 18, 9, 19, 9, 20, 10, 21, 10, 22, 11, 23, 11, 24, 12, 25, 12, 26, 13, 27, 13, 28, 14, 29, 14, 30, 15, 31, 15, 32, 16, 33, 16, 34, 17, 35, 17, 36, 18, 37, 18, 38, 19, 39, 19, 40, 20, 41, 20, 42, 21, 43, 21, 44, 22, 45, 22, 47, 23, 48, 24, 49, 24, 50, 25), directed = FALSE)
plot(d)

# ..............................................................................
library(networkD3)
dat <- data_frame(from = as.character(direkt$KekuleNr), to = as.character(direkt$Kind))
p <- simpleNetwork(dat, height="100px", width="100px")
p

# ..............................................................................

# create data:
direkt <- data |> 
  filter(KekuleNr > 1 & KekuleNr <= 50) |> 
  select(KekuleNr, VORNAME, NACHNAME, Kind)

links=data.frame(
  source=as.character(direkt$KekuleNr),
  target=as.character(direkt$Kind))

# Turn it into igraph object
network <- graph_from_data_frame(d=links, directed=F) 
plot(network)

# # Count the number of degree for each node:
# deg <- degree(network, mode="all")
# 
# # Plot
# plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )
