if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("leaflet")) install.packages("leaflet"); library(leaflet)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("janitor")) install.packages("janitor"); library(janitor)
if (!require("scales")) install.packages("scales"); library(scales)
if (!require("knitr")) install.packages("knitr"); library(knitr)
if (!require("kableExtra")) install.packages("kableExtra"); library(kableExtra)
if (!require("ggiraph")) install.packages("ggiraph"); library(ggiraph)
if (!require("igraph")) install.packages("igraph"); library(igraph)
if (!require("treemap")) install.packages("treemap"); library(treemap)
if (!require("ggraph")) install.packages("ggraph"); library(ggraph)

source("./R/0_functions.R")
source("./R/1_DatenEinlesen.R")
source("./R/2_DatenSelektieren.R")
source("./R/3_DatenAufbereiten.R")
source("./R/4_FamilienDaten.R")


# ..............................................................................
dataDIREKT <- dataDIREKT |> 
  mutate(Stamm = KekuleNr %/% (2^(GenerationNr-1)), .after = KekuleNr) |> 
  mutate(Stamm = if_else(Stamm == 2, "Wolfgang", "Renate"))

table(dataDIREKT$Stamm, useNA = "ifany")

# besser:
dataDIREKT <- data |> filter(Vorfahren == "Direkt")

# Wolfgang oder Renate
dataDIREKT <- dataDIREKT |> 
  mutate(Stamm = KekuleNr %/% (2^(GenerationNr-1)), .after = KekuleNr) |> 
  mutate(Stamm = if_else(GenerationNr == 0, 1, Stamm)) |> 
  mutate(Stamm = if_else(Stamm == 2, "Wolfgang", as.character(Stamm))) |> 
  mutate(Stamm = if_else(Stamm == "3", "Renate", Stamm)) |> 
  mutate(colorStamm = case_when(
    Stamm == "Wolfgang" ~ "blue",
    Stamm == "Renate" ~ "red",
    .default = "grey"
  ))
# ..............................................................................
# data: edge list
edges <- data.frame(
  from=as.character(dataDIREKT$Kind),
  to=as.character(dataDIREKT$KekuleNr))

# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))

vertices <- data.frame(name = as.numeric(name)) |> 
  left_join(dataDIREKT |> 
              select(KekuleNr, VORNAME, NACHNAME, Generation, Stamm, colorStamm), 
            by = join_by(name == KekuleNr))
# nur ein Knoten für J+U
vertices[1, 1] <- 1 
vertices[1, 2] <- "Ulrike + Juliane"  
vertices[1, 3] <- "NIEMANN" 
vertices[1, 5] <- "1" 
vertices[1, 6] <- "grey" 
vertices <- vertices[-c(2:3), ]
edges <- edges[-c(1:2), ]

mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'tree', circular = FALSE) +
  geom_edge_link() +
  geom_node_text(aes(label = name, vjust = 1.5), size = 2) +
  geom_node_point(aes(color = Stamm)) +
  theme_void()

# ..............................................................................
layout <- create_layout(mygraph, layout = 'tree', circular = FALSE)

ggraph(layout, layout = 'tree', circular = FALSE) +
  geom_edge_link() +
  geom_node_text(aes(label = name, vjust = 1.5), size = 2) +
  geom_node_point(aes(color = Stamm)) +
  scale_color_manual(values=vertices$colorStamm)+
  theme_void()



