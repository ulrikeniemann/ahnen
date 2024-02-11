# ..............................................................................
#
# Ulrike Niemann
# Januar 2024
#
# Ahnenforschung Wolfgang Niemann
#
# ..............................................................................

# librarys laden
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("leaflet")) install.packages("leaflet"); library(leaflet)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("janitor")) install.packages("janitor"); library(janitor)
if (!require("scales")) install.packages("scales"); library(scales)
if (!require("knitr")) install.packages("knitr"); library(knitr)
if (!require("kableExtra")) install.packages("kableExtra"); library(kableExtra)

# ..............................................................................

# Daten einlesen
data <- read_xlsx("./data/Ulrike 17.Jan 2024.xlsx", skip = 1)

# bisschen aufbereiten
data <- data |> 
  mutate(Generation = str_split(GENERATION, " ", simplify = TRUE)[, 1] |> unlist(),
         lat1 = `GEO1 BREITE` |> parse_number(),
         lon1 = `GEO1 LÄNGE` |> parse_number(),
         lat2 = `GEO2 BREITE` |> parse_number(),
         lon2 = `GEO2 LÄNGE` |> parse_number(),
         Geburt = (data$`GEB JAHR`/100) |> floor(),
         Tod = (data$`TOD JAHR`/100) |> floor()) |> 
  mutate(Geburt = factor(str_c(Geburt, "00 - ", Geburt, "99")) |> 
           fct_explicit_na("(keine Angabe)")) |> 
  mutate(Tod = factor(str_c(Tod, "00 - ", Tod, "99")) |> 
           fct_explicit_na("(keine Angabe)"))

# ..............................................................................
getTable <- function(var) {
  tab <- data |> 
    group_by({{var}}) |> 
    summarise(Anzahl = n()) |> 
    mutate(Prozent = (Anzahl/sum(Anzahl))) |> 
    adorn_totals("row") |> 
    mutate(Prozent = Prozent |> percent(accuracy = 0.1, decimal.mark = ","))
  tab |> 
    kable(align = c("l", "c", "c")) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F) |> 
    row_spec(dim(tab)[1], bold = T)
} 
# angucken
# Generation
getTable(Generation)

ggplot(data, aes(y = Generation)) + 
  geom_bar(aes(fill = "#009933")) +
  scale_y_discrete(limits=rev, name = "Generation / Ebene") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = ..count..), stat = "count", hjust = -0.5) +
  scale_fill_identity(guide = "none") +
  ggtitle("Anzahl Vorfahren nach Generation") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size=16, face="bold", 
                              margin = margin(0, 0, 30, 0))
    )

# Geburt / Jahrhundert
getTable(Geburt)
ggplot(data, aes(y = Geburt)) + 
  geom_bar(aes(fill = "#009933")) +
  scale_y_discrete(limits=rev, name = "Jahrhundert Geburt") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = ..count..), stat = "count", hjust = -0.5) +
  scale_fill_identity(guide = "none") +
  ggtitle("Anzahl Vorfahren nach Geburtsjahrhundert") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size=16, face="bold", 
                              margin = margin(0, 0, 30, 0))
  )

# Generation + Geburt
ggplot(data, aes(y = Generation)) + 
  geom_bar(aes(fill = Geburt)) +
  scale_y_discrete(limits=rev, name = "Generation / Ebene") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = ..count..), stat = "count", hjust = -0.5) +
  scale_fill_manual(values = 
                      c(brewer.pal(length(levels(data$Geburt)), "Greens")[-1], "#aaaaaa")) +
  ggtitle("Anzahl Vorfahren nach Generation und Geburtsjahrhundert") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size=16, face="bold", 
                             margin = margin(0, 0, 30, 0))
  )

# 
getTable(GEBURTSORT)




# Geo Dublikate ?
duplicated(data[,c("lat1", "lon1")]) |> table()
# leichtes Rauschen
data$lat1x <- jitter(data$lat1, factor = 1)
data$lon1x <- jitter(data$lon1, factor = 1)
data$lat2x <- jitter(data$lat2, factor = 1)
data$lon2x <- jitter(data$lon2, factor = 1)

# Hintergründe
# https://thinking-spatial.org/courses/geoinformationen_kommunizieren/kurs3/
names(providers)

# Karte V1: mit icons
leaflet(data) |> 
  addTiles() |> 
  addProviderTiles("OpenStreetMap.DE",
                   options = providerTileOptions(noWrap = TRUE)) |>
  #addProviderTiles("Jawg.Sunny") |> 
  addMarkers(~lon1x, ~lat1x, 
             icon = makeIcon("./data/birth.png", 18, 18),
             popup = ~str_c(VORNAME, " ", NAME)) |> 
  addMarkers(~lon2x, ~lat2x, 
             icon = makeIcon("./data/death.png", 18, 18),
             label = ~str_c(VORNAME, " ", NAME))

# V2: mit Kreisen und Farben
# https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker
col <- c(brewer.pal(length(levels(data$Geburt)), "Greens")[-1], "#aaaaaa")
lev <- levels(data$Geburt)
cols <- colorFactor(palette = col, data$Geburt)
leaflet(data) |> 
  addTiles() |> 
  # addProviderTiles("OpenStreetMap.DE",
  #                  options = providerTileOptions(noWrap = TRUE)) |>
  #addProviderTiles(providers$CartoDB.Positron) |> 
  addProviderTiles(providers$Esri.NatGeoWorldMap) |> 
  addCircleMarkers(~lon1x, ~lat1x, radius = 2, #fillOpacity = 0.4,
                   #clusterOptions = 1,
                   color = ~cols(Geburt), 
             popup = ~str_c(VORNAME, " ", NAME)) |> 
  addLegend('bottomright', pal = cols, values = lev,
          title = 'Geburt', 
          opacity = 1)
  