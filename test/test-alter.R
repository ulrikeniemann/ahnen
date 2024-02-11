
# librarys laden
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("leaflet")) install.packages("leaflet"); library(leaflet)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("janitor")) install.packages("janitor"); library(janitor)
if (!require("scales")) install.packages("scales"); library(scales)
if (!require("knitr")) install.packages("knitr"); library(knitr)
if (!require("kableExtra")) install.packages("kableExtra"); library(kableExtra)
if (!require("ggiraph")) install.packages("ggiraph"); library(ggiraph)

# Daten einlesen
data <- read_xlsx("./data/Ulrike 17.Jan 2024.xlsx", skip = 1)

# aufbereiten
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
           fct_explicit_na("(keine Angabe)")) |> 
  mutate(Alter = `TOD JAHR` - `GEB JAHR`)

# ..............................................................................

# Alter anschauen...
table(data$Alter, useNA = "ifany")

ggplot(data |> filter(!is.na(Alter)), aes(x = Alter)) + 
  geom_histogram_interactive(fill = "#08519C", 
                             aes(tooltip = str_c("n = ", after_stat(count)))) +
  ggtitle("Erreichtes Alter") +
  scale_y_continuous(name = "Anzahl", breaks = 5) +
  geom_vline(aes(xintercept=mean(Alter, na.rm = TRUE)),
             color="blue", linetype="dashed", size=1) +
  #facet_wrap(vars(Geburt)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size=16, face="bold", 
                              margin = margin(0, 0, 30, 0))
  )
#girafe(g)

data <- data |> 
  mutate(Altersgruppe = case_when(
    Alter < 40 ~ "unter 40 Jahre",
    Alter >=40 & Alter < 50 ~ "40 bis unter 50 Jahre",
    Alter >=50 & Alter < 60 ~ "50 bis unter 60 Jahre",  
    Alter >=60 & Alter < 70 ~ "60 bis unter 70 Jahre",
    Alter >=70 & Alter < 80 ~ "70 bis unter 80 Jahre",
    Alter >=80 ~ "80 Jahre und älter",
    is.na(Alter) ~ "keine Angabe"
  ) |> fct_reorder(Alter))

table(data$Altersgruppe)

# ..............................................................................

computeTable <- function(var) {
  data |> 
    group_by({{var}}) |> 
    summarise(Anzahl = n()) |> 
    mutate(Prozent = (Anzahl/sum(Anzahl))) |> 
    adorn_totals("row") |> 
    mutate(Prozent = Prozent |> percent(accuracy = 0.1, decimal.mark = ",")) |> 
    mutate({{var}} := factor({{var}}) |> fct_inorder())
}
getTable <- function(tab) {
  tab |> 
    kable(align = c("l", "c", "c")) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F) |> 
    row_spec(dim(tab)[1], bold = T)
} 
computeTable(Altersgruppe) |> getTable()
dat <- computeTable(Altersgruppe) |> filter(row_number() <= n()-1)

g <- ggplot(dat, aes(y = Altersgruppe, x = Anzahl)) + 
  geom_bar_interactive(stat = "identity", 
                       aes(fill = "#08519C", 
                           tooltip = str_c(Altersgruppe, ": n = ", Anzahl))) +
  scale_y_discrete(limits=rev, name = "Altersgruppe") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = Anzahl), hjust = -0.5) +
  scale_fill_identity(guide = "none") +
  ggtitle("Anzahl Vorfahren nach Alter bei Tod") +
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
#g
girafe(ggobj = g)

tab <- data |> 
  filter(!is.na(Alter)) |> 
  group_by(Geburt) |> 
  summarise(`Mittelwert Alter` = mean(Alter),
            Anzahl = n()) |> 
  add_row(Geburt = "Alle Vorfahren",
          `Mittelwert Alter` = mean(data$Alter, na.rm = TRUE), 
          Anzahl = nrow(data |> filter(!is.na(Alter))))


tab <- data |> 
  filter(!is.na(Alter)) |> 
  group_by(Geburt) |> 
  summarise(`Mittelwert Alter` = round(mean(Alter), 1),
            Anzahl = n()) |> 
  add_row(Geburt = "Alle Vorfahren",
          `Mittelwert Alter` = round(mean(data$Alter, na.rm = TRUE), 1), 
          Anzahl = nrow(data |> filter(!is.na(Alter))))

g <- ggplot(tab, aes(y = Geburt, x = `Mittelwert Alter`)) + 
  geom_bar_interactive(stat = "identity", width = 0.5,
           aes(fill = "#08519C", 
               tooltip = str_c("Geburt: ", Geburt, " - Mittelwert erreichtes Alter: ", 
                               round(`Mittelwert Alter`, 1), " Jahre (n = ",
                               Anzahl, ")"))) +
  geom_text(aes(label = round(`Mittelwert Alter`, 1)), hjust = -0.1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_identity(guide = "none") +
  ggtitle("Mittleres Alter bei Tod, Alle und nach Jahrhundert der Geburt") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    plot.title = element_text(size=16, face="bold", 
                              margin = margin(0, 0, 30, 0))
  )
girafe(ggobj = g)
