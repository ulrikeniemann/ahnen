# ..............................................................................

# rpivottable

# libs 
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("rpivotTable")) install.packages("rpivotTable"); library(rpivotTable)

source("./R/0_functions.R")
source("./R/1_DatenEinlesen.R")
source("./R/2_DatenSelektieren.R")
source("./R/3_DatenAufbereiten.R")
source("./R/4_FamilienDaten.R")


rpivotTable(data)

dataDIREKT <- data |> filter(Vorfahren == "Direkt")
rpivotTable(dataDIREKT)
