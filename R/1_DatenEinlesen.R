# ..............................................................................
#
# Ahnen
#
# Daten einlesen
#
# ..............................................................................



###################################################################
#  1. GedTool einlesen, reduzieren und Überschriftszeile Klartext #
###################################################################

# GedTool einlesen
AhnenDatei <- "./data/GedTool.xlsm"

# die ersten 8 Zeilen einlesen
AhnTab <- read_excel(AhnenDatei, 
                     sheet = '1 INDI', 
                     col_names = FALSE, n_max = 8)

# Überschrift 1.Spalte
AhnTab [1,1] <- "ID"
AhnTab [2,1] <- "1"

# Spaltennamen generieren - neue Zeile
SpaltenNamen <- t(AhnTab)  |> 
  as.data.frame() |> 
  unite(SpaltenNamen, sep="_", remove= TRUE, na.rm = TRUE)

# als Vektor speichern
SpaltenNamen <- SpaltenNamen$SpaltenNamen

# jetzt den Rest einlesen und Spaltennamen vergeben
AhnTab <- read_excel(AhnenDatei , 
                     sheet = '1 INDI', 
                     skip = 9,
                     col_names = SpaltenNamen)


# ..............................................................................

