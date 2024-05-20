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

# die Kopfzeilen einlesen
# Kopf alt: 9 Zeilen, neu: 5 Zeilen

AhnTab <- read_excel(AhnenDatei, 
                     sheet = '1 INDI', 
                     col_names = FALSE, n_max = 4)

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
                     skip = 5,
                     col_names = SpaltenNamen)


# ..............................................................................

