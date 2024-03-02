# ..............................................................................
#
# Ahnen
#
# Daten einlesen
#
# ..............................................................................

###########################################################################
# V.003 8.2.2024                                                          #
# 1. Anpassung an neue Datenquelle                                        #
# NAME ==> NACHNAME, GEB JAHR ==> GEB_JAHR, GEO1 BREITE ==> GO_GEO_BREITE #
# GEO1 LÄNGE ==> GO_GEO_LAENGE, TOD JAHR ==> TOD_JAHR,                    #
# GEO2 BREITE ==> TO_GEO_BREITE, GEO2 LÄNGE ==> TO_GEO_LAENGE             #
# GEB JAHR und TOD JAHR müssen aus GEBURTSDATUM und TODESDATUM berechnet  #
# werden                                                                  #
###########################################################################

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