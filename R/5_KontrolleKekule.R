############################################################################
#    Berechnung der KEKULE-Nummern aus der GedTool-Datei zur Kontrolle     #
############################################################################

library(installr)
library(dplyr)

# Bereitstellung der notwendigen Daten aus "data" und "FamTab"

# Personendaten
data_Personen <- data |> 
  select(PERSONEN_ID, NACHNAME, VORNAME,
         GEBURTSDATUM, ELTERN, KEKULE)
data_Personen  <- tibble::add_column(data_Personen, Kekule_1 = NA)
data_Personen  <- tibble::add_column(data_Personen, Kekule_2 = NA)

# Familiendaten
data_Familien <- FamTab |> 
  select(FamID, HUSB, WIFE)

names(data_Familien) <- c("ELTERN", "VATER", "MUTTER")

# beide zusammenführen zur weiteren Verwendung 
data_KEK <- data_Personen |>
  left_join(data_Familien)


######################### KEKULE berechnen ###########################################

# PERSONEN_ID des Probanden auslesen
BLATT_HEAD <- read_excel(AhnenDatei, sheet = '1 HEAD') |> suppressMessages()
Person_Proband <- as.character(BLATT_HEAD[3, '_HOME'])

# Proband bekommt KEKULE "1"
akt_Kekule <- 1
akt_Person <- Person_Proband
akt_Zeile <- which(data_KEK$PERSONEN_ID == akt_Person)
data_KEK[akt_Zeile, 'KEKULE'] <- as.character(akt_Kekule)
data_KEK[akt_Zeile, 'Kekule_1'] <- as.character(akt_Kekule)

# Geschwister des Probanden entfernen
# Eltern Proband
Eltern_Proband <- data_KEK[akt_Zeile, 'ELTERN']
data_KEK <- filter(data_KEK, ELTERN != Eltern_Proband | KEKULE == "1" | is.na(ELTERN))


# Position der Spalten VATER und MUTTER
y <- which(names (data_KEK) == "VATER")
z <- which(names (data_KEK) == "MUTTER")

# Anzahl der Generationen
Gen_Zahl <- 15

# Schleife
Ende <- 2 ^ Gen_Zahl

for (i in 1:Ende) {
  
  #####################################################
  # if(i == 16) stop("ELTERN_Kekule 16")
  #####################################################
  
  akt_Kekule <- i
  akt_Zeile <- which(data_KEK$Kekule_1 == akt_Kekule)
  if( is.empty(akt_Zeile)) {
    akt_Zeile <- which(data_KEK$Kekule_2 == akt_Kekule)
  }
  
  # Wechsel VATER (m = 0) - MUTTER (m = 1)
  m <- 0
  for(j in y:z) {
    ELTERN_Kekule <- akt_Kekule * 2 + m
    
    if( !is.empty(akt_Zeile)) {
      akt_Person <- data_KEK[akt_Zeile, y + m]
      
      ELTERN_Zeile <- which(data_KEK$PERSONEN_ID == akt_Person)
      
      if ( !is.empty(ELTERN_Zeile)) {
        if( is.na(data_KEK[ELTERN_Zeile, 'Kekule_1'])){
          data_KEK[ELTERN_Zeile, 'Kekule_1'] <- as.character(ELTERN_Kekule)
        } else {
          data_KEK[ELTERN_Zeile, 'Kekule_2'] <- as.character(ELTERN_Kekule)

        }
      }    
    
      m <- 1
    }  
  }
}

# berechnete Kekule zusammenfassen 
data_KEK <- data_KEK |> 
  mutate(KEKULE_berechnet = case_when 
         (is.na(Kekule_2)  ~ Kekule_1 , !is.na(Kekule_2)  ~ paste(Kekule_1, '&', Kekule_2) ),
         .after = Kekule_2)

# Implex (Ahnenschwund) ausweisen

IMPLEX <- filter(data_KEK, !is.na(Kekule_2))
IMPLEX <- IMPLEX |> 
  select(NACHNAME, VORNAME, KEKULE, Kekule_1)
IMPLEX <- IMPLEX |> 
  mutate(IMPL_Generation = as.integer(Kekule_1))
IMPLEX <- IMPLEX |> 
  mutate(IMPL_Generation = floor(log(IMPL_Generation, 2)))
IMPLEX <- IMPLEX[order(IMPLEX$IMPL_Generation),]
IMPLEX <- IMPLEX[ , -4]

view(IMPLEX)

############################### Auswertung ###############################

data_KEK<- data_KEK |> 
  mutate(VERGLEICH = case_when 
         (KEKULE == KEKULE_berechnet ~ "OK" , KEKULE != KEKULE_berechnet | is.na(KEKULE_berechnet) | is.na(KEKULE) ~ "UNGLEICH" ),
         .after = KEKULE_berechnet)

# nur AHNEN 
data_KEK <- filter(data_KEK, !is.na(KEKULE) | !is.na(KEKULE_berechnet)) 


# Spalten entfernen
y <- which(names (data_KEK) == "Kekule_1")
data_KEK <- data_KEK[ ,-y]
y <- which(names (data_KEK) == "Kekule_2")
data_KEK <- data_KEK[ ,-y]

Kekule_Fehler <- filter(data_KEK, VERGLEICH == 'UNGLEICH')

if (nrow(Kekule_Fehler) == 0) {
  KEK_Text <- "KEIN FEHLER BEI DEN KEKULE-NUMMERN."
  } else {
    KEK_Text <- "ACHTUNG, KEKULE-FEHLER!"
    view(Kekule_Fehler)}

print(KEK_Text)
################################### ENDE ##########################################

