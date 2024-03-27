# ..............................................................................
#
# Ahnen
#
# Daten aufbereiten
#
# ..............................................................................

# ..............................................................................
# ab hier mit data weiterarbeiten
data <- data.frame(AhnTab)
# ..............................................................................

# Anzahl Personen und Anzahl der direkten Vorfahren
# direkte Vorfahren kennzeichnen
data <- data |> 
  mutate(Vorfahren = if_else(!is.na(KEKULE), "Direkt", "weitere"))

# ..............................................................................
# Variablen zur Anzeigen im Dokument generieren
ZahlPers <- nrow(data)

ZahlDirektAhnen <- nrow(data |> filter(Vorfahren == "Direkt"))

# Datum der letzten Aktualisierung der Ahnendaten auslesen
head <- read_excel(AhnenDatei, sheet = '1 HEAD') |> suppressMessages()

# Datum ist immer auf 5, 3
LetzteAktualisierung <- dmy(head[5, 3])
LetzteAktualisierung <- format(LetzteAktualisierung, format = "%d.%m.%Y")

# ..............................................................................
# Generation
# 
# data <- data |> 
#   # Generation ohne ID
#   mutate(Generation = str_split(GENERATION, " ", simplify = TRUE)[, 1] |> unlist()) |> 
#   mutate(Generation = as_factor(Generation)) |> 
#   mutate(Generation = fct_recode(Generation, 
#                                  "Generation I (Eltern)" = "I", 
#                                  "Generation II (Großeltern)" = "II", 
#                                  "Generation III (Urgroßeltern)" = "III",
#                                  "Generation IV (Alteltern (Ur-Urgroßeltern))" = "IV",
#                                  "Generation V (Altgroßeltern)" = "V",
#                                  "Generation VI (Alturgroßeltern)" = "VI",
#                                  "Generation VII (Obereltern)" = "VII",
#                                  "Generation VIII (Obergroßeltern)" = "VIII",
#                                  "Generation IX (Oberurgroßeltern)" = "IX",
#                                  "Generation X (Stammeltern)" = "X",
#                                  "Generation XI (Stammgroßeltern)" = "XI",
#                                  "Generation XII (Stammurgroßeltern)" = "XII",
#                                  "Generation XIII (Ahneneltern)" = "XIII",
#                                  "Generation XIV (Ahnengroßeltern)" = "XIV",
#                                  "Generation XV (Ahnenurgroßeltern)" = "XV"
#                                  ))

# neu, Wolfgang:
##### GENERATION (numerisch) aus KEKULE #####

KekuleNr <- as.numeric(data[['KEKULE']])
KekuleNr[is.na(KekuleNr) == TRUE] <- 0
KekuleNr <- KekuleNr - KekuleNr %% 2
GenerationNr <- KekuleNr

for (i in 1:nrow(data)){
  Gen<-0
  while(KekuleNr[i] > 1) {
    Gen <- Gen + 1
    KekuleNr[i] <- KekuleNr[i]/2
    if(KekuleNr[i]>1){
      KekuleNr[i] <- KekuleNr[i] - KekuleNr[i] %% 2}
  }
  GenerationNr[i] <- Gen
}

# neue Spalten einfügen
data <- data |> 
  mutate(KekuleNr = parse_number(KEKULE), .after = KEKULE) |> 
  mutate(GenerationNr, .after = KEKULE) |> 
  mutate(Generation = if_else(GenerationNr == 0, NA,
                              as.factor(GenerationNr)), .after = GenerationNr) |> 
  mutate(Generation = fct_recode(Generation,
                                 "Generation I (Eltern)" = "1",
                                 "Generation II (Großeltern)" = "2",
                                 "Generation III (Urgroßeltern)" = "3",
                                 "Generation IV (Alteltern (Ur-Urgroßeltern))" = "4",
                                 "Generation V (Altgroßeltern)" = "5",
                                 "Generation VI (Alturgroßeltern)" = "6",
                                 "Generation VII (Obereltern)" = "7",
                                 "Generation VIII (Obergroßeltern)" = "8",
                                 "Generation IX (Oberurgroßeltern)" = "9",
                                 "Generation X (Stammeltern)" = "10",
                                 "Generation XI (Stammgroßeltern)" = "11",
                                 "Generation XII (Stammurgroßeltern)" = "12",
                                 "Generation XIII (Ahneneltern)" = "13",
                                 "Generation XIV (Ahnengroßeltern)" = "14",
                                 "Generation XV (Ahnenurgroßeltern)" = "15"
  )) |> suppressWarnings()

# Hilfsvariablen löschen
rm(KekuleNr, GenerationNr, Gen, i)

# ..............................................................................
# Geo-Daten aufbereiten
data <- data |> mutate(
  lat1 = `GO_GEO_BREITE` |> parse_number(),
  lon1 = `GO_GEO_LAENGE` |> parse_number(),
  lat2 = `TO_GEO_BREITE` |> parse_number(),
  lon2 = `TO_GEO_LAENGE` |> parse_number())

# ..............................................................................
# Datümer aufbereiten
data <- data |>
  rename(TODESDATUMorig = TODESDATUM,
         GEBURTSDATUMorig = GEBURTSDATUM) |> 
  mutate(TODESDATUM = TODESDATUMorig, .after = TODESDATUMorig) |>  
  mutate(GEBURTSDATUM = GEBURTSDATUMorig, .after = GEBURTSDATUMorig)
# fehlende Werte ersetzen
data <- data |> 
  # Tod
  mutate(TODESDATUM = if_else(str_detect(TODESDATUM, "ABT "), "", TODESDATUM)) |> 
  mutate(TODESDATUM = if_else(str_detect(TODESDATUM, "FROM "), "", TODESDATUM)) |> 
  mutate(TODESDATUM = if_else(str_detect(TODESDATUM, "TO "), "", TODESDATUM)) |> 
  mutate(TODESDATUM = if_else(str_detect(TODESDATUM, "BEF "), "", TODESDATUM)) |> 
  mutate(TODESDATUM = if_else(str_detect(TODESDATUM, "AFT "), "", TODESDATUM)) |> 
  #
  mutate(TODESDATUM = if_else(nchar(TODESDATUM) == 4, 
                              str_c("1 JAN ", TODESDATUM), TODESDATUM)) |> 
  mutate(TODESDATUM = if_else(nchar(TODESDATUM) == 8, 
                              str_c("1 ", TODESDATUM), TODESDATUM)) |> 
  mutate(TODESDATUM = dmy(TODESDATUM)) |> 
  # Geburt
  mutate(GEBURTSDATUM = if_else(str_detect(GEBURTSDATUM, "ABT "), "", GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = if_else(str_detect(GEBURTSDATUM, "FROM "), "", GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = if_else(str_detect(GEBURTSDATUM, "TO "), "", GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = if_else(str_detect(GEBURTSDATUM, "BEF "), "", GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = if_else(str_detect(GEBURTSDATUM, "AFT "), "", GEBURTSDATUM)) |> 
  #
  mutate(GEBURTSDATUM = if_else(nchar(GEBURTSDATUM) == 4, 
                                str_c("1 JAN ", GEBURTSDATUM), GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = if_else(nchar(GEBURTSDATUM) == 8, 
                                str_c("1 ", GEBURTSDATUM), GEBURTSDATUM)) |> 
  mutate(GEBURTSDATUM = dmy(GEBURTSDATUM))

# ..............................................................................
# Alter 
data <- data |> 
  mutate(
    # genaues Alter
    Alter = as.integer(interval(GEBURTSDATUM, TODESDATUM)/ years(1)),
    # Geburt und Tod - nur Jahrhundert
    Geburt = (year(GEBURTSDATUM)/100) |> floor(),
    Tod = (year(TODESDATUM)/100) |> floor()) |> 
  mutate(Geburt = factor(str_c(Geburt, "00 - ", Geburt, "99")) |> 
           fct_na_value_to_level("(keine Angabe)")) |> 
  mutate(Tod = factor(str_c(Tod, "00 - ", Tod, "99")) |> 
           fct_na_value_to_level("(keine Angabe)"))

# ..............................................................................
# Altersgruppen 
data <- data |> 
  mutate(Altersgruppe = case_when(
    Alter < 5 ~ "unter 5 Jahre",
    Alter >=5 & Alter < 10 ~ "5 bis unter 10 Jahre",   
    Alter >=10 & Alter < 20 ~ "10 bis unter 20 Jahre",    
    Alter >=20 & Alter < 30 ~ "20 bis unter 30 Jahre",
    Alter >=30 & Alter < 40 ~ "30 bis unter 40 Jahre",
    Alter >=40 & Alter < 50 ~ "40 bis unter 50 Jahre",
    Alter >=50 & Alter < 60 ~ "50 bis unter 60 Jahre",  
    Alter >=60 & Alter < 70 ~ "60 bis unter 70 Jahre",
    Alter >=70 & Alter < 80 ~ "70 bis unter 80 Jahre",
    Alter >=80 ~ "80 Jahre und älter") |> 
    fct_reorder(Alter, .na_rm = TRUE) |> 
    fct_na_value_to_level("keine Angabe"))

# ..............................................................................
# ..............................................................................
# ..............................................................................