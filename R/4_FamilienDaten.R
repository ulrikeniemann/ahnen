# ..............................................................................
#
# Ahnen
#
# Familien Daten anfügen - Anzahl Kinder, Familie, zugehörige Partner
#
# ..............................................................................

# Familiendaten einlesen
# die ersten 8 Zeilen einlesen
FamTab <- read_excel(AhnenDatei, 
                     sheet = '1 FAM', 
                     col_names = FALSE, n_max = 8)
# Überschrift 1.Spalte
FamTab [1,1] <- "FamID"

# Spaltennamen generieren - neue Zeile
SpaltenNamen <- t(FamTab)  |> 
  as.data.frame() |> 
  unite(SpaltenNamen, sep="_", remove= TRUE, na.rm = TRUE)

# als Vektor speichern
SpaltenNamen <- SpaltenNamen$SpaltenNamen

# jetzt den Rest einlesen und Spaltennamen vergeben
FamTab <- read_excel(AhnenDatei , 
                     sheet = '1 FAM', 
                     skip = 9,
                     col_names = SpaltenNamen)

# Anzahl Kinder berechnen
FamTab <- FamTab %>% 
  rowwise() %>% 
  mutate(AnzahlKinder = sum(!is.na(across(starts_with("CHIL")))))

# ..............................................................................

# beide Partner erhalten erst mal die Nummer des Ehemanns
# Männer sind die geraden Nummern, Frauen die darauf folgenden ungeraden!
# das ist noch nicht ganz sauber so im Datensatz?
data <- data |> 
  mutate(Paar = if_else(KekuleNr %% 2 == 0, KekuleNr, 
                        if_else(KekuleNr>1, KekuleNr-1, NA)), 
         .after = KekuleNr) 

# # Vergleich Familien - für die Paare soll die gemeinsame Familie gefunden werden
# # ich gehe davon aus das Männer immer gerade und 
# # Frauen immer darauffolgend ungerade sind!!!
# data <- data |> 
#   group_by(Paar) |> arrange(KekuleNr) |> 
#   mutate(Familie = if_else(!is.na(KekuleNr), 
#                            case_when(
#                              # Vgl. Familie 1 .......................
#                              # Familie - Vgl. Mann zu Frau (folgend)
#                              FAMILIE_1 == lead(FAMILIE_1) ~ FAMILIE_1,
#                              FAMILIE_1 == lead(FAMILIE_2) ~ FAMILIE_1, 
#                              FAMILIE_1 == lead(FAMILIE_3) ~ FAMILIE_1, 
#                              FAMILIE_1 == lead(FAMILIE_4) ~ FAMILIE_1, 
#                              # Familie - Vgl. Frau zu Mann (vorab)
#                              FAMILIE_1 == lag(FAMILIE_1) ~ FAMILIE_1,
#                              FAMILIE_2 == lag(FAMILIE_1) ~ FAMILIE_2, 
#                              FAMILIE_3 == lag(FAMILIE_1) ~ FAMILIE_3, 
#                              FAMILIE_4 == lag(FAMILIE_1) ~ FAMILIE_4,
#                              # Vgl. Familie 2 .......................
#                              # Familie - Vgl. Mann zu Frau (folgend)
#                              FAMILIE_2 == lead(FAMILIE_1) ~ FAMILIE_2,
#                              FAMILIE_2 == lead(FAMILIE_2) ~ FAMILIE_2, 
#                              FAMILIE_2 == lead(FAMILIE_3) ~ FAMILIE_2, 
#                              FAMILIE_2 == lead(FAMILIE_4) ~ FAMILIE_2, 
#                              # Familie - Vgl. Frau zu Mann (vorab)
#                              FAMILIE_1 == lag(FAMILIE_2) ~ FAMILIE_1,
#                              FAMILIE_2 == lag(FAMILIE_2) ~ FAMILIE_2, 
#                              FAMILIE_3 == lag(FAMILIE_2) ~ FAMILIE_3, 
#                              FAMILIE_4 == lag(FAMILIE_2) ~ FAMILIE_4,
#                              # Vgl. Familie 3 .......................
#                              # Familie - Vgl. Mann zu Frau (folgend)
#                              FAMILIE_3 == lead(FAMILIE_1) ~ FAMILIE_3,
#                              FAMILIE_3 == lead(FAMILIE_2) ~ FAMILIE_3, 
#                              FAMILIE_3 == lead(FAMILIE_3) ~ FAMILIE_3, 
#                              FAMILIE_3 == lead(FAMILIE_4) ~ FAMILIE_3, 
#                              # Familie - Vgl. Frau zu Mann (vorab)
#                              FAMILIE_1 == lag(FAMILIE_3) ~ FAMILIE_1, 
#                              FAMILIE_2 == lag(FAMILIE_3) ~ FAMILIE_2, 
#                              FAMILIE_3 == lag(FAMILIE_3) ~ FAMILIE_3, 
#                              FAMILIE_4 == lag(FAMILIE_3) ~ FAMILIE_4,
#                              # Vgl. Familie 4 .......................
#                              # Familie - Vgl. Mann zu Frau (folgend)
#                              FAMILIE_4 == lead(FAMILIE_1) ~ FAMILIE_4,
#                              FAMILIE_4 == lead(FAMILIE_2) ~ FAMILIE_4, 
#                              FAMILIE_4 == lead(FAMILIE_3) ~ FAMILIE_4, 
#                              FAMILIE_4 == lead(FAMILIE_4) ~ FAMILIE_4, 
#                              # Familie - Vgl. Frau zu Mann (vorab)
#                              FAMILIE_1 == lag(FAMILIE_4) ~ FAMILIE_1,
#                              FAMILIE_2 == lag(FAMILIE_4) ~ FAMILIE_2, 
#                              FAMILIE_3 == lag(FAMILIE_4) ~ FAMILIE_3, 
#                              FAMILIE_4 == lag(FAMILIE_4) ~ FAMILIE_4
#                            ), NA), .after = FAMILIE_4)
# 
# # die die keinen Partner haben kriegen die Familie 1 wenn keine 2. vorhanden
# data <- data |> 
#   mutate(Familie = if_else(is.na(Familie) & is.na(FAMILIE_2) & !is.na(KekuleNr), 
#                            FAMILIE_1, Familie))
# 
# # wer hat keine eineindeutige Familie?
# data |> filter(Vorfahren == "Direkt" & is.na(Familie)) |> 
#   count(Familie)
# 
# # das jetzt matchen ... mit den Familiendaten
# # und die Spalten jetzt an den großen Datensatz ranhängen...
# data <- data |> 
#   left_join(FamTab |> select(FamID, AnzahlKinder, HUSB_1, WIFE_1), 
#             by = join_by(Familie == FamID)) |> 
#   relocate(c(Familie, WIFE_1, HUSB_1, AnzahlKinder), .after = FAMILIE_4) |> 
#   ungroup()

# ..............................................................................
# und noch ein anderer Versuch: 
# Kind ist Kekule / 2 -> davon ganzzahiger Rest
data <- data |> 
  mutate(Kind = if_else(!is.na(KekuleNr), floor(KekuleNr / 2), NA), 
         .after = Paar)

# temporärer Versuch
kind <- data |> select(Kind)
kind <- unique(kind)
kind <- kind |> 
  left_join(data |> select(KekuleNr, ELTERN) |> filter(!is.na(KekuleNr)),
            by = join_by(Kind == KekuleNr))
# Sonderfall KekuleNr 1 ist doppelt
kind <- unique(kind)
# FAMC umbenennen zum zurückspielen
kind <- kind |> rename(FamilieKind = ELTERN)

# und jetzt zurückspielen
data <- data |> left_join(kind, by = join_by(Kind == Kind))

# Spalte verschieben
data <- data |> 
  relocate(FamilieKind, .after = Kind)

# # jetzt Abgleich mit der Methode von oben
# data |> select(KekuleNr, Familie, FamilieKind) |> 
#   filter(!is.na(KekuleNr) & (is.na(Familie) | Familie != FamilieKind)) 
# # passt alles und besser ... also ist das die bessere Methode

# das jetzt matchen ... mit den Familiendaten
# und die Spalten jetzt an den großen Datensatz ranhängen...
data <- data |> 
  left_join(FamTab |> select(FamID, AnzahlKinder), 
            by = join_by(FamilieKind == FamID)) |> 
  relocate(AnzahlKinder, .after = FamilieKind) |> 
  ungroup()

# ..............................................................................

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
# ..............................................................................
# ..............................................................................
