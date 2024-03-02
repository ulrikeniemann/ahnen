# neuer Punkt: Anzahl Kinder pro Person
# 
# nur für direkte Vorfahren
# also alle die eine Kekule-Nr haben
data |> filter(!is.na(KEKULE)) |> nrow()
table(data$Vorfahren)
# 
# immer die die gleiche Familie haben, und in der direkten Vorlinie ist
# 
# 1.
# alle geraden Nummern aus der KekuleNr sind deine männlichen Vorfahren
# die weiblichen die dazugehören -> die männlichen+1
# Wolfgang + Renate: 2 + 3 (eine Familie)
# 
# Kekule Spalte BG (FACT)
# Spalte FAMS (BL) -> Kekule 3+4 (M+P) @F3Q@ -> gemeinsame Familien F3
# 
# 
# es gibt 4 Spalten für FAMS -> Paarungen
# BL14+BM15 -> F4 -> beide eine Familie
# 
# 1)
# Paare identifizieren
# Mann-Nr + 1 = dazugehörige Frau

# # ich gebe dem Mann seine Kekule-Nr
# test <- data |> 
#   mutate(Partner1 = if_else(KekuleNr %% 2 == 0, 
#                            KekuleNr, NA), .after = KekuleNr) |> 
#   # nur zu Testzwecken: nur relevante Spalten
#   select(KEKULE, KekuleNr, Partner1, starts_with("Familie"))
# 
# 
# # ich suche den Frauen den dazugehörigen Mann (?)
# findePartner <- function(nr) {
#   #browser()
#   partner <- 0
#   # nur für die ungeraden Nummern suchen wir den Partner
#   if(!is.na(nr) & nr > 1 & (nr %% 2 != 0)) {
#     partner <- test |> filter(KekuleNr == (nr-1)) |> select(Partner1) |> pull()
#     # wenn kein Partner gefunden wurde setzen wir diesen auf 0
#     if(is_empty(partner)) {partner <- 0}
#   }  
#   return(partner)
# }

# # findePartner(3)
#  findePartner(1)
# # 
# 
# # hier wird alles gefunden
# map_int(test$KekuleNr[1:46], findePartner)
# # Ausnahme nr-1 nicht vorhanden
# findePartner(47)
# 
# # test$KekuleNr[47]
# # x <- test |> filter(KekuleNr == 46) |> select(Familie) |> pull()
# # x
# # is_empty(x)
# table(test$KekuleNr, useNA = "ifany")
# map_int(test$KekuleNr, findePartner)
# 
#   
# test <- test |> 
#   mutate(Partner = map_int(KekuleNr, findePartner)) |> 
#   select(KEKULE, KekuleNr, Familie, Partner, starts_with("Familie_"))
# 
# test <- test |> rowwise() |> 
#   mutate(Partner2 = findePartner(KekuleNr), .after = Familie)
# 
# all.equal(test$Partner, test$Partner2)
# 
# test |> filter(Partner != Partner2)
# test <- test |> rowwise() |>
#   mutate(Partner2 = findePartner(KekuleNr), .after = Partner1)


# beide Partner erhalten erst mal die Nummer des Ehemanns
# Männer sind die geraden Nummern, Frauen die darauf folgenden ungeraden
test <- data |> 
  mutate(Paar = if_else(KekuleNr %% 2 == 0, KekuleNr, 
                        if_else(KekuleNr>1, KekuleNr-1, NA)), 
         .after = KekuleNr) |>
   # nur zu Testzwecken: nur relevante Spalten
  select(KEKULE, KekuleNr, Paar, starts_with("Familie"))


# 2)
# Familien-Nr des Paares identifizieren
# haben diese Paare eine gemeinsame Familie?
#   welche Familien-Nr
# 
# Bsp: 4+5 gehören zusammen
# FAM = F4 (BL) (BM) (hier sind 4 spalten zu durchsuchen)
# 
# Ziel:
#   neue Spalte die die gemeinsame Familien-Nr des Paares enthält
# M+P = F3
# die Eltern meines Vaters = F4
# 
# Frage:
#   wieviele Kinder hat diese Familie (nur gemeinsame Kinder der Vorfahren):
#   Blatt 1_FAM
# ab Spalte B und folgende: Einträge zählen in CHIL-Spalten

test <- test |> 
  group_by(Paar) |> 
  # Fall 1: beide hatten nur eine einzige Familie -> Familie = Familie_1
  mutate(test = if_else(!is.na(Paar) & is.na(FAMILIE_2), FAMILIE_1, NA)) 

# Fall 2: Vergleich Familien
# ich gehe davon aus das Männer immer gerade und 
# Frauen immer darauffolgend ungerade sind!!!
test <- test |> 
  group_by(Paar) |> arrange(KekuleNr) |> 
  mutate(Familie = if_else(!is.na(KekuleNr), 
           case_when(
             # Vgl. Familie 1 .......................
             # Familie - Vgl. Mann zu Frau (folgend)
             FAMILIE_1 == lead(FAMILIE_1) ~ FAMILIE_1,
             FAMILIE_1 == lead(FAMILIE_2) ~ FAMILIE_1, 
             FAMILIE_1 == lead(FAMILIE_3) ~ FAMILIE_1, 
             FAMILIE_1 == lead(FAMILIE_4) ~ FAMILIE_1, 
             # Familie - Vgl. Frau zu Mann (vorab)
             FAMILIE_1 == lag(FAMILIE_1) ~ FAMILIE_1,
             FAMILIE_2 == lag(FAMILIE_1) ~ FAMILIE_2, 
             FAMILIE_3 == lag(FAMILIE_1) ~ FAMILIE_3, 
             FAMILIE_4 == lag(FAMILIE_1) ~ FAMILIE_4,
             # Vgl. Familie 2 .......................
             # Familie - Vgl. Mann zu Frau (folgend)
             FAMILIE_2 == lead(FAMILIE_1) ~ FAMILIE_2,
             FAMILIE_2 == lead(FAMILIE_2) ~ FAMILIE_2, 
             FAMILIE_2 == lead(FAMILIE_3) ~ FAMILIE_2, 
             FAMILIE_2 == lead(FAMILIE_4) ~ FAMILIE_2, 
             # Familie - Vgl. Frau zu Mann (vorab)
             FAMILIE_1 == lag(FAMILIE_2) ~ FAMILIE_1,
             FAMILIE_2 == lag(FAMILIE_2) ~ FAMILIE_2, 
             FAMILIE_3 == lag(FAMILIE_2) ~ FAMILIE_3, 
             FAMILIE_4 == lag(FAMILIE_2) ~ FAMILIE_4,
             # Vgl. Familie 3 .......................
             # Familie - Vgl. Mann zu Frau (folgend)
             FAMILIE_3 == lead(FAMILIE_1) ~ FAMILIE_3,
             FAMILIE_3 == lead(FAMILIE_2) ~ FAMILIE_3, 
             FAMILIE_3 == lead(FAMILIE_3) ~ FAMILIE_3, 
             FAMILIE_3 == lead(FAMILIE_4) ~ FAMILIE_3, 
             # Familie - Vgl. Frau zu Mann (vorab)
             FAMILIE_1 == lag(FAMILIE_3) ~ FAMILIE_1, 
             FAMILIE_2 == lag(FAMILIE_3) ~ FAMILIE_2, 
             FAMILIE_3 == lag(FAMILIE_3) ~ FAMILIE_3, 
             FAMILIE_4 == lag(FAMILIE_3) ~ FAMILIE_4,
             # Vgl. Familie 4 .......................
             # Familie - Vgl. Mann zu Frau (folgend)
             FAMILIE_4 == lead(FAMILIE_1) ~ FAMILIE_4,
             FAMILIE_4 == lead(FAMILIE_2) ~ FAMILIE_4, 
             FAMILIE_4 == lead(FAMILIE_3) ~ FAMILIE_4, 
             FAMILIE_4 == lead(FAMILIE_4) ~ FAMILIE_4, 
             # Familie - Vgl. Frau zu Mann (vorab)
             FAMILIE_1 == lag(FAMILIE_4) ~ FAMILIE_1,
             FAMILIE_2 == lag(FAMILIE_4) ~ FAMILIE_2, 
             FAMILIE_3 == lag(FAMILIE_4) ~ FAMILIE_3, 
             FAMILIE_4 == lag(FAMILIE_4) ~ FAMILIE_4
    ), NA))

# die die keinen Partner haben kriegen die Familie 1 wenn keine 2. vorhanden
test <- test |> 
  mutate(Familie = if_else(is.na(Familie) & is.na(FAMILIE_2), 
         FAMILIE_1, Familie))

# wer hat keine eineindeutige Familie?
table(test$Familie, useNA = "ifany")

################################################################################

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
# 
# # das jetzt matchen ... erst mal temporär weil bisschen hin und her...
# # für Ehemänner
# temp <- data |> select(PERSONEN_ID) |> 
#   left_join(FamTab |> select(FamID, AnzahlKinder, HUSB_1, WIFE_1), 
#             by = join_by(PERSONEN_ID == HUSB_1)) |> 
#   # damit die Männer-Angaben gleich nicht überschrieben werden umbenennen
# rename(FamID_M = FamID, AnzahlKinder_M = AnzahlKinder)
# 
# # für Ehefrauen
# temp <- temp |> 
#   left_join(FamTabTemp, by = join_by(PERSONEN_ID == WIFE_1))
# 
# # jetzt die Familien und Anzahl Kinder wieder in eine Spalte...
# temp <- temp |> 
#   mutate(FamID = if_else(is.na(FamID), FamID_M, FamID)) |> 
#   mutate(AnzahlKinder = 
#            if_else(is.na(AnzahlKinder), AnzahlKinder_M, AnzahlKinder)) |> 
#   select(PERSONEN_ID, FamID, WIFE_1, HUSB_1, AnzahlKinder)
# 
# # und die Spalten jetzt an den großen Datensatz ranhängen...
# data <- data |> left_join(temp, by = join_by(PERSONEN_ID))
# 
# # zum Vergleich die Spalten verschieben...
# data <- data |> 
#   relocate(c(FamID, WIFE_1, HUSB_1, AnzahlKinder), .after = FAMILIE_4)
  