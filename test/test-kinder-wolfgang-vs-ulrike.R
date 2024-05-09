
# ..............................................................................
# Version WOLFGANG Kinder Anzahl Familie 1
data_KIDS <- data |> 
  filter(KekuleNr > 1)|> 
  select(PERSONEN_ID, KekuleNr, NACHNAME, VORNAME,
         FAMILIE_1, FAMILIE_2, FAMILIE_3, FAMILIE_4)
# Anzahl der Kinder aus den einzelnen Verbindungen
# VERBINDUNG 1
data_KIDS <- data_KIDS |>
  left_join(FamTab |> select(FamID, AnzahlKinder), 
            by = join_by(FAMILIE_1 == FamID)) |> 
  rename("KINDER_1" = "AnzahlKinder")
# ..............................................................................
# Version ULRIKE
# FamilieKind ist kompliziert berechnet siehe Skript 4_FamilienDaten.R


# bei welchen stimmt FamilieKind nicht mit Original FAMILIE_1 überein?
diff <- dataDIREKT |> filter(FamilieKind != FAMILIE_1 | is.na(FamilieKind)) |> 
  select(PERSONEN_ID, Kind, FamilieKind, FAMILIE_1, FAMILIE_2, AnzahlKinder)
# wir haben 27 Unterschiede  

# Bsp @I507@
# Kind ist hier 3650 -> hat bei FAMC (Eltern) keine Angabe in der Spalte!

# Bsp @I6@
# FAMILIE_1 ist @F6@
# @F6@ hat im Blatt 1 FAM gar keine Kinder gelistet!
# nach meiner Rechnung wäre Familie des direkten Vorfahren-Kindes aber @I4@ gewesen
# -> ok, @I6@ hat FAMILIE_2 @I4@ -> richtig ist Version Ulrike

# @I29@
# hat hat @F20@ als FAMILIE_2
# @I30@
# hat hat @F20@ als FAMILIE_2



# # Unterschiede Version Wolfgang vs. Version Ulrike
# test <- data_KIDS |> select(PERSONEN_ID, KINDER_1) |> 
#   left_join(dataDIREKT |> select(PERSONEN_ID, AnzahlKinder)) |> 
#   rename(Kinder_1_Wolfgang = KINDER_1,
#          Kinder_1_Ulrike = AnzahlKinder)
# 
# diff2 <- test |> 
#   filter(Kinder_1_Wolfgang != Kinder_1_Ulrike | is.na(Kinder_1_Ulrike))

# ..............................................................................
# V1 Wolfgang

data_KIDS <- data |> 
  filter(KekuleNr > 0) |> 
  select(PERSONEN_ID, KekuleNr, NACHNAME, VORNAME, ELTERN)

# für gemeinsame Kinder: nur die, die Eltern haben

data_ELTERN <- data_KIDS |> filter(KekuleNr > 0)#filter(KekuleNr > 0, ! is.na(ELTERN))

data_ELTERN <- data_ELTERN |>
  left_join(FamTab |> select(FamID, AnzahlKinder), 
            by = join_by(ELTERN == FamID)) |> 
  rename(Kinderzahl_der_Eltern = AnzahlKinder)

test <- data_ELTERN |> 
  left_join(dataDIREKT |> 
              select(PERSONEN_ID, Kind, FamilieKind, FAMILIE_1, AnzahlKinder))
