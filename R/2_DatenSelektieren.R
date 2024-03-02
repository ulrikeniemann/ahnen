# ..............................................................................
#
# Ahnen
#
# Daten auswählen
#
# ..............................................................................

# Jetzt die Spalten auswählen und umbenennen.

# Übersetzungs"-Datei für Spaltenkopf einlesen
GedComDatei <- "./data/GedCom.xlsx" 

GedComTab <- read_excel(GedComDatei)

# nicht benötigte Daten löschen (Zeilen mit NA)
GedComTab <- GedComTab[complete.cases(GedComTab), ] 

Auswahl <- GedComTab$SpaltenName

# Spalten auswählen: Anzahl der Spalten benötigte reduzieren
AhnTab <- subset(AhnTab, select = c(Auswahl)) 

# neue Überschriften 
colnames(AhnTab) <- GedComTab$Inhalt 

# ..............................................................................