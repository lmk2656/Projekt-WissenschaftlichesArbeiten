# Skript zur Analyse des Titanic-Datensatzes mit Funktionen aus Skript 1

# 1. Datensatz einlesen
titanic2 <- read.csv("titanic2.csv", stringsAsFactors = TRUE)

# Bereinigung des Datensatzes
# Entferne Anführungszeichen aus Spaltennamen
colnames(titanic2) <- gsub('"', '', colnames(titanic2))

# Entferne Anführungszeichen aus allen Zeichenketten in der Daten
titanic2 <- data.frame(lapply(titanic2, function(x) {
  if(is.character(x)) {
    return(gsub('"', '', x))
  } else {
    return(x)
  }
}))

# Konvertiere relevante Spalten in Faktoren
titanic2$Survived <- as.factor(titanic2$Survived)
titanic2$Pclass <- as.factor(titanic2$Pclass)
titanic2$Sex <- as.factor(titanic2$Sex)

# 2. Deskriptive Statistik und Visualisierung

# (i) metrisch() Funktion anwenden
# Untersuche das Alter
metrisch(titanic2$Age)

# Untersuche den Fahrpreis
metrisch(titanic2$Fare)

# (ii) kategorial() Funktion anwenden
# Untersuche die Überlebensrate
kategorial(titanic2$Survived)

# Untersuche die Ticketklasse
kategorial(titanic2$Pclass)

# (iii) zusammenhang_kategorial() Funktion anwenden
# Untersuche den Zusammenhang zwischen Überleben und Geschlecht
zusammenhang_kategorial(titanic2$Survived, titanic2$Sex)

# Untersuche den Zusammenhang zwischen Überleben und Ticketklasse
zusammenhang_kategorial(titanic2$Survived, titanic2$Pclass)

# (iv) zusammenhang_metrisch_dichotom() Funktion anwenden
# Untersuche den Zusammenhang zwischen Alter und Überleben
titanic2$Survived_numeric <- as.numeric(titanic2$Survived) -1 #Erstelle numerische Variable für Überleben
zusammenhang_metrisch_dichotom(titanic2$Age, titanic2$Survived_numeric)

# Untersuche den Zusammenhang zwischen Fahrpreis und Überleben
zusammenhang_metrisch_dichotom(titanic2$Fare, titanic2$Survived_numeric)

# (v) visualisierung_ueberlebensrate_klasse() Funktion anwenden
# Visualisierung der Überlebensrate nach Klasse und Geschlecht
visualisierung_ueberlebensrate_klasse(titanic2, "Pclass", "Survived", "Sex")

# Visualisierung der Überlebensrate nach Geschlecht und Klasse
visualisierung_ueberlebensrate_klasse(titanic2, "Sex", "Survived", "Pclass")

# (vi) Weitere Funktionen
# Verwende summary(), um einen Überblick über den Datensatz zu erhalten
summary(titanic2)

# Boxplot für den Fahrpreis nach Überleben
boxplot(titanic2$Fare ~ titanic2$Survived,
        col = "lightgreen",
        xlab = "Überlebt",
        ylab = "Fahrpreis",
        main = "Fahrpreis nach Überleben")

# Korrelation zwischen Alter und Fahrpreis
korrelation_metrisch(titanic2$Age, titanic2$Fare)

