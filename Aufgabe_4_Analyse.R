# Skript zur Analyse des Titanic-Datensatzes mit Funktionen aus Skript 1

## Aufgabe 3

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
titanic2$Pclass <- factor(titanic2$Pclass, levels = c(1,2,3), ordered = TRUE)
titanic2$Sex <- as.factor(titanic2$Sex)

# 2. Deskriptive Statistik und Visualisierung

# (i) metrisch() Funktion anwenden
# Untersuche das Alter
metrisch(titanic2$Age)

##Ergebnis:
#Mittelwert (Durchschnitt): 28,29 Jahre
#Median: 26 Jahre
#Varianz: 123,08
#Standardabweichung: 11,09
#Minimum: 3 Jahre
#Maximum: 56 Jahre
#Spannweite: 53 Jahre


# Untersuche den Fahrpreis
metrisch(titanic2$Fare)

##Ergebnis:
#Mittelwert (Durchschnitt): 19,71
#Median: 10,51
#Varianz: 515,26
#Standardabweichung: 22,71
#Minimum: 5,00
#Maximum: 83,16
#Spannweite: 78,16


# (ii) kategorial() Funktion anwenden
# Untersuche die Überlebensrate
kategorial(titanic2$Survived)

##Ergebnis:
#Absolute Häufigkeit:
  #Überlebt (1): 7
  #Nicht überlebt (0): 15
#Relative Häufigkeit:
  #Überlebt: 31,8 %
  #Nicht überlebt: 68,2 %
#Modus (häufigste Kategorie):
  #0 (Nicht überlebt)

# Untersuche die Ticketklasse
kategorial(titanic2$Pclass)

##Ergebnis:
#Absolute Häufigkeit:
#1. Klasse: 40
#2. Klasse: 36
#3. Klasse: 94
#Relative Häufigkeit:
#1. Klasse: 23,5%
#2. Klasse: 21.2%
#3. Klasse: 55,30%
#Modus (häufigste Kategorie):
#Die 3. Klasse ist die häufigste Ticketklasse


# (iii) zusammenhang_kategorial() Funktion anwenden
# Untersuche den Zusammenhang zwischen Überleben und Geschlecht
zusammenhang_kategorial(titanic2$Survived, titanic2$Sex)

#Ergebnis:
#Kreuztabelle:
  #Von den Frauen überlebten 6 und 3 starben.
  #Von den Männern überlebten 2 und 13 starben.

#Chi-Quadrat-Test:
  #Der p-Wert ist 0.006743, was deutlich unter dem üblichen Signifikanzniveau von 0.05 liegt.
  #Dies bedeutet, dass der Zusammenhang zwischen Geschlecht und Überleben statistisch signifikant ist.
  #Wir können die Nullhypothese, dass kein Zusammenhang besteht, mit hoher Sicherheit ablehnen.

#Stärke des Zusammenhangs:
   #Der Wert 0.5777108 zeigt eine moderate bis starke Assoziation zwischen den Variablen.
   #Dieser Wert liegt auf einer Skala von 0 (kein Zusammenhang) bis 1 (perfekter Zusammenhang).

#Schlussfolgerung:
  #Es besteht ein statistisch signifikanter und relativ starker Zusammenhang zwischen dem Geschlecht einer Person und ihrer Überlebenschance auf der Titanic. 
  #Frauen hatten eine deutlich höhere Überlebenswahrscheinlichkeit als Männer.
# Untersuche den Zusammenhang zwischen Überleben und Ticketklasse


## Untersuche den Zusammenhang zwischen Überleben und Klasse
zusammenhang_kategorial(titanic2$Survived, titanic2$Pclass)

#Ergebnis:
#Kreuztabelle:
  #In der 1. Klasse starben 74 Passagiere und 58 überlebten.
  #In der 2. Klasse starben 54 Passagiere und 41 überlebten.
  #In der 3. Klasse starben 134 Passagiere und 71 überlebten.

#Chi-Quadrat-Test:
  #Der p-Wert beträgt 6.507e-07, was deutlich unter dem üblichen Signifikanzniveau von 0.05 liegt.
  #Dies bedeutet, dass der Zusammenhang zwischen Ticketklasse und Überleben statistisch signifikant ist.
  #Wir können die Nullhypothese, dass kein Zusammenhang besteht, mit hoher Sicherheit ablehnen.

#Stärke des Zusammenhangs:
  #Der Wert 0.1147854 zeigt eine geringe Assoziation zwischen den Variablen.
  #Dieser Wert liegt auf einer Skala von 0 (kein Zusammenhang) bis 1 (perfekter Zusammenhang).

#Schlussfolgerung:
  #Es besteht ein statistisch signifikanter Zusammenhang zwischen der Ticketklasse und der Überlebenschance auf der Titanic. Es deutet sich an, dass Passagiere der höheren Klassen tendenziell bessere Überlebenschancen hatten, aber der Zusammenhang ist nicht sehr stark.


# (iv) zusammenhang_metrisch_dichotom() Funktion anwenden
# Untersuche den Zusammenhang zwischen Alter und Überleben
titanic2$Survived_factor <- as.factor(titanic2$Survived)-1 # Variable muss factor sein
zusammenhang_metrisch_dichotom(titanic2$Age, titanic2$Survived_factor)
zusammenhang_metrisch_dichotom(titanic2$Fare, titanic2$Survived_factor)

#Ergebnis:
#Gesamtstatistik:
  #Das Durchschnittsalter aller Passagiere beträgt etwa 29.7 Jahre.
  #Die Standardabweichung des Alters beträgt etwa 14.5 Jahre, was auf eine breite Altersverteilung hindeutet.

#Statistik nach Gruppen:
  #Nicht-Überlebende (0):
    #Durchschnittsalter: 30.6 Jahre
    #Standardabweichung: 14.2 Jahre
  #Überlebende (1):
    #Durchschnittsalter: 28.3 Jahre
    #Standardabweichung: 15.0 Jahre

#T-Test:
  #Der p-Wert beträgt 0.04845, was knapp unter dem üblichen Signifikanzniveau von 0.05 liegt.
  #Dies deutet auf einen statistisch signifikanten Unterschied im Durchschnittsalter zwischen Überlebenden und Nicht-Überlebenden hin.
  #Die Differenz der Mittelwerte beträgt etwa 2.3 Jahre (30.6 - 28.3).

#Schlussfolgerung:
  #Es gibt einen statistisch signifikanten, aber relativ kleinen Unterschied im Durchschnittsalter zwischen Überlebenden und Nicht-Überlebenden der Titanic-Katastrophe. 
  #Überlebende waren im Durchschnitt etwas jünger als Nicht-Überlebende, aber der Unterschied ist mit etwa 2.3 Jahren nicht sehr groß.
# Untersuche den Zusammenhang zwischen Fahrpreis und Überleben


# (v) visualisierung_ueberlebensrate_klasse() Funktion anwenden
# Visualisierung der Überlebensrate nach Klasse und Geschlecht
visualisierung_ueberlebensrate_klasse(titanic2, "Pclass", "Survived", "Sex")

# Visualisierung der Überlebensrate nach Geschlecht und Klasse
visualisierung_ueberlebensrate_klasse(titanic2, "Sex", "Survived", "Pclass")

#Ergebnis:
#Überlebensraten nach Klasse (innerhalb jedes Geschlechts):
  #Für Männer: Ja, die Überlebensrate in der 1. Klasse war deutlich höher als in der 3. Klasse. Dies ist ein sehr deutlicher Trend im Plot.
  #Für Frauen: Ja, auch für Frauen war die Überlebensrate in der 1. Klasse tendenziell höher als in der 3. Klasse. Der Unterschied war jedoch möglicherweise nicht ganz so ausgeprägt wie bei den Männern.

# (vi) Weitere Funktionen
# Verwende summary(), um einen Überblick über den Datensatz zu erhalten
summary(titanic2)

# Boxplot für den Fahrpreis nach Überleben
boxplot(titanic2$Fare ~ titanic2$Survived,
        col = "lightgreen",
        xlab = "Überlebt",
        ylab = "Fahrpreis",
        main = "Zusammenhang zwischen dem Fahrpreis und dem Überleben")

#Ergebnis:
  #Es besteht ein positiver Zusammenhang zwischen dem Fahrpreis und der Überlebenswahrscheinlichkeit.
  #Passagiere mit teureren Tickets (wahrscheinlich in höheren Klassen) hatten im Durchschnitt bessere Überlebenschancen.
  #Die größere Streuung bei den Überlebenden deutet darauf hin, dass auch einige Passagiere mit günstigeren Tickets überlebten, aber die Tendenz geht zu höheren Überlebenschancen bei höheren Fahrpreisen.

# Korrelation zwischen Alter und Fahrpreis
korrelation_metrisch(titanic2$Age, titanic2$Fare)

#Ergebnis:
  #Die Korrelation von 0.09263824 deutet auf eine sehr schwache positive lineare Beziehung zwischen Alter und Fahrpreis hin. 
  #Das bedeutet, dass es keine starke Tendenz gibt, dass ältere Passagiere tendenziell deutlich höhere oder niedrigere Fahrpreise bezahlt haben.
  #Der Zusammenhang ist sehr gering.
