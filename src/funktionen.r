# Aufgabe 2: Funktionen für die Auswertung und Visualisierung

# R1i Statistiken für metrische Variablen
# Variablen muessen numerisch sein:
str(titanic)
is.numeric(titanic$Age) # TRUE
is.numeric(titanic$SibSp) # TRUE
is.numeric(titanic$Parch) # TRUE
is.numeric(titanic$Fare) # TRUE
## Alle anderen Variablen sind FALSE, also nicht numerisch und können nicht in diese Funktion eingesetzt werden.

# Bei numerischen Variablen kann man den Mittelwert, den Median, die Varianz, die Standardabweichung, das Minimum, das Maximum 
# und die Spannweite berechnen lassen:
metrisch <- function(x){
    print(list(Mittelwert = mean(x, na.rm = TRUE),
               Median = median(x, na.rm = TRUE),
               Varianz = var(x, na.rm = TRUE),
               Standardabweichung = sd(x, na.rm = TRUE),
               Minimum = min(x, na.rm = TRUE),
               Maximum = max(x, na.rm = TRUE),
               Spannweite = diff(range(x))))
}

# R1ii Statistiken für kategoriale Variablen
## Variablen sollten als Klasse "factor" oder "character" haben:
str(titanic)
class(titanic$Survived) # factor
class(titanic$Pclass) # ordered factor
class(titanic$Sex) # factor
class(titanic$Embarked) # factor
class(titanic$Anrede) # character
class(titanic$Bordseite) # character
class(titanic$Deck) # character

# Bei kategorialen Variablen kann man die absolute Haeufigkeit, die relative Haeufigkeit und den Modus berechnen lassen:
kategorial <- function(y){
  print(list(absolute.Haeufigkeit = table(y),
             relative.Haeufigkeit = prop.table(table(y)),
             Modus = names(table(y)[which.max(table(y))])))
}

# R1iii Zusammenhang kategoriale Variablen

# R1iv metrische und dichotomen Variablen

# R1v Visualisierung kategoriale Variablen
## geeignete Visualisierung von drei oder vier kategorialen Variablen
visualisierung_ueberlebensrate_klasse <- function(daten, x_var, fill_var, aufteilung) {
  ggplot(daten, aes(x = x_var, fill = fill_var)) +
    geom_bar() +
    facet_wrap(~ aufteilung) +
    labs(title = paste("Visualisierung von ", x_var, " nach ", fill_var, " und ", aufteilung),
         x = x_var,
         y = "Anzahl")
}
# Aufruf: visualisierung_ueberlebensrate_klasse(titanic, "Pclass", "Survived", "Sex")


# R1vi weitere Funktionen
