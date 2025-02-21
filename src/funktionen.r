# Aufgabe 2: Funktionen für die Auswertung und Visualisierung

# neuen Datensatz einlesen:
titanic2 <- read.csv("titanic2.csv")

# R1i Statistiken für metrische Variablen
# Variablen muessen numerisch sein:
str(titanic2)
is.numeric(titanic2$Age) # TRUE
is.numeric(titanic2$SibSp) # TRUE
is.numeric(titanic2$Parch) # TRUE
is.numeric(titanic2$Fare) # TRUE
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
str(titanic2)
class(titanic2$Survived) # factor
class(titanic2$Pclass) # ordered factor
class(titanic2$Sex) # factor
class(titanic2$Embarked) # factor
class(titanic2$Anrede) # character
class(titanic2$Bordseite) # character
class(titanic2$Deck) # character

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
# Aufruf: visualisierung_ueberlebensrate_klasse(titanic, "Pclass", "Survived", "Bordseite")
# Aufruf: visualisierung_ueberlebensrate_klasse(titanic, "Deck", "Pclass", "Survived")


# R1vi weitere Funktionen
summary(titanic2)

boxplot(titanic2$Pclass ~ titanic2$Survived, col = "lightblue", xlab = "Überlebt", 
        ylab = "Klasse", yaxt = "n", las = 1, 
        main = "Zusammenhang zwischen der Klasse und dem Überleben")
axis(2, 1:3, las = 1, labels = c("1", "2", "3"))

