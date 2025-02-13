# Funktionen für die Auswertung und Visualisierung

# R1i Statistiken für metrische Variablen

# R1ii Statistiken für kategoriale Variablen

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
