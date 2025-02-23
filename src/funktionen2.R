# Funktionen-R-Skript 2: Interne Hilferfunktionen

# Hilfsfunktion zur Überprüfung, ob eine Variable numerisch ist
is.numeric(x)

# Hilfsfunktion zur Berechnung der Spannweite
calculate_range <- function(x) {
  return(diff(range(x, na.rm = TRUE)))
}

# Hilfsfunktion zur Ermittlung des Modus
find_mode <- function(x) {
  return(names(table(x)[which.max(table(x))]))
}

# Hilfsfunktion zur Berechnung der Stärke des Zusammenhangs
calculate_association_strength <- function(chi_statistic, n, min_dim) {
  return(sqrt(chi_statistic / (n * (min_dim - 1))))
}

# Hilfsfunktion zur Überprüfung, ob eine Variable dichotom ist
is_dichotomous <- function(x) {
  return(is.factor(x) || is.logical(x) || length(unique(x)) == 2)
}

# Hilfsfunktion zur Berechnung von Mittelwert und Standardabweichung
calculate_mean_sd <- function(x) {
  return(list(
    Mittelwert = mean(x, na.rm = TRUE),
    Standardabweichung = sd(x, na.rm = TRUE)
  ))
}
