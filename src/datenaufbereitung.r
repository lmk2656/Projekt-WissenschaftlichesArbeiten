#laden Titanic-Datensatz
titanic <- read.csv("titanic.csv")

#1
#Extrahieren der Anrede aus dem Namen
x <- function(name){
	Aufteilung <- unlist(strsplit(name, " "))
	Anreden <- c("Mr.", "Mrs.", "Miss.", "Ms.", "Master.", "Dr.", "Rev.", "Mme.", "Major.", "Lady", "Sir.", "Mlle.", "Capt.")
	if(Aufteilung[2] %in% Anreden){
		return(Aufteilung[2])
		}
	if(Aufteilung[3] %in% Anreden){
		return(Aufteilung[3])
		}
	if(Aufteilung[4] %in% Anreden){
		return(Aufteilung[4])
		}
	else{
		return(NA)
		}
	
}

titanic$Anrede <- sapply(titanic$Name, x)

#2
# einheitliche Anrede erstellen
titanic$Anrede <- gsub("Ms.", "Miss.", titanic$Anrede)
titanic$Anrede <- gsub("Mlle.", "Miss.", titanic$Anrede)
titanic$Anrede <- gsub("Mme.", "Mrs.", titanic$Anrede)

#3
# Umkodieren der Variablen als Factor
titanic$Survived <- as.factor(titanic$Survived) # 0 = No, 1 = Yes
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked) # C = Cherbourg, Q = Queenstown, S = Southhamptons


#4
# Pclass als ordered-factor 
titanic$Pclass <- factor(titanic$Pclass, levels = c(1,2,3), ordered = TRUE)
	
#5	
# Durchschnittsalter berechnen
# Median
median(titanic$Age, na.rm = TRUE) # 28
# Mittelwert
mean(Titanic$Age, na.rm = TRUE) # 29.36158
## Zur Vereinfachung wird mit dem Median weitergearbeitet. Dieser ist auch Ausreißerrobust.
# Personen ohne Altersangabe den Median als Alter zuordnen
titanic$Age[which(is.na(titanic$Age))] <- 28
