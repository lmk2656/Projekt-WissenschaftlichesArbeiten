#Pakete laden
library(dplyr)
library(stringr)

#laden Titanic-Datensatz
titanic <- read.csv("titanic.csv". stringsAsFactors = FALSE)


#1
#Extrahieren der Anrede aus dem Namen
titanic$Anrede <- str_extract(titanic$Name, "(?<=, )[^.]+\\.")


#2
#einheitliche Anrede erstellen
titanic <- titanic %>%
	mutate(Anrede = recode(Anrede, 
	#anreden werden geändert bspw. wird Ms. zu Miss.
		"Ms." = "Miss.", 
		"Mlle." = "Miss.", 
		"Mme." = "Mrs.", 
		"Master." = "Master." 
	))


#3
#Umkodieren der Variablen
titanic <- titanic %>%
	mutate(
	#variablen werden geändert, wie in teil 2
		Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes")),
		Sex = factor(Sex, levels = c("male", "female"), labels = c("Male", "Female")),
		Embarked = factor(Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southhamptons"))
	)


#4
#Pclass als factor speichern
titanic <- titanic %>%
	mutate(Pclass, levels = c(1,2,3), ordered = TRUE))
	
	
#5	
#fehlende Werte in Age erkennen
#durchschnittsalter berechnen
avg_age_per_title <- titanic %>%
	group_by(Anrede) %>%
	summarise(Mittelwert_Alter = mean(Age, na.rm = TRUE))
#in datensatz einfügen
titanic <- titanic %>%
	left_join(avg_age_per_title, by = "Anrede") %>%
	mutate(Age = ifelse(is.na(Age), Mittelwert_Alter, Age)) %>%
	select(-Mittelwert_Alter) #hilfsspalte entf
	
str(titanic) #struktur des datensatzes anzeigen
sum(is.na(titanic$Age)) #prüfen, ob alle alterswerte gefüllt wurden
