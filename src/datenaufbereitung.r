## Aufgabe 1:

# laden Titanic-Datensatz
titanic <- read.csv("titanic.csv")

# 1
# Extrahieren der Anrede aus dem Namen
Anreden <- function(name){
	Aufteilung <- unlist(strsplit(name, " "))
	Anrede <- c("Mr.", "Mrs.", "Miss.", "Ms.", "Master.", "Dr.", "Rev.", "Mme.", "Major.", "Lady.", "Sir.", "Mlle.", "Capt.", "Don.", "Col.", "Countess.", "Jonkheer.")
	if(Aufteilung[2] %in% Anrede){
		return(Aufteilung[2])
	}
	if(Aufteilung[3] %in% Anrede){
		return(Aufteilung[3])
	}
	if(Aufteilung[4] %in% Anrede){
		return(Aufteilung[4])
	}
	else{
		return(NA)
	}
}

titanic$Anrede <- sapply(titanic$Name, Anreden)

# einheitliche Anrede erstellen
titanic$Anrede <- gsub("Ms.", "Miss.", titanic$Anrede)
titanic$Anrede <- gsub("Mlle.", "Miss.", titanic$Anrede)
titanic$Anrede <- gsub("Mme.", "Mrs.", titanic$Anrede)

# 2
# Umkodieren der Variablen als Factor
titanic$Survived <- as.factor(titanic$Survived) # 0 = No, 1 = Yes
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked) # C = Cherbourg, Q = Queenstown, S = Southhamptons


# 3
# Pclass als ordered-factor 
titanic$Pclass <- factor(titanic$Pclass, levels = c(1,2,3), ordered = TRUE)
	
# 4	
# Durchschnittsalter der einzelnen Altersgruppen berechnen
# Prüfen, welche der Personen keine Altersangaben und bestimmte Anreden haben
which(is.na(titanic$Age) & titanic$Anrede == "Mrs.")
which(is.na(titanic$Age) & titanic$Anrede == "Miss.")
which(is.na(titanic$Age) & titanic$Anrede == "Mr.")
which(is.na(titanic$Age) & titanic$Anrede == "Master.")
which(is.na(titanic$Age) & titanic$Anrede == "Dr.")

# Median
Unterteilung1 <- subset(titanic, titanic$Anrede == "Mrs.")
median(Unterteilung1$Age, na.rm = TRUE) # 35
Unterteilung2 <- subset(titanic, titanic$Anrede == "Miss.")
median(Unterteilung2$Age, na.rm = TRUE) # 21
Unterteilung3 <- subset(titanic, titanic$Anrede == "Mr.")
median(Unterteilung3$Age, na.rm = TRUE) # 30
Unterteilung4 <- subset(titanic, titanic$Anrede == "Master.")
median(Unterteilung4$Age, na.rm = TRUE) # 3.5
Unterteilung5 <- subset(titanic, titanic$Anrede == "Dr.")
median(Unterteilung5$Age, na.rm = TRUE) # 46.5

# Mittelwert
mean(Unterteilung1$Age, na.rm = TRUE) # 35.78899
mean(Unterteilung2$Age, na.rm = TRUE) # 21.80405
mean(Unterteilung3$Age, na.rm = TRUE) # 32.36809
mean(Unterteilung4$Age, na.rm = TRUE) # 4.574167
mean(Unterteilung5$Age, na.rm = TRUE) # 42

# Zur Vereinfachung wird mit dem Median weitergearbeitet. Dieser ist auch Ausreißerrobust.

# Diesen Personen den Median als Alter zuordnen:
titanic$Age[which(is.na(titanic$Age) & titanic$Anrede == "Mrs.")] <- 35
titanic$Age[which(is.na(titanic$Age) & titanic$Anrede == "Miss.")] <- 21
titanic$Age[which(is.na(titanic$Age) & titanic$Anrede == "Mr.")] <- 30
titanic$Age[which(is.na(titanic$Age) & titanic$Anrede == "Master.")] <- 3.5
titanic$Age[which(is.na(titanic$Age) & titanic$Anrede == "Dr.")] <- 46.5

# Prüfen, ob allen Personen nun ein Alter zugeordnet wurde:
which(is.na(titanic$Age)) 

# 5
# Variable "Bordseite" erstellen mit den Zahlen und sonst NA:
titanic$Bordseite<- gsub("[[:alpha:]]+", "", titanic$Cabin)
# Manche Nummern wie z.B. "23 25 27" in "23" umwandeln, da nur relevant, ob ungerade oder gerade Zahl:
titanic$Bordseite[28] <- "23"
titanic$Bordseite[89] <- "23"
titanic$Bordseite[98] <- "10"
titanic$Bordseite[119] <- "58"
titanic$Bordseite[298] <- "22"
titanic$Bordseite[306] <- "22"
titanic$Bordseite[342] <- "23"
titanic$Bordseite[391] <- "96"
titanic$Bordseite[436] <- "96"
titanic$Bordseite[439] <- "23"
titanic$Bordseite[499] <- "22"
titanic$Bordseite[680] <- "51"
titanic$Bordseite[700] <- "63"
titanic$Bordseite[701] <- "62"
titanic$Bordseite[764] <- "96"
titanic$Bordseite[790] <- "82"
titanic$Bordseite[499] <- "22"
titanic$Bordseite[803] <- "96"
titanic$Bordseite[873] <- "51"

titanic$Bordseite[312] <- "57.66" # da sowohl gerade als auch ungerade Zahlen als Zimmernummer
titanic$Bordseite[743] <- "57.66" # da sowohl gerade als auch ungerade Zahlen als Zimmernummer

un_gerade <- as.numeric(titanic$Bordseite) %% 2 
titanic$Bordseite <- ifelse(un_gerade == 0, "Backbord", ifelse(un_gerade == "1.66", "Backbord und Steuerbord", "Steuerbord"))

titanic$Bordseite[which(titanic$Bordseite == "")] <- "NA"

# Varibale "Deck" erstellen mit den Buchstaben und sonst NA:
titanic$Deck<- gsub("[0-9]+", "", titanic$Cabin)
titanic$Deck[which(titanic$Deck == "")] <- "NA"

# 6
# Die Variablen „PassengerID“, „Name“, „Ticket“ und „Cabin“ aus dem Datensatz entfernen:
titanic <- titanic[,-1] # Variable "PassengerID" entfernt
titanic <- titanic[,-3] # Variable "Name" entfernt
titanic <- titanic[,-7] # Variable "Ticket" entfernt
titanic <- titanic[,-8] # Variable "Cabin" entfernt


## neuer Datensatz:
titanic
