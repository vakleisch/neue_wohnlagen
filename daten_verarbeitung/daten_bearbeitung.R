library(here)
source(here("daten_verarbeitung", "daten_einlesen.R"))


# Daten betrachten

summary(data_raw)
head(data_raw)
str(data_raw)

# NA's im ganzen Datensatz zählen
na_counts <- sapply(data_raw, function(x) sum(is.na(x)))
na_counts

# Zeilen mit NA's entfernen
data_complete <- na.omit(data_raw)


# Übersicht über Wohnlage
table(data_complete$wohnlage_ebene)
table(data_raw$wohnlage_ebene) 

# Ohne imputen fallen verhältnismäßig viele Wohnlagen "6" weg
table(data_complete$wohnlage_ebene)/table(data_raw$wohnlage_ebene) 


