library(here)
library(dplyr)
source(here("daten_verarbeitung", "daten_einlesen.R"))


# Daten betrachten

summary(raeumliche_daten)
head(raeumliche_daten)
str(raeumliche_daten)

data <- raeumliche_daten

# Fixe geometry Spalte
st_geometry(data) <- "geom"

# Datentypen anpassen --------------------
# Kategoriale Variablen sicher in Faktoren umwandeln, OHNE die Geometrie zu berühren
data <- data %>% 
  mutate(across(c(gemarkung_, layer, geb_nutz, zentraler_bereich, grundsch_k, 
                  spielpl_k, kita_k, ortsz_k, wohnlage_ebene, wohnlage_bedeutung), 
                as.factor))


# Modelldatensatz
model_data <- data %>% 
  select(gemarkung_,
         layer,
         zentraler_bereich,
         ewo_adr,
         we_adr,
         anteil_gebauede_1bis2_we_adr,
         anteil_we_gebaeude_4bis9_geschosse_adr,
         geschossflaeche_laeden_gastro_adr,
         erreichbarkeit_gr10ha_in_metern_adr, # (!) haben beide die gleiche 
         erreichbarkeit_u10ha_in_metern_adr,   # Eigenschaften 
         erreichbarkeit_innenstadt_in_minuten_adr,
         erreichbarkeit_naechstehaltestelle_in_minuten_adr,
         wohnflaeche_je_ew_adr,
         wohnberech, # nochmal schauen was diese variablen beschreiben
         wohnbere_1, #
         wohnbere_2, #
         grundschul,
         spielplatz,
         kitakigaho,
         ortszentru,
         wohnlage_bedeutung,
         brw,
         kaufkraft_pro_kopf_blosei,
         kaufkraft_pro_kopf_blo,
         flaeche_qm_sv,
        # sum_flaeche_vf_sv, # evt hinzufügen
         anteil_vf_sv,
        # sum_gf_sv,
         anteil_gf_sv,
        # sum_ve_sv,
         anteil_ve_sv,
        # sum_beb_sv,
         anteil_beb_sv,
         geom)
model_data_complete <- na.omit(model_data) # Zeilen mit NA entfernen




# Modelldatensatz erstellen

# NA's im ganzen Datensatz zählen
na_counts <- sapply(model_data, function(x) sum(is.na(x)))
na_counts

# Zeilen mit NA's entfernen
data_complete <- na.omit(data)


# Übersicht über Wohnlage
table(data_complete$wohnlage_ebene)
table(data$wohnlage_ebene) 
table(data_complete$wohnlage_bedeutung)
table(data$wohnlage_bedeutung) 

# Ohne imputen fallen verhältnismäßig viele Wohnlagen "6" weg
table(data_complete$wohnlage_ebene)/table(data_raw$wohnlage_ebene) 

# Gebäudenutzung nur privat? 
table(data_complete$geb_nutz)

# Anzahl an Ausprägungen zählen, schwierige Variablen identifizieren
sapply(data_complete, function(x) length(unique(x)))


