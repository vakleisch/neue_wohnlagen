library(here)
library(dplyr)
source(here("daten_verarbeitung", "daten_einlesen.R"))


mapping <- data.frame(
  EBENE = 1:6,
  Wohnlage = c(
    "durchschnittliche Lage",
    "gute Lage",
    "beste Lage",
    "zentrale durchschnittliche Lage",
    "zentrale gute Lage",
    "zentrale beste Lage"
  )
)

wohnlagen_muc2 <- wohnlagen_muc %>%
  left_join(mapping, by = "EBENE")

# Daten betrachten

summary(raeumliche_daten)
head(raeumliche_daten)
str(raeumliche_daten)

data <- raeumliche_daten

# Fixe geometry Spalte
st_geometry(data) <- "geom"

# Reihenfolge der Wohnlagen logisch festlegen
data$wohnlage_bedeutung <- factor(
  data$wohnlage_bedeutung,
  levels = c(
    "durchschnittliche Lage",
    "gute Lage",
    "beste Lage",
    "zentrale durchschnittliche Lage",
    "zentrale gute Lage",
    "zentrale beste Lage"
  )
)

# Datentypen anpassen --------------------
# Kategoriale Variablen sicher in Faktoren umwandeln, OHNE die Geometrie zu berühren
data <- data %>% 
  mutate(across(c(gemarkung_, layer, geb_nutz, zentraler_bereich, grundsch_k, 
                  spielpl_k, kita_k, ortsz_k, wohnlage_ebene, wohnlage_bedeutung), 
                as.factor))

# Kita, grundschul, ortszentru und kita variablen fixen (von char zu num)
data <- data %>%
  mutate(
    across(
      # 1. Welche Spalten sollen bearbeitet werden?
      c(kitakigaho, grundschul, spielplatz, ortszentru), 
      # 2. Was soll mit diesen Spalten passieren? (.x steht für die jeweilige Spalte)
      ~ case_when(
        .x == "n.A." ~ NA_real_, 
        TRUE ~ (as.numeric(sub(" - .*", "", .x)) + 
                  as.numeric(sub(".* - ", "", .x))) / 2
      ),
      # 3. Wie sollen die neuen Spalten heißen? (z.B. grundschul_num)
      .names = "{.col}_num" 
    )
  )



# Log transformieren (Rechtsschiefe Vars)

log_vars <- c(
  "ewo_adr", 
  "we_adr", 
  "anteil_we_gebaeude_4bis9_geschosse_adr", 
  "geschossflaeche_laeden_gastro_adr", 
  "wohnflaeche_je_ew_adr", 
  "wohnberech", 
  "wohnbere_1", 
  "wohnbere_2", 
  "brw", 
  "sum_gf_sv"
)


# Negative Werte korrigieren
summary(data %>% select(all_of(log_vars)))
# Probleme:  we_adr, wohnflaeche_je_ew_adr, geschossflaeche_laeden_gastro_adr

data <- data %>%
  # Schritt A: Mache aus allen Werten unter 0 saubere NA-Werte
 # mutate(across(
 #   all_of(log_vars), 
 #   ~ ifelse(. < 0, NA, .)
  #)) %>%
  # Schritt B: Jetzt gefahrlos logarithmieren
  mutate(across(
    all_of(log_vars), 
    ~ log1p(.), 
    .names = "{.col}_log"
  ))




# Modelldatensatz
model_data <- data %>% 
  select(#gemarkung_,
        # ewo_adr_log,
        # we_adr_log,
         erreichbarkeit_gr10ha_in_metern_adr,
         erreichbarkeit_innenstadt_in_minuten_adr,
         erreichbarkeit_naechstehaltestelle_in_minuten_adr,
         grundschul_num,
         spielplatz_num,
         kitakigaho_num,
         ortszentru_num,
         wohnlage_ebene,
         wohnlage_bedeutung,
         brw_log,
         anteil_vf_sv,
         anteil_gf_sv,
        # anteil_beb_sv,
         geom)
model_data_complete <- na.omit(model_data) # Zeilen mit NA entfernen

# Für gam modell Ausprägungen Zielvariable anpassen
model_data_complete$wohnlage_ebene <- as.numeric(model_data_complete$wohnlage_ebene) - 1

# Nach zentral und nicht-zentral unterscheiden
# zentral (filter for Wohnlage = 3, 4, 5)
model_data_complete_zentral <- model_data_complete %>%
  filter(wohnlage_ebene %in% c(3, 4, 5)) %>%
  mutate(wohnlage_ebene = wohnlage_ebene - 3) # Encoding anpassen

# außerhalb (filter for Wohnlage = 0, 1, 2)
model_data_complete_ausserhalb <- model_data_complete %>%
  filter(wohnlage_ebene %in% c(0, 1, 2))



# NA's im Modelldatensatz zählen
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


