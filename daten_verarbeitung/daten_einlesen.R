library(sf)
library(dplyr)
library(tidyr)
library(here)

# Daten einlesen
mietspiegel <- read.csv("daten/ADR_MSP27_20260513.csv", sep = ";",
                        colClasses = c(adressid = "character"))
geänderte_wohnlagen <- read.csv("daten/ADR_Wohnlage_Abschlagszonen_20260504.csv", sep = ";",
              colClasses = c(adressid = "character"))
data_raw <- read.csv("daten/ADR_Daten_Muenchen_20260224.csv", sep = ";",
                     colClasses = c(adressid = "character"))
lärm <- read.csv("daten/ADR_Laerm_Muenchen_20260504.csv", sep = ";",
                 colClasses = c(adressid = "character"))


lärm <- lärm %>%
  mutate(
    across(
      c(laerm_eisenbahn, laerm_industrie, laerm_strassen),
      ~ as.numeric(replace_na(., 0))
    )
  ) %>%
  mutate(
    laerm = pmax(laerm_eisenbahn, laerm_industrie, laerm_strassen)
  ) %>%
  select(-starts_with("laerm_"))

lärm <- lärm %>%
  mutate(adressid = format(adressid, scientific = FALSE, trim = TRUE))

data_raw <- data_raw %>%
  mutate(adressid = format(adressid, scientific = FALSE, trim = TRUE))

data_raw <- data_raw %>%
  left_join(
    lärm %>% select(adressid, laerm),
    by = "adressid"
  )

# Räumliche Daten einlesen
st_layers("daten/ADR_Daten_Muenchen_20260224.gpkg")
raeumliche_daten <- st_read("daten/ADR_Daten_Muenchen_20260224.gpkg")


raeumliche_daten <- raeumliche_daten %>%
  mutate(adressid = format(adressid, scientific = FALSE, trim = TRUE))

raeumliche_daten <- raeumliche_daten %>%
  left_join(
    lärm %>% select(adressid, laerm),
    by = "adressid"
  )

# Filzstiftgebiete ändern

raeumliche_daten <- raeumliche_daten %>%
  left_join(
    geänderte_wohnlagen %>%
      select(adressid, wohnlage_ebene_neu),
    by = "adressid"
  ) %>%
  mutate(
    wohnlage_ebene = coalesce(wohnlage_ebene_neu, wohnlage_ebene)
  ) %>%
  select(-wohnlage_ebene_neu)


# Räumliche Daten der Gebiete einlesen
new_mu_fl <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Miet_Flaeche_2023.shp"))
new_zent_b <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Zentraler_Bereich_2023.shp"))
new_miet_l <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Miet_Linie_2023.shp"))


# Umbenennen
wohnlagen_muc <- new_mu_fl
polygon_zentraler_bereich <- new_zent_b
wohnlage_grenzen <- new_miet_l
  

