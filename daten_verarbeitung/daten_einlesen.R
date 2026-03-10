library(sf)

# Daten einlesen
data_raw <- read.csv("daten/ADR_Daten_Muenchen_20260224.csv", sep = ";")

# Räumliche Daten einlesen
st_layers("daten/ADR_Daten_Muenchen_20260224.gpkg")
raeumliche_daten <- st_read("daten/ADR_Daten_Muenchen_20260224.gpkg")

# Räumliche Daten der Gebiete einlesen
new_mu_fl <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Miet_Flaeche_2023.shp"))
new_zent_b <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Zentraler_Bereich_2023.shp"))
new_miet_l <- st_read(here("daten/Datenabgabe_TU_LMU_2024_02_01/Miet_Linie_2023.shp"))


# Umbenennen
wohnlagen_muc <- new_mu_fl
polygon_zentraler_bereich <- new_zent_b
wohnlage_grenzen <- new_miet_l
  

