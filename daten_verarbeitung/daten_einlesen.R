library(sf)

# Daten einlesen
data_raw <- read.csv("daten/ADR_Daten_Muenchen_20260224.csv", sep = ";")

# Räumliche Daten einlesen
st_layers("daten/ADR_Daten_Muenchen_20260224.gpkg")
raeumliche_daten <- st_read("daten/ADR_Daten_Muenchen_20260224.gpkg")
  

