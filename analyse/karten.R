library(rgl)
library(here)
library(ggplot2)
library(sf)
library(patchwork)
library(dplyr)
library(leaflet)
library(osmdata)
library(htmlwidgets)
library(tmaptools)
source(here("analyse", "model_evaluation.R"))
source(here("analyse", "plot_funktionen.R"))
source(here("daten_verarbeitung", "daten_bearbeitung.R"))

# Modelle laden
model_gam_zentral <- readRDS("modelle/gam_model_zentral.rds")
model_gam_ausserhalb<- readRDS("modelle/gam_model_ausserhalb_b.rds")

wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)

# Lagen der Stadt
wohnlagen_muc_zentral <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
wohnlagen_muc_ausserhalb <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("durchschnittliche Lage",
                         "gute Lage",
                         "beste Lage"))

# Wohnlagegrenzen für zentral und nicht zentral
# Finde alle Linien, die innerhalb zentraler Wohnlagen liegen
linien_in_zentral <- st_intersects(wohnlage_grenzen, wohnlagen_muc_zentral, sparse = FALSE)

# Nur die Linien, die je passen
wohnlage_grenzen_ausserhalb <- wohnlage_grenzen[!apply(linien_in_zentral, 1, any), ]
wohnlage_grenzen_zentral <- wohnlage_grenzen[apply(linien_in_zentral, 1, any), ]


# Daten erstellen
fehler_model_gam_zentral <- missclassification_data_zentral(model_gam_zentral, 
                                                            data = model_data_complete_zentral,
                                                            predict_fun = predict_labels_discr)
fehler_model_gam_ausserhalb <- missclassification_data_ausserhalb(model_gam_ausserhalb, 
                                                                  data = model_data_complete_ausserhalb,
                                                                  predict_fun = predict_labels_discr)
korrekt_model_gam_zentral <- korrekte_vorhersagen_zentral(model_gam_zentral, 
                                                          data = model_data_complete_zentral,
                                                          predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb, 
                                                                data = model_data_complete_ausserhalb,
                                                                predict_fun = predict_labels_discr)

# in sf Objekte umwandeln
fehler_model_gam_zentral <- st_as_sf(fehler_model_gam_zentral)
fehler_model_gam_zentral <- fehler_model_gam_zentral %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_ausserhalb <- st_as_sf(fehler_model_gam_ausserhalb)
fehler_model_gam_ausserhalb <- fehler_model_gam_ausserhalb %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage",
                         "gute Lage",
                         "beste Lage"))
korrekt_model_gam_zentral <- st_as_sf(korrekt_model_gam_zentral)
korrekt_model_gam_zentral <- korrekt_model_gam_zentral %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb <- st_as_sf(korrekt_model_gam_ausserhalb)
korrekt_model_gam_ausserhalb <- korrekt_model_gam_ausserhalb %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage",
                         "gute Lage",
                         "beste Lage"))
model_data_complete_sf <- st_as_sf(model_data_complete)
model_data_complete_zentral_sf <- st_as_sf(model_data_complete_zentral)

# Hilfsfunktion zum Bereinigen von sf-Objekten
prepare_sf_object <- function(sf_obj) {
  sf_obj %>%
    st_zm(drop = TRUE, what = "ZM") %>%     # Z- und M-Dimensionen entfernen
    st_make_valid()                         # ungültige Geometrien reparieren
}

# Wende es auf alle sf-Objekte an
wohnlagen_muc2 <- prepare_sf_object(wohnlagen_muc2)
# Wende es auf alle sf-Objekte an
wohnlagen_muc2 <- prepare_sf_object(wohnlagen_muc2)
wohnlagen_muc_zentral <- prepare_sf_object(wohnlagen_muc_zentral)
wohnlagen_muc_ausserhalb <- prepare_sf_object(wohnlagen_muc_ausserhalb)

model_data_complete <- prepare_sf_object(model_data_complete)
model_data_complete_sf <- prepare_sf_object(model_data_complete_sf)
model_data_complete_zentral <- prepare_sf_object(model_data_complete_zentral)
model_data_complete_zentral_sf <- prepare_sf_object(model_data_complete_zentral_sf)
model_data_complete_ausserhalb <- prepare_sf_object(model_data_complete_ausserhalb)

fehler_model_gam_zentral <- prepare_sf_object(fehler_model_gam_zentral)
fehler_model_gam_ausserhalb <- prepare_sf_object(fehler_model_gam_ausserhalb)

korrekt_model_gam_zentral <- prepare_sf_object(korrekt_model_gam_zentral)
korrekt_model_gam_ausserhalb <- prepare_sf_object(korrekt_model_gam_ausserhalb)

wohnlage_grenzen <- prepare_sf_object(wohnlage_grenzen)
wohnlage_grenzen_zentral <- prepare_sf_object(wohnlage_grenzen_zentral)
wohnlage_grenzen_ausserhalb <- prepare_sf_object(wohnlage_grenzen_ausserhalb)


# Beide fehler datensätze kombinieren:
# Gleiche Levels für beide Spalten vor dem Zusammenfügen
levels_kombiniert <- c(
  "durchschnittliche Lage",
  "gute Lage",
  "beste Lage",
  "zentrale durchschnittliche Lage",
  "zentrale gute Lage",
  "zentrale beste Lage"
)

# Setze beide Faktoren auf denselben Level-Satz
# normal
fehler_model_gam_zentral$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_zentral$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb$Wohnlage_wahr,
  levels = levels_kombiniert)

# Jetzt kombinieren 
fehler_model_gam_kombiniert <- rbind(fehler_model_gam_zentral, fehler_model_gam_ausserhalb)
korrekt_model_gam_kombiniert <- rbind(korrekt_model_gam_zentral, korrekt_model_gam_ausserhalb)

# WGS84 sicherstellen
wohnlagen_muc_wgs <- wohnlagen_muc2 %>%
  st_transform(4326) %>%
  mutate(color = case_when(
    Wohnlage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage == "gute Lage" ~ "#afe391",
    Wohnlage == "beste Lage" ~ "#7FCDBB",
    Wohnlage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage == "zentrale beste Lage" ~ "#271352"
  ))

model_data_complete_wgs <- st_transform(model_data_complete, crs = 4326)

fehler_model_gam_ausserhalb_wgs <- st_transform(fehler_model_gam_ausserhalb, crs = 4326)
fehler_model_gam_zentral_wgs    <- st_transform(fehler_model_gam_zentral,    crs = 4326)

fehler_model_gam_kombiniert_wgs <- st_transform(fehler_model_gam_kombiniert, crs = 4326)

korrekt_model_gam_kombiniert_wgs <- st_transform(korrekt_model_gam_kombiniert, crs = 4326)

# Farbzuordung der Punkte
fehler_model_gam_kombiniert_wgs <- fehler_model_gam_kombiniert_wgs %>% 
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))

model_data_complete_wgs <- model_data_complete_wgs %>%
  mutate(color = case_when(wohnlage_ebene == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           wohnlage_ebene == "gute Lage (außerhalb)" ~ "#afe391",
                           wohnlage_ebene == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           wohnlage_ebene == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           wohnlage_ebene == "zentrale gute Lage" ~ "#1f5a82",
                           wohnlage_ebene == "zentrale beste Lage" ~ "#271352"))

korrekt_model_gam_kombiniert_wgs <- korrekt_model_gam_kombiniert_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))

# Wohnlage_grenzen
wohnlage_grenzen_wgs <- st_transform(wohnlage_grenzen, crs = 4326)


# Interaktive Karten erstellen
# Hintergrunddaten: Straßen aus OpenStreetMap

# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_wgs,
    fillColor = fehler_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_wgs,
    fillColor = korrekt_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~wohnlage_bedeutung,
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model

# Speichern als html
saveWidget(interaktive_karte_model, file = "interaktive_karten/interaktive_karte_model.html", selfcontained = TRUE)
#browseURL("interaktive_karten/interaktive_karte_model.html")


interaktive_karte_model_werte <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_wgs,
    fillColor = fehler_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    popup = ~paste0(
      "<b>Distanz Grünfläche (>10ha):</b> ", erreichbarkeit_gr10ha_in_metern_adr, "<br>",
      "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", erreichbarkeit_innenstadt_in_minuten_adr, "<br>",
      "<b>Erreichbarkeit nächste Haltestelle (min):</b> ", erreichbarkeit_naechstehaltestelle_in_minuten_adr, "<br>",
      "<b>Fußweg (m) Grundschule:</b> ", grundschul_num, "<br>",
      "<b>Fußweg (m) Spielplatz:</b> ", spielplatz_num, "<br>",
      "<b>Fußweg (m) Kita:</b> ", kitakigaho_num, "<br>",
      "<b>Fußweg (m) Ortszentrum:</b> ", ortszentru_num, "<br>", 
      "<b>log(Bodenrichtwert):</b> ", round(brw_log,2), "<br>",
      "<b>Bodenrichtwert:</b> ", brw, "<br>",
      "<b>Anteil Verkehrsfläche im Viertel (%):</b> ", round(anteil_vf_sv,2), "<br>",
      "<b>Anteil Grünfläche im Viertel (%):</b> ", round(anteil_gf_sv, 2), "<br>"   
    ),
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_wgs,
    fillColor = korrekt_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~wohnlage_bedeutung,
    popup = ~paste0(
      "<b>Distanz Grünfläche (>10ha):</b> ", erreichbarkeit_gr10ha_in_metern_adr, "<br>",
      "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", erreichbarkeit_innenstadt_in_minuten_adr, "<br>",
      "<b>Erreichbarkeit nächste Haltestelle (min):</b> ", erreichbarkeit_naechstehaltestelle_in_minuten_adr, "<br>",
      "<b>Fußweg (m) Grundschule:</b> ", grundschul_num, "<br>",
      "<b>Fußweg (m) Spielplatz:</b> ", spielplatz_num, "<br>",
      "<b>Fußweg (m) Kita:</b> ", kitakigaho_num, "<br>",
      "<b>Fußweg (m) Ortszentrum:</b> ", ortszentru_num, "<br>", 
      "<b>log(Bodenrichtwert):</b> ", round(brw_log,2), "<br>",
      "<b>Bodenrichtwert:</b> ", brw, "<br>",
      "<b>Anteil Verkehrsfläche im Viertel (%):</b> ", round(anteil_vf_sv,2), "<br>",
      "<b>Anteil Grünfläche im Viertel (%):</b> ", round(anteil_gf_sv, 2), "<br>" 
    ),
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model_werte

saveWidget(interaktive_karte_model_werte, file = "interaktive_karten/interaktive_karte_model_werte.html", selfcontained = TRUE)
#browseURL("interaktive_karten/interaktive_karte_model_werte.html")



# VALIDIERUNG

# Räumliche Verschneidung (Spatial Join)
# Wir ziehen uns einfach die EBENE aus dem Flächen-Polygon
punkte_validierung <- st_join(punkte_sf, wohnlagen_muc, join = st_intersects)

# Kontrolle und Abgleich
punkte_validierung <- punkte_validierung %>%
  mutate(
    # Wir übersetzen die Zahlen direkt in deine Kategorien
    Erwartete_Wohnlage_Raum = case_when(
      EBENE == 1 ~ "durchschnittliche Lage",
      EBENE == 2 ~ "gute Lage",
      EBENE == 3 ~ "beste Lage",
      EBENE == 4 ~ "zentrale durchschnittliche Lage",
      EBENE == 5 ~ "zentrale gute Lage",
      EBENE == 6 ~ "zentrale beste Lage",
      TRUE ~ NA_character_ # Falls ein Punkt ins Leere fällt (z.B. außerhalb Münchens)
    ),
    
    # Abgleich: Stimmt unsere übersetzte Geodaten-Lage mit deinen echten Labels überein?
    Lage_stimmt_ueberein = as.character(Wohnlage_wahr) == Erwartete_Wohnlage_Raum
  )

# Auswertung anzeigen
cat("\n=== ERGEBNIS DER VALIDIERUNG ===\n")
print(table(Korrekt_Platziert = punkte_validierung$Lage_stimmt_ueberein, useNA = "always"))

# Abweichler extrahieren, falls es welche gibt
punkte_fehlerhaft_platziert <- punkte_validierung %>% 
  filter(Lage_stimmt_ueberein == FALSE | is.na(Lage_stimmt_ueberein)) %>%
  select(Wohnlage_wahr, Erwartete_Wohnlage_Raum, EBENE)

cat("\nAnzahl der Fehler: ", nrow(punkte_fehlerhaft_platziert), "\n")





library(leaflet)
library(sf)
library(dplyr)

# 1. Daten in die zwei Fehler-Gruppen aufteilen und für Leaflet transformieren (WGS84)
punkte_echte_fehler_wgs <- punkte_fehlerhaft_platziert %>%
  filter(!is.na(Erwartete_Wohnlage_Raum)) %>%
  st_transform(4326)

punkte_na_faelle_wgs <- punkte_fehlerhaft_platziert %>%
  filter(is.na(Erwartete_Wohnlage_Raum)) %>%
  st_transform(4326)

# 2. Die interaktive Karte mit Ebenen-Steuerung (Layer Control) erstellen
karte_validierung_gefiltert <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrund: Die Wohnlagen-Flächen zur Orientierung
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.4, 
    color = "black",
    weight = 0.5,
    group = "Wohnlagen (Hintergrund)"
  ) %>%
  
  # Gruppe 1: Die "echten" Fehler (Falsch klassifiziert im Polygon) -> ROTE PUNKTE
  addCircleMarkers(
    data = punkte_echte_fehler_wgs,
    color = "blue", fillColor = "blue", fillOpacity = 0.8, radius = 6, weight = 2,
    popup = ~paste0(
      "<b>Art:</b> Abweichende Wohnlage<br>",
      "<b>Wahre Wohnlage Datensatz:</b> ", Wohnlage_wahr, "<br>",
      "<b>Wohnlage basierend auf Flächen:</b> ", Erwartete_Wohnlage_Raum, "<br>"
    ),
    group = "Abweichende Wohnlagen"
  ) %>%
  
  # Gruppe 2: Die NA-Fälle (Punkt fällt ins Leere) -> GELBE PUNKTE
  addCircleMarkers(
    data = punkte_na_faelle_wgs,
    color = "black", fillColor = "yellow", fillOpacity = 0.8, radius = 6, weight = 1,
    popup = ~paste0(
      "<b>Art:</b> Wohnlage aus Fläche unermittelbar<br>",
      "<b>Wahre Wohnlage Datensatz:</b> ", Wohnlage_wahr, "<br>",
      "<b>Problem:</b> Punkt liegt in keinem Wohnlagen-Polygon."
    ),
    group = "Fehlende Fläche"
  ) %>%
  
  # 3. DAS KÄSTCHEN OBEN RECHTS (Layers Control)
  addLayersControl(
    overlayGroups = c(
      "Fehlende Fläche", "Abweichende Wohnlagen"),
    options = layersControlOptions(collapsed = FALSE) # Kästchen bleibt offen
  )

# Karte im Viewer anzeigen
karte_validierung_gefiltert

saveWidget(karte_validierung_gefiltert, 
           file = "interaktive_karten/karte_validierung_gefiltert.html",
           selfcontained = TRUE)
