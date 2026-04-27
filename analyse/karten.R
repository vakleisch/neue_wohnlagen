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
model_linear_zentral <- readRDS("modelle/linear_model_zentral.rds")
model_linear_ausserhalb <- readRDS("modelle/linear_model_ausserhalb_b.rds")

model_gam_zentral_brw <- readRDS("modelle/gam_model_zentral_ohne_brw.rds")

model_gam_ausserhalb_brw <- readRDS("modelle/gam_model_ausserhalb_ohne_brw.rds")

model_gam_zentral_brw_less <- readRDS("modelle/gam_model_zentral_ohne_brw_less.rds")

model_gam_ausserhalb_brw_less <- readRDS("modelle/gam_model_ausserhalb_ohne_brw_less.rds")

model_gam_zentral_less <- readRDS("modelle/gam_model_zentral_less.rds")

model_gam_ausserhalb_less <- readRDS("modelle/gam_model_ausserhalb_less.rds")

model_gam_zentral_spatial <- readRDS("modelle/gam_model_zentral_spatial.rds")

model_gam_ausserhalb_spatial <- readRDS("modelle/gam_model_ausserhalb_spatial.rds")

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

saveRDS(wohnlagen_muc_wgs, "daten/wohnlagen_flächen.rds")



#-------------------------------------------------------------------------------
# Interaktive Karten erstellen
#-------------------------------------------------------------------------------
# Hintergrunddaten: Straßen aus OpenStreetMap



# Einfache Interaktive Karte nur mit den Korrekten und Falschen Wohnlagen
interaktive_karte_model <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen, transformierte Version!
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
  # Grenzen (transformiert)
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









# ==============================================================================
# ISOLIERTER BLOCK: verbesserte hauptkarte
# ==============================================================================

library(leaflet)
library(htmlwidgets)
library(dplyr)


# 1. Isolierte Kopien der bestehenden Datensätze erstellen
fehler_fuer_karte <- fehler_model_gam_kombiniert_wgs
korrekt_fuer_karte <- korrekt_model_gam_kombiniert_wgs

# 2. Hilfsfunktion: Wahrscheinlichkeiten sicher berechnen
# (Prüft automatisch, ob der Punkt zentral ist und wendet das richtige Modell an)
berechne_probs_sicher <- function(df, mod_zentral, mod_ausserhalb) {
  df$prob_durchschnittlich <- NA
  df$prob_gut <- NA
  df$prob_beste <- NA
  
  # Filter: Welche Punkte sind zentral, welche außerhalb?
  idx_zentral <- df$Wohnlage_wahr %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")
  idx_ausserhalb <- !idx_zentral
  
  # Zentrales Modell anwenden
  if(any(idx_zentral)) {
    p_z <- predict(mod_zentral, newdata = df[idx_zentral, ], type = "response")
    df$prob_durchschnittlich[idx_zentral] <- p_z[, 1]
    df$prob_gut[idx_zentral]              <- p_z[, 2]
    df$prob_beste[idx_zentral]            <- p_z[, 3]
  }
  
  # Ausserhalb Modell anwenden
  if(any(idx_ausserhalb)) {
    p_a <- predict(mod_ausserhalb, newdata = df[idx_ausserhalb, ], type = "response")
    df$prob_durchschnittlich[idx_ausserhalb] <- p_a[, 1]
    df$prob_gut[idx_ausserhalb]              <- p_a[, 2]
    df$prob_beste[idx_ausserhalb]            <- p_a[, 3]
  }
  
  return(df)
}

# 3. Wahrscheinlichkeiten an die Kopien anhängen
fehler_fuer_karte <- berechne_probs_sicher(fehler_fuer_karte, model_gam_zentral, model_gam_ausserhalb)
korrekt_fuer_karte <- berechne_probs_sicher(korrekt_fuer_karte, model_gam_zentral, model_gam_ausserhalb)

# 4. HTML Popups generieren
erstelle_popup_html <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>", 
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Anteil Verkehrsfläche im Viertel:</b> ", round(df$anteil_vf_sv, 2), " %<br>",
    "<b>Anteil Grünfläche im Viertel:</b> ", round(df$anteil_gf_sv, 2), " %<br>"
  )
}

# Popup-Texte fest in die Datensätze schreiben
fehler_fuer_karte <- fehler_fuer_karte %>% mutate(popup_text = erstelle_popup_html(.))
korrekt_fuer_karte <- korrekt_fuer_karte %>% mutate(popup_text = erstelle_popup_html(.))


# karte bauen
punkt_groesse <- 6 


interaktive_karte_model_werte <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrund: Wohnlagen
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  # Linien: Grenzen
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE (Dünner schwarzer Rand, auf der Karte untenliegend)
  addCircleMarkers(
    data = korrekt_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt"
  ) %>%
  
  # FEHLERHAFTE PUNKTE (Dicker roter Rand, auf der Karte obenliegend)
  addCircleMarkers(
    data = fehler_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler"
  ) %>%
  
  # Legende für die Wohnlagen (unname nicht vergessen!)
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  
  # Interaktives Kontrollkästchen oben rechts
  addLayersControl(
    overlayGroups = c("Fehler", "Korrekt"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Karte im Viewer anzeigen
print(interaktive_karte_model_werte)

# Karte als HTML speichern
if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(interaktive_karte_model_werte, file = "interaktive_karten/interaktive_karte_model_werte.html", selfcontained = TRUE)

#-------------------------------------------------------------------------------














# VALIDIERUNG DER SPALTE DER WAHREN WOHNLAGEN

# Räumliche Verschneidung (Spatial Join)
# Wir ziehen uns einfach die EBENE aus dem Flächen-Polygon
# 1. Beide Datensätze auf exakt dasselbe Koordinatensystem zwingen
model_data_complete_sf <- st_transform(model_data_complete_sf, st_crs(wohnlagen_muc2))

# 2. Jetzt klappt der Spatial Join fehlerfrei!
punkte_validierung <- st_join(model_data_complete_sf, wohnlagen_muc2, join = st_intersects)

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
    Lage_stimmt_ueberein = as.character(wohnlage_bedeutung) == Erwartete_Wohnlage_Raum
  )

# Auswertung anzeigen
cat("\n=== ERGEBNIS DER VALIDIERUNG ===\n")
print(table(Korrekt_Platziert = punkte_validierung$Lage_stimmt_ueberein, useNA = "always"))

# Abweichler extrahieren, falls es welche gibt
punkte_fehlerhaft_platziert <- punkte_validierung %>% 
  filter(Lage_stimmt_ueberein == FALSE | is.na(Lage_stimmt_ueberein)) %>%
  select(wohnlage_bedeutung, Erwartete_Wohnlage_Raum, EBENE)

cat("\nAnzahl der Fehler: ", nrow(punkte_fehlerhaft_platziert), "\n")


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
      "<b>Wahre Wohnlage Datensatz:</b> ", wohnlage_bedeutung, "<br>",
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
      "<b>Wahre Wohnlage Datensatz:</b> ", wohnlage_bedeutung, "<br>",
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







# Variablenkarten: Validierung der Daten


library(leaflet)
library(htmlwidgets)
library(dplyr)
library(sf)

ziel_ordner <- "interaktive_karten/variablen"

if (!dir.exists(ziel_ordner)) {
  dir.create(ziel_ordner, recursive = TRUE)
}

# 1. Variablen definieren
variablen_liste <- c(
  "Distanz Grünfläche (>10ha) [m]" = "erreichbarkeit_gr10ha_in_metern_adr",
  "Fahrtzeit Innenstadt (ÖPNV) [min]" = "erreichbarkeit_innenstadt_in_minuten_adr",
  "Erreichbarkeit nächste Haltestelle [min]" = "erreichbarkeit_naechstehaltestelle_in_minuten_adr",
  "Fußweg Grundschule [m]" = "grundschul_num",
  "Fußweg Spielplatz [m]" = "spielplatz_num",
  "Fußweg Kita [m]" = "kitakigaho_num",
  "Fußweg Ortszentrum [m]" = "ortszentru_num",
  "log(Bodenrichtwert)" = "brw_log",
  "Bodenrichtwert [€/m²]" = "brw",
  "Anteil Verkehrsfläche im Viertel [%]" = "anteil_vf_sv",
  "Anteil Grünfläche im Viertel [%]" = "anteil_gf_sv"
)

# 2. Farben definieren
wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)



# 3. Schleife für alle Karten
lapply(names(variablen_liste), function(var_titel) {
  
  var_spalte <- variablen_liste[[var_titel]]
  
  if (!var_spalte %in% names(model_data_complete_wgs)) {
    cat("Variable fehlt:", var_spalte, "\n")
    return(NULL)
  }
  
  daten <- model_data_complete_wgs %>%
    mutate(
      wert = .data[[var_spalte]],
      label = paste0(var_titel, ": ", round(wert, 2))
    )
  
  pal <- colorNumeric(palette = "magma", domain = daten$wert)
  
  daten <- daten %>% mutate(farbe = pal(wert))
  
  # Canvas-Modus für maximale Geschwindigkeit
  karte <- leaflet(data = daten, options = leafletOptions(preferCanvas = TRUE)) %>%
    
    addProviderTiles("CartoDB.Positron") %>%
    
    addPolygons(
      data = wohnlagen_muc_wgs,
      fillColor = ~color,
      fillOpacity = 0.5,
      color = "black",
      weight = 0.5,
      label = ~as.character(Wohnlage)
    ) %>%
    
    addLegend(
      position = "bottomleft",
      colors = unname(wohnlage_farben), 
      labels = names(wohnlage_farben),
      title = "Wohnlage (Hintergrund)",
      opacity = 0.8
    ) %>%
    
    addPolylines(
      data = wohnlage_grenzen_wgs,
      color = "black",
      weight = 0.5
    ) %>%
    
    addCircleMarkers(
      radius = 3,
      stroke = FALSE,
      fillColor = ~farbe,
      fillOpacity = 0.9,
      label = ~label 
    ) %>%
    
    addLegend(
      position = "bottomright",
      pal = pal,
      values = daten$wert,
      title = var_titel,
      opacity = 1
    ) %>%
    
    setView(lng = 11.5761, lat = 48.1371, zoom = 11)
  
  dateiname <- paste0(ziel_ordner, "/interaktive_karte_", var_spalte, ".html")
  saveWidget(karte, file = dateiname, selfcontained = TRUE)
  
  cat("Karte gespeichert (Canvas + Labels):", var_spalte, "\n")
})





# LINEARE MODELLE


#  Daten erstellen (Fehler und Korrekt)
fehler_model_linear_zentral <- missclassification_data_zentral(
  model_linear_zentral, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
fehler_model_linear_ausserhalb <- missclassification_data_ausserhalb(
  model_linear_ausserhalb, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)
korrekt_model_linear_zentral <- korrekte_vorhersagen_zentral(
  model_linear_zentral, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
korrekt_model_linear_ausserhalb <- korrekte_vorhersagen_ausserhalb(
  model_linear_ausserhalb, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)

#  In sf Objekte umwandeln und nach Lagen filtern
fehler_model_linear_zentral <- st_as_sf(fehler_model_linear_zentral) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))

fehler_model_linear_ausserhalb <- st_as_sf(fehler_model_linear_ausserhalb) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

korrekt_model_linear_zentral <- st_as_sf(korrekt_model_linear_zentral) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))

korrekt_model_linear_ausserhalb <- st_as_sf(korrekt_model_linear_ausserhalb) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

#  Hilfsfunktion zum Bereinigen (st_zm und st_make_valid) anwenden
# (Ich gehe davon aus, dass prepare_sf_object in deinem Skript noch definiert ist)
fehler_model_linear_zentral <- prepare_sf_object(fehler_model_linear_zentral)
fehler_model_linear_ausserhalb <- prepare_sf_object(fehler_model_linear_ausserhalb)
korrekt_model_linear_zentral <- prepare_sf_object(korrekt_model_linear_zentral)
korrekt_model_linear_ausserhalb <- prepare_sf_object(korrekt_model_linear_ausserhalb)

#  Faktoren angleichen, damit es beim rbind keine Probleme gibt
levels_kombiniert <- c(
  "durchschnittliche Lage", "gute Lage", "beste Lage",
  "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"
)

fehler_model_linear_zentral$Wohnlage_vorhersage <- factor(fehler_model_linear_zentral$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_linear_zentral$Wohnlage_wahr <- factor(fehler_model_linear_zentral$Wohnlage_wahr, levels = levels_kombiniert)

fehler_model_linear_ausserhalb$Wohnlage_vorhersage <- factor(fehler_model_linear_ausserhalb$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_linear_ausserhalb$Wohnlage_wahr <- factor(fehler_model_linear_ausserhalb$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_linear_zentral$Wohnlage_vorhersage <- factor(korrekt_model_linear_zentral$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_linear_zentral$Wohnlage_wahr <- factor(korrekt_model_linear_zentral$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_linear_ausserhalb$Wohnlage_vorhersage <- factor(korrekt_model_linear_ausserhalb$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_linear_ausserhalb$Wohnlage_wahr <- factor(korrekt_model_linear_ausserhalb$Wohnlage_wahr, levels = levels_kombiniert)

#  Zentral und Außerhalb zusammenfügen (rbind)
fehler_model_linear_kombiniert <- rbind(fehler_model_linear_zentral, fehler_model_linear_ausserhalb)
korrekt_model_linear_kombiniert <- rbind(korrekt_model_linear_zentral, korrekt_model_linear_ausserhalb)

#  WGS84 sicherstellen (st_transform)
fehler_model_linear_kombiniert_wgs <- st_transform(fehler_model_linear_kombiniert, crs = 4326)
korrekt_model_linear_kombiniert_wgs <- st_transform(korrekt_model_linear_kombiniert, crs = 4326)

#  Farbzuordnung der Punkte für die Leaflet Karte
fehler_model_linear_kombiniert_wgs <- fehler_model_linear_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

korrekt_model_linear_kombiniert_wgs <- korrekt_model_linear_kombiniert_wgs %>%
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))


# ==============================================================================
# ISOLIERTER BLOCK: HAUPTKARTE FÜR LINEARE MODELLE
# ==============================================================================

library(leaflet)
library(htmlwidgets)
library(dplyr)



#  Isolierte Kopien der bestehenden LINEAREN Datensätze erstellen
fehler_linear_fuer_karte <- fehler_model_linear_kombiniert_wgs
korrekt_linear_fuer_karte <- korrekt_model_linear_kombiniert_wgs

#  Hilfsfunktion: Wahrscheinlichkeiten sicher berechnen
berechne_probs_sicher_linear <- function(df, mod_zentral, mod_ausserhalb) {
  df$prob_durchschnittlich <- NA
  df$prob_gut <- NA
  df$prob_beste <- NA
  
  # Filter: Welche Punkte sind zentral, welche außerhalb?
  idx_zentral <- df$Wohnlage_wahr %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")
  idx_ausserhalb <- !idx_zentral
  
  # Zentrales Modell anwenden
  if(any(idx_zentral)) {
    p_z <- predict(mod_zentral, newdata = df[idx_zentral, ], type = "response")
    df$prob_durchschnittlich[idx_zentral] <- p_z[, 1]
    df$prob_gut[idx_zentral]              <- p_z[, 2]
    df$prob_beste[idx_zentral]            <- p_z[, 3]
  }
  
  # Ausserhalb Modell anwenden
  if(any(idx_ausserhalb)) {
    p_a <- predict(mod_ausserhalb, newdata = df[idx_ausserhalb, ], type = "response")
    df$prob_durchschnittlich[idx_ausserhalb] <- p_a[, 1]
    df$prob_gut[idx_ausserhalb]              <- p_a[, 2]
    df$prob_beste[idx_ausserhalb]            <- p_a[, 3]
  }
  
  return(df)
}

#  Wahrscheinlichkeiten an die Kopien anhängen (mit den LINEAREN Modellen)
fehler_linear_fuer_karte <- berechne_probs_sicher_linear(fehler_linear_fuer_karte, model_linear_zentral, model_linear_ausserhalb)
korrekt_linear_fuer_karte <- berechne_probs_sicher_linear(korrekt_linear_fuer_karte, model_linear_zentral, model_linear_ausserhalb)

# HTML Popups generieren
erstelle_popup_html <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>", 
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Anteil Verkehrsfläche im Viertel:</b> ", round(df$anteil_vf_sv, 2), " %<br>",
    "<b>Anteil Grünfläche im Viertel:</b> ", round(df$anteil_gf_sv, 2), " %<br>"
  )
}

# Popup-Texte fest in die Datensätze schreiben
fehler_linear_fuer_karte <- fehler_linear_fuer_karte %>% mutate(popup_text = erstelle_popup_html(.))
korrekt_linear_fuer_karte <- korrekt_linear_fuer_karte %>% mutate(popup_text = erstelle_popup_html(.))


# Karte

punkt_groesse <- 6 

cat("Zeichne die Karte für die linearen Modelle...\n")

interaktive_karte_linear_werte <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrund: Wohnlagen
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  # Linien: Grenzen
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE (Dünner schwarzer Rand, auf der Karte untenliegend)
  addCircleMarkers(
    data = korrekt_linear_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt"
  ) %>%
  
  # FEHLERHAFTE PUNKTE (Dicker roter Rand, auf der Karte obenliegend)
  addCircleMarkers(
    data = fehler_linear_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler"
  ) %>%
  
  # Legende für die Wohnlagen
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  
  # Interaktives Kontrollkästchen oben rechts
  addLayersControl(
    overlayGroups = c("Fehler", "Korrekt"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Karte im Viewer anzeigen
print(interaktive_karte_linear_werte)

# Karte als HTML speichern
if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")

# NEUER DATEINAME: interaktive_karte_linear_werte.html
saveWidget(interaktive_karte_linear_werte, file = "interaktive_karten/interaktive_karte_linear_werte.html", selfcontained = TRUE)






# ==============================================================================
# SPATIAL OUTLIER DETECTION (Im hellen Design mit vollständigen Popups)
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)

cat("Bereite Daten für detaillierte Ausreißer-Karte vor...\n")

# 1. Komplette Vorhersage-Daten zusammenführen (OHNE 'select', damit Popups bleiben!)
alle_vorhersagen_voll <- rbind(
  fehler_model_gam_kombiniert_wgs,
  korrekt_model_gam_kombiniert_wgs
)

# 2. In lokales metrisches System (UTM) transformieren
alle_vorhersagen_utm <- st_transform(alle_vorhersagen_voll, 25832)

# 3. Radius definieren & Nachbarn finden
such_radius_meter <- 150 
cat(paste("Suche nach Nachbarn im Umkreis von", such_radius_meter, "Metern...\n"))
nachbarn_liste <- st_is_within_distance(alle_vorhersagen_utm, dist = such_radius_meter)

# 4. Ausreißer-Algorithmus anwenden
cat("Identifiziere Ausreißer...\n")
ist_ausreisser <- sapply(seq_along(nachbarn_liste), function(i) {
  nachbar_indices <- setdiff(nachbarn_liste[[i]], i)
  
  if(length(nachbar_indices) < 2) return(FALSE) 
  
  eigene_klasse <- as.character(alle_vorhersagen_utm$Wohnlage_vorhersage[i])
  nachbar_klassen <- as.character(alle_vorhersagen_utm$Wohnlage_vorhersage[nachbar_indices])
  
  häufigkeiten <- sort(table(nachbar_klassen), decreasing = TRUE)
  modus_klasse <- names(häufigkeiten)[1]
  
  return(eigene_klasse != modus_klasse)
})

# 5. Zurück nach WGS84 und aufteilen
alle_vorhersagen_wgs <- st_transform(alle_vorhersagen_utm, 4326)
alle_vorhersagen_wgs$Ausreisser <- ist_ausreisser

# In zwei Gruppen teilen für unterschiedliches Styling
daten_normal <- alle_vorhersagen_wgs %>% filter(Ausreisser == FALSE)
daten_ausreisser <- alle_vorhersagen_wgs %>% filter(Ausreisser == TRUE)

cat(paste("Fertig!", nrow(daten_ausreisser), "Ausreißer gefunden. Zeichne Karte...\n"))


# ==============================================================================
# 6. HTML POPUPS GENERIEREN (Kugelsicher!)
# ==============================================================================
cat("4. Generiere detaillierte HTML-Popups...\n")

erstelle_popup_html <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    # Falls du die Wahrscheinlichkeiten berechnet hast, werden sie hier angezeigt:
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>", 
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Anteil Verkehrsfläche im Viertel:</b> ", round(df$anteil_vf_sv, 2), " %<br>",
    "<b>Anteil Grünfläche im Viertel:</b> ", round(df$anteil_gf_sv, 2), " %<br>"
  )
}

# HIER IST DER FIX: Wir zwingen die Textspalte hart in die Datensätze
daten_normal$popup_text <- erstelle_popup_html(daten_normal)
daten_ausreisser$popup_text <- erstelle_popup_html(daten_ausreisser)


# ==============================================================================
# 7. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("5. Zeichne Karte...\n")

karte_ausreisser_detail <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>% # Heller Hintergrund
  
  # Hintergrundflächen (Wohnlagen)
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.4,   # Etwas blasser
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  # Linien (Grenzen)
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # NORMALE PUNKTE (Normale Größe, schwarzer Rand)
  addCircleMarkers(
    data = daten_normal,
    fillColor = ~color,       
    fillOpacity = 1,
    color = "black",          
    stroke = TRUE,
    weight = 1,
    radius = 4,               
    popup = ~popup_text,      # Die neu generierten Popups!
    group = "Normale Punkte"
  ) %>%
  
  # AUSREISSER PUNKTE (Größer, dicker roter Rand)
  addCircleMarkers(
    data = daten_ausreisser,
    fillColor = ~color,       
    fillOpacity = 1,
    color = "red",            # Dicker roter Rand!
    stroke = TRUE,
    weight = 3,               
    radius = 7,               # Größer!
    popup = ~popup_text,      # Die neu generierten Popups!
    group = "Ausreißer (Inseln)"
  ) %>%
  
  # Legende
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Vorhersage & Hintergrund",
    opacity = 1
  ) %>%
  
  # Ebenen-Steuerung
  addLayersControl(
    overlayGroups = c("Ausreißer (Inseln)", "Normale Punkte"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Karte anzeigen
print(karte_ausreisser_detail)

# Speichern
if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_ausreisser_detail, file = "interaktive_karten/karte_raum_ausreisser_detail.html", selfcontained = TRUE)












# ==============================================================================
# SPATIAL OUTLIER DETECTION (KOMPLETT & DETAILLIERT)
# Inklusive Wahrscheinlichkeiten, HTML-Popups und Kartengenerierung
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)

cat("1. Bereite Daten für detaillierte Ausreißer-Karte vor...\n")

# 1. Komplette Vorhersage-Daten zusammenführen
alle_vorhersagen_voll <- rbind(
  fehler_model_gam_kombiniert_wgs,
  korrekt_model_gam_kombiniert_wgs
)

# ==============================================================================
# NEU: 2. WAHRSCHEINLICHKEITEN BERECHNEN (Damit die Popups funktionieren!)
# ==============================================================================
cat("2. Berechne Klassenwahrscheinlichkeiten...\n")

berechne_probs_sicher <- function(df, mod_zentral, mod_ausserhalb) {
  df$prob_durchschnittlich <- NA
  df$prob_gut <- NA
  df$prob_beste <- NA
  
  idx_zentral <- df$Wohnlage_wahr %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")
  idx_ausserhalb <- !idx_zentral
  
  if(any(idx_zentral)) {
    p_z <- predict(mod_zentral, newdata = df[idx_zentral, ], type = "response")
    df$prob_durchschnittlich[idx_zentral] <- p_z[, 1]
    df$prob_gut[idx_zentral]              <- p_z[, 2]
    df$prob_beste[idx_zentral]            <- p_z[, 3]
  }
  
  if(any(idx_ausserhalb)) {
    p_a <- predict(mod_ausserhalb, newdata = df[idx_ausserhalb, ], type = "response")
    df$prob_durchschnittlich[idx_ausserhalb] <- p_a[, 1]
    df$prob_gut[idx_ausserhalb]              <- p_a[, 2]
    df$prob_beste[idx_ausserhalb]            <- p_a[, 3]
  }
  return(df)
}

alle_vorhersagen_voll <- berechne_probs_sicher(alle_vorhersagen_voll, model_gam_zentral, model_gam_ausserhalb)


# ==============================================================================
# 3. AUSREISSER (SPECKLES) FINDEN
# ==============================================================================
cat("3. Suche nach räumlichen Ausreißern (150m Radius)...\n")

alle_vorhersagen_utm <- st_transform(alle_vorhersagen_voll, 25832)
nachbarn_liste <- st_is_within_distance(alle_vorhersagen_utm, dist = 150)

ist_ausreisser <- sapply(seq_along(nachbarn_liste), function(i) {
  nachbar_indices <- setdiff(nachbarn_liste[[i]], i)
  if(length(nachbar_indices) < 2) return(FALSE) 
  
  eigene_klasse <- as.character(alle_vorhersagen_utm$Wohnlage_vorhersage[i])
  nachbar_klassen <- as.character(alle_vorhersagen_utm$Wohnlage_vorhersage[nachbar_indices])
  
  häufigkeiten <- sort(table(nachbar_klassen), decreasing = TRUE)
  modus_klasse <- names(häufigkeiten)[1]
  
  return(eigene_klasse != modus_klasse)
})

# Zurück nach WGS84 und aufteilen
alle_vorhersagen_wgs <- st_transform(alle_vorhersagen_utm, 4326)
alle_vorhersagen_wgs$Ausreisser <- ist_ausreisser

daten_normal <- alle_vorhersagen_wgs %>% filter(Ausreisser == FALSE)
daten_ausreisser <- alle_vorhersagen_wgs %>% filter(Ausreisser == TRUE)


# ==============================================================================
# 4. HTML POPUPS GENERIEREN (Kugelsicher!)
# ==============================================================================
cat("4. Generiere detaillierte HTML-Popups...\n")

erstelle_popup_html <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>", 
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Anteil Verkehrsfläche im Viertel:</b> ", round(df$anteil_vf_sv, 2), " %<br>",
    "<b>Anteil Grünfläche im Viertel:</b> ", round(df$anteil_gf_sv, 2), " %<br>"
  )
}

daten_normal$popup_text <- erstelle_popup_html(daten_normal)
daten_ausreisser$popup_text <- erstelle_popup_html(daten_ausreisser)


# ==============================================================================
# 5. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("5. Zeichne Karte...\n")

karte_ausreisser_detail <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrundflächen (Wohnlagen)
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.4,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  # Linien (Grenzen)
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # NORMALE PUNKTE (Normale Größe, schwarzer Rand)
  addCircleMarkers(
    data = daten_normal,
    fillColor = ~color,       
    fillOpacity = 1,
    color = "black",          
    stroke = TRUE,
    weight = 1,
    radius = 4,               
    popup = ~popup_text,
    group = "Normale Punkte"
  ) %>%
  
  # AUSREISSER PUNKTE (Größer, dicker roter Rand)
  addCircleMarkers(
    data = daten_ausreisser,
    fillColor = ~color,       
    fillOpacity = 1,
    color = "red",            
    stroke = TRUE,
    weight = 3,               
    radius = 7,               
    popup = ~popup_text,
    group = "Ausreißer (Inseln)"
  ) %>%
  
  # Legende
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Vorhersage & Hintergrund",
    opacity = 1
  ) %>%
  
  # Ebenen-Steuerung
  addLayersControl(
    overlayGroups = c("Ausreißer (Inseln)", "Normale Punkte"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Karte anzeigen
print(karte_ausreisser_detail)

# Speichern
if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_ausreisser_detail, file = "interaktive_karten/karte_raum_ausreisser_detail.html", selfcontained = TRUE)

cat("✓ Erfolgreich gespeichert unter: interaktive_karten/karte_raum_ausreisser_detail.html\n")









# ==============================================================================
# SPATIAL HETEROGENEITY (Lokale Mischzonen / Flickenteppiche erkennen)
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)

cat("Berechne lokale Heterogenität (Mix-Score)...\n")

# 1. Datenbasis: Alle Vorhersagen aus dem GAM-Modell
# (Wir nutzen wieder die bestehenden Daten aus deinem Skript)
alle_vorhersagen <- rbind(
  fehler_model_gam_kombiniert_wgs %>% select(Wohnlage_vorhersage, color),
  korrekt_model_gam_kombiniert_wgs %>% select(Wohnlage_vorhersage, color)
)

# 2. In metrisches System (UTM) umwandeln für exakte Distanzen
alle_vorhersagen_utm <- st_transform(alle_vorhersagen, 25832)

# 3. Radius definieren
such_radius_meter <- 150
nachbarn_liste <- st_is_within_distance(alle_vorhersagen_utm, dist = such_radius_meter)

# 4. Mix-Score berechnen
cat("Analysiere Nachbarschaften...\n")

heterogenitaets_daten <- lapply(seq_along(nachbarn_liste), function(i) {
  
  nachbar_indices <- nachbarn_liste[[i]]
  
  # Wenn der Punkt quasi alleine steht, gibt es keine Heterogenität
  if(length(nachbar_indices) < 3) {
    return(data.frame(Mix_Score = 0, Dominanz_Prozent = 100, Anzahl_Klassen = 1))
  }
  
  # Alle vorhergesagten Klassen im Umkreis sammeln
  nachbar_klassen <- as.character(alle_vorhersagen_utm$Wohnlage_vorhersage[nachbar_indices])
  
  # Wie viele unterschiedliche Klassen gibt es hier?
  anzahl_klassen <- length(unique(nachbar_klassen))
  
  # Wie stark ist die häufigste Klasse? (Dominanz)
  häufigkeiten <- sort(table(nachbar_klassen), decreasing = TRUE)
  dominante_klasse_anteil <- max(häufigkeiten) / length(nachbar_klassen)
  
  # Unser Mix-Score: 1 minus Dominanz. 
  # (0 = Alle sind gleich. 0.6 = Nur 40% sind gleich, extrem gemischt!)
  mix_score <- 1 - dominante_klasse_anteil
  
  return(data.frame(
    Mix_Score = mix_score, 
    Dominanz_Prozent = round(dominante_klasse_anteil * 100, 1),
    Anzahl_Klassen = anzahl_klassen
  ))
})

# Die berechneten Werte als Spalten an unseren Datensatz hängen
heterogenitaets_df <- bind_rows(heterogenitaets_daten)
alle_vorhersagen_wgs <- st_transform(alle_vorhersagen_utm, 4326)
alle_vorhersagen_wgs <- bind_cols(alle_vorhersagen_wgs, heterogenitaets_df)

# ==============================================================================
# 5. DIE HETEROGENITÄTS-KARTE BAUEN
# ==============================================================================

cat("Zeichne die Heterogenitäts-Karte...\n")

# Farbpalette: Weiß/Gelb (Homogen) bis tiefes Blutrot (Flickenteppich/Sehr gemischt)
pal_mix <- colorNumeric(
  palette = "YlOrRd", 
  domain = alle_vorhersagen_wgs$Mix_Score
)

karte_mischzonen <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrundflächen (Wohnlagen)
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.3,   # Etwas blasser, damit Punkte besser wirken
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  # Linien (Grenzen)
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # Die berechneten Punkte
  addCircleMarkers(
    data = alle_vorhersagen_wgs,
    fillColor = ~pal_mix(Mix_Score),
    fillOpacity = 0.8,
    color = "black",   # Dünner Rand für Kontrast
    stroke = TRUE,
    weight = 0.5,
    radius = 4,
    
    # Das Popup verrät uns exakt, was an diesem Punkt los ist
    popup = ~paste0(
      "<b>Mischzonen-Analyse (", such_radius_meter, "m Radius)</b><br><hr>",
      "<b>Mix-Score:</b> ", round(Mix_Score, 2), " (Je höher, desto bunter)<br>",
      "<b>Verschiedene Klassen im Umkreis:</b> ", Anzahl_Klassen, " Stück<br>",
      "<b>Dominanz der stärksten Klasse:</b> ", Dominanz_Prozent, " %<br><br>",
      "<i>Eigene Vorhersage hier: ", Wohnlage_vorhersage, "</i>"
    )
  ) %>%
  
  # Legende
  addLegend(
    position = "bottomright",
    pal = pal_mix,
    values = alle_vorhersagen_wgs$Mix_Score,
    title = "Flickenteppich-Score<br>(0 = Homogen, >0.5 = Stark Gemischt)",
    opacity = 1
  )

# Karte anzeigen und Speichern
print(karte_mischzonen)

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_mischzonen, file = "interaktive_karten/karte_mischzonen.html", selfcontained = TRUE)

cat("Erfolgreich gespeichert unter: interaktive_karten/karte_mischzonen.html\n")




# ==============================================================================
# HAUPTKARTE: GAM MODELLE OHNE BODENRICHTWERT (BRW)
# ==============================================================================
cat("Generiere Daten für Modelle OHNE BRW...\n")

# 1. Daten erstellen mit deinen Custom-Funktionen
fehler_model_gam_zentral_brw <- missclassification_data_zentral(
  model_gam_zentral_brw, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
fehler_model_gam_ausserhalb_brw <- missclassification_data_ausserhalb(
  model_gam_ausserhalb_brw, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_zentral_brw <- korrekte_vorhersagen_zentral(
  model_gam_zentral_brw, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_ausserhalb_brw <- korrekte_vorhersagen_ausserhalb(
  model_gam_ausserhalb_brw, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)

# 2. In sf Objekte umwandeln und filtern
fehler_model_gam_zentral_brw <- st_as_sf(fehler_model_gam_zentral_brw) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
fehler_model_gam_ausserhalb_brw <- st_as_sf(fehler_model_gam_ausserhalb_brw) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

korrekt_model_gam_zentral_brw <- st_as_sf(korrekt_model_gam_zentral_brw) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_brw <- st_as_sf(korrekt_model_gam_ausserhalb_brw) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

# 3. sf-Objekte bereinigen (mit deiner Hilfsfunktion)
fehler_model_gam_zentral_brw <- prepare_sf_object(fehler_model_gam_zentral_brw)
fehler_model_gam_ausserhalb_brw <- prepare_sf_object(fehler_model_gam_ausserhalb_brw)
korrekt_model_gam_zentral_brw <- prepare_sf_object(korrekt_model_gam_zentral_brw)
korrekt_model_gam_ausserhalb_brw <- prepare_sf_object(korrekt_model_gam_ausserhalb_brw)

# 4. Levels angleichen
levels_kombiniert <- c(
  "durchschnittliche Lage", "gute Lage", "beste Lage",
  "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"
)

fehler_model_gam_zentral_brw$Wohnlage_vorhersage <- factor(fehler_model_gam_zentral_brw$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_zentral_brw$Wohnlage_wahr <- factor(fehler_model_gam_zentral_brw$Wohnlage_wahr, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_brw$Wohnlage_vorhersage <- factor(fehler_model_gam_ausserhalb_brw$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_brw$Wohnlage_wahr <- factor(fehler_model_gam_ausserhalb_brw$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_gam_zentral_brw$Wohnlage_vorhersage <- factor(korrekt_model_gam_zentral_brw$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_zentral_brw$Wohnlage_wahr <- factor(korrekt_model_gam_zentral_brw$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_brw$Wohnlage_vorhersage <- factor(korrekt_model_gam_ausserhalb_brw$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_brw$Wohnlage_wahr <- factor(korrekt_model_gam_ausserhalb_brw$Wohnlage_wahr, levels = levels_kombiniert)

# 5. Kombinieren und nach WGS84 transformieren
fehler_brw_kombiniert_wgs <- rbind(fehler_model_gam_zentral_brw, fehler_model_gam_ausserhalb_brw) %>% st_transform(4326)
korrekt_brw_kombiniert_wgs <- rbind(korrekt_model_gam_zentral_brw, korrekt_model_gam_ausserhalb_brw) %>% st_transform(4326)

# 6. Farbzuordnung
fehler_brw_kombiniert_wgs <- fehler_brw_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

korrekt_brw_kombiniert_wgs <- korrekt_brw_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

# ==============================================================================
# 7. WAHRSCHEINLICHKEITEN BERECHNEN UND POPUPS GENERIEREN
# ==============================================================================
cat("Berechne Wahrscheinlichkeiten und Popups...\n")

# Wir nutzen deine Hilfsfunktion, übergeben aber die _brw Modelle!
fehler_brw_fuer_karte <- berechne_probs_sicher(fehler_brw_kombiniert_wgs, model_gam_zentral_brw, model_gam_ausserhalb_brw)
korrekt_brw_fuer_karte <- berechne_probs_sicher(korrekt_brw_kombiniert_wgs, model_gam_zentral_brw, model_gam_ausserhalb_brw)


# Leicht angepasstes Popup

erstelle_popup_html_brw <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage <span style='color:red;'>(ohne BRW)</span>:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>"
  )
}

# (Ich nutze die kugelsichere $ Zuweisung, damit es bei sf-Objekten keine Probleme gibt)
fehler_brw_fuer_karte$popup_text <- erstelle_popup_html_brw(fehler_brw_fuer_karte)
korrekt_brw_fuer_karte$popup_text <- erstelle_popup_html_brw(korrekt_brw_fuer_karte)

# ==============================================================================
# 8. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("Zeichne Karte...\n")

punkt_groesse <- 6 

karte_ohne_brw <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE
  addCircleMarkers(
    data = korrekt_brw_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt (ohne BRW)"
  ) %>%
  
  # FEHLERHAFTE PUNKTE
  addCircleMarkers(
    data = fehler_brw_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler (ohne BRW)"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Wohnlage (Modell ohne BRW)",
    opacity = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Fehler (ohne BRW)", "Korrekt (ohne BRW)"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(karte_ohne_brw)

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_ohne_brw, file = "interaktive_karten/interaktive_karte_model_werte_ohne_brw.html", selfcontained = TRUE)

cat("✓ Fertig! Karte erfolgreich als 'interaktive_karte_model_werte_ohne_brw.html' gespeichert.\n")




# ==============================================================================
# HAUPTKARTE: GAM MODELLE MIT WENIGER KNOTEN (OHNE BRW & LESS)
# ==============================================================================
cat("Generiere Daten für stark reduzierte Modelle (OHNE BRW & LESS)...\n")

# 1. Daten erstellen mit deinen Custom-Funktionen
fehler_model_gam_zentral_brw_less <- missclassification_data_zentral(
  model_gam_zentral_brw_less, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
fehler_model_gam_ausserhalb_brw_less <- missclassification_data_ausserhalb(
  model_gam_ausserhalb_brw_less, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_zentral_brw_less <- korrekte_vorhersagen_zentral(
  model_gam_zentral_brw_less, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_ausserhalb_brw_less <- korrekte_vorhersagen_ausserhalb(
  model_gam_ausserhalb_brw_less, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)

# 2. In sf Objekte umwandeln und filtern
fehler_model_gam_zentral_brw_less <- st_as_sf(fehler_model_gam_zentral_brw_less) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
fehler_model_gam_ausserhalb_brw_less <- st_as_sf(fehler_model_gam_ausserhalb_brw_less) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

korrekt_model_gam_zentral_brw_less <- st_as_sf(korrekt_model_gam_zentral_brw_less) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_brw_less <- st_as_sf(korrekt_model_gam_ausserhalb_brw_less) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

# 3. sf-Objekte bereinigen (mit deiner Hilfsfunktion)
fehler_model_gam_zentral_brw_less <- prepare_sf_object(fehler_model_gam_zentral_brw_less)
fehler_model_gam_ausserhalb_brw_less <- prepare_sf_object(fehler_model_gam_ausserhalb_brw_less)
korrekt_model_gam_zentral_brw_less <- prepare_sf_object(korrekt_model_gam_zentral_brw_less)
korrekt_model_gam_ausserhalb_brw_less <- prepare_sf_object(korrekt_model_gam_ausserhalb_brw_less)

# 4. Levels angleichen
levels_kombiniert <- c(
  "durchschnittliche Lage", "gute Lage", "beste Lage",
  "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"
)

fehler_model_gam_zentral_brw_less$Wohnlage_vorhersage <- factor(fehler_model_gam_zentral_brw_less$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_zentral_brw_less$Wohnlage_wahr <- factor(fehler_model_gam_zentral_brw_less$Wohnlage_wahr, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_brw_less$Wohnlage_vorhersage <- factor(fehler_model_gam_ausserhalb_brw_less$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_brw_less$Wohnlage_wahr <- factor(fehler_model_gam_ausserhalb_brw_less$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_gam_zentral_brw_less$Wohnlage_vorhersage <- factor(korrekt_model_gam_zentral_brw_less$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_zentral_brw_less$Wohnlage_wahr <- factor(korrekt_model_gam_zentral_brw_less$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_brw_less$Wohnlage_vorhersage <- factor(korrekt_model_gam_ausserhalb_brw_less$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_brw_less$Wohnlage_wahr <- factor(korrekt_model_gam_ausserhalb_brw_less$Wohnlage_wahr, levels = levels_kombiniert)

# 5. Kombinieren und nach WGS84 transformieren
fehler_brw_less_kombiniert_wgs <- rbind(fehler_model_gam_zentral_brw_less, fehler_model_gam_ausserhalb_brw_less) %>% st_transform(4326)
korrekt_brw_less_kombiniert_wgs <- rbind(korrekt_model_gam_zentral_brw_less, korrekt_model_gam_ausserhalb_brw_less) %>% st_transform(4326)

# 6. Farbzuordnung
fehler_brw_less_kombiniert_wgs <- fehler_brw_less_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

korrekt_brw_less_kombiniert_wgs <- korrekt_brw_less_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

# ==============================================================================
# 7. WAHRSCHEINLICHKEITEN BERECHNEN UND POPUPS GENERIEREN
# ==============================================================================
cat("Berechne Wahrscheinlichkeiten und Popups...\n")

# Wahrscheinlichkeiten ziehen (wir übergeben die _brw_less Modelle)
fehler_brw_less_fuer_karte <- berechne_probs_sicher(fehler_brw_less_kombiniert_wgs, model_gam_zentral_brw_less, model_gam_ausserhalb_brw_less)
korrekt_brw_less_fuer_karte <- berechne_probs_sicher(korrekt_brw_less_kombiniert_wgs, model_gam_zentral_brw_less, model_gam_ausserhalb_brw_less)

# Spezielles Popup für die stark reduzierte Variante
erstelle_popup_html_brw_less <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage <span style='color:red;'>(weniger Knoten)</span>:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>"
    )

}

fehler_brw_less_fuer_karte$popup_text <- erstelle_popup_html_brw_less(fehler_brw_less_fuer_karte)
korrekt_brw_less_fuer_karte$popup_text <- erstelle_popup_html_brw_less(korrekt_brw_less_fuer_karte)

# ==============================================================================
# 8. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("Zeichne Karte...\n")

punkt_groesse <- 6 

karte_ohne_brw_less <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE
  addCircleMarkers(
    data = korrekt_brw_less_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt (Reduziert)"
  ) %>%
  
  # FEHLERHAFTE PUNKTE
  addCircleMarkers(
    data = fehler_brw_less_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler (Reduziert)"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Vorhersage (Weniger Knoten)",
    opacity = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Fehler (Reduziert)", "Korrekt (Reduziert)"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(karte_ohne_brw_less)

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_ohne_brw_less, file = "interaktive_karten/interaktive_karte_model_werte_ohne_brw_less.html", selfcontained = TRUE)

cat("✓ Fertig! Karte erfolgreich als 'interaktive_karte_model_werte_ohne_brw_less.html' gespeichert.\n")
















# ==============================================================================
# HAUPTKARTE: GAM MODELLE REDUZIERT (LESS)
# ==============================================================================
cat("Generiere Daten für reduzierte Modelle (LESS)...\n")

# 1. Daten erstellen mit deinen Custom-Funktionen
fehler_model_gam_zentral_less <- missclassification_data_zentral(
  model_gam_zentral_less, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
fehler_model_gam_ausserhalb_less <- missclassification_data_ausserhalb(
  model_gam_ausserhalb_less, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_zentral_less <- korrekte_vorhersagen_zentral(
  model_gam_zentral_less, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_ausserhalb_less <- korrekte_vorhersagen_ausserhalb(
  model_gam_ausserhalb_less, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)

# 2. In sf Objekte umwandeln und filtern
fehler_model_gam_zentral_less <- st_as_sf(fehler_model_gam_zentral_less) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
fehler_model_gam_ausserhalb_less <- st_as_sf(fehler_model_gam_ausserhalb_less) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

korrekt_model_gam_zentral_less <- st_as_sf(korrekt_model_gam_zentral_less) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_less <- st_as_sf(korrekt_model_gam_ausserhalb_less) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

# 3. sf-Objekte bereinigen (mit deiner Hilfsfunktion)
fehler_model_gam_zentral_less <- prepare_sf_object(fehler_model_gam_zentral_less)
fehler_model_gam_ausserhalb_less <- prepare_sf_object(fehler_model_gam_ausserhalb_less)
korrekt_model_gam_zentral_less <- prepare_sf_object(korrekt_model_gam_zentral_less)
korrekt_model_gam_ausserhalb_less <- prepare_sf_object(korrekt_model_gam_ausserhalb_less)

# 4. Levels angleichen
levels_kombiniert <- c(
  "durchschnittliche Lage", "gute Lage", "beste Lage",
  "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"
)

fehler_model_gam_zentral_less$Wohnlage_vorhersage <- factor(fehler_model_gam_zentral_less$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_zentral_less$Wohnlage_wahr <- factor(fehler_model_gam_zentral_less$Wohnlage_wahr, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_less$Wohnlage_vorhersage <- factor(fehler_model_gam_ausserhalb_less$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_less$Wohnlage_wahr <- factor(fehler_model_gam_ausserhalb_less$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_gam_zentral_less$Wohnlage_vorhersage <- factor(korrekt_model_gam_zentral_less$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_zentral_less$Wohnlage_wahr <- factor(korrekt_model_gam_zentral_less$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_less$Wohnlage_vorhersage <- factor(korrekt_model_gam_ausserhalb_less$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_less$Wohnlage_wahr <- factor(korrekt_model_gam_ausserhalb_less$Wohnlage_wahr, levels = levels_kombiniert)

# 5. Kombinieren und nach WGS84 transformieren
fehler_less_kombiniert_wgs <- rbind(fehler_model_gam_zentral_less, fehler_model_gam_ausserhalb_less) %>% st_transform(4326)
korrekt_less_kombiniert_wgs <- rbind(korrekt_model_gam_zentral_less, korrekt_model_gam_ausserhalb_less) %>% st_transform(4326)

# 6. Farbzuordnung
fehler_less_kombiniert_wgs <- fehler_less_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

korrekt_less_kombiniert_wgs <- korrekt_less_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

# ==============================================================================
# 7. WAHRSCHEINLICHKEITEN BERECHNEN UND POPUPS GENERIEREN
# ==============================================================================
cat("Berechne Wahrscheinlichkeiten und Popups...\n")

# Wahrscheinlichkeiten ziehen (wir übergeben die _less Modelle)
fehler_less_fuer_karte <- berechne_probs_sicher(fehler_less_kombiniert_wgs, model_gam_zentral_less, model_gam_ausserhalb_less)
korrekt_less_fuer_karte <- berechne_probs_sicher(korrekt_less_kombiniert_wgs, model_gam_zentral_less, model_gam_ausserhalb_less)

# Spezielles Popup für die "less" Variante
erstelle_popup_html_less <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage <span style='color:red;'>(weniger Knoten)</span>:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<i>Die Eckdaten des Standorts:</i><br>",
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>"

  )
}

fehler_less_fuer_karte$popup_text <- erstelle_popup_html_less(fehler_less_fuer_karte)
korrekt_less_fuer_karte$popup_text <- erstelle_popup_html_less(korrekt_less_fuer_karte)

# ==============================================================================
# 8. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("Zeichne Karte...\n")

punkt_groesse <- 6 

karte_less <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE
  addCircleMarkers(
    data = korrekt_less_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt (Less)"
  ) %>%
  
  # FEHLERHAFTE PUNKTE
  addCircleMarkers(
    data = fehler_less_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler (Less)"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Vorhersage (Reduziertes Modell)",
    opacity = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Fehler (Less)", "Korrekt (Less)"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(karte_less)

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_less, file = "interaktive_karten/interaktive_karte_model_werte_less.html", selfcontained = TRUE)

cat("✓ Fertig! Karte erfolgreich als 'interaktive_karte_model_werte_less.html' gespeichert.\n")






# ==============================================================================
# HAUPTKARTE: GAM MODELLE MIT RÄUMLICHER KOMPONENTE (SPATIAL)
# ==============================================================================
cat("Generiere Daten für räumliche Modelle (SPATIAL)...\n")

# 1. Koordinaten für den Datensatz 'Ausserhalb' entpacken
model_data_complete_ausserhalb <- model_data_complete_ausserhalb %>%
  mutate(
    long = st_coordinates(.)[, 1], # Die X-Koordinate aus 'geom' ziehen
    lat  = st_coordinates(.)[, 2]  # Die Y-Koordinate aus 'geom' ziehen
  )

# 2. Koordinaten für den Datensatz 'Zentral' entpacken
model_data_complete_zentral <- model_data_complete_zentral %>%
  mutate(
    long = st_coordinates(.)[, 1],
    lat  = st_coordinates(.)[, 2]
  )

# 1. Daten erstellen mit deinen Custom-Funktionen
fehler_model_gam_zentral_spatial <- missclassification_data_zentral(
  model_gam_zentral_spatial, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
fehler_model_gam_ausserhalb_spatial <- missclassification_data_ausserhalb(
  model_gam_ausserhalb_spatial, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_zentral_spatial <- korrekte_vorhersagen_zentral(
  model_gam_zentral_spatial, 
  data = model_data_complete_zentral,
  predict_fun = predict_labels_discr
)
korrekt_model_gam_ausserhalb_spatial <- korrekte_vorhersagen_ausserhalb(
  model_gam_ausserhalb_spatial, 
  data = model_data_complete_ausserhalb,
  predict_fun = predict_labels_discr
)

# 2. In sf Objekte umwandeln und filtern
fehler_model_gam_zentral_spatial <- st_as_sf(fehler_model_gam_zentral_spatial) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
fehler_model_gam_ausserhalb_spatial <- st_as_sf(fehler_model_gam_ausserhalb_spatial) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

korrekt_model_gam_zentral_spatial <- st_as_sf(korrekt_model_gam_zentral_spatial) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_spatial <- st_as_sf(korrekt_model_gam_ausserhalb_spatial) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

# 3. sf-Objekte bereinigen (mit deiner Hilfsfunktion)
fehler_model_gam_zentral_spatial <- prepare_sf_object(fehler_model_gam_zentral_spatial)
fehler_model_gam_ausserhalb_spatial <- prepare_sf_object(fehler_model_gam_ausserhalb_spatial)
korrekt_model_gam_zentral_spatial <- prepare_sf_object(korrekt_model_gam_zentral_spatial)
korrekt_model_gam_ausserhalb_spatial <- prepare_sf_object(korrekt_model_gam_ausserhalb_spatial)

# 4. Levels angleichen
levels_kombiniert <- c(
  "durchschnittliche Lage", "gute Lage", "beste Lage",
  "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"
)

fehler_model_gam_zentral_spatial$Wohnlage_vorhersage <- factor(fehler_model_gam_zentral_spatial$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_zentral_spatial$Wohnlage_wahr <- factor(fehler_model_gam_zentral_spatial$Wohnlage_wahr, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_spatial$Wohnlage_vorhersage <- factor(fehler_model_gam_ausserhalb_spatial$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_ausserhalb_spatial$Wohnlage_wahr <- factor(fehler_model_gam_ausserhalb_spatial$Wohnlage_wahr, levels = levels_kombiniert)

korrekt_model_gam_zentral_spatial$Wohnlage_vorhersage <- factor(korrekt_model_gam_zentral_spatial$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_zentral_spatial$Wohnlage_wahr <- factor(korrekt_model_gam_zentral_spatial$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_spatial$Wohnlage_vorhersage <- factor(korrekt_model_gam_ausserhalb_spatial$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_spatial$Wohnlage_wahr <- factor(korrekt_model_gam_ausserhalb_spatial$Wohnlage_wahr, levels = levels_kombiniert)

# 5. Kombinieren und nach WGS84 transformieren
fehler_spatial_kombiniert_wgs <- rbind(fehler_model_gam_zentral_spatial, fehler_model_gam_ausserhalb_spatial) %>% st_transform(4326)
korrekt_spatial_kombiniert_wgs <- rbind(korrekt_model_gam_zentral_spatial, korrekt_model_gam_ausserhalb_spatial) %>% st_transform(4326)

# 6. Farbzuordnung
fehler_spatial_kombiniert_wgs <- fehler_spatial_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

korrekt_spatial_kombiniert_wgs <- korrekt_spatial_kombiniert_wgs %>% 
  mutate(color = case_when(
    Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
    Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
    Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"
  ))

# ==============================================================================
# 7. WAHRSCHEINLICHKEITEN BERECHNEN UND POPUPS GENERIEREN
# ==============================================================================
cat("Berechne Wahrscheinlichkeiten und Popups...\n")

# Wahrscheinlichkeiten ziehen (wir übergeben die _spatial Modelle)
fehler_spatial_fuer_karte <- berechne_probs_sicher(fehler_spatial_kombiniert_wgs, model_gam_zentral_spatial, model_gam_ausserhalb_spatial)
korrekt_spatial_fuer_karte <- berechne_probs_sicher(korrekt_spatial_kombiniert_wgs, model_gam_zentral_spatial, model_gam_ausserhalb_spatial)

# Spezielles Popup für die "spatial" Variante
erstelle_popup_html_spatial <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage <span style='color:purple;'>(mit räumlicher Komponente)</span>:</b> ", df$Wohnlage_vorhersage, "<br>",
    "<hr>",
    "<b>Klassenwahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnittlich * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gut * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<i>Die Eckdaten des Standorts:</i><br>",
    "<b>log(Bodenrichtwert):</b> ", round(df$brw_log, 2), "<br>",
    "<b>Bodenrichtwert:</b> ", df$brw, " €/m²<br>",
    "<b>Distanz Grünfläche (>10ha):</b> ", df$erreichbarkeit_gr10ha_in_metern_adr, " m<br>",
    "<b>Fahrtzeit Innenstadt (ÖPNV):</b> ", df$erreichbarkeit_innenstadt_in_minuten_adr, " min<br>",
    "<b>Erreichbarkeit nächste Haltestelle:</b> ", df$erreichbarkeit_naechstehaltestelle_in_minuten_adr, " min<br>",
    "<b>Fußweg Grundschule:</b> ", df$grundschul_num, " m<br>",
    "<b>Fußweg Spielplatz:</b> ", df$spielplatz_num, " m<br>",
    "<b>Fußweg Kita:</b> ", df$kitakigaho_num, " m<br>",
    "<b>Fußweg Ortszentrum:</b> ", df$ortszentru_num, " m<br>"
  )
}

fehler_spatial_fuer_karte$popup_text <- erstelle_popup_html_spatial(fehler_spatial_fuer_karte)
korrekt_spatial_fuer_karte$popup_text <- erstelle_popup_html_spatial(korrekt_spatial_fuer_karte)

# ==============================================================================
# 8. KARTE BAUEN UND SPEICHERN
# ==============================================================================
cat("Zeichne Karte...\n")

punkt_groesse <- 6 

karte_spatial <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = ~as.character(Wohnlage)
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5
  ) %>%
  
  # KORREKTE PUNKTE
  addCircleMarkers(
    data = korrekt_spatial_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Korrekt (Spatial)"
  ) %>%
  
  # FEHLERHAFTE PUNKTE
  addCircleMarkers(
    data = fehler_spatial_fuer_karte,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2,         
    opacity = 1,        
    radius = punkt_groesse,
    popup = ~popup_text, 
    group = "Fehler (Spatial)"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben), 
    labels = names(wohnlage_farben),
    title = "Vorhersage (Räumliches GAM)",
    opacity = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Fehler (Spatial)", "Korrekt (Spatial)"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(karte_spatial)

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_spatial, file = "interaktive_karten/interaktive_karte_model_werte_spatial.html", selfcontained = FALSE)

cat("✓ Fertig! Karte erfolgreich als 'interaktive_karte_model_werte_spatial.html' gespeichert.\n")