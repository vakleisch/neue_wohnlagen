

# ==========================================
# 1. LIBRARIES LADEN
# ==========================================
library(shiny)
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

# ==========================================
# 2. GLOBALE DATENVORBEREITUNG 
# (Wird nur einmal beim Start der App ausgeführt)
# ==========================================
source(here("analyse", "model_evaluation.R"))
source(here("analyse", "plot_funktionen.R"))
source(here("daten_verarbeitung", "daten_bearbeitung.R"))

# Modelle laden
model_gam_zentral <- readRDS("modelle/gam_model_zentral.rds")
model_gam_ausserhalb <- readRDS("modelle/gam_model_ausserhalb_b.rds")

wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)

# Lagen der Stadt filtern
wohnlagen_muc_zentral <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
wohnlagen_muc_ausserhalb <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("durchschnittliche Lage",
                         "gute Lage",
                         "beste Lage"))

# Wohnlagegrenzen für zentral und nicht zentral
linien_in_zentral <- st_intersects(wohnlage_grenzen, wohnlagen_muc_zentral, sparse = FALSE)
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

# In sf Objekte umwandeln und filtern
fehler_model_gam_zentral <- st_as_sf(fehler_model_gam_zentral) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
fehler_model_gam_ausserhalb <- st_as_sf(fehler_model_gam_ausserhalb) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))
korrekt_model_gam_zentral <- st_as_sf(korrekt_model_gam_zentral) %>%
  filter(wohnlage_bedeutung %in% c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
korrekt_model_gam_ausserhalb <- st_as_sf(korrekt_model_gam_ausserhalb) %>%
  filter(wohnlage_bedeutung %in% c("durchschnittliche Lage", "gute Lage", "beste Lage"))

# Hilfsfunktion zum Bereinigen von sf-Objekten
prepare_sf_object <- function(sf_obj) {
  sf_obj %>%
    st_zm(drop = TRUE, what = "ZM") %>%     
    st_make_valid()                          
}

# Bereinigung anwenden
wohnlagen_muc2 <- prepare_sf_object(wohnlagen_muc2)
wohnlagen_muc_zentral <- prepare_sf_object(wohnlagen_muc_zentral)
wohnlagen_muc_ausserhalb <- prepare_sf_object(wohnlagen_muc_ausserhalb)
fehler_model_gam_zentral <- prepare_sf_object(fehler_model_gam_zentral)
fehler_model_gam_ausserhalb <- prepare_sf_object(fehler_model_gam_ausserhalb)
korrekt_model_gam_zentral <- prepare_sf_object(korrekt_model_gam_zentral)
korrekt_model_gam_ausserhalb <- prepare_sf_object(korrekt_model_gam_ausserhalb)
wohnlage_grenzen <- prepare_sf_object(wohnlage_grenzen)
wohnlage_grenzen_zentral <- prepare_sf_object(wohnlage_grenzen_zentral)
wohnlage_grenzen_ausserhalb <- prepare_sf_object(wohnlage_grenzen_ausserhalb)

# Faktoren angleichen
levels_kombiniert <- c("durchschnittliche Lage", "gute Lage", "beste Lage",
                       "zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")

fehler_model_gam_zentral$Wohnlage_vorhersage <- factor(fehler_model_gam_zentral$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_zentral$Wohnlage_wahr <- factor(fehler_model_gam_zentral$Wohnlage_wahr, levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(fehler_model_gam_ausserhalb$Wohnlage_vorhersage, levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_wahr <- factor(fehler_model_gam_ausserhalb$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_zentral$Wohnlage_vorhersage <- factor(korrekt_model_gam_zentral$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(korrekt_model_gam_ausserhalb$Wohnlage_vorhersage, levels = levels_kombiniert)
korrekt_model_gam_zentral$Wohnlage_wahr <- factor(korrekt_model_gam_zentral$Wohnlage_wahr, levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_wahr <- factor(korrekt_model_gam_ausserhalb$Wohnlage_wahr, levels = levels_kombiniert)

# ==========================================
# WAHRSCHEINLICHKEITEN MIT PREDICT BERECHNEN
# ==========================================

# 1. Wahrscheinlichkeiten für das ZENTRALE Modell berechnen
# (Ergibt eine Matrix mit 3 Spalten: 1. durchschnittlich, 2. gut, 3. beste)
probs_fehler_zentral <- predict(model_gam_zentral, newdata = fehler_model_gam_zentral, type = "response")
probs_korrekt_zentral <- predict(model_gam_zentral, newdata = korrekt_model_gam_zentral, type = "response")

# Die 3 Spalten an die Datensätze anhängen
fehler_model_gam_zentral$prob_durchschnittlich <- probs_fehler_zentral[, 1]
fehler_model_gam_zentral$prob_gut              <- probs_fehler_zentral[, 2]
fehler_model_gam_zentral$prob_beste            <- probs_fehler_zentral[, 3]

korrekt_model_gam_zentral$prob_durchschnittlich <- probs_korrekt_zentral[, 1]
korrekt_model_gam_zentral$prob_gut              <- probs_korrekt_zentral[, 2]
korrekt_model_gam_zentral$prob_beste            <- probs_korrekt_zentral[, 3]


# 2. Wahrscheinlichkeiten für das AUSSERHALB Modell berechnen
probs_fehler_ausserhalb <- predict(model_gam_ausserhalb, newdata = fehler_model_gam_ausserhalb, type = "response")
probs_korrekt_ausserhalb <- predict(model_gam_ausserhalb, newdata = korrekt_model_gam_ausserhalb, type = "response")

# Auch hier die 3 Spalten anhängen
fehler_model_gam_ausserhalb$prob_durchschnittlich <- probs_fehler_ausserhalb[, 1]
fehler_model_gam_ausserhalb$prob_gut              <- probs_fehler_ausserhalb[, 2]
fehler_model_gam_ausserhalb$prob_beste            <- probs_fehler_ausserhalb[, 3]

korrekt_model_gam_ausserhalb$prob_durchschnittlich <- probs_korrekt_ausserhalb[, 1]
korrekt_model_gam_ausserhalb$prob_gut              <- probs_korrekt_ausserhalb[, 2]
korrekt_model_gam_ausserhalb$prob_beste            <- probs_korrekt_ausserhalb[, 3]


# DAtensätze Kombinieren
fehler_model_gam_kombiniert <- rbind(fehler_model_gam_zentral, fehler_model_gam_ausserhalb)
korrekt_model_gam_kombiniert <- rbind(korrekt_model_gam_zentral, korrekt_model_gam_ausserhalb)

# WGS84 sicherstellen
wohnlagen_muc_wgs <- st_transform(wohnlagen_muc2, 4326) %>%
  mutate(color = case_when(
    Wohnlage == "durchschnittliche Lage" ~ "#e8f5a4",
    Wohnlage == "gute Lage" ~ "#afe391",
    Wohnlage == "beste Lage" ~ "#7FCDBB",
    Wohnlage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage == "zentrale beste Lage" ~ "#271352"
  ))

fehler_model_gam_kombiniert_wgs <- st_transform(fehler_model_gam_kombiniert, crs = 4326)
korrekt_model_gam_kombiniert_wgs <- st_transform(korrekt_model_gam_kombiniert, crs = 4326)
wohnlage_grenzen_wgs <- st_transform(wohnlage_grenzen, crs = 4326)

# ==========================================
# POPUPS VORBEREITEN & FARBEN SETZEN
# ==========================================

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

# Farben zuweisen und Popups an die Datensätze anhängen
fehler_model_gam_kombiniert_wgs <- fehler_model_gam_kombiniert_wgs %>% 
  mutate(
    color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                      Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                      Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                      Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                      Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                      Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"),
    popup_text = erstelle_popup_html(.)
  )

korrekt_model_gam_kombiniert_wgs <- korrekt_model_gam_kombiniert_wgs %>%
  mutate(
    color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                      Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                      Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                      Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                      Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                      Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"),
    popup_text = erstelle_popup_html(.)
  )

# Alle Punkte für Tab 3 zusammenfügen
alle_punkte_wgs <- rbind(fehler_model_gam_kombiniert_wgs, korrekt_model_gam_kombiniert_wgs)

fehler_model_gam_kombiniert_wgs <- fehler_model_gam_kombiniert_wgs %>% 
  mutate(
    color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                      Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                      Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                      Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                      Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                      Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"),
    popup_text = erstelle_popup_html(.)
  )

korrekt_model_gam_kombiniert_wgs <- korrekt_model_gam_kombiniert_wgs %>%
  mutate(
    color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage" ~ "#e8f5a4",
                      Wohnlage_vorhersage == "gute Lage" ~ "#afe391",
                      Wohnlage_vorhersage == "beste Lage" ~ "#7FCDBB",
                      Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                      Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                      Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"),
    popup_text = erstelle_popup_html(.)
  )

# Alle Punkte für Tab 3
alle_punkte_wgs <- rbind(fehler_model_gam_kombiniert_wgs, korrekt_model_gam_kombiniert_wgs)

# ==========================================
# LEAFLET BASE MAP MIT CANVAS
# ==========================================
map_base <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = wohnlagen_muc_wgs, fillColor = ~color,
              fillOpacity = 0.5, color = "black", weight = 0.5, label = ~Wohnlage) %>%
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)

# Dropdown-Optionen
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

# ==========================================
# 3. SHINY UI 
# ==========================================
ui <- fluidPage(
  titlePanel("Evaluierung der Wohnlagen-Modelle (München)"),
  
  tabsetPanel(
    tabPanel("Einfache Ansicht", 
             br(),
             leafletOutput("map_simple", height = "800px")
    ),
    tabPanel("Detailansicht (mit Popups)", 
             br(),
             leafletOutput("map_detailed", height = "800px")
    ),
    tabPanel("Einflussgrößen (Variablen)", 
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Karteneinstellungen"),
                 selectInput("var_auswahl", 
                             "Wähle eine Einflussgröße aus:", 
                             choices = variablen_liste),
                 helpText("Die Punkte färben sich automatisch passend zum Wert der ausgewählten Variable."),
                 width = 3
               ),
               mainPanel(
                 leafletOutput("map_variables", height = "800px"),
                 width = 9
               )
             )
    )
  )
)

# ==========================================
# 4. SHINY SERVER
# ==========================================
server <- function(input, output, session) {
  
  # GLOBALE PUNKTGRÖSSE
  punkt_groesse <- 6 
  
  # Karte 1: Einfach (OHNE HTML Popups, nur Label)
  output$map_simple <- renderLeaflet({
    map_base %>%
      addLegend(position = "bottomright", colors = wohnlage_farben, 
                labels = names(wohnlage_farben), title = "Wohnlage", opacity = 1) %>%
      
      # 1. KORREKTE PUNKTE (Unten) - Dünner, schwarzer Rand
      addCircleMarkers(data = korrekt_model_gam_kombiniert_wgs, fillColor = ~color, fillOpacity = 1,
                       color = "black", stroke = TRUE, weight = 1, opacity = 1, # opacity = 1 macht den Rand massiv
                       radius = punkt_groesse, popup = ~wohnlage_bedeutung, group = "Korrekt") %>%
      
      # 2. FEHLER (Oben) - Dicker, leuchtend roter Rand
      addCircleMarkers(data = fehler_model_gam_kombiniert_wgs, fillColor = ~color, fillOpacity = 1,
                       color = "red", stroke = TRUE, weight = 2, opacity = 1, # weight = 4 macht ihn dick, opacity = 1 massiv rot
                       radius = punkt_groesse, popup = ~Wohnlage_vorhersage, group = "Fehler") %>%
      
      addLayersControl(overlayGroups = c("Fehler", "Korrekt"), options = layersControlOptions(collapsed = FALSE))
  })
  
  # Karte 2: Detailliert (MIT HTML Popups)
  output$map_detailed <- renderLeaflet({
    map_base %>%
      addLegend(position = "bottomright", colors = wohnlage_farben, 
                labels = names(wohnlage_farben), title = "Wohnlage", opacity = 1) %>%
      
      # 1. KORREKTE PUNKTE (Unten)
      addCircleMarkers(data = korrekt_model_gam_kombiniert_wgs, fillColor = ~color, fillOpacity = 1,
                       color = "black", stroke = TRUE, weight = 1, opacity = 1,
                       radius = punkt_groesse, popup = ~popup_text, group = "Korrekt") %>%
      
      # 2. FEHLER (Oben) - Dicker, leuchtend roter Rand
      addCircleMarkers(data = fehler_model_gam_kombiniert_wgs, fillColor = ~color, fillOpacity = 1,
                       color = "red", stroke = TRUE, weight = 2, opacity = 1, 
                       radius = punkt_groesse, popup = ~popup_text, group = "Fehler") %>%
      
      addLayersControl(overlayGroups = c("Fehler", "Korrekt"), options = layersControlOptions(collapsed = FALSE))
  })
  
  # Karte 3: Dynamische Variablenkarte
  output$map_variables <- renderLeaflet({
    map_base %>%
      addLegend(position = "bottomright", colors = wohnlage_farben, 
                labels = names(wohnlage_farben), title = "Wohnlage (Hintergrund)", opacity = 0.8)
  })
  
  observe({
    req(input$var_auswahl) 
    gewaehlte_var <- input$var_auswahl
    werte <- alle_punkte_wgs[[gewaehlte_var]]
    
    # Farbskala berechnen
    farb_palette <- colorNumeric(palette = "YlOrRd", domain = werte, na.color = "transparent")
    legenden_titel <- names(variablen_liste)[variablen_liste == gewaehlte_var]
    
    # Farben an den Datensatz anhängen
    alle_punkte_wgs$dyn_color <- farb_palette(werte)
    
    leafletProxy("map_variables") %>%
      clearGroup("Variablen") %>%
      removeControl("var_legend") %>% 
      addCircleMarkers(data = alle_punkte_wgs,
                       fillColor = ~dyn_color,  
                       fillOpacity = 0.9,
                       color = "black", weight = 0.5,
                       radius = punkt_groesse,    
                       popup = ~popup_text,     
                       group = "Variablen") %>%
      addLegend(
        position = "bottomleft", 
        pal = farb_palette, 
        values = werte,
        title = legenden_titel,
        opacity = 1, 
        layerId = "var_legend" 
      )
  })
}

# ==========================================
# 5. APP STARTEN
# ==========================================
shinyApp(ui = ui, server = server)
