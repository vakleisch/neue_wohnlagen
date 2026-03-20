# ==========================================
# 1. LIBRARIES LADEN
# ==========================================
# Wir brauchen nur noch diese vier Basis-Pakete!
library(shiny)
library(leaflet)
library(sf)
library(dplyr)



# ==========================================
# 2. FERTIGE DATEN LADEN (Blitzschnell!)
# ==========================================
# Die App lädt beim Start nur noch die drei fertigen WGS84-Dateien
alle_punkte_wgs <- readRDS("alle_punkte_wgs.rds")
wohnlagen_muc_wgs <- readRDS("wohnlagen_muc_wgs.rds")
wohnlage_grenzen_wgs <- readRDS("wohnlage_grenzen_wgs.rds")

# Wir trennen die Punkte schnell wieder in Fehler und Korrekt, 
# damit wir sie auf der Karte getrennt ein- und ausschalten können.
fehler_punkte <- alle_punkte_wgs %>% filter(as.character(Wohnlage_wahr) != as.character(Wohnlage_vorhersage))
korrekt_punkte <- alle_punkte_wgs %>% filter(as.character(Wohnlage_wahr) == as.character(Wohnlage_vorhersage))

# Feste Variablen definieren
wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)

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
# 3. BASE MAP ERSTELLEN
# ==========================================
map_base <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = wohnlagen_muc_wgs, fillColor = ~color,
              fillOpacity = 0.5, color = "black", weight = 0.5, label = ~Wohnlage) %>%
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)

# ==========================================
# 4. SHINY UI 
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
# 5. SHINY SERVER
# ==========================================
server <- function(input, output, session) {
  
  punkt_groesse <- 6 
  
  # Karte 1: Einfach (OHNE HTML Popups, nur Label)
  output$map_simple <- renderLeaflet({
    map_base %>%
      addLegend(position = "bottomright", colors = wohnlage_farben, 
                labels = names(wohnlage_farben), title = "Wohnlage", opacity = 1) %>%
      
      # 1. KORREKTE PUNKTE (Unten)
      addCircleMarkers(data = korrekt_punkte, fillColor = ~color, fillOpacity = 1,
                       color = "black", stroke = TRUE, weight = 1, opacity = 1, 
                       radius = punkt_groesse, popup = ~Wohnlage_wahr, group = "Korrekt") %>%
      
      # 2. FEHLER (Oben)
      addCircleMarkers(data = fehler_punkte, fillColor = ~color, fillOpacity = 1,
                       color = "red", stroke = TRUE, weight = 4, opacity = 1, 
                       radius = punkt_groesse, popup = ~Wohnlage_vorhersage, group = "Fehler") %>%
      
      addLayersControl(overlayGroups = c("Fehler", "Korrekt"), options = layersControlOptions(collapsed = FALSE))
  })
  
  # Karte 2: Detailliert (MIT HTML Popups)
  output$map_detailed <- renderLeaflet({
    map_base %>%
      addLegend(position = "bottomright", colors = wohnlage_farben, 
                labels = names(wohnlage_farben), title = "Wohnlage", opacity = 1) %>%
      
      # 1. KORREKTE PUNKTE (Unten)
      addCircleMarkers(data = korrekt_punkte, fillColor = ~color, fillOpacity = 1,
                       color = "black", stroke = TRUE, weight = 1, opacity = 1,
                       radius = punkt_groesse, popup = ~popup_text, group = "Korrekt") %>%
      
      # 2. FEHLER (Oben)
      addCircleMarkers(data = fehler_punkte, fillColor = ~color, fillOpacity = 1,
                       color = "red", stroke = TRUE, weight = 4, opacity = 1, 
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
    
    farb_palette <- colorNumeric(palette = "YlOrRd", domain = werte, na.color = "transparent")
    legenden_titel <- names(variablen_liste)[variablen_liste == gewaehlte_var]
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
# 6. APP STARTEN
# ==========================================
shinyApp(ui = ui, server = server)
