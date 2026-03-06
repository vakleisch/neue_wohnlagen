library(here)
source(here("daten_verarbeitung", "daten_bearbeitung.R"))
library(ggplot2)
library(data.table)
library(dplyr)

# Variablen betrachten
# Kategoriale Variablen 

# 1. Spaltennamen extrahieren (ohne die geom-Spalte!)
data_colnames_categorial <- data %>% 
  st_drop_geometry() %>% # <--- Das ist der Lebensretter!
  dplyr::select(gemarkung_, layer, zentraler_bereich, wohnlage_bedeutung) %>% 
  colnames()

# 2. Plots in der Schleife erstellen
for(i in data_colnames_categorial) {
  
  plot <- ggplot(model_data_complete, aes(x = .data[[i]])) +
    geom_bar(color = "black") +
    labs(title = paste("Verteilung von", i), x = i, y = "Häufigkeit") +
    theme_bw()
  
  print(plot) # <--- Wichtig: In einer neuen Zeile OHNE das + davor!
}




# 1. Alle numerischen Spalten automatisch finden (ohne die geom-Spalte!)
data_colnames_numeric <- model_data_complete %>% 
  st_drop_geometry() %>% 
  dplyr::select(where(is.numeric)) %>% # Wählt automatisch alle Zahlen-Spalten aus
  colnames()

# 2. Schleife für die Dichtefunktionen erstellen
for(i in data_colnames_numeric) {
  
  plot <- ggplot(model_data_complete, aes(x = .data[[i]])) +
    # geom_density anstelle von geom_bar nutzen
    geom_density(fill = "steelblue", alpha = 0.5, color = "black") + 
    labs(title = paste("Density of", i), x = i, y = "Density") +
    theme_bw()
  
  print(plot)
}

wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)

karte_punkte <- ggplot(data = model_data_complete) +
  # HIER DIE ÄNDERUNG: wohnlage_bedeutung statt wohnlage_ebene
  geom_sf(aes(color = wohnlage_bedeutung), size = 0.1, alpha = 0.3) +
  scale_color_manual(values = wohnlage_farben) +
  labs(
    fill = "Wohnlage",
    color = "Wohnlage"
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 2)) 
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),       
    axis.ticks = element_blank(),      
    panel.grid = element_blank(),      
    panel.border = element_blank(),    
    axis.title = element_blank()
  )

karte_punkte
