library(here)
library(yardstick)
library(dplyr)
library(caret)
library(gratia)
library(ggplot2)
library(corrplot)
library(car)
library(mgcv)
library(reshape2)
library(mgcViz)
library(confintr)
library(rcompanion)
library(forcats)
library(stringr)


# visualize_part_effects
# Input: Modell, Subordnername, Dateiname-Präfix
# Ouput: Plots der partiellen Effekte der Smooth-Terme des Modells als .png Bilder
visualize_part_effects <- function(model, file_name_prefix, subfolder_name) {
  folder_path <- file.path("plots", subfolder_name)
  
  # Erstelle den Ordner, falls er noch nicht existiert
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # Hole Namen der Smooth Terms
  smooths_namen <- smooths(model)
  
  # --- SCHLEIFE FÜR PNG-DATEIEN ---
  for (i in smooths_namen) {
    
    # 1. Extrahiere den reinen Variablennamen aus dem Smooth-Term
    var_name <- str_extract(i, "(?<=\\().*(?=\\))")
    
    # 2. Erstelle die saubere Achsenbeschriftung basierend auf deinen Regeln
    x_label <- case_when(
      var_name == "distanz_bahnhof"    ~ "Distanz zum Bahnhof",
      var_name == "nahversorgungs_index" ~ "Nahversorgungsindex",
      var_name == "opnv_index"           ~ "ÖPNV-Index",
      var_name == "hauspreis_index" ~ "Hauspreisindex",
      var_name == "distanz_mittelzentrum" ~ "Distanz zum Mittelzentrum",
      var_name == "distanz_unterzentrum" ~ "Distanz zum Unterzentrum",
      var_name == "distanz_ubahn" ~ "Distanz zur U-Bahn",
      var_name == "distanz_bushaltestelle" ~ "Distanz zur Bushaltestelle",
      TRUE                             ~ var_name # Fallback, falls Name nicht passt
    )
    
    # 3. Erstelle den Plot mit der neuen Beschriftung und ohne Titel
    p <- draw(model, select = i, partial_match = TRUE) + 
      theme_minimal() +
      labs(
        title = NULL,               # Entfernt den Haupttitel
        x = x_label,                # Setzt die neue X-Achsen-Beschriftung
        y = "Partieller Effekt"
      ) +
      geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
      theme(
        axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
        axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
        axis.text = element_text(size = 14)
      )
    
    file_name <- file.path(folder_path, paste0(file_name_prefix, "_", i, ".png"))
    ggsave(file_name, plot = p, width = 7, height = 6)
    
  }
  
  # SCHLEIFE FÜR PDF-DATEI 
  pdf(paste0("plots/", subfolder_name, "/_all_effects.pdf", sep = ""), width = 7, height = 6)
  for(i in smooths_namen) {
    
    # Dieselben Änderungen wie oben
    var_name <- str_extract(i, "(?<=\\().*(?=\\))")
    x_label <- case_when(
      var_name == "distanz_bahnhof"    ~ "Distanz zum Bahnhof",
      var_name == "nahversorgungs_index" ~ "Nahversorgungsindex",
      var_name == "opnv_index"           ~ "ÖPNV-Index",
      var_name == "hauspreis_index" ~ "Hauspreisindex",
      var_name == "distanz_mittelzentrum" ~ "Distanz zum Mittelzentrum",
      var_name == "distanz_unterzentrum" ~ "Distanz zum Unterzentrum",
      var_name == "distanz_ubahn" ~ "Distanz zur U-Bahn",
      var_name == "distanz_bushaltestelle" ~ "Distanz zur Bushaltestelle",
      TRUE                             ~ var_name
    )
    
    p <- draw(model, select = i, partial_match = TRUE) + 
      theme_minimal() +
      labs(
        title = NULL,
        x = x_label,
      ) +
      geom_hline(yintercept = 0, color = "red", linetype = 2)
    
    print(p) 
  }
  dev.off()
  
}


# Odds Eatio Funktion
# Input: Modell, Subordnername, Dateiname, Plot Dimensionen, Schriftgröße
# Output: Odds-Ratio Plots als .png Bilder und Data frame mit 
# Informationen zu den Odds Ratios
visualize_odds_ratios <- function(model, subfolder = "plots", file_name,
                                  width = 10, height = 6, fontsize = 16) {
  
  # Koeffizienten extrahieren
  coef_df <- data.frame(estimate = coef(model), se = summary(model)$se)
  coef_df$term <- rownames(coef_df)
  
  # Daten aufbereiten
  df <- coef_df %>%
    filter(grepl("^straßentyp_gruppe", term)) %>%
    select(term, estimate, se) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(estimate - 1.96 * se),
      CI_upper = exp(estimate + 1.96 * se),
      label = case_match(term, 
                         "straßentyp_gruppeSammelstraße" ~ "Sammelstraße, gute Lage", 
                         "straßentyp_gruppeWohnstraße" ~ "Wohnstraße, gute Lage",
                         "straßentyp_gruppeFußgängerbereich" ~ "Fußgängerbereich, gute Lage",
                         "straßentyp_gruppeSammelstraße.1" ~ "Sammelstraße, beste Lage",
                         "straßentyp_gruppeWohnstraße.1" ~ "Wohnstraße, beste Lage",
                         "straßentyp_gruppeFußgängerbereich.1" ~ "Fußgängerbereich, beste Lage"
      ),
      # Hilfsspalten für die Sortierung
      lage_gruppe = ifelse(str_detect(term, "\\.1$"), "Beste Lage", "Gute Lage"),
      strassen_typ_label = str_remove_all(term, "straßentyp_gruppe|\\.1")
    ) %>%
    # --- HIER IST DIE ENTSCHEIDENDE ÄNDERUNG ---
    # Sortiere zuerst nach der Hauptgruppe, dann nach dem Straßentyp
    arrange(lage_gruppe, strassen_typ_label) %>%
    # -------------------------------------------
  mutate(label = fct_inorder(label)) 
  
  # Plot erstellen
  p <- ggplot(df, aes(x = OR, y = label)) +
    geom_point(color = "darkblue", size = 3) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "gray40") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    geom_text(
      aes(label = sprintf("%.2f", OR)),
      vjust = -1, size = 3.5, color = "black"
    ) +
    xlim(0, max(df$CI_upper) * 1.2) +
    labs(
      x = "Odds Ratio",
      y = "Straßentyp (Referenz: Hauptstraße)"
    ) +
    scale_x_log10() +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = fontsize, face = "bold"),
      axis.title = element_text(size = (fontsize - 2)),
      axis.text = element_text(size = (fontsize -4))
    )
  
  # Speichern
  ggsave(file.path(subfolder, file_name), plot = p, width = width, height = height, dpi = 300)
  
  return(df %>% select(label, OR, CI_lower, CI_upper ))
}
