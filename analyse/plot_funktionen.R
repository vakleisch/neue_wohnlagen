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
library(ggeffects)




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


#Effekte für die linearen Modelle (als Wahrscheinlichkeiten)
# NEU: Das Argument 'klassen_labels' hinzugefügt
visualize_linear_effects_sicher <- function(model, file_name_prefix, subfolder_name, klassen_labels) {
  
  folder_path <- file.path("plots", subfolder_name)
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  daten <- model$model
  ziel_var <- names(daten)[1]
  
  variablen_namen <- setdiff(names(daten), ziel_var)
  variablen_namen <- variablen_namen[!grepl("^\\(", variablen_namen)] 
  
  cat("Berechne manuelle Effekte für", length(variablen_namen), "Variablen...\n")
  
  pdf_pfad <- file.path(folder_path, "_all_linear_effects.pdf")
  pdf(pdf_pfad, width = 8, height = 6)
  
  wohnlage_farben <- c(
    "durchschnittliche Lage"          = "#e8f5a4",
    "gute Lage"                       = "#afe391",
    "beste Lage"                      = "#7FCDBB",
    "zentrale durchschnittliche Lage" = "#41B6C4",
    "zentrale gute Lage"              = "#1f5a82",
    "zentrale beste Lage"             = "#271352"
  )
  
  get_typical <- function(x) {
    if (is.numeric(x)) median(x, na.rm = TRUE) else names(sort(table(x), decreasing = TRUE))[1]
  }
  
  base_df <- data.frame(lapply(daten, get_typical), stringsAsFactors = FALSE)
  
  for (var_name in variablen_namen) {
    
    x_label <- case_when(
      var_name == "erreichbarkeit_gr10ha_in_metern_adr" ~ "Distanz Grünfläche (>10ha) [m]",
      var_name == "erreichbarkeit_innenstadt_in_minuten_adr" ~ "Fahrtzeit Innenstadt (min)",
      var_name == "erreichbarkeit_naechstehaltestelle_in_minuten_adr" ~ "Erreichbarkeit Haltestelle (min)",
      var_name == "brw_log" ~ "log(Bodenrichtwert)",
      var_name == "grundschul_num" ~ "Fußweg Grundschule [m]",
      var_name == "kitakigaho_num" ~ "Fußweg Kita [m]",
      var_name == "ortszentru_num" ~ "Fußweg Ortszentrum [m]",
      var_name == "spielplatz_num" ~ "Fußweg Spielplatz [m]",
      var_name == "anteil_vf_sv" ~ "Anteil Verkehrsfläche [%]",
      var_name == "anteil_gf_sv" ~ "Anteil Grünfläche [%]",
      TRUE ~ var_name
    )
    
    if (is.numeric(daten[[var_name]])) {
      seq_vals <- seq(min(daten[[var_name]], na.rm = TRUE), max(daten[[var_name]], na.rm = TRUE), length.out = 100)
    } else {
      seq_vals <- unique(na.omit(daten[[var_name]]))
    }
    
    new_data <- base_df[rep(1, length(seq_vals)), , drop = FALSE]
    new_data[[var_name]] <- seq_vals
    
    preds <- predict(model, newdata = new_data, type = "response")
    
    plot_df <- data.frame(x = seq_vals)
    
    # HIER IST DER FIX: Wir nutzen DEINE übergebenen Labels, nicht die 0,1,2 vom Modell!
    for(j in seq_along(klassen_labels)) {
      plot_df[[klassen_labels[j]]] <- preds[, j]
    }
    
    plot_df_long <- pivot_longer(
      plot_df,
      cols = all_of(klassen_labels),
      names_to = "Wohnlage",
      values_to = "Wahrscheinlichkeit"
    )
    
    plot_df_long$Wohnlage <- factor(plot_df_long$Wohnlage, levels = klassen_labels)
    
    p <- ggplot(plot_df_long, aes(x = x, y = Wahrscheinlichkeit, color = Wohnlage)) + 
      geom_line(linewidth = 1) +
      scale_color_manual(values = wohnlage_farben, drop = FALSE) + 
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      theme_minimal() +
      labs(title = NULL, x = x_label, y = "Vorhergesagte Wahrscheinlichkeit") +
      theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    file_name <- file.path(folder_path, paste0(file_name_prefix, "_", var_name, ".png"))
    ggsave(file_name, plot = p, width = 8, height = 6)
    print(p)
  }
  
  dev.off() 
  cat("✓ Alle linearen Effekte erfolgreich berechnet und gespeichert!\n")
}



# Effkte für die linearen modelle: logOdds


visualize_logodds_effects <- function(model, file_name_prefix, subfolder_name, klassen_labels) {
  
  folder_path <- file.path("plots", subfolder_name)
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  daten <- model$model
  ziel_var <- names(daten)[1]
  
  variablen_namen <- setdiff(names(daten), ziel_var)
  variablen_namen <- variablen_namen[!grepl("^\\(", variablen_namen)] 
  
  cat("Berechne lineare Log-Odds für", length(variablen_namen), "Variablen...\n")
  
  pdf_pfad <- file.path(folder_path, "_all_logodds_effects.pdf")
  pdf(pdf_pfad, width = 8, height = 6)
  
  wohnlage_farben <- c(
    "durchschnittliche Lage"          = "#e8f5a4",
    "gute Lage"                       = "#afe391",
    "beste Lage"                      = "#7FCDBB",
    "zentrale durchschnittliche Lage" = "#41B6C4",
    "zentrale gute Lage"              = "#1f5a82",
    "zentrale beste Lage"             = "#271352"
  )
  
  get_typical <- function(x) {
    if (is.numeric(x)) median(x, na.rm = TRUE) else names(sort(table(x), decreasing = TRUE))[1]
  }
  
  base_df <- data.frame(lapply(daten, get_typical), stringsAsFactors = FALSE)
  
  for (var_name in variablen_namen) {
    
    x_label <- case_when(
      var_name == "erreichbarkeit_gr10ha_in_metern_adr" ~ "Distanz Grünfläche (>10ha) [m]",
      var_name == "erreichbarkeit_innenstadt_in_minuten_adr" ~ "Fahrtzeit Innenstadt (min)",
      var_name == "erreichbarkeit_naechstehaltestelle_in_minuten_adr" ~ "Erreichbarkeit Haltestelle (min)",
      var_name == "brw_log" ~ "log(Bodenrichtwert)",
      var_name == "grundschul_num" ~ "Fußweg Grundschule [m]",
      var_name == "kitakigaho_num" ~ "Fußweg Kita [m]",
      var_name == "ortszentru_num" ~ "Fußweg Ortszentrum [m]",
      var_name == "spielplatz_num" ~ "Fußweg Spielplatz [m]",
      var_name == "anteil_vf_sv" ~ "Anteil Verkehrsfläche [%]",
      var_name == "anteil_gf_sv" ~ "Anteil Grünfläche [%]",
      TRUE ~ var_name
    )
    
    if (is.numeric(daten[[var_name]])) {
      seq_vals <- seq(min(daten[[var_name]], na.rm = TRUE), max(daten[[var_name]], na.rm = TRUE), length.out = 100)
    } else {
      seq_vals <- unique(na.omit(daten[[var_name]]))
    }
    
    new_data <- base_df[rep(1, length(seq_vals)), , drop = FALSE]
    new_data[[var_name]] <- seq_vals
    
  
    #type = "link" zieht die nackten, linearen Log-Odds!
    # mgcv gibt hier bei 3 Kategorien nur 2 Spalten zurück (Vergleich zur Referenz)
    # ==========================================================================
    preds <- predict(model, newdata = new_data, type = "link")
    
    plot_df <- data.frame(x = seq_vals)
    
    
    # Referenzklasse bekommt fest die Null, der Rest die Linien
    
    plot_df[[klassen_labels[1]]] <- 0           # Referenzklasse (flache Linie bei 0)
    plot_df[[klassen_labels[2]]] <- preds[, 1]  # Vergleich Klasse 2 vs Referenz
    plot_df[[klassen_labels[3]]] <- preds[, 2]  # Vergleich Klasse 3 vs Referenz
    
    plot_df_long <- pivot_longer(
      plot_df,
      cols = all_of(klassen_labels),
      names_to = "Wohnlage",
      values_to = "Log_Odds"
    )
    
    plot_df_long$Wohnlage <- factor(plot_df_long$Wohnlage, levels = klassen_labels)
    
    
    # Y-Achse angepasst (keine Prozente mehr)
    p <- ggplot(plot_df_long, aes(x = x, y = Log_Odds, color = Wohnlage)) + 
      geom_line(linewidth = 1) +
      scale_color_manual(values = wohnlage_farben, drop = FALSE) + 
      # Eine hilfslinie bei 0 (unsere Referenz) ziehen:
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
      theme_minimal() +
      labs(
        title = NULL, 
        x = x_label, 
        y = paste("Log-Odds (Relativ zu:", klassen_labels[1], ")")
      ) +
      theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 12), 
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    file_name <- file.path(folder_path, paste0(file_name_prefix, "_", var_name, ".png"))
    ggsave(file_name, plot = p, width = 8, height = 6)
    print(p)
  }
  
  dev.off() 
  cat("✓ Alle linearen Log-Odds erfolgreich geplottet!\n")
}


# Überarbeitete Effektfunktionen für die nicht linearen modelle (gams)

# WAHRSCHEINLICHKEITEN (0% bis 100%) FÜR GAMs
visualize_gam_probabilities <- function(model, file_name_prefix, subfolder_name, klassen_labels) {
  
  folder_path <- file.path("plots", subfolder_name)
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  daten <- model$model
  ziel_var <- names(daten)[1]
  variablen_namen <- setdiff(names(daten), ziel_var)
  variablen_namen <- variablen_namen[!grepl("^\\(", variablen_namen)] 
  
  cat("Berechne GAM-Wahrscheinlichkeiten für", length(variablen_namen), "Variablen...\n")
  
  pdf_pfad <- file.path(folder_path, "_all_gam_probabilities.pdf")
  pdf(pdf_pfad, width = 8, height = 6)
  
  wohnlage_farben <- c(
    "durchschnittliche Lage"          = "#e8f5a4",
    "gute Lage"                       = "#afe391",
    "beste Lage"                      = "#7FCDBB",
    "zentrale durchschnittliche Lage" = "#41B6C4",
    "zentrale gute Lage"              = "#1f5a82",
    "zentrale beste Lage"             = "#271352"
  )
  
  get_typical <- function(x) {
    if (is.numeric(x)) median(x, na.rm = TRUE) else names(sort(table(x), decreasing = TRUE))[1]
  }
  base_df <- data.frame(lapply(daten, get_typical), stringsAsFactors = FALSE)
  
  for (var_name in variablen_namen) {
    
    x_label <- case_when(
      var_name == "erreichbarkeit_gr10ha_in_metern_adr" ~ "Distanz Grünfläche (>10ha) [m]",
      var_name == "erreichbarkeit_innenstadt_in_minuten_adr" ~ "Fahrtzeit Innenstadt (min)",
      var_name == "erreichbarkeit_naechstehaltestelle_in_minuten_adr" ~ "Erreichbarkeit Haltestelle (min)",
      var_name == "brw_log" ~ "log(Bodenrichtwert)",
      var_name == "grundschul_num" ~ "Fußweg Grundschule [m]",
      var_name == "kitakigaho_num" ~ "Fußweg Kita [m]",
      var_name == "ortszentru_num" ~ "Fußweg Ortszentrum [m]",
      var_name == "spielplatz_num" ~ "Fußweg Spielplatz [m]",
      var_name == "anteil_vf_sv" ~ "Anteil Verkehrsfläche [%]",
      var_name == "anteil_gf_sv" ~ "Anteil Grünfläche [%]",
      TRUE ~ var_name
    )
    
    if (is.numeric(daten[[var_name]])) {
      # HIER: 200 Punkte für smoothe GAM-Kurven
      seq_vals <- seq(min(daten[[var_name]], na.rm = TRUE), max(daten[[var_name]], na.rm = TRUE), length.out = 200)
    } else {
      seq_vals <- unique(na.omit(daten[[var_name]]))
    }
    
    new_data <- base_df[rep(1, length(seq_vals)), , drop = FALSE]
    new_data[[var_name]] <- seq_vals
    
    preds <- predict(model, newdata = new_data, type = "response")
    
    plot_df <- data.frame(x = seq_vals)
    for(j in seq_along(klassen_labels)) {
      plot_df[[klassen_labels[j]]] <- preds[, j]
    }
    
    plot_df_long <- pivot_longer(plot_df, cols = all_of(klassen_labels), names_to = "Wohnlage", values_to = "Wahrscheinlichkeit")
    plot_df_long$Wohnlage <- factor(plot_df_long$Wohnlage, levels = klassen_labels)
    
    p <- ggplot(plot_df_long, aes(x = x, y = Wahrscheinlichkeit, color = Wohnlage)) + 
      geom_line(linewidth = 1) +
      scale_color_manual(values = wohnlage_farben, drop = FALSE) + 
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      theme_minimal() +
      labs(title = NULL, x = x_label, y = "Vorhergesagte Wahrscheinlichkeit") +
      theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    file_name <- file.path(folder_path, paste0(file_name_prefix, "_", var_name, ".png"))
    ggsave(file_name, plot = p, width = 8, height = 6)
    print(p)
  }
  dev.off() 
  cat("✓ GAM-Wahrscheinlichkeiten erfolgreich geplottet!\n")
}



# LINEARE PRÄDIKTOREN (LOG-ODDS) FÜR GAMs
# (Zeigt die echten, nicht-linearen Smooth-Kurven des Modells)
visualize_gam_logodds <- function(model, file_name_prefix, subfolder_name, klassen_labels) {
  
  folder_path <- file.path("plots", subfolder_name)
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  daten <- model$model
  ziel_var <- names(daten)[1]
  variablen_namen <- setdiff(names(daten), ziel_var)
  variablen_namen <- variablen_namen[!grepl("^\\(", variablen_namen)] 
  
  cat("Berechne GAM-Log-Odds für", length(variablen_namen), "Variablen...\n")
  
  pdf_pfad <- file.path(folder_path, "_all_gam_logodds.pdf")
  pdf(pdf_pfad, width = 8, height = 6)
  
  wohnlage_farben <- c(
    "durchschnittliche Lage"          = "#e8f5a4",
    "gute Lage"                       = "#afe391",
    "beste Lage"                      = "#7FCDBB",
    "zentrale durchschnittliche Lage" = "#41B6C4",
    "zentrale gute Lage"              = "#1f5a82",
    "zentrale beste Lage"             = "#271352"
  )
  
  get_typical <- function(x) {
    if (is.numeric(x)) median(x, na.rm = TRUE) else names(sort(table(x), decreasing = TRUE))[1]
  }
  base_df <- data.frame(lapply(daten, get_typical), stringsAsFactors = FALSE)
  
  for (var_name in variablen_namen) {
    
    x_label <- case_when(
      var_name == "erreichbarkeit_gr10ha_in_metern_adr" ~ "Distanz Grünfläche (>10ha) [m]",
      var_name == "erreichbarkeit_innenstadt_in_minuten_adr" ~ "Fahrtzeit Innenstadt (min)",
      var_name == "erreichbarkeit_naechstehaltestelle_in_minuten_adr" ~ "Erreichbarkeit Haltestelle (min)",
      var_name == "brw_log" ~ "log(Bodenrichtwert)",
      var_name == "grundschul_num" ~ "Fußweg Grundschule [m]",
      var_name == "kitakigaho_num" ~ "Fußweg Kita [m]",
      var_name == "ortszentru_num" ~ "Fußweg Ortszentrum [m]",
      var_name == "spielplatz_num" ~ "Fußweg Spielplatz [m]",
      var_name == "anteil_vf_sv" ~ "Anteil Verkehrsfläche [%]",
      var_name == "anteil_gf_sv" ~ "Anteil Grünfläche [%]",
      TRUE ~ var_name
    )
    
    if (is.numeric(daten[[var_name]])) {
      seq_vals <- seq(min(daten[[var_name]], na.rm = TRUE), max(daten[[var_name]], na.rm = TRUE), length.out = 200)
    } else {
      seq_vals <- unique(na.omit(daten[[var_name]]))
    }
    
    new_data <- base_df[rep(1, length(seq_vals)), , drop = FALSE]
    new_data[[var_name]] <- seq_vals
    
    # type = "link" liefert die nackten Log-Odds der Smooth-Terme
    preds <- predict(model, newdata = new_data, type = "link")
    
    plot_df <- data.frame(x = seq_vals)
    plot_df[[klassen_labels[1]]] <- 0           # Referenzklasse (flache Linie bei 0)
    plot_df[[klassen_labels[2]]] <- preds[, 1]  # Vergleich Klasse 2 vs Referenz
    plot_df[[klassen_labels[3]]] <- preds[, 2]  # Vergleich Klasse 3 vs Referenz
    
    plot_df_long <- pivot_longer(plot_df, cols = all_of(klassen_labels), names_to = "Wohnlage", values_to = "Log_Odds")
    plot_df_long$Wohnlage <- factor(plot_df_long$Wohnlage, levels = klassen_labels)
    
    p <- ggplot(plot_df_long, aes(x = x, y = Log_Odds, color = Wohnlage)) + 
      geom_line(linewidth = 1) +
      scale_color_manual(values = wohnlage_farben, drop = FALSE) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
      theme_minimal() +
      labs(title = NULL, x = x_label, y = paste("Log-Odds (Relativ zu:", klassen_labels[1], ")")) +
      theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 12), 
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    file_name <- file.path(folder_path, paste0(file_name_prefix, "_", var_name, ".png"))
    ggsave(file_name, plot = p, width = 8, height = 6)
    print(p)
  }
  dev.off() 
  cat("✓ GAM-Log-Odds erfolgreich geplottet!\n")
}








# Odds Ratio Funktion
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
