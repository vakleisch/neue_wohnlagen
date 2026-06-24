# ==============================================================================
# KONFUSIONSMATRIZEN ALS PDF
#
# Enthaltene prior_scale-Werte:
# - 0
# - 7.5
# - 30
#
# Teil 1:
# - eine Matrix je prior_scale
# - Diagonale = richtige Klassifikation -> grün
# - Nebendiagonale = Fehlklassifikation -> rot
#
# Teil 2 hinten:
# - absolute Zahlen je prior_scale, grün eingefärbt
# - relative Zeilenprozente je prior_scale, rot bis grün
#
# Alle Plots ohne Legende.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

out_dir <- "results_lin_disc"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

prior_scales_conf <- c(0, 7.5, 30)

pdf_file <- file.path(
  out_dir,
  "konfusionsmatrizen_prior_scale_0_7_5_30_alle_varianten.pdf"
)

csv_file <- file.path(
  out_dir,
  "konfusionsmatrizen_prior_scale_0_7_5_30.csv"
)

wohnlage_order <- c(
  "durchschnittliche Lage",
  "gute Lage",
  "beste Lage"
)


# ==============================================================================
# 2. DATEN LADEN
# ==============================================================================

data_munich <- readRDS(
  "results_lin_disc/data_munich_modellbasis_mit_laerm.rds"
)

SCORE_munich <- readRDS(
  "results_lin_disc/SCORE_munich_mit_laerm.rds"
)

stopifnot(nrow(SCORE_munich) == nrow(data_munich))

if (!is.null(colnames(SCORE_munich))) {
  levels_c <- colnames(SCORE_munich)
} else {
  levels_c <- levels(data_munich$c)
}

data_munich$c <- factor(
  as.character(data_munich$c),
  levels = levels_c
)


# ==============================================================================
# 3. PRIOR-TRANSFORMATION
# ==============================================================================

apply_prior_scale_for_confusion <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(
    1,
    nrow = N,
    ncol = k
  )
  
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted) * PRIOR
  PROB <- PROB / rowSums(PROB)
  
  colnames(PROB) <- levels_c
  
  pred <- levels_c[max.col(PROB)]
  
  return(pred)
}


# ==============================================================================
# 4. KONFUSIONSMATRIX BERECHNEN
# ==============================================================================

make_conf_matrix <- function(prior_scale_i) {
  
  pred_i <- apply_prior_scale_for_confusion(
    SCORE = SCORE_munich,
    data = data_munich,
    levels_c = levels_c,
    prior_scale = prior_scale_i
  )
  
  conf_data <- data_munich %>%
    mutate(
      prior_scale = prior_scale_i,
      wohnlage_alt = as.character(c),
      wohnlage_neu = pred_i
    ) %>%
    filter(
      !is.na(wohnlage_alt),
      !is.na(wohnlage_neu)
    ) %>%
    mutate(
      wohnlage_alt = factor(
        wohnlage_alt,
        levels = wohnlage_order
      ),
      wohnlage_neu = factor(
        wohnlage_neu,
        levels = wohnlage_order
      )
    )
  
  conf_matrix <- conf_data %>%
    count(
      prior_scale,
      wohnlage_alt,
      wohnlage_neu,
      name = "n"
    ) %>%
    complete(
      prior_scale = prior_scale_i,
      wohnlage_alt = factor(wohnlage_order, levels = wohnlage_order),
      wohnlage_neu = factor(wohnlage_order, levels = wohnlage_order),
      fill = list(n = 0)
    ) %>%
    group_by(prior_scale, wohnlage_alt) %>%
    mutate(
      n_true = sum(n),
      anteil = ifelse(n_true > 0, n / n_true, NA_real_),
      anteil_prozent = round(anteil * 100, 1),
      korrekt = wohnlage_alt == wohnlage_neu,
      
      # Haupt-Farblogik:
      # Diagonale: positiver Wert -> grün
      # Fehlklassifikation: negativer Wert -> rot
      farbwert_richtig_falsch = ifelse(
        korrekt,
        anteil_prozent,
        -anteil_prozent
      ),
      
      label_kombi = paste0(
        n,
        "\n",
        anteil_prozent,
        " %"
      ),
      
      label_anzahl = as.character(n),
      label_prozent = paste0(anteil_prozent, " %")
    ) %>%
    ungroup()
  
  return(conf_matrix)
}

conf_matrix_all <- bind_rows(
  lapply(prior_scales_conf, make_conf_matrix)
)

write.csv(
  conf_matrix_all,
  csv_file,
  row.names = FALSE
)


# ==============================================================================
# 5. PLOT 1:
#    RICHTIG = GRÜN, FALSCH = ROT
# ==============================================================================

make_conf_plot_richtig_falsch <- function(conf_matrix_i, prior_scale_i) {
  
  accuracy_i <- conf_matrix_i %>%
    summarise(
      korrekt_n = sum(n[korrekt], na.rm = TRUE),
      gesamt_n = sum(n, na.rm = TRUE),
      accuracy = korrekt_n / gesamt_n
    )
  
  ggplot(
    conf_matrix_i,
    aes(
      x = wohnlage_neu,
      y = wohnlage_alt,
      fill = farbwert_richtig_falsch
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 1.2
    ) +
    geom_text(
      aes(label = label_kombi),
      size = 5.4,
      fontface = "bold",
      color = "black",
      lineheight = 0.9
    ) +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      limits = c(-100, 100),
      guide = "none"
    ) +
    labs(
      title = paste0(
        "Konfusionsmatrix bei prior_scale = ",
        prior_scale_i
      ),
      subtitle = paste0(
        "Grün = richtige Klassifikation, Rot = Fehlklassifikation | Accuracy = ",
        round(accuracy_i$accuracy * 100, 2),
        " %"
      ),
      x = "Vorhergesagte Wohnlage",
      y = "Wahre Wohnlage"
    ) +
    coord_equal() +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      panel.grid = element_blank()
    )
}


# ==============================================================================
# 6. PLOT 2:
#    ABSOLUTE ZAHLEN
#    Diagonale: hohe Anzahl = grün
#    Nebendiagonale: hohe Anzahl = rot
# ==============================================================================

make_conf_plot_anzahl <- function(conf_matrix_i, prior_scale_i) {
  
  max_n_i <- max(conf_matrix_i$n, na.rm = TRUE)
  
  conf_matrix_i <- conf_matrix_i %>%
    mutate(
      farbwert_anzahl = ifelse(
        korrekt,
        n,
        -n
      )
    )
  
  ggplot(
    conf_matrix_i,
    aes(
      x = wohnlage_neu,
      y = wohnlage_alt,
      fill = farbwert_anzahl
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 1.2
    ) +
    geom_text(
      aes(label = label_anzahl),
      size = 6,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      limits = c(-max_n_i, max_n_i),
      guide = "none"
    ) +
    labs(
      title = paste0(
        "Konfusionsmatrix: absolute Zahlen bei prior_scale = ",
        prior_scale_i
      ),
      subtitle = "Grün = korrekt klassifiziert, Rot = Fehlklassifikation",
      x = "Vorhergesagte Wohnlage",
      y = "Wahre Wohnlage"
    ) +
    coord_equal() +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      panel.grid = element_blank()
    )
}

# ==============================================================================
# 7. PLOT 3:
#    ZEILENPROZENTE
#    Diagonale: hoher Anteil = grün
#    Nebendiagonale: hoher Anteil = rot
# ==============================================================================

make_conf_plot_prozent <- function(conf_matrix_i, prior_scale_i) {
  
  conf_matrix_i <- conf_matrix_i %>%
    mutate(
      farbwert_prozent = ifelse(
        korrekt,
        anteil_prozent,
        -anteil_prozent
      )
    )
  
  ggplot(
    conf_matrix_i,
    aes(
      x = wohnlage_neu,
      y = wohnlage_alt,
      fill = farbwert_prozent
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 1.2
    ) +
    geom_text(
      aes(label = label_prozent),
      size = 5.5,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      limits = c(-100, 100),
      guide = "none"
    ) +
    labs(
      title = paste0(
        "Konfusionsmatrix: Zeilenprozente bei prior_scale = ",
        prior_scale_i
      ),
      subtitle = "Grün = korrekt klassifiziert, Rot = Fehlklassifikation",
      x = "Vorhergesagte Wohnlage",
      y = "Wahre Wohnlage"
    ) +
    coord_equal() +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      panel.grid = element_blank()
    )
}


# ==============================================================================
# 8. PDF ERSTELLEN
# ==============================================================================

pdf(
  file = pdf_file,
  width = 8.8,
  height = 6.8
)

# ------------------------------------------------------------------------------
# Teil 1:
# Hauptvariante: richtig grün, falsch rot
# ------------------------------------------------------------------------------

for (p in prior_scales_conf) {
  
  conf_i <- conf_matrix_all %>%
    filter(prior_scale == p)
  
  print(
    make_conf_plot_richtig_falsch(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    )
  )
}

# ------------------------------------------------------------------------------
# Teil 2:
# Hinten angehängt: absolute und relative Variante
# ------------------------------------------------------------------------------

for (p in prior_scales_conf) {
  
  conf_i <- conf_matrix_all %>%
    filter(prior_scale == p)
  
  print(
    make_conf_plot_anzahl(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    )
  )
  
  print(
    make_conf_plot_prozent(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    )
  )
}

dev.off()


# ==============================================================================
# 9. ZUSÄTZLICH PNGS SPEICHERN
# ==============================================================================

for (p in prior_scales_conf) {
  
  conf_i <- conf_matrix_all %>%
    filter(prior_scale == p)
  
  p_suffix <- gsub("\\.", "_", as.character(p))
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0(
        "konfusionsmatrix_prior_scale_",
        p_suffix,
        "_richtig_gruen_falsch_rot.png"
      )
    ),
    plot = make_conf_plot_richtig_falsch(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    ),
    width = 8.8,
    height = 6.8,
    dpi = 300
  )
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0(
        "konfusionsmatrix_prior_scale_",
        p_suffix,
        "_absolute_anzahlen_gruen.png"
      )
    ),
    plot = make_conf_plot_anzahl(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    ),
    width = 8.8,
    height = 6.8,
    dpi = 300
  )
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0(
        "konfusionsmatrix_prior_scale_",
        p_suffix,
        "_zeilenprozente_rot_gruen.png"
      )
    ),
    plot = make_conf_plot_prozent(
      conf_matrix_i = conf_i,
      prior_scale_i = p
    ),
    width = 8.8,
    height = 6.8,
    dpi = 300
  )
}


# ==============================================================================
# 10. KURZE AUSGABE
# ==============================================================================

accuracy_summary <- conf_matrix_all %>%
  group_by(prior_scale) %>%
  summarise(
    n_gesamt = sum(n, na.rm = TRUE),
    n_korrekt = sum(n[korrekt], na.rm = TRUE),
    n_falsch = sum(n[!korrekt], na.rm = TRUE),
    accuracy_prozent = round(n_korrekt / n_gesamt * 100, 2),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("KONFUSIONSMATRIZEN FERTIG\n")
cat("====================================\n")
print(accuracy_summary)
cat("\nPDF gespeichert:\n")
cat(pdf_file, "\n")
cat("\nCSV gespeichert:\n")
cat(csv_file, "\n")
cat("====================================\n")