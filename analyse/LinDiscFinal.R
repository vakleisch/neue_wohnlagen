# ==============================================================================
# GESAMTWORKFLOW GANZ MÜNCHEN
# Neuer Ansatz von Göran zur Behandlung des Lärm-Problems
#
# Schritte:
# 1. Daten laden
# 2. Wahre Wohnlage aus wohnlage_ebene ableiten
# 3. y-Variablen erzeugen
# 4. GAM-Modell mit Lärm schätzen
# 5. Score-Matrix berechnen
# 6. Prior-Transformation mit priorscale_7_5
# 7. Wohnlagenflächenanalyse und Problemgebiete
# 8. Validierung: Delta vs. Prior Scale
# 9. kNN-Zuordnung der Hochlärm-Punkte + Abwertung
# 10. Mietspiegelpunkte gegen Problemgebiete prüfen
# 11. Interaktive Gesamtkarte München erstellen
#
# ==============================================================================


# ==============================================================================
# 0. PAKETE
# ==============================================================================

library(sf)
library(dplyr)
library(mgcv)
library(leaflet)
library(htmlwidgets)

if (!requireNamespace("FNN", quietly = TRUE)) {
  install.packages("FNN")
}
library(FNN)

source("daten_verarbeitung/daten_bearbeitung.R")


# ==============================================================================
# 1. ZENTRALE EINSTELLUNGEN
# ==============================================================================

prior_scale <- 7.5
suffix <- "prior_scale_7_5"

k_neighbors <- 10

# Problemgebiet-Regel
min_wohnungen_problemgebiet <- 20
min_anteil_geaendert_problemgebiet <- 0.20

cat("====================================\n")
cat("EINSTELLUNGEN\n")
cat("====================================\n")

cat("Entsprechender prior.scale:", round(prior_scale, 4), "\n")
cat("k für kNN:", k_neighbors, "\n")
cat("Problemgebiet: mindestens",
    min_wohnungen_problemgebiet,
    "Wohnungen und mindestens",
    min_anteil_geaendert_problemgebiet * 100,
    "% geändert\n")
cat("====================================\n")



# ==============================================================================
# 3. DATEN LADEN
# ==============================================================================

# --------------------------------------------------------------------------
# Hauptdatensatz: Punkte ohne hohen Lärm, für Modellschätzung
# --------------------------------------------------------------------------

data_munich <- readRDS("daten/model_munich_data2.rds")
data_munich_laerm <- readRDS("daten/model_data_hoherlärm.rds")


cat("Hauptdatensatz München geladen:", nrow(data_munich), "Zeilen\n")
cat("Hochlärm-Datensatz München geladen:", nrow(data_munich_laerm), "Zeilen\n")


# ==============================================================================
# 4. WAHRE 3-KATEGORIEN-WOHNLAGE AUS wohnlage_ebene ABLEITEN
# ==============================================================================

# Erwartete Kodierung:
# 0 = durchschnittliche Lage
# 1 = gute Lage
# 2 = beste Lage
# 3 = zentrale durchschnittliche Lage
# 4 = zentrale gute Lage
# 5 = zentrale beste Lage
#
# Zentral und nicht-zentral werden auf 3 Kategorien zusammengelegt.

derive_wohnlage_3cat <- function(x) {
  
  vals <- sort(unique(na.omit(x)))
  
  # Fall A: 0 bis 5
  if (all(vals %in% 0:5)) {
    out <- case_when(
      x %in% c(0, 3) ~ "durchschnittliche Lage",
      x %in% c(1, 4) ~ "gute Lage",
      x %in% c(2, 5) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  }
  
  # Fall B: 1 bis 6
  else if (all(vals %in% 1:6)) {
    out <- case_when(
      x %in% c(1, 4) ~ "durchschnittliche Lage",
      x %in% c(2, 5) ~ "gute Lage",
      x %in% c(3, 6) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  }
  
  else {
    stop("Unbekannte Kodierung von wohnlage_ebene.")
  }
  
  return(out)
}


data_munich$c <- derive_wohnlage_3cat(data_munich$wohnlage_ebene)

data_munich$c <- factor(
  data_munich$c,
  levels = c(
    "beste Lage",
    "durchschnittliche Lage",
    "gute Lage"
  )
)

cat("\nVerteilung der wahren 3-Kategorien-Wohnlage im Hauptdatensatz:\n")
print(table(data_munich$c, useNA = "ifany"))

levels_c <- levels(data_munich$c)
k <- length(levels_c)

cat("Wohnlagenklassen:", paste(levels_c, collapse = ", "), "\n")


# Hochlärm-Datensatz ebenfalls mit alter 3-Kategorien-Wohnlage versehen
if ("wohnlage_ebene" %in% names(data_munich_laerm)) {
  
  data_munich_laerm$wohnlage_alt_3cat <-
    derive_wohnlage_3cat(data_munich_laerm$wohnlage_ebene)
  
  cat("\nVerteilung der wahren 3-Kategorien-Wohnlage im Hochlärm-Datensatz:\n")
  print(table(data_munich_laerm$wohnlage_alt_3cat, useNA = "ifany"))
}


# ==============================================================================
# 5. Y-VARIABLEN ERZEUGEN
# ==============================================================================

data_munich$y1  <- data_munich$erreichbarkeit_gr10ha_in_metern_adr
data_munich$y2  <- data_munich$erreichbarkeit_innenstadt_in_minuten_adr
data_munich$y3  <- data_munich$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data_munich$y4  <- data_munich$grundschul_num
data_munich$y5  <- data_munich$spielplatz_num
data_munich$y6  <- data_munich$kitakigaho_num
data_munich$y7  <- data_munich$ortszentru_num
data_munich$y8  <- data_munich$brw_log
data_munich$y9  <- data_munich$anteil_vf_sv
data_munich$y10 <- data_munich$anteil_gf_sv
data_munich$y11 <- data_munich$laerm


# ==============================================================================
# 6. VOLLSTÄNDIGE ZEILEN FÜR MODELL MIT LÄRM
# ==============================================================================

# Genau wie beim bisherigen Modell:
# y8 = brw_log wird NICHT als Response verwendet.

y_vars_model <- c(
  "y1", "y2", "y3", "y4", "y5",
  "y6", "y7", "y9", "y10", "y11"
)

vars_needed <- c(
  "c",
  "s.long",
  "s.lat",
  y_vars_model
)

missing_vars <- setdiff(vars_needed, names(data_munich))

if (length(missing_vars) > 0) {
  stop(
    paste(
      "Im Hauptdatensatz fehlen Variablen:",
      paste(missing_vars, collapse = ", ")
    )
  )
}

data_munich <- data_munich %>%
  filter(complete.cases(across(all_of(vars_needed))))

N <- nrow(data_munich)
d <- length(y_vars_model)

cat("\nMünchen-Punkte nach complete.cases:", N, "\n")
cat("Anzahl Modell-Responses d:", d, "\n")

saveRDS(
  data_munich,
  "results_lin_disc/data_munich_modellbasis_mit_laerm.rds"
)


# ==============================================================================
# 7. MODELL GANZ MÜNCHEN SCHÄTZEN ODER LADEN
# ==============================================================================

model_file <- "results_lin_disc/model_munich_mit_laerm.rds"

if (file.exists(model_file)) {
  
  cat("\nVorhandenes München-Modell wird geladen:\n")
  cat(model_file, "\n")
  
  model_munich <- readRDS(model_file)
  
} else {
  
  cat("\nSchätze München-Modell mit Lärm neu...\n")
  
  start_zeit <- Sys.time()
  
  model_munich <- gam(
    list(
      y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
      y10 ~ s(s.long, s.lat, by = c, k = 15) + c,
      y11 ~ s(s.long, s.lat, by = c, k = 15) + c
    ),
    family = mvn(d = d),
    data = data_munich,
    optimizer = "efs",
    control = gam.control(trace = TRUE)
  )
  
  cat("\nModell geschätzt in:\n")
  print(Sys.time() - start_zeit)
  
  saveRDS(
    model_munich,
    model_file
  )
}


# ==============================================================================
# 8. VARIANZSTRUKTUR
# ==============================================================================

VAR <- solve(crossprod(model_munich$family$data$R))
INV_VAR <- solve(VAR)

saveRDS(
  list(VAR = VAR, INV_VAR = INV_VAR),
  "results_lin_disc/varianzstruktur_munich_mit_laerm.rds"
)


# ==============================================================================
# 9. SCORE-MATRIX BERECHNEN ODER LADEN
# ==============================================================================

score_file <- "results_lin_disc/SCORE_munich_mit_laerm.rds"

if (file.exists(score_file)) {
  
  cat("\nVorhandene Score-Matrix wird geladen:\n")
  cat(score_file, "\n")
  
  SCORE_munich <- readRDS(score_file)
  
} else {
  
  cat("\nBerechne Score-Matrix für ganz München...\n")
  
  Y_munich <- as.matrix(data_munich[, y_vars_model])
  
  SCORE_munich <- matrix(
    0,
    nrow = N,
    ncol = k
  )
  
  colnames(SCORE_munich) <- levels_c
  
  start_score <- Sys.time()
  
  for (j in seq_len(k)) {
    
    cat("Berechne Score für Klasse",
        j, "von", k, ":", levels_c[j], "\n")
    
    tmp <- data_munich
    tmp$c <- factor(
      levels_c[j],
      levels = levels_c
    )
    
    fit <- predict(model_munich, newdata = tmp)
    
    diff <- Y_munich - fit
    
    score_temp <- (diff %*% INV_VAR) * diff
    
    SCORE_munich[, j] <- rowSums(score_temp)
  }
  
  cat("\nScore-Matrix berechnet in:\n")
  print(Sys.time() - start_score)
  
  saveRDS(
    SCORE_munich,
    score_file
  )
}

stopifnot(nrow(SCORE_munich) == nrow(data_munich))


# ==============================================================================
# 10. PRIOR-TRANSFORMATION MIT PRIOR.SCALE AUS DELTA
# ==============================================================================

apply_prior_scale_direct <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  
  colnames(PROB_prior) <- levels_c
  
  pred_idx <- max.col(PROB_prior)
  pred <- levels_c[pred_idx]
  
  old_idx <- match(as.character(data$c), levels_c)
  
  current_score <- SCORE[cbind(seq_len(N), old_idx)]
  new_score <- SCORE[cbind(seq_len(N), pred_idx)]
  best_score <- apply(SCORE, 1, min)
  
  return(list(
    PROB = PROB_prior,
    pred_idx = pred_idx,
    pred = pred,
    current_score = current_score,
    new_score = new_score,
    best_score = best_score,
    realized_improvement = current_score - new_score,
    potential_improvement = current_score - best_score
  ))
}


prior_result_file <- paste0(
  "results_lin_disc/prior_result_munich_",
  suffix,
  ".rds"
)

if (file.exists(prior_result_file)) {
  
  cat("\nVorhandene Prior-Ergebnisse werden geladen:\n")
  cat(prior_result_file, "\n")
  
  prior_res_munich <- readRDS(prior_result_file)
  
} else {
  
  cat("\nBerechne Prior-Transformation für ganz München...\n")
  
  prior_res_munich <- apply_prior_scale_direct(
    SCORE = SCORE_munich,
    data = data_munich,
    levels_c = levels_c,
    prior_scale = prior_scale
  )
  
  saveRDS(
    prior_res_munich,
    prior_result_file
  )
}


# ==============================================================================
# 11. PRIOR-ERGEBNISSE AN MÜNCHEN-DATEN ANHÄNGEN
# ==============================================================================

data_munich <- data_munich %>%
  mutate(
    wohnlage_alt = as.character(c),
    wohnlage_neu = prior_res_munich$pred,
    changed = wohnlage_alt != wohnlage_neu,
    
    score_alt = prior_res_munich$current_score,
    score_neu = prior_res_munich$new_score,
    score_best = prior_res_munich$best_score,
    
    realized_improvement =
      prior_res_munich$realized_improvement,
    
    potential_improvement =
      prior_res_munich$potential_improvement,
    
    prob_max = apply(prior_res_munich$PROB, 1, max)
  )

cat("\n====================================\n")
cat("PRIOR-ERGEBNIS GANZ MÜNCHEN\n")
cat("====================================\n")
cat("Änderungsrate:",
    round(mean(data_munich$changed) * 100, 2),
    "%\n")
cat("Realisierte mittlere Verbesserung:",
    round(mean(data_munich$realized_improvement), 4),
    "\n")
cat("====================================\n")

saveRDS(
  data_munich,
  "results_lin_disc/data_munich_prior_scale_7_5.rds"
)

saveRDS(
  prior_res_munich$PROB,
  "results_lin_disc/PROB_munich_prior_scale_7_5.rds"
)


# ==============================================================================
# 12. WOHNLAGENFLÄCHEN LADEN UND FLÄCHEN-ID VERGEBEN
# ==============================================================================

wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds") %>%
  st_as_sf() %>%
  mutate(
    flaechen_id = row_number()
  )

cat("\nAnzahl Wohnlagenflächen insgesamt:",
    nrow(wohnlagen_muc_wgs), "\n")


# ==============================================================================
# 13. MÜNCHEN-PUNKTE EINDEUTIG EINER FLÄCHE ZUORDNEN
# ==============================================================================

data_munich_sf <- st_as_sf(
  data_munich,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

if (st_crs(data_munich_sf) != st_crs(wohnlagen_muc_wgs)) {
  data_munich_sf <- st_transform(
    data_munich_sf,
    st_crs(wohnlagen_muc_wgs)
  )
}

idx_list <- st_intersects(
  data_munich_sf,
  wohnlagen_muc_wgs
)

n_matches <- lengths(idx_list)

cat("\nZuordnung Punkte -> Flächen:\n")
cat("Punkte ohne Fläche:",
    sum(n_matches == 0), "\n")
cat("Punkte mit genau einer Fläche:",
    sum(n_matches == 1), "\n")
cat("Punkte mit mehreren Flächen:",
    sum(n_matches > 1), "\n")

polygon_idx <- sapply(idx_list, function(x) {
  if (length(x) == 0) {
    NA_integer_
  } else {
    x[1]
  }
})

data_munich_joined <- data_munich_sf %>%
  mutate(
    flaechen_id = wohnlagen_muc_wgs$flaechen_id[polygon_idx],
    Wohnlage_flaeche = wohnlagen_muc_wgs$Wohnlage[polygon_idx]
  )

saveRDS(
  data_munich_joined,
  "results_lin_disc/data_munich_joined_prior_scale_7_5.rds"
)


# ==============================================================================
# 14. PUNKTE OHNE FLÄCHE IGNORIEREN
# ==============================================================================

data_munich_in_area <- data_munich_joined %>%
  filter(!is.na(flaechen_id))

cat("\nMünchen-Punkte insgesamt:",
    nrow(data_munich_joined), "\n")
cat("München-Punkte mit Fläche:",
    nrow(data_munich_in_area), "\n")
cat("Ignorierte Punkte ohne Fläche:",
    nrow(data_munich_joined) - nrow(data_munich_in_area),
    "\n")

saveRDS(
  data_munich_in_area,
  "results_lin_disc/data_munich_in_area_prior_scale_7_5.rds"
)


# ==============================================================================
# 15. ÄNDERUNGSANTEIL JE WOHNLAGENFLÄCHE IN GANZ MÜNCHEN
# ==============================================================================

gebiet_summary_munich <- data_munich_in_area %>%
  st_drop_geometry() %>%
  group_by(flaechen_id) %>%
  summarise(
    n_wohnungen = n(),
    n_geaendert = sum(changed, na.rm = TRUE),
    
    anteil_geaendert =
      n_geaendert / n_wohnungen,
    
    anteil_geaendert_prozent =
      round(anteil_geaendert * 100, 2),
    
    mittlere_realisierte_verbesserung =
      mean(realized_improvement, na.rm = TRUE),
    
    mittlere_potenzielle_verbesserung =
      mean(potential_improvement, na.rm = TRUE),
    
    mittlere_max_prob =
      mean(prob_max, na.rm = TRUE),
    
    alte_lage_haeufig =
      names(sort(table(wohnlage_alt), decreasing = TRUE))[1],
    
    neue_lage_haeufig =
      names(sort(table(wohnlage_neu), decreasing = TRUE))[1],
    
    .groups = "drop"
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("\nTop 50 Gebiete nach Änderungsanteil:\n")
print(head(gebiet_summary_munich, 50))

saveRDS(
  gebiet_summary_munich,
  "results_lin_disc/gebiet_summary_munich_prior_scale_7_5.rds"
)


# ==============================================================================
# 16. SUMMARY AN FLÄCHENOBJEKTE JOINEN
# ==============================================================================

wohnlagen_munich_analyse <- wohnlagen_muc_wgs %>%
  left_join(
    gebiet_summary_munich,
    by = "flaechen_id"
  ) %>%
  filter(!is.na(n_wohnungen))

cat("\nWohnlagenflächen mit mindestens einem analysierten Punkt:",
    nrow(wohnlagen_munich_analyse), "\n")

saveRDS(
  wohnlagen_munich_analyse,
  "results_lin_disc/wohnlagen_munich_analyse_prior_scale_7_5.rds"
)


# ==============================================================================
# 17. POTENZIELLE PROBLEMGEBIETE GANZ MÜNCHEN
# ==============================================================================

problemgebiete_munich <- wohnlagen_munich_analyse %>%
  filter(
    n_wohnungen >= min_wohnungen_problemgebiet,
    anteil_geaendert >= min_anteil_geaendert_problemgebiet
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("\n====================================\n")
cat("PROBLEMGEBIETE GANZ MÜNCHEN\n")
cat("====================================\n")
cat("Anzahl Problemgebiete:",
    nrow(problemgebiete_munich), "\n")
cat("====================================\n")

problemgebiete_munich %>%
  st_drop_geometry() %>%
  select(
    flaechen_id,
    Wohnlage,
    n_wohnungen,
    n_geaendert,
    anteil_geaendert_prozent,
    mittlere_realisierte_verbesserung,
    alte_lage_haeufig,
    neue_lage_haeufig
  ) %>%
  head(50) %>%
  print()

saveRDS(
  problemgebiete_munich,
  "results_lin_disc/problemgebiete_munich_prior_scale_7_5.rds"
)


# ==============================================================================
# 18. VALIDIERUNG:
#     SIND DELTA UND ÄQUIVALENTER PRIOR SCALE IDENTISCH?
# ==============================================================================

chosen_delta <- log1p(prior_scale)
equivalent_prior_scale <- prior_scale
same_numeric_prior_scale <- prior_scale

apply_prior_scale_compare <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted) * PRIOR
  PROB <- PROB / rowSums(PROB)
  colnames(PROB) <- levels_c
  
  pred_idx <- max.col(PROB)
  pred <- levels_c[pred_idx]
  
  old_class <- as.character(data$c)
  changed <- pred != old_class
  
  return(list(
    prior_scale = prior_scale,
    delta_equivalent = log(1 + prior_scale),
    PROB = PROB,
    pred_idx = pred_idx,
    pred = pred,
    changed = changed
  ))
}

apply_prior_delta_compare <- function(SCORE, data, levels_c, delta) {
  
  prior_scale <- exp(delta) - 1
  
  res <- apply_prior_scale_compare(
    SCORE = SCORE,
    data = data,
    levels_c = levels_c,
    prior_scale = prior_scale
  )
  
  res$delta <- delta
  
  return(res)
}

res_delta <- apply_prior_delta_compare(
  SCORE = SCORE_munich,
  data = data_munich,
  levels_c = levels_c,
  delta = chosen_delta
)

res_prior_equivalent <- apply_prior_scale_compare(
  SCORE = SCORE_munich,
  data = data_munich,
  levels_c = levels_c,
  prior_scale = equivalent_prior_scale
)

res_prior_same_numeric <- apply_prior_scale_compare(
  SCORE = SCORE_munich,
  data = data_munich,
  levels_c = levels_c,
  prior_scale = same_numeric_prior_scale
)

old_class <- as.character(data_munich$c)

vergleich_equivalent_munich <- data.frame(
  wohnlage_alt = old_class,
  
  wohnlage_neu_delta = res_delta$pred,
  wohnlage_neu_prior_equiv = res_prior_equivalent$pred,
  
  changed_delta = res_delta$changed,
  changed_prior_equiv = res_prior_equivalent$changed,
  
  gleiche_neue_klasse =
    res_delta$pred == res_prior_equivalent$pred,
  
  gleiche_aenderungsentscheidung =
    res_delta$changed == res_prior_equivalent$changed
)

cat("\n====================================\n")
cat("VALIDIERUNG DELTA VS. ÄQUIVALENTER PRIOR SCALE\n")
cat("====================================\n")
cat("Alle neuen Klassen identisch:",
    all(vergleich_equivalent_munich$gleiche_neue_klasse), "\n")
cat("Alle Änderungsentscheidungen identisch:",
    all(vergleich_equivalent_munich$gleiche_aenderungsentscheidung), "\n")
cat("Abweichende Klassen:",
    sum(!vergleich_equivalent_munich$gleiche_neue_klasse), "\n")
cat("Abweichende Änderungsentscheidungen:",
    sum(!vergleich_equivalent_munich$gleiche_aenderungsentscheidung), "\n")
cat("====================================\n")

saveRDS(
  vergleich_equivalent_munich,
  "results_lin_disc/vergleich_delta_vs_prior_equivalent_munich.rds"
)


# ==============================================================================
# 19. KNN-ZUORDNUNG DER HOCHLÄRM-PUNKTE
# ==============================================================================

required_reference_vars <- c(
  "s.long",
  "s.lat",
  "wohnlage_neu"
)

missing_reference_vars <- setdiff(
  required_reference_vars,
  names(data_munich)
)

if (length(missing_reference_vars) > 0) {
  stop(
    paste(
      "In data_munich fehlen:",
      paste(missing_reference_vars, collapse = ", ")
    )
  )
}

required_laerm_vars <- c(
  "s.long",
  "s.lat"
)

missing_laerm_vars <- setdiff(
  required_laerm_vars,
  names(data_munich_laerm)
)

if (length(missing_laerm_vars) > 0) {
  stop(
    paste(
      "Im Hochlärm-Datensatz fehlen:",
      paste(missing_laerm_vars, collapse = ", ")
    )
  )
}

reference_sf <- st_as_sf(
  data_munich,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

laerm_sf <- st_as_sf(
  data_munich_laerm,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

reference_sf_utm <- st_transform(reference_sf, 25832)
laerm_sf_utm <- st_transform(laerm_sf, 25832)

reference_coords <- st_coordinates(reference_sf_utm)
laerm_coords <- st_coordinates(laerm_sf_utm)

knn_file <- paste0(
  "results_lin_disc/knn_munich_hochlaerm_k",
  k_neighbors,
  ".rds"
)

if (file.exists(knn_file)) {
  
  cat("\nVorhandenes kNN-Ergebnis wird geladen:\n")
  cat(knn_file, "\n")
  
  knn_res <- readRDS(knn_file)
  
} else {
  
  cat("\nBerechne kNN für Hochlärm-Punkte...\n")
  
  knn_res <- FNN::get.knnx(
    data = reference_coords,
    query = laerm_coords,
    k = k_neighbors,
    algorithm = "kd_tree"
  )
  
  saveRDS(
    knn_res,
    knn_file
  )
}

neighbor_index_matrix <- knn_res$nn.index
neighbor_distance_matrix <- knn_res$nn.dist


wohnlage_order <- c(
  "durchschnittliche Lage",
  "gute Lage",
  "beste Lage"
)

majority_vote_conservative <- function(classes) {
  
  tab <- table(factor(classes, levels = wohnlage_order))
  
  max_count <- max(tab)
  winner_classes <- names(tab)[tab == max_count]
  
  winner_idx <- match(winner_classes, wohnlage_order)
  
  winner <- wohnlage_order[min(winner_idx)]
  
  return(winner)
}

wohnlage_knn_basis <- apply(
  neighbor_index_matrix,
  1,
  function(idx) {
    neighbor_classes <- data_munich$wohnlage_neu[idx]
    majority_vote_conservative(neighbor_classes)
  }
)

knn_vote_share <- apply(
  neighbor_index_matrix,
  1,
  function(idx) {
    
    neighbor_classes <- data_munich$wohnlage_neu[idx]
    
    tab <- table(factor(neighbor_classes, levels = wohnlage_order))
    
    max(tab) / sum(tab)
  }
)

downgrade_one_level <- function(x) {
  
  case_when(
    x == "beste Lage" ~ "gute Lage",
    x == "gute Lage" ~ "durchschnittliche Lage",
    x == "durchschnittliche Lage" ~ "durchschnittliche Lage",
    TRUE ~ NA_character_
  )
}

wohnlage_nach_laerm <- downgrade_one_level(
  wohnlage_knn_basis
)

data_munich_laerm_knn <- data_munich_laerm %>%
  mutate(
    knn_k = k_neighbors,
    
    wohnlage_knn_basis = wohnlage_knn_basis,
    wohnlage_nach_laerm = wohnlage_nach_laerm,
    
    knn_vote_share = knn_vote_share,
    
    distanz_naechster_nachbar_m =
      neighbor_distance_matrix[, 1],
    
    distanz_mittlere_k_nachbarn_m =
      rowMeans(neighbor_distance_matrix),
    
    abgewertet_durch_laerm =
      wohnlage_knn_basis != wohnlage_nach_laerm
  )

cat("\n====================================\n")
cat("KNN-HOCHLÄRM-AUSWERTUNG GANZ MÜNCHEN\n")
cat("====================================\n")
cat("Anzahl Hochlärm-Punkte:",
    nrow(data_munich_laerm_knn), "\n")

cat("\nWohnlage aus kNN-Votum:\n")
print(table(data_munich_laerm_knn$wohnlage_knn_basis, useNA = "ifany"))

cat("\nWohnlage nach Lärm-Abwertung:\n")
print(table(data_munich_laerm_knn$wohnlage_nach_laerm, useNA = "ifany"))

cat("\nAnteil tatsächlich abgewertet:",
    round(mean(data_munich_laerm_knn$abgewertet_durch_laerm) * 100, 2),
    "%\n")

cat("\nEindeutigkeit des kNN-Votums:\n")
print(summary(data_munich_laerm_knn$knn_vote_share))

cat("====================================\n")

saveRDS(
  data_munich_laerm_knn,
  "results_lin_disc/data_munich_hochlaerm_knn_abgewertet.rds"
)


# ==============================================================================
# 20. MIETSPIEGELDATENSATZ LADEN UND GEGEN PROBLEMGEBIETE PRÜFEN
# ==============================================================================

mietspiegel <- read.csv(
  "daten/ADR_MSP27_20260513.csv",
  sep = ";",
  colClasses = c(adressid = "character")
)

mietspiegel <- mietspiegel %>%
  mutate(adressid = as.character(adressid))

raeumliche_daten <- raeumliche_daten %>%
  mutate(adressid = as.character(adressid))

mietspiegel_geo <- mietspiegel %>%
  left_join(
    raeumliche_daten %>%
      select(adressid, geom),
    by = "adressid"
  ) %>%
  st_as_sf(sf_column_name = "geom")

mietspiegel_geo <- mietspiegel_geo %>%
  filter(!st_is_empty(geom))

if (st_crs(mietspiegel_geo) != st_crs(problemgebiete_munich)) {
  mietspiegel_geo <- st_transform(
    mietspiegel_geo,
    st_crs(problemgebiete_munich)
  )
}

trefferliste_mietspiegel <- st_intersects(
  mietspiegel_geo,
  problemgebiete_munich
)

mietspiegel_geo$in_problemgebiet <-
  lengths(trefferliste_mietspiegel) > 0

mietspiegel_geo$problemgebiet_flaechen_id <- sapply(
  trefferliste_mietspiegel,
  function(x) {
    if (length(x) == 0) {
      NA_integer_
    } else {
      problemgebiete_munich$flaechen_id[x[1]]
    }
  }
)

cat("\n====================================\n")
cat("MIETSPIEGEL UND PROBLEMGEBIETE\n")
cat("====================================\n")
cat("Mietspiegel-Punkte mit Geometrie:",
    nrow(mietspiegel_geo), "\n")
cat("Davon in Problemgebieten:",
    sum(mietspiegel_geo$in_problemgebiet), "\n")
cat("Anteil:",
    round(mean(mietspiegel_geo$in_problemgebiet) * 100, 2),
    "%\n")
cat("====================================\n")

mietspiegel_problemgebiet_summary <- mietspiegel_geo %>%
  st_drop_geometry() %>%
  filter(in_problemgebiet == TRUE) %>%
  count(
    problemgebiet_flaechen_id,
    name = "anzahl_mietspiegel_punkte"
  ) %>%
  arrange(desc(anzahl_mietspiegel_punkte))

print(mietspiegel_problemgebiet_summary)

saveRDS(
  mietspiegel_geo,
  "results_lin_disc/mietspiegel_geo_problemgebiete_munich.rds"
)

saveRDS(
  mietspiegel_problemgebiet_summary,
  "results_lin_disc/mietspiegel_problemgebiet_summary_munich.rds"
)


# ==============================================================================
# 21. INTERAKTIVE GESAMTKARTE GANZ MÜNCHEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

clean_wohnlage <- function(x) {
  trimws(gsub("zentrale", "", as.character(x), ignore.case = TRUE))
}

data_munich_joined_wgs <- st_transform(data_munich_joined, 4326)
wohnlagen_munich_analyse_wgs <- st_transform(wohnlagen_munich_analyse, 4326)
problemgebiete_munich_wgs <- st_transform(problemgebiete_munich, 4326)

if (inherits(data_munich_laerm_knn, "sf")) {
  data_munich_laerm_knn_wgs <- st_transform(data_munich_laerm_knn, 4326)
} else {
  data_munich_laerm_knn_wgs <- st_as_sf(
    data_munich_laerm_knn,
    coords = c("s.long", "s.lat"),
    crs = 4326,
    remove = FALSE
  )
}

wohnlagen_munich_analyse_wgs <- wohnlagen_munich_analyse_wgs %>%
  mutate(
    Wohnlage_3cat = clean_wohnlage(Wohnlage),
    flaechenfarbe = unname(wohnlage_farben_3[Wohnlage_3cat])
  )

problemgebiete_munich_wgs <- problemgebiete_munich_wgs %>%
  mutate(
    Wohnlage_3cat = clean_wohnlage(Wohnlage)
  )

modellpunkte_changed <- data_munich_joined_wgs %>%
  filter(changed == TRUE) %>%
  mutate(
    punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
  )

modellpunkte_unchanged <- data_munich_joined_wgs %>%
  filter(changed == FALSE) %>%
  mutate(
    punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
  )

data_munich_laerm_knn_wgs <- data_munich_laerm_knn_wgs %>%
  mutate(
    punktfarbe_laerm =
      unname(wohnlage_farben_3[wohnlage_nach_laerm])
  )

mietspiegel_geo_wgs <- st_transform(mietspiegel_geo, 4326)

coords_mietspiegel <- st_coordinates(mietspiegel_geo_wgs)

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    s.long = coords_mietspiegel[, 1],
    s.lat = coords_mietspiegel[, 2]
  )

mietspiegel_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == TRUE)

mietspiegel_nicht_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == FALSE)


# ==============================================================================
# 21A. POPUPS MODELLPUNKTE
# ==============================================================================

popup_modellpunkte <- function(df) {
  paste0(
    "<b>Modellierter München-Punkt</b><br>",
    "<hr>",
    "<b>Alte Wohnlage:</b> ", df$wohnlage_alt, "<br>",
    "<b>Neue Wohnlage:</b> ",
    ifelse(
      df$changed,
      paste0(
        "<span style='color:red; font-weight:bold;'>",
        df$wohnlage_neu,
        "</span>"
      ),
      df$wohnlage_neu
    ),
    "<br>",
    "<b>Umklassifiziert:</b> ",
    ifelse(df$changed, "Ja", "Nein"),
    "<br>",
    "<b>Lärmwert:</b> ",
    ifelse(is.na(df$laerm), "n. v.", df$laerm),
    "<br>",
    "<hr>",
    "<b>Score alt:</b> ",
    round(df$score_alt, 3),
    "<br>",
    "<b>Score neu:</b> ",
    round(df$score_neu, 3),
    "<br>",
    "<b>Realisierte Verbesserung:</b> ",
    round(df$realized_improvement, 3),
    "<br>",
    "<b>Potenzielle Verbesserung:</b> ",
    round(df$potential_improvement, 3),
    "<br>",
    "<b>Max. Prior-Wahrscheinlichkeit:</b> ",
    round(df$prob_max * 100, 1),
    " %<br>",
    "<hr>",
    "<b>Flächen-ID:</b> ",
    ifelse(is.na(df$flaechen_id), "keine Zuordnung", df$flaechen_id),
    "<br>"
  )
}

modellpunkte_changed$popup_text <-
  popup_modellpunkte(modellpunkte_changed)

modellpunkte_unchanged$popup_text <-
  popup_modellpunkte(modellpunkte_unchanged)


# ==============================================================================
# 21B. POPUPS LÄRMPUNKTE
# ==============================================================================

popup_laermpunkte <- function(df) {
  paste0(
    "<b>Hochlärm-Punkt</b><br>",
    "<hr>",
    if ("wohnlage_alt_3cat" %in% names(df)) {
      paste0(
        "<b>Alte Wohnlage:</b> ",
        df$wohnlage_alt_3cat,
        "<br>"
      )
    } else {
      ""
    },
    "<b>Lärmwert:</b> ",
    ifelse(is.na(df$laerm), "n. v.", df$laerm),
    "<br>",
    "<b>kNN-Basiswohnlage:</b> ",
    df$wohnlage_knn_basis,
    "<br>",
    "<b>Finale Wohnlage nach Lärm-Abwertung:</b> ",
    "<span style='color:red; font-weight:bold;'>",
    df$wohnlage_nach_laerm,
    "</span><br>",
    "<b>Abgewertet durch Lärm:</b> ",
    ifelse(df$abgewertet_durch_laerm, "Ja", "Nein"),
    "<br>",
    "<hr>",
    "<b>k:</b> ",
    df$knn_k,
    "<br>",
    "<b>Mehrheitsanteil kNN:</b> ",
    round(df$knn_vote_share * 100, 1),
    " %<br>",
    "<b>Distanz nächster Nachbar:</b> ",
    round(df$distanz_naechster_nachbar_m, 1),
    " m<br>",
    "<b>Mittlere Distanz der k Nachbarn:</b> ",
    round(df$distanz_mittlere_k_nachbarn_m, 1),
    " m<br>"
  )
}

data_munich_laerm_knn_wgs$popup_text <-
  popup_laermpunkte(data_munich_laerm_knn_wgs)


# ==============================================================================
# 21C. POPUPS FLÄCHEN
# ==============================================================================

wohnlagen_munich_analyse_wgs <- wohnlagen_munich_analyse_wgs %>%
  mutate(
    popup_flaeche = paste0(
      "<b>Wohnlagenfläche München</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ", flaechen_id, "<br>",
      "<b>Wohnlage:</b> ", Wohnlage_3cat, "<br>",
      "<hr>",
      "<b>Anzahl Wohnungen/Punkte:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Anzahl geändert:</b> ",
      n_geaendert,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<hr>",
      "<b>Mittlere realisierte Verbesserung:</b> ",
      round(mittlere_realisierte_verbesserung, 3),
      "<br>",
      "<b>Mittlere potenzielle Verbesserung:</b> ",
      round(mittlere_potenzielle_verbesserung, 3),
      "<br>",
      "<b>Mittlere max. Wahrscheinlichkeit:</b> ",
      round(mittlere_max_prob * 100, 1),
      " %<br>",
      "<hr>",
      "<b>Häufigste alte Lage:</b> ",
      alte_lage_haeufig,
      "<br>",
      "<b>Häufigste neue Lage:</b> ",
      neue_lage_haeufig,
      "<br>"
    )
  )

problemgebiete_munich_wgs <- problemgebiete_munich_wgs %>%
  mutate(
    popup_problemgebiet = paste0(
      "<b>Potenzielles Problemgebiet</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ",
      flaechen_id,
      "<br>",
      "<b>Wohnlage:</b> ",
      Wohnlage_3cat,
      "<br>",
      "<b>Anzahl Wohnungen/Punkte:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Anzahl geändert:</b> ",
      n_geaendert,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<b>Mittlere realisierte Verbesserung:</b> ",
      round(mittlere_realisierte_verbesserung, 3),
      "<br>"
    )
  )


# ==============================================================================
# 21D. POPUPS MIETSPIEGEL
# ==============================================================================

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    popup_mietspiegel = paste0(
      "<b>Mietspiegel-Punkt</b><br>",
      "<hr>",
      "<b>Adress-ID:</b> ",
      adressid,
      "<br>",
      ifelse(
        "adresse_1" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Adresse:</b> ", adresse_1, "<br>"),
        ""
      ),
      ifelse(
        "adresse_2" %in% names(mietspiegel_geo_wgs),
        paste0("<b>PLZ:</b> ", adresse_2, "<br>"),
        ""
      ),
      ifelse(
        "adresse_3" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Ort:</b> ", adresse_3, "<br>"),
        ""
      ),
      ifelse(
        "adresse_4" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Gebiet:</b> ", adresse_4, "<br>"),
        ""
      ),
      "<hr>",
      "<b>In Problemgebiet:</b> ",
      ifelse(in_problemgebiet, "Ja", "Nein"),
      "<br>",
      "<b>Problemgebiet-Flächen-ID:</b> ",
      ifelse(
        is.na(problemgebiet_flaechen_id),
        "-",
        problemgebiet_flaechen_id
      ),
      "<br>"
    )
  )

mietspiegel_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == TRUE)

mietspiegel_nicht_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == FALSE)


# ==============================================================================
# 21E. KARTENMITTELPUNKT
# ==============================================================================

bbox_munich <- st_bbox(wohnlagen_munich_analyse_wgs)

map_center_lng <- mean(c(bbox_munich["xmin"], bbox_munich["xmax"]))
map_center_lat <- mean(c(bbox_munich["ymin"], bbox_munich["ymax"]))


# ==============================================================================
# 21F. KARTE ERSTELLEN
# ==============================================================================

karte_munich_gesamt <- leaflet(
  options = leafletOptions(preferCanvas = TRUE)
) %>%
  
  addProviderTiles(
    "CartoDB.Positron",
    group = "Basiskarte"
  ) %>%
  
  setView(
    lng = map_center_lng,
    lat = map_center_lat,
    zoom = 11
  ) %>%
  
  addPolygons(
    data = wohnlagen_munich_analyse_wgs,
    fillColor = ~flaechenfarbe,
    fillOpacity = 0.60,
    color = "grey35",
    weight = 1,
    popup = ~popup_flaeche,
    label = ~paste0(
      "Fläche ",
      flaechen_id,
      " | ",
      Wohnlage_3cat,
      " | ",
      round(anteil_geaendert_prozent, 1),
      " % geändert"
    ),
    group = "Wohnlagenflächen"
  ) %>%
  
  addPolygons(
    data = problemgebiete_munich_wgs,
    fillColor = "transparent",
    fillOpacity = 0,
    color = "red",
    weight = 4,
    popup = ~popup_problemgebiet,
    label = ~paste0(
      "Problemgebiet Fläche ",
      flaechen_id,
      " | ",
      round(anteil_geaendert_prozent, 1),
      " % geändert"
    ),
    group = "Problemgebiete"
  ) %>%
  
  addCircleMarkers(
    data = modellpunkte_unchanged,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = ~punktfarbe,
    fillOpacity = 0.70,
    color = "black",
    stroke = TRUE,
    weight = 0.7,
    radius = 3,
    popup = ~popup_text,
    group = "Modellpunkte: unverändert"
  ) %>%
  
  addCircleMarkers(
    data = modellpunkte_changed,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = ~punktfarbe,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1.8,
    radius = 5,
    popup = ~popup_text,
    group = "Modellpunkte: umklassifiziert"
  ) %>%
  
  addCircleMarkers(
    data = data_munich_laerm_knn_wgs,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = ~punktfarbe_laerm,
    fillOpacity = 1,
    color = "#6a0000",
    stroke = TRUE,
    weight = 2.3,
    radius = 6,
    popup = ~popup_text,
    group = "Hochlärm-Punkte: kNN + Abwertung"
  ) %>%
  
  addCircleMarkers(
    data = mietspiegel_problem,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = "red",
    fillOpacity = 1,
    color = "darkred",
    stroke = TRUE,
    weight = 2.5,
    radius = 8,
    popup = ~popup_mietspiegel,
    group = "Mietspiegel: in Problemgebiet"
  ) %>%
  
  addCircleMarkers(
    data = mietspiegel_nicht_problem,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = "blue",
    fillOpacity = 0.85,
    color = "darkblue",
    stroke = TRUE,
    weight = 1.5,
    radius = 6,
    popup = ~popup_mietspiegel,
    group = "Mietspiegel: nicht in Problemgebiet"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben_3),
    labels = names(wohnlage_farben_3),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  
  addLayersControl(
    baseGroups = c("Basiskarte"),
    overlayGroups = c(
      "Wohnlagenflächen",
      "Problemgebiete",
      "Modellpunkte: unverändert",
      "Modellpunkte: umklassifiziert",
      "Hochlärm-Punkte: kNN + Abwertung",
      "Mietspiegel: in Problemgebiet",
      "Mietspiegel: nicht in Problemgebiet"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup("Modellpunkte: unverändert") %>%
  hideGroup("Mietspiegel: nicht in Problemgebiet")


# ==============================================================================
# 22. KARTE SPEICHERN
# ==============================================================================

saveWidget(
  karte_munich_gesamt,
  file = "interaktive_karten/karte_munich_gesamtanalyse_mit_mietspiegel_7_5.html",
  selfcontained = FALSE
)

cat("\n✓ Interaktive München-Karte gespeichert:\n")
cat("interaktive_karten/karte_munich_gesamtanalyse_mit_mietspiegel.html\n")


# ==============================================================================
# 23. ABSCHLUSS-ZUSAMMENFASSUNG
# ==============================================================================

cat("\n====================================\n")
cat("ZUSAMMENFASSUNG GESAMTWORKFLOW MÜNCHEN\n")
cat("====================================\n")

cat("Modellpunkte gesamt:",
    nrow(data_munich_joined_wgs), "\n")

cat("Davon umklassifiziert:",
    nrow(modellpunkte_changed), "\n")

cat("Änderungsrate Modellpunkte:",
    round(mean(data_munich_joined_wgs$changed) * 100, 2),
    "%\n")

cat("Wohnlagenflächen mit analysierten Punkten:",
    nrow(wohnlagen_munich_analyse_wgs), "\n")

cat("Problemgebiete:",
    nrow(problemgebiete_munich_wgs), "\n")

cat("Hochlärm-Punkte:",
    nrow(data_munich_laerm_knn_wgs), "\n")

cat("Mietspiegel-Punkte gesamt:",
    nrow(mietspiegel_geo_wgs), "\n")

cat("Mietspiegel-Punkte in Problemgebieten:",
    nrow(mietspiegel_problem), "\n")

cat("Mietspiegel-Punkte nicht in Problemgebieten:",
    nrow(mietspiegel_nicht_problem), "\n")

cat("====================================\n")









# ==============================================================================
# ELBOW PLOT: prior_scale vs. Anzahl Problemgebiete
# OHNE Modell oder Score-Matrix neu zu berechnen
# ==============================================================================

library(sf)
library(dplyr)
library(ggplot2)

# --------------------------------------------------------------------------
# 1. Relevante gespeicherte Daten laden
# --------------------------------------------------------------------------

SCORE_munich <- readRDS("results_lin_disc/SCORE_munich_mit_laerm.rds")

data_joined <- readRDS(
  "results_lin_disc/data_munich_joined_prior_scale_7_5.rds"
)

# Geometrie entfernen, weil wir für den Elbow Plot nur flaechen_id brauchen
data_eval <- data_joined %>%
  st_drop_geometry()

stopifnot(nrow(SCORE_munich) == nrow(data_eval))

# Klassenreihenfolge aus SCORE übernehmen, falls vorhanden
if (!is.null(colnames(SCORE_munich))) {
  levels_c <- colnames(SCORE_munich)
} else {
  levels_c <- levels(data_eval$c)
}

data_eval$c <- factor(as.character(data_eval$c), levels = levels_c)

# Nur Punkte verwenden, die einer Wohnlagenfläche zugeordnet wurden
idx_area <- !is.na(data_eval$flaechen_id)

SCORE_area <- SCORE_munich[idx_area, , drop = FALSE]
data_area  <- data_eval[idx_area, ]

cat("Punkte mit Flächenzuordnung:", nrow(data_area), "\n")
cat("Anzahl Flächen:", length(unique(data_area$flaechen_id)), "\n")


# --------------------------------------------------------------------------
# 2. Problemgebiet-Definition
# --------------------------------------------------------------------------

min_wohnungen_problemgebiet <- 20
min_anteil_geaendert_problemgebiet <- 0.20


# --------------------------------------------------------------------------
# 3. Funktion: Prior anwenden und Anzahl Problemgebiete zählen
# --------------------------------------------------------------------------

count_problemgebiete_for_prior <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  # Numerisch stabilisieren
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  # Prior-Matrix
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  # Prior-Wahrscheinlichkeiten
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  colnames(PROB_prior) <- levels_c
  
  # Neue Klasse
  pred <- levels_c[max.col(PROB_prior)]
  
  # Hat sich der Punkt geändert?
  changed <- pred != as.character(data$c)
  
  # Zusammenfassung je Fläche
  gebiet_summary <- data.frame(
    flaechen_id = data$flaechen_id,
    changed = changed
  ) %>%
    group_by(flaechen_id) %>%
    summarise(
      n_wohnungen = n(),
      n_geaendert = sum(changed, na.rm = TRUE),
      anteil_geaendert = n_geaendert / n_wohnungen,
      .groups = "drop"
    )
  
  # Problemgebiete nach deiner Regel
  problemgebiete <- gebiet_summary %>%
    filter(
      n_wohnungen >= min_wohnungen_problemgebiet,
      anteil_geaendert >= min_anteil_geaendert_problemgebiet
    )
  
  data.frame(
    prior_scale = prior_scale,
    delta_equiv = log1p(prior_scale),
    n_problemgebiete = nrow(problemgebiete),
    n_geaenderte_punkte = sum(changed, na.rm = TRUE),
    anteil_geaenderte_punkte = mean(changed, na.rm = TRUE)
  )
}


# --------------------------------------------------------------------------
# 4. Prior-Scale-Werte testen: feineres Grid
# --------------------------------------------------------------------------

prior_scales <- sort(unique(c(
  # sehr feiner Bereich nahe 0
  seq(0, 5, by = 0.25),
  
  # feiner Bereich, wo oft der Knick liegt
  seq(5, 30, by = 0.5),
  
  # mittlerer Bereich
  seq(30, 100, by = 2),
  
  # größerer Bereich
  seq(100, 300, by = 5),
  
  # sehr großer Bereich
  seq(300, 1000, by = 25)
)))

cat("Anzahl getesteter prior_scale-Werte:", length(prior_scales), "\n")

elbow_df <- bind_rows(
  lapply(
    prior_scales,
    function(p) {
      count_problemgebiete_for_prior(
        SCORE = SCORE_area,
        data = data_area,
        levels_c = levels_c,
        prior_scale = p
      )
    }
  )
)

print(elbow_df)


# --------------------------------------------------------------------------
# 5. Optional: einfachen Elbow-Punkt automatisch markieren
# --------------------------------------------------------------------------

find_elbow <- function(x, y) {
  
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  x1 <- x_norm[1]
  y1 <- y_norm[1]
  x2 <- x_norm[length(x_norm)]
  y2 <- y_norm[length(y_norm)]
  
  dist <- abs(
    (y2 - y1) * x_norm -
      (x2 - x1) * y_norm +
      x2 * y1 -
      y2 * x1
  ) / sqrt((y2 - y1)^2 + (x2 - x1)^2)
  
  which.max(dist)
}

elbow_idx <- find_elbow(
  elbow_df$delta_equiv,
  elbow_df$n_problemgebiete
)

elbow_point <- elbow_df[elbow_idx, ]

cat("\nAutomatisch geschätzter Elbow:\n")
print(elbow_point)


# ==============================================================================
# ELBOW PLOT: nur relevanter prior_scale-Bereich
# ==============================================================================

x_max_plot <- 50   # bei Bedarf z.B. auf 150 oder 200 setzen

p_elbow <- ggplot(
  elbow_df,
  aes(x = prior_scale, y = n_problemgebiete)
) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  
  geom_vline(
    xintercept = elbow_point$prior_scale,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  geom_point(
    data = elbow_point,
    aes(x = prior_scale, y = n_problemgebiete),
    size = 4,
    color = "black"
  ) +
  annotate(
    "label",
    x = elbow_point$prior_scale + 8,
    y = elbow_point$n_problemgebiete + 15,
    label = paste0(
      "Elbow\n",
      "prior_scale = ", round(elbow_point$prior_scale, 2), "\n",
      "Problemgebiete = ", elbow_point$n_problemgebiete
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.7,
    label.size = 0.25,
    fill = "white"
  ) +
  
  coord_cartesian(
    xlim = c(0, x_max_plot)
  ) +
  
  scale_x_continuous(
    breaks = seq(0, x_max_plot, by = 10),
    minor_breaks = seq(0, x_max_plot, by = 2.5)
  ) +
  
  labs(
    subtitle = paste0(
      "Problemgebiet: mindestens ",
      min_wohnungen_problemgebiet,
      " Punkte und mindestens ",
      min_anteil_geaendert_problemgebiet * 100,
      "% geänderte Punkte je Fläche"
    ),
    x = "prior_scale",
    y = "Anzahl Problemgebiete"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor.y = element_blank()
  )

print(p_elbow)


# --------------------------------------------------------------------------
# 7. Ergebnisse speichern
# --------------------------------------------------------------------------

if (!dir.exists("results_lin_disc")) {
  dir.create("results_lin_disc")
}

write.csv(
  elbow_df,
  "results_lin_disc/elbow_prior_scale_problemgebiete.csv",
  row.names = FALSE
)

ggsave(
  filename = "results_lin_disc/elbow_prior_scale_problemgebiete.png",
  plot = p_elbow,
  width = 9,
  height = 5,
  dpi = 300
)

cat("\nGespeichert:\n")
cat("results_lin_disc/elbow_prior_scale_problemgebiete.csv\n")
cat("results_lin_disc/elbow_prior_scale_problemgebiete.png\n")
