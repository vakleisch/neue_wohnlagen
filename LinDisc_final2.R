# ==============================================================================
# GESAMTWORKFLOW GANZ MÜNCHEN
#
# Lärm-Ansatz:
# - Normale Modellpunkte laufen durch GAM + Prior-Transformation
# - Hochlärm-Punkte laufen NICHT durch kNN
# - Hochlärm-Punkte werden direkt um eine Wohnlagenkategorie abgewertet
# - Hochlärm-Punkte zählen anschließend in die Problemgebietsauswertung mit rein
#
# Flächenzuordnung:
# - Punkte werden geometrisch einer Wohnlagenfläche zugeordnet
# - Wenn ein Punkt mehrere Flächen trifft, wird NICHT mehr x[1] genommen
# - Stattdessen geht der Punkt immer in die kleinste getroffene Fläche
#
# Zusatzanalyse:
# - Punkte ohne Fläche werden separat ausgewertet
# - Sensitivitätsanalyse: Punkte ohne Fläche werden der nächstgelegenen Fläche
#   zugeschlagen, sofern sie höchstens 25 m entfernt sind
# ==============================================================================


# ==============================================================================
# 0. PAKETE
# ==============================================================================

library(sf)
library(dplyr)
library(mgcv)
library(leaflet)
library(htmlwidgets)
library(ggplot2)

source("daten_verarbeitung/daten_bearbeitung.R")


# ==============================================================================
# 1. ZENTRALE EINSTELLUNGEN
# ==============================================================================

prior_scale <- 7.5
suffix <- "prior_scale_7_5"

# Problemgebiet-Regel
min_wohnungen_problemgebiet <- 20
min_anteil_geaendert_problemgebiet <- 0.20

# Sensitivität für Punkte ohne Fläche
max_dist_ohne_flaeche_m <- 25

cat("====================================\n")
cat("EINSTELLUNGEN\n")
cat("====================================\n")
cat("Prior Scale:", round(prior_scale, 4), "\n")
cat("Hochlärm-Behandlung: direkte Abwertung um eine Wohnlagenkategorie\n")
cat("Flächenzuordnung bei Überschneidung: kleinste getroffene Fläche\n")
cat("Problemgebiet: mindestens",
    min_wohnungen_problemgebiet,
    "Wohnungen und mindestens",
    min_anteil_geaendert_problemgebiet * 100,
    "% geändert\n")
cat("Sensitivität Punkte ohne Fläche: nächstgelegene Fläche bis",
    max_dist_ohne_flaeche_m,
    "m\n")
cat("====================================\n")


# ==============================================================================
# 2. ORDNER ANLEGEN
# ==============================================================================

if (!dir.exists("results_lin_disc")) {
  dir.create("results_lin_disc", recursive = TRUE)
}

if (!dir.exists("interaktive_karten")) {
  dir.create("interaktive_karten", recursive = TRUE)
}


# ==============================================================================
# 3. DATEN LADEN
# ==============================================================================

data_munich <- readRDS("daten/model_munich_data2.rds")
data_munich_laerm <- readRDS("daten/model_data_hoherlärm.rds")

cat("Hauptdatensatz München geladen:", nrow(data_munich), "Zeilen\n")
cat("Hochlärm-Datensatz München geladen:", nrow(data_munich_laerm), "Zeilen\n")


# ==============================================================================
# 4. WAHRE 3-KATEGORIEN-WOHNLAGE AUS wohnlage_ebene ABLEITEN
# ==============================================================================

derive_wohnlage_3cat <- function(x) {
  
  vals <- sort(unique(na.omit(x)))
  
  if (all(vals %in% 0:5)) {
    out <- case_when(
      x %in% c(0, 3) ~ "durchschnittliche Lage",
      x %in% c(1, 4) ~ "gute Lage",
      x %in% c(2, 5) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  } else if (all(vals %in% 1:6)) {
    out <- case_when(
      x %in% c(1, 4) ~ "durchschnittliche Lage",
      x %in% c(2, 5) ~ "gute Lage",
      x %in% c(3, 6) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  } else {
    stop("Unbekannte Kodierung von wohnlage_ebene.")
  }
  
  return(out)
}

wohnlage_order <- c(
  "durchschnittliche Lage",
  "gute Lage",
  "beste Lage"
)

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

cat("Wohnlagenklassen im Modell:", paste(levels_c, collapse = ", "), "\n")

if ("wohnlage_ebene" %in% names(data_munich_laerm)) {
  
  data_munich_laerm$wohnlage_alt_3cat <-
    derive_wohnlage_3cat(data_munich_laerm$wohnlage_ebene)
  
  cat("\nVerteilung der alten 3-Kategorien-Wohnlage im Hochlärm-Datensatz:\n")
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
# 10. PRIOR-TRANSFORMATION MIT PRIOR_SCALE
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
  
  if (length(prior_res_munich$pred) != nrow(data_munich)) {
    stop("Die geladene Prior-Datei passt nicht zur aktuellen data_munich-Zeilenzahl. Bitte prior_result_file löschen und neu berechnen.")
  }
  
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
    
    prob_max = apply(prior_res_munich$PROB, 1, max),
    
    punkt_typ = "modellpunkt"
  )

cat("\n====================================\n")
cat("PRIOR-ERGEBNIS GANZ MÜNCHEN\n")
cat("====================================\n")
cat("Änderungsrate normale Modellpunkte:",
    round(mean(data_munich$changed) * 100, 2),
    "%\n")
cat("Realisierte mittlere Verbesserung:",
    round(mean(data_munich$realized_improvement), 4),
    "\n")
cat("====================================\n")

saveRDS(
  data_munich,
  paste0("results_lin_disc/data_munich_", suffix, ".rds")
)

saveRDS(
  prior_res_munich$PROB,
  paste0("results_lin_disc/PROB_munich_", suffix, ".rds")
)


# ==============================================================================
# 12. WOHNLAGENFLÄCHEN LADEN UND FLÄCHEN-ID VERGEBEN
# ==============================================================================

wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(
    flaechen_id = row_number()
  )

cat("\nAnzahl Wohnlagenflächen insgesamt:",
    nrow(wohnlagen_muc_wgs), "\n")


# ==============================================================================
# 13. HILFSFUNKTION:
#     Punkte eindeutig einer Wohnlagenfläche zuordnen
#
# Regel:
# - keine getroffene Fläche       -> NA
# - eine getroffene Fläche        -> diese Fläche
# - mehrere getroffene Flächen    -> kleinste getroffene Fläche
# ==============================================================================

assign_points_to_smallest_flaeche <- function(points_sf, flaechen_sf, label = "") {
  
  if (!identical(st_crs(points_sf), st_crs(flaechen_sf))) {
    points_sf <- st_transform(
      points_sf,
      st_crs(flaechen_sf)
    )
  }
  
  flaechen_area_qm <- as.numeric(
    st_area(
      st_transform(flaechen_sf, 25832)
    )
  )
  
  idx_list <- st_intersects(
    points_sf,
    flaechen_sf
  )
  
  n_matches <- lengths(idx_list)
  
  polygon_idx <- vapply(
    idx_list,
    function(x) {
      
      if (length(x) == 0) {
        return(NA_integer_)
      }
      
      if (length(x) == 1) {
        return(x[1])
      }
      
      x[which.min(flaechen_area_qm[x])]
    },
    integer(1)
  )
  
  cat("\n====================================\n")
  cat("FLÄCHENZUORDNUNG:", label, "\n")
  cat("====================================\n")
  cat("Punkte insgesamt:", nrow(points_sf), "\n")
  cat("Punkte ohne Fläche:", sum(n_matches == 0), "\n")
  cat("Punkte mit genau einer Fläche:", sum(n_matches == 1), "\n")
  cat("Punkte mit mehreren Flächen:", sum(n_matches > 1), "\n")
  cat("====================================\n")
  
  points_sf %>%
    mutate(
      flaechen_id = flaechen_sf$flaechen_id[polygon_idx],
      Wohnlage_flaeche = flaechen_sf$Wohnlage[polygon_idx],
      n_getroffene_flaechen = n_matches,
      mehrfachtreffer_kleinste_flaeche = n_matches > 1
    )
}


# ==============================================================================
# 14. NORMALE MODELLPUNKTE EINER FLÄCHE ZUORDNEN
#     Bei Überschneidung: kleinste getroffene Fläche
# ==============================================================================

data_munich_sf <- st_as_sf(
  data_munich,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

data_munich_joined <- assign_points_to_smallest_flaeche(
  points_sf = data_munich_sf,
  flaechen_sf = wohnlagen_muc_wgs,
  label = "normale Modellpunkte"
) %>%
  mutate(
    punkt_typ = "modellpunkt"
  )

saveRDS(
  data_munich_joined,
  paste0("results_lin_disc/data_munich_joined_", suffix, ".rds")
)


# ==============================================================================
# 15. DIREKTE ABWERTUNG DER HOCHLÄRM-PUNKTE
# ==============================================================================

required_laerm_vars <- c(
  "s.long",
  "s.lat",
  "wohnlage_ebene"
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

if (!"wohnlage_alt_3cat" %in% names(data_munich_laerm)) {
  
  data_munich_laerm$wohnlage_alt_3cat <-
    derive_wohnlage_3cat(data_munich_laerm$wohnlage_ebene)
}

downgrade_one_level <- function(x) {
  
  case_when(
    x == "beste Lage" ~ "gute Lage",
    x == "gute Lage" ~ "durchschnittliche Lage",
    x == "durchschnittliche Lage" ~ "durchschnittliche Lage",
    TRUE ~ NA_character_
  )
}

data_munich_laerm_direkt <- data_munich_laerm %>%
  mutate(
    wohnlage_alt_3cat = factor(
      wohnlage_alt_3cat,
      levels = wohnlage_order
    ),
    
    wohnlage_nach_laerm = downgrade_one_level(
      as.character(wohnlage_alt_3cat)
    ),
    
    wohnlage_nach_laerm = factor(
      wohnlage_nach_laerm,
      levels = wohnlage_order
    ),
    
    wohnlage_alt = as.character(wohnlage_alt_3cat),
    wohnlage_neu = as.character(wohnlage_nach_laerm),
    changed = wohnlage_alt != wohnlage_neu,
    
    abgewertet_durch_laerm =
      as.character(wohnlage_alt_3cat) != as.character(wohnlage_nach_laerm),
    
    methode_hochlaerm = "direkte Abwertung ohne kNN",
    punkt_typ = "hochlaerm_direkt_abgewertet",
    
    score_alt = NA_real_,
    score_neu = NA_real_,
    score_best = NA_real_,
    realized_improvement = NA_real_,
    potential_improvement = NA_real_,
    prob_max = NA_real_
  )

if (!"laerm" %in% names(data_munich_laerm_direkt)) {
  data_munich_laerm_direkt$laerm <- NA_real_
}

cat("\n====================================\n")
cat("HOCHLÄRM-AUSWERTUNG: DIREKTE ABWERTUNG\n")
cat("====================================\n")
cat("Anzahl Hochlärm-Punkte:",
    nrow(data_munich_laerm_direkt), "\n")

cat("\nAlte Wohnlage der Hochlärm-Punkte:\n")
print(table(data_munich_laerm_direkt$wohnlage_alt_3cat, useNA = "ifany"))

cat("\nWohnlage nach direkter Lärm-Abwertung:\n")
print(table(data_munich_laerm_direkt$wohnlage_nach_laerm, useNA = "ifany"))

cat("\nAnteil abgewertet:",
    round(mean(data_munich_laerm_direkt$abgewertet_durch_laerm, na.rm = TRUE) * 100, 2),
    "%\n")
cat("====================================\n")

saveRDS(
  data_munich_laerm_direkt,
  "results_lin_disc/data_munich_hochlaerm_direkt_abgewertet.rds"
)


# ==============================================================================
# 16. HOCHLÄRM-PUNKTE EINER FLÄCHE ZUORDNEN
#     Bei Überschneidung: kleinste getroffene Fläche
# ==============================================================================

data_munich_laerm_direkt_sf <- st_as_sf(
  data_munich_laerm_direkt,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

data_munich_laerm_joined <- assign_points_to_smallest_flaeche(
  points_sf = data_munich_laerm_direkt_sf,
  flaechen_sf = wohnlagen_muc_wgs,
  label = "Hochlärm-Punkte"
) %>%
  mutate(
    punkt_typ = "hochlaerm_direkt_abgewertet"
  )

saveRDS(
  data_munich_laerm_joined,
  paste0("results_lin_disc/data_munich_hochlaerm_direkt_joined_", suffix, ".rds")
)


# ==============================================================================
# 17. WOHNLAGE-MATCH CHECK:
#     Passt Punkt-Wohnlage zur zugeordneten Flächen-Wohnlage?
# ==============================================================================

clean_wohnlage_3cat <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("zentrale", "", x, ignore.case = TRUE)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  
  case_when(
    grepl("durchschnittliche Lage", x, ignore.case = TRUE) ~
      "durchschnittliche Lage",
    grepl("gute Lage", x, ignore.case = TRUE) ~
      "gute Lage",
    grepl("beste Lage", x, ignore.case = TRUE) ~
      "beste Lage",
    TRUE ~ x
  )
}

check_wohnlage_match <- function(x, datensatz_name) {
  
  punkt_lage_col <- case_when(
    "wohnlage_alt_3cat" %in% names(x) ~ "wohnlage_alt_3cat",
    "wohnlage_alt" %in% names(x) ~ "wohnlage_alt",
    TRUE ~ NA_character_
  )
  
  if (is.na(punkt_lage_col)) {
    stop("Keine passende Wohnlage-Spalte gefunden.")
  }
  
  x %>%
    st_drop_geometry() %>%
    mutate(
      datensatz = datensatz_name,
      wohnlage_punkt_3cat = clean_wohnlage_3cat(.data[[punkt_lage_col]]),
      wohnlage_flaeche_3cat = clean_wohnlage_3cat(Wohnlage_flaeche),
      wohnlage_passt_zur_flaeche =
        wohnlage_punkt_3cat == wohnlage_flaeche_3cat
    )
}

check_modell <- check_wohnlage_match(
  data_munich_joined,
  "Modellpunkte"
)

check_hochlaerm <- check_wohnlage_match(
  data_munich_laerm_joined,
  "Hochlärm-Punkte"
)

wohnlage_match_check <- bind_rows(
  check_modell,
  check_hochlaerm
)

cat("\n====================================\n")
cat("CHECK: PUNKT-WOHNLAGE VS. FLÄCHEN-WOHNLAGE\n")
cat("====================================\n")
print(table(
  wohnlage_match_check$datensatz,
  wohnlage_match_check$wohnlage_passt_zur_flaeche,
  useNA = "ifany"
))
cat("====================================\n")

wohnlage_mismatches <- wohnlage_match_check %>%
  filter(
    is.na(wohnlage_passt_zur_flaeche) |
      wohnlage_passt_zur_flaeche == FALSE
  )

write.csv(
  wohnlage_mismatches,
  paste0("results_lin_disc/check_wohnlage_mismatch_", suffix, ".csv"),
  row.names = FALSE
)


# ==============================================================================
# 18. NORMALE MODELLPUNKTE + HOCHLÄRM-PUNKTE ZUSAMMENFÜHREN
# ==============================================================================

data_munich_in_area <- data_munich_joined %>%
  filter(!is.na(flaechen_id))

data_munich_laerm_in_area <- data_munich_laerm_joined %>%
  filter(!is.na(flaechen_id))

common_cols <- intersect(
  names(data_munich_in_area),
  names(data_munich_laerm_in_area)
)

data_munich_all_in_area <- bind_rows(
  data_munich_in_area[, common_cols],
  data_munich_laerm_in_area[, common_cols]
)

cat("\nPunkte insgesamt mit Fläche:\n")
cat("Normale Modellpunkte:",
    nrow(data_munich_in_area), "\n")
cat("Hochlärm-Punkte:",
    nrow(data_munich_laerm_in_area), "\n")
cat("Alle Punkte zusammen:",
    nrow(data_munich_all_in_area), "\n")

cat("\nÄnderungsrate inklusive Hochlärm-Punkte:",
    round(mean(data_munich_all_in_area$changed, na.rm = TRUE) * 100, 2),
    "%\n")

saveRDS(
  data_munich_in_area,
  paste0("results_lin_disc/data_munich_in_area_", suffix, ".rds")
)

saveRDS(
  data_munich_all_in_area,
  paste0("results_lin_disc/data_munich_all_in_area_", suffix, ".rds")
)


# ==============================================================================
# 19. PUNKTE OHNE FLÄCHE:
#     separate Ausschlussdiagnose + Sensitivitätsvorbereitung
# ==============================================================================

punkte_ohne_flaeche_modell <- data_munich_joined %>%
  filter(is.na(flaechen_id)) %>%
  mutate(
    wohnlage_alt = as.character(wohnlage_alt),
    wohnlage_neu = as.character(wohnlage_neu),
    changed = wohnlage_alt != wohnlage_neu
  )

cat("\n====================================\n")
cat("PUNKTE OHNE FLÄCHE: NORMALE MODELLPUNKTE\n")
cat("====================================\n")
cat("Punkte ohne Fläche:", nrow(punkte_ohne_flaeche_modell), "\n")
cat("Davon vorhergesagte Wohnlage abweichend von wahrer Wohnlage:",
    sum(punkte_ohne_flaeche_modell$changed, na.rm = TRUE), "\n")
cat("Abweichungsrate:",
    round(mean(punkte_ohne_flaeche_modell$changed, na.rm = TRUE) * 100, 2),
    "%\n")
cat("====================================\n")

umklassifizierung_ohne_flaeche_modell <- punkte_ohne_flaeche_modell %>%
  st_drop_geometry() %>%
  count(
    wohnlage_alt,
    wohnlage_neu,
    changed,
    name = "n_punkte"
  ) %>%
  arrange(desc(changed), desc(n_punkte)) %>%
  mutate(
    anteil_prozent = round(n_punkte / sum(n_punkte) * 100, 2)
  )

write.csv(
  umklassifizierung_ohne_flaeche_modell,
  paste0("results_lin_disc/umklassifizierung_modellpunkte_ohne_flaeche_", suffix, ".csv"),
  row.names = FALSE
)

common_cols_all <- intersect(
  names(data_munich_joined),
  names(data_munich_laerm_joined)
)

punkte_ohne_flaeche_all <- bind_rows(
  data_munich_joined[, common_cols_all],
  data_munich_laerm_joined[, common_cols_all]
) %>%
  filter(is.na(flaechen_id))

cat("\nPunkte ohne Fläche insgesamt inklusive Hochlärm:",
    nrow(punkte_ohne_flaeche_all), "\n")

if (nrow(punkte_ohne_flaeche_all) > 0) {
  
  wohnlagen_muc_metric <- st_transform(
    wohnlagen_muc_wgs,
    25832
  )
  
  punkte_ohne_flaeche_metric <- st_transform(
    punkte_ohne_flaeche_all,
    25832
  )
  
  nearest_idx <- st_nearest_feature(
    punkte_ohne_flaeche_metric,
    wohnlagen_muc_metric
  )
  
  nearest_dist_m <- as.numeric(
    st_distance(
      st_geometry(punkte_ohne_flaeche_metric),
      st_geometry(wohnlagen_muc_metric[nearest_idx, ]),
      by_element = TRUE
    )
  )
  
  punkte_ohne_flaeche_nahe <- punkte_ohne_flaeche_metric %>%
    mutate(
      nearest_flaechen_id =
        wohnlagen_muc_metric$flaechen_id[nearest_idx],
      nearest_Wohnlage_flaeche =
        wohnlagen_muc_metric$Wohnlage[nearest_idx],
      dist_zur_naechsten_flaeche_m =
        nearest_dist_m,
      in_sensitivitaet =
        dist_zur_naechsten_flaeche_m <= max_dist_ohne_flaeche_m
    )
  
  cat("\n====================================\n")
  cat("SENSITIVITÄT: PUNKTE OHNE FLÄCHE\n")
  cat("====================================\n")
  cat("Maximale Distanz:", max_dist_ohne_flaeche_m, "m\n")
  cat("Punkte ohne Fläche insgesamt:", nrow(punkte_ohne_flaeche_nahe), "\n")
  cat("Davon nahe genug für Sensitivität:",
      sum(punkte_ohne_flaeche_nahe$in_sensitivitaet, na.rm = TRUE), "\n")
  cat("Davon geändert:",
      sum(
        punkte_ohne_flaeche_nahe$changed &
          punkte_ohne_flaeche_nahe$in_sensitivitaet,
        na.rm = TRUE
      ), "\n")
  cat("====================================\n")
  
  ohne_flaeche_nahe_summary <- punkte_ohne_flaeche_nahe %>%
    filter(in_sensitivitaet) %>%
    st_drop_geometry() %>%
    group_by(flaechen_id = nearest_flaechen_id) %>%
    summarise(
      n_ohne_flaeche_nahe = n(),
      n_geaendert_ohne_flaeche_nahe = sum(changed, na.rm = TRUE),
      anteil_geaendert_ohne_flaeche_nahe =
        n_geaendert_ohne_flaeche_nahe / n_ohne_flaeche_nahe,
      mittlere_distanz_ohne_flaeche_m =
        mean(dist_zur_naechsten_flaeche_m, na.rm = TRUE),
      max_distanz_ohne_flaeche_m =
        max(dist_zur_naechsten_flaeche_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(
    punkte_ohne_flaeche_nahe %>%
      st_drop_geometry(),
    paste0("results_lin_disc/punkte_ohne_flaeche_naechste_flaeche_", suffix, ".csv"),
    row.names = FALSE
  )
  
} else {
  
  punkte_ohne_flaeche_nahe <- data.frame()
  
  ohne_flaeche_nahe_summary <- data.frame(
    flaechen_id = integer(),
    n_ohne_flaeche_nahe = integer(),
    n_geaendert_ohne_flaeche_nahe = integer(),
    anteil_geaendert_ohne_flaeche_nahe = numeric(),
    mittlere_distanz_ohne_flaeche_m = numeric(),
    max_distanz_ohne_flaeche_m = numeric()
  )
}

write.csv(
  ohne_flaeche_nahe_summary,
  paste0("results_lin_disc/punkte_ohne_flaeche_nahe_summary_", suffix, ".csv"),
  row.names = FALSE
)


# ==============================================================================
# 20. HILFSFUNKTIONEN FÜR SUMMARY
# ==============================================================================

safe_mean <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}

mode_safe <- function(x) {
  x <- na.omit(as.character(x))
  if (length(x) == 0) {
    NA_character_
  } else {
    names(sort(table(x), decreasing = TRUE))[1]
  }
}


# ==============================================================================
# 21. ÄNDERUNGSANTEIL JE WOHNLAGENFLÄCHE IN GANZ MÜNCHEN
#     WICHTIG: inklusive Hochlärm-Punkte
# ==============================================================================

gebiet_summary_munich <- data_munich_all_in_area %>%
  st_drop_geometry() %>%
  group_by(flaechen_id) %>%
  summarise(
    n_wohnungen = n(),
    
    n_modellpunkte =
      sum(punkt_typ == "modellpunkt", na.rm = TRUE),
    
    n_hochlaerm_punkte =
      sum(punkt_typ == "hochlaerm_direkt_abgewertet", na.rm = TRUE),
    
    n_geaendert = sum(changed, na.rm = TRUE),
    
    n_geaendert_modellpunkte =
      sum(changed & punkt_typ == "modellpunkt", na.rm = TRUE),
    
    n_geaendert_hochlaerm =
      sum(changed & punkt_typ == "hochlaerm_direkt_abgewertet", na.rm = TRUE),
    
    anteil_geaendert =
      n_geaendert / n_wohnungen,
    
    anteil_geaendert_prozent =
      round(anteil_geaendert * 100, 2),
    
    mittlere_realisierte_verbesserung =
      safe_mean(realized_improvement),
    
    mittlere_potenzielle_verbesserung =
      safe_mean(potential_improvement),
    
    mittlere_max_prob =
      safe_mean(prob_max),
    
    alte_lage_haeufig =
      mode_safe(wohnlage_alt),
    
    neue_lage_haeufig =
      mode_safe(wohnlage_neu),
    
    .groups = "drop"
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("\nTop 50 Gebiete nach Änderungsanteil inklusive Hochlärm:\n")
print(head(gebiet_summary_munich, 50))

saveRDS(
  gebiet_summary_munich,
  paste0("results_lin_disc/gebiet_summary_munich_", suffix, ".rds")
)


# ==============================================================================
# 22. SUMMARY AN FLÄCHENOBJEKTE JOINEN
#     Zusätzlich: Sensitivitätspunkte ohne Fläche als Zusatzspalten
# ==============================================================================

wohnlagen_munich_analyse <- wohnlagen_muc_wgs %>%
  left_join(
    gebiet_summary_munich,
    by = "flaechen_id"
  ) %>%
  filter(!is.na(n_wohnungen)) %>%
  left_join(
    ohne_flaeche_nahe_summary,
    by = "flaechen_id"
  ) %>%
  mutate(
    n_ohne_flaeche_nahe =
      ifelse(is.na(n_ohne_flaeche_nahe), 0L, n_ohne_flaeche_nahe),
    
    n_geaendert_ohne_flaeche_nahe =
      ifelse(
        is.na(n_geaendert_ohne_flaeche_nahe),
        0L,
        n_geaendert_ohne_flaeche_nahe
      ),
    
    anteil_geaendert_ohne_flaeche_nahe =
      ifelse(
        is.na(anteil_geaendert_ohne_flaeche_nahe),
        0,
        anteil_geaendert_ohne_flaeche_nahe
      ),
    
    mittlere_distanz_ohne_flaeche_m =
      ifelse(
        is.na(mittlere_distanz_ohne_flaeche_m),
        NA_real_,
        mittlere_distanz_ohne_flaeche_m
      ),
    
    max_distanz_ohne_flaeche_m =
      ifelse(
        is.na(max_distanz_ohne_flaeche_m),
        NA_real_,
        max_distanz_ohne_flaeche_m
      )
  )

cat("\nWohnlagenflächen mit mindestens einem analysierten Punkt:",
    nrow(wohnlagen_munich_analyse), "\n")

saveRDS(
  wohnlagen_munich_analyse,
  paste0("results_lin_disc/wohnlagen_munich_analyse_", suffix, ".rds")
)


# ==============================================================================
# 23. POTENZIELLE PROBLEMGEBIETE GANZ MÜNCHEN
#     Hauptdefinition: nur Punkte mit echter Flächenzuordnung
# ==============================================================================

problemgebiete_munich <- wohnlagen_munich_analyse %>%
  filter(
    n_wohnungen >= min_wohnungen_problemgebiet,
    anteil_geaendert >= min_anteil_geaendert_problemgebiet
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("\n====================================\n")
cat("PROBLEMGEBIETE GANZ MÜNCHEN, INKL. HOCHLÄRM\n")
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
    n_modellpunkte,
    n_hochlaerm_punkte,
    n_geaendert,
    n_geaendert_modellpunkte,
    n_geaendert_hochlaerm,
    anteil_geaendert_prozent,
    n_ohne_flaeche_nahe,
    n_geaendert_ohne_flaeche_nahe,
    alte_lage_haeufig,
    neue_lage_haeufig
  ) %>%
  head(50) %>%
  print()

saveRDS(
  problemgebiete_munich,
  paste0("results_lin_disc/problemgebiete_munich_", suffix, ".rds")
)


# ==============================================================================
# 23B. AUFFÄLLIGE KLEINGEBIETE
#      Separate Analyse für Flächen mit 1 bis 19 Punkten
#
# Definition:
# - mindestens 1 Punkt
# - höchstens 19 Punkte
# - mindestens 20% geänderte Punkte
#
# Diese Gebiete werden NICHT als Haupt-Problemgebiete gezählt,
# sondern separat berichtet.
# ==============================================================================

min_punkte_kleingebiet <- 1
max_punkte_kleingebiet <- 19
min_anteil_geaendert_kleingebiet <- 0.20

auffaellige_kleingebiete_munich <- wohnlagen_munich_analyse %>%
  filter(
    n_wohnungen >= min_punkte_kleingebiet,
    n_wohnungen <= max_punkte_kleingebiet,
    anteil_geaendert >= min_anteil_geaendert_kleingebiet
  ) %>%
  arrange(
    desc(anteil_geaendert),
    desc(n_geaendert),
    desc(n_wohnungen)
  )

cat("\n====================================\n")
cat("AUFFÄLLIGE KLEINGEBIETE GANZ MÜNCHEN\n")
cat("====================================\n")
cat("Definition: 1 bis 19 Punkte und mindestens",
    min_anteil_geaendert_kleingebiet * 100,
    "% geändert\n")
cat("Anzahl auffällige Kleingebiete:",
    nrow(auffaellige_kleingebiete_munich), "\n")
cat("====================================\n")

auffaellige_kleingebiete_munich %>%
  st_drop_geometry() %>%
  select(
    flaechen_id,
    Wohnlage,
    n_wohnungen,
    n_modellpunkte,
    n_hochlaerm_punkte,
    n_geaendert,
    n_geaendert_modellpunkte,
    n_geaendert_hochlaerm,
    anteil_geaendert_prozent,
    n_ohne_flaeche_nahe,
    n_geaendert_ohne_flaeche_nahe,
    alte_lage_haeufig,
    neue_lage_haeufig
  )

saveRDS(
  auffaellige_kleingebiete_munich,
  paste0("results_lin_disc/auffaellige_kleingebiete_munich_", suffix, ".rds")
)

write.csv(
  auffaellige_kleingebiete_munich %>%
    st_drop_geometry(),
  paste0("results_lin_disc/auffaellige_kleingebiete_munich_", suffix, ".csv"),
  row.names = FALSE
)

# ==============================================================================
# 24. SENSITIVITÄTS-PROBLEMGEBIETE
#     Was wäre, wenn nahe Punkte ohne Fläche der nächsten Fläche zugeschlagen würden?
# ==============================================================================

wohnlagen_munich_analyse_sensitiv <- wohnlagen_munich_analyse %>%
  mutate(
    n_wohnungen_sensitiv =
      n_wohnungen + n_ohne_flaeche_nahe,
    
    n_geaendert_sensitiv =
      n_geaendert + n_geaendert_ohne_flaeche_nahe,
    
    anteil_geaendert_sensitiv =
      n_geaendert_sensitiv / n_wohnungen_sensitiv,
    
    anteil_geaendert_sensitiv_prozent =
      round(anteil_geaendert_sensitiv * 100, 2)
  )

problemgebiete_munich_sensitiv <- wohnlagen_munich_analyse_sensitiv %>%
  filter(
    n_wohnungen_sensitiv >= min_wohnungen_problemgebiet,
    anteil_geaendert_sensitiv >= min_anteil_geaendert_problemgebiet
  ) %>%
  arrange(
    desc(anteil_geaendert_sensitiv),
    desc(n_geaendert_sensitiv)
  )

cat("\n====================================\n")
cat("PROBLEMGEBIETE: ORIGINAL VS. SENSITIVITÄT\n")
cat("====================================\n")
cat("Originale Problemgebiete:", nrow(problemgebiete_munich), "\n")
cat("Problemgebiete Sensitivität:", nrow(problemgebiete_munich_sensitiv), "\n")
cat("====================================\n")

vergleich_problemgebiete <- full_join(
  problemgebiete_munich %>%
    st_drop_geometry() %>%
    transmute(
      flaechen_id,
      problemgebiet_original = TRUE
    ),
  
  problemgebiete_munich_sensitiv %>%
    st_drop_geometry() %>%
    transmute(
      flaechen_id,
      problemgebiet_sensitiv = TRUE
    ),
  
  by = "flaechen_id"
) %>%
  mutate(
    problemgebiet_original =
      ifelse(is.na(problemgebiet_original), FALSE, problemgebiet_original),
    
    problemgebiet_sensitiv =
      ifelse(is.na(problemgebiet_sensitiv), FALSE, problemgebiet_sensitiv),
    
    status_vergleich = case_when(
      problemgebiet_original & problemgebiet_sensitiv ~
        "in beiden Varianten Problemgebiet",
      problemgebiet_original & !problemgebiet_sensitiv ~
        "nur original Problemgebiet",
      !problemgebiet_original & problemgebiet_sensitiv ~
        "nur sensitiv Problemgebiet",
      TRUE ~
        "kein Problemgebiet"
    )
  )

print(table(vergleich_problemgebiete$status_vergleich))

write.csv(
  vergleich_problemgebiete,
  paste0("results_lin_disc/vergleich_problemgebiete_original_vs_sensitiv_", suffix, ".csv"),
  row.names = FALSE
)

saveRDS(
  wohnlagen_munich_analyse_sensitiv,
  paste0("results_lin_disc/wohnlagen_munich_analyse_sensitiv_", suffix, ".rds")
)

saveRDS(
  problemgebiete_munich_sensitiv,
  paste0("results_lin_disc/problemgebiete_munich_sensitiv_ohne_flaeche_", suffix, ".rds")
)

# ==============================================================================
# 24B. AUFFÄLLIGE KLEINGEBIETE IN DER SENSITIVITÄTSANALYSE
#      inklusive nahe Punkte ohne Fläche
# ==============================================================================

auffaellige_kleingebiete_munich_sensitiv <- wohnlagen_munich_analyse_sensitiv %>%
  filter(
    n_wohnungen_sensitiv >= min_punkte_kleingebiet,
    n_wohnungen_sensitiv <= max_punkte_kleingebiet,
    anteil_geaendert_sensitiv >= min_anteil_geaendert_kleingebiet
  ) %>%
  arrange(
    desc(anteil_geaendert_sensitiv),
    desc(n_geaendert_sensitiv),
    desc(n_wohnungen_sensitiv)
  )

cat("\n====================================\n")
cat("AUFFÄLLIGE KLEINGEBIETE SENSITIVITÄT\n")
cat("====================================\n")
cat("Definition: 1 bis 19 Punkte inkl. nahe Punkte ohne Fläche und mindestens",
    min_anteil_geaendert_kleingebiet * 100,
    "% geändert\n")
cat("Anzahl auffällige Kleingebiete Sensitivität:",
    nrow(auffaellige_kleingebiete_munich_sensitiv), "\n")
cat("====================================\n")

saveRDS(
  auffaellige_kleingebiete_munich_sensitiv,
  paste0("results_lin_disc/auffaellige_kleingebiete_munich_sensitiv_", suffix, ".rds")
)

write.csv(
  auffaellige_kleingebiete_munich_sensitiv %>%
    st_drop_geometry(),
  paste0("results_lin_disc/auffaellige_kleingebiete_munich_sensitiv_", suffix, ".csv"),
  row.names = FALSE
)


# ==============================================================================
# 25. VALIDIERUNG: DELTA UND ÄQUIVALENTER PRIOR SCALE
# ==============================================================================

chosen_delta <- log1p(prior_scale)
equivalent_prior_scale <- prior_scale

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
# 26. MIETSPIEGELDATENSATZ LADEN UND GEGEN PROBLEMGEBIETE PRÜFEN
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
# 27. INTERAKTIVE GESAMTKARTE GANZ MÜNCHEN
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
data_munich_laerm_direkt_wgs <- st_transform(data_munich_laerm_joined, 4326)
data_munich_all_in_area_wgs <- st_transform(data_munich_all_in_area, 4326)

wohnlagen_munich_analyse_wgs <- st_transform(wohnlagen_munich_analyse, 4326)
problemgebiete_munich_wgs <- st_transform(problemgebiete_munich, 4326)

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

data_munich_laerm_direkt_wgs <- data_munich_laerm_direkt_wgs %>%
  mutate(
    punktfarbe_laerm =
      unname(wohnlage_farben_3[as.character(wohnlage_nach_laerm)])
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
# 27A. POPUPS MODELLPUNKTE
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
    "<b>Punkttyp:</b> Modellpunkt<br>",
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
    "<br>",
    "<b>Getroffene Flächen:</b> ",
    df$n_getroffene_flaechen,
    "<br>",
    "<b>Mehrfachtreffer kleinste Fläche:</b> ",
    ifelse(df$mehrfachtreffer_kleinste_flaeche, "Ja", "Nein"),
    "<br>"
  )
}

modellpunkte_changed$popup_text <-
  popup_modellpunkte(modellpunkte_changed)

modellpunkte_unchanged$popup_text <-
  popup_modellpunkte(modellpunkte_unchanged)


# ==============================================================================
# 27B. POPUPS HOCHLÄRM-PUNKTE
# ==============================================================================

popup_laermpunkte <- function(df) {
  paste0(
    "<b>Hochlärm-Punkt</b><br>",
    "<hr>",
    "<b>Alte Wohnlage:</b> ",
    df$wohnlage_alt_3cat,
    "<br>",
    "<b>Lärmwert:</b> ",
    ifelse(is.na(df$laerm), "n. v.", df$laerm),
    "<br>",
    "<b>Finale Wohnlage nach direkter Lärm-Abwertung:</b> ",
    "<span style='color:red; font-weight:bold;'>",
    df$wohnlage_nach_laerm,
    "</span><br>",
    "<b>Abgewertet durch Lärm:</b> ",
    ifelse(df$abgewertet_durch_laerm, "Ja", "Nein"),
    "<br>",
    "<b>Methode:</b> direkte Abwertung ohne kNN<br>",
    "<hr>",
    "<b>Flächen-ID:</b> ",
    ifelse(is.na(df$flaechen_id), "keine Zuordnung", df$flaechen_id),
    "<br>",
    "<b>Getroffene Flächen:</b> ",
    df$n_getroffene_flaechen,
    "<br>",
    "<b>Mehrfachtreffer kleinste Fläche:</b> ",
    ifelse(df$mehrfachtreffer_kleinste_flaeche, "Ja", "Nein"),
    "<br>"
  )
}

data_munich_laerm_direkt_wgs$popup_text <-
  popup_laermpunkte(data_munich_laerm_direkt_wgs)


# ==============================================================================
# 27C. POPUPS FLÄCHEN
# ==============================================================================

wohnlagen_munich_analyse_wgs <- wohnlagen_munich_analyse_wgs %>%
  mutate(
    popup_flaeche = paste0(
      "<b>Wohnlagenfläche München</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ", flaechen_id, "<br>",
      "<b>Wohnlage:</b> ", Wohnlage_3cat, "<br>",
      "<hr>",
      "<b>Anzahl Punkte insgesamt:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Normale Modellpunkte:</b> ",
      n_modellpunkte,
      "<br>",
      "<b>Hochlärm-Punkte:</b> ",
      n_hochlaerm_punkte,
      "<br>",
      "<hr>",
      "<b>Anzahl geändert insgesamt:</b> ",
      n_geaendert,
      "<br>",
      "<b>Geänderte Modellpunkte:</b> ",
      n_geaendert_modellpunkte,
      "<br>",
      "<b>Geänderte Hochlärm-Punkte:</b> ",
      n_geaendert_hochlaerm,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<hr>",
      "<b>Punkte ohne Fläche nahe dieser Fläche:</b> ",
      n_ohne_flaeche_nahe,
      "<br>",
      "<b>Davon geändert:</b> ",
      n_geaendert_ohne_flaeche_nahe,
      "<br>",
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
      "<hr>",
      "<b>Anzahl Punkte insgesamt:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Normale Modellpunkte:</b> ",
      n_modellpunkte,
      "<br>",
      "<b>Hochlärm-Punkte:</b> ",
      n_hochlaerm_punkte,
      "<br>",
      "<hr>",
      "<b>Anzahl geändert insgesamt:</b> ",
      n_geaendert,
      "<br>",
      "<b>Geänderte Modellpunkte:</b> ",
      n_geaendert_modellpunkte,
      "<br>",
      "<b>Geänderte Hochlärm-Punkte:</b> ",
      n_geaendert_hochlaerm,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<hr>",
      "<b>Punkte ohne Fläche nahe dieser Fläche:</b> ",
      n_ohne_flaeche_nahe,
      "<br>",
      "<b>Davon geändert:</b> ",
      n_geaendert_ohne_flaeche_nahe,
      "<br>"
    )
  )


# ==============================================================================
# 27D. POPUPS MIETSPIEGEL
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
# 27E. KARTENMITTELPUNKT
# ==============================================================================

bbox_munich <- st_bbox(wohnlagen_munich_analyse_wgs)

map_center_lng <- mean(c(bbox_munich["xmin"], bbox_munich["xmax"]))
map_center_lat <- mean(c(bbox_munich["ymin"], bbox_munich["ymax"]))

# Extra: auffällige kleingebiete
auffaellige_kleingebiete_munich_wgs <- st_transform(
  auffaellige_kleingebiete_munich,
  4326
) %>%
  mutate(
    Wohnlage_3cat = clean_wohnlage(Wohnlage),
    popup_kleingebiet = paste0(
      "<b>Auffälliges Kleingebiet</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ", flaechen_id, "<br>",
      "<b>Wohnlage:</b> ", Wohnlage_3cat, "<br>",
      "<hr>",
      "<b>Anzahl Punkte:</b> ", n_wohnungen, "<br>",
      "<b>Normale Modellpunkte:</b> ", n_modellpunkte, "<br>",
      "<b>Hochlärm-Punkte:</b> ", n_hochlaerm_punkte, "<br>",
      "<hr>",
      "<b>Anzahl geändert:</b> ", n_geaendert, "<br>",
      "<b>Geänderte Modellpunkte:</b> ", n_geaendert_modellpunkte, "<br>",
      "<b>Geänderte Hochlärm-Punkte:</b> ", n_geaendert_hochlaerm, "<br>",
      "<b>Anteil geändert:</b> ", round(anteil_geaendert_prozent, 2), " %<br>",
      "<hr>",
      "<b>Hinweis:</b> Separat ausgewertet, da weniger als 20 Punkte."
    )
  )


# ==============================================================================
# 27F. KARTE ERSTELLEN
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
  
  addPolygons(
    data = auffaellige_kleingebiete_munich_wgs,
    fillColor = "transparent",
    fillOpacity = 0,
    color = "orange",
    weight = 3,
    dashArray = "6,4",
    popup = ~popup_kleingebiet,
    label = ~paste0(
      "Auffälliges Kleingebiet Fläche ",
      flaechen_id,
      " | ",
      n_wohnungen,
      " Punkte | ",
      round(anteil_geaendert_prozent, 1),
      " % geändert"
    ),
    group = "Auffällige Kleingebiete"
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
    data = data_munich_laerm_direkt_wgs,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = ~punktfarbe_laerm,
    fillOpacity = 1,
    color = "violet",
    stroke = TRUE,
    weight = 2.3,
    radius = 6,
    popup = ~popup_text,
    group = "Hochlärm-Punkte: direkte Abwertung"
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
      "Auffällige Kleingebiete",
      "Modellpunkte: unverändert",
      "Modellpunkte: umklassifiziert",
      "Hochlärm-Punkte: direkte Abwertung",
      "Mietspiegel: in Problemgebiet",
      "Mietspiegel: nicht in Problemgebiet"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup("Modellpunkte: unverändert") %>%
  hideGroup("Mietspiegel: nicht in Problemgebiet")


# ==============================================================================
# 28. KARTE SPEICHERN
# ==============================================================================

saveWidget(
  karte_munich_gesamt,
  file = paste0(
    "interaktive_karten/karte_munich_gesamtanalyse_mit_mietspiegel_",
    suffix,
    ".html"
  ),
  selfcontained = FALSE
)

cat("\n✓ Interaktive München-Karte gespeichert:\n")
cat(
  paste0(
    "interaktive_karten/karte_munich_gesamtanalyse_mit_mietspiegel_",
    suffix,
    ".html\n"
  )
)


# ==============================================================================
# 29. ABSCHLUSS-ZUSAMMENFASSUNG
# ==============================================================================

cat("\n====================================\n")
cat("ZUSAMMENFASSUNG GESAMTWORKFLOW MÜNCHEN\n")
cat("====================================\n")

cat("Normale Modellpunkte gesamt:",
    nrow(data_munich_joined_wgs), "\n")

cat("Davon normale Modellpunkte umklassifiziert:",
    nrow(modellpunkte_changed), "\n")

cat("Normale Modellpunkte ohne Fläche:",
    sum(is.na(data_munich_joined$flaechen_id)), "\n")

cat("Hochlärm-Punkte gesamt:",
    nrow(data_munich_laerm_direkt_wgs), "\n")

cat("Hochlärm-Punkte ohne Fläche:",
    sum(is.na(data_munich_laerm_joined$flaechen_id)), "\n")

cat("Alle Punkte für Problemgebietsauswertung:",
    nrow(data_munich_all_in_area), "\n")

cat("Davon normale Modellpunkte:",
    sum(data_munich_all_in_area$punkt_typ == "modellpunkt", na.rm = TRUE), "\n")

cat("Davon Hochlärm-Punkte:",
    sum(data_munich_all_in_area$punkt_typ == "hochlaerm_direkt_abgewertet", na.rm = TRUE), "\n")

cat("Änderungsrate inklusive Hochlärm-Punkte:",
    round(mean(data_munich_all_in_area$changed, na.rm = TRUE) * 100, 2),
    "%\n")

cat("Wohnlagenflächen mit analysierten Punkten:",
    nrow(wohnlagen_munich_analyse_wgs), "\n")

cat("Problemgebiete original:",
    nrow(problemgebiete_munich_wgs), "\n")

cat("Problemgebiete Sensitivität:",
    nrow(problemgebiete_munich_sensitiv), "\n")

cat("Mietspiegel-Punkte gesamt:",
    nrow(mietspiegel_geo_wgs), "\n")

cat("Mietspiegel-Punkte in Problemgebieten:",
    nrow(mietspiegel_problem), "\n")

cat("Mietspiegel-Punkte nicht in Problemgebieten:",
    nrow(mietspiegel_nicht_problem), "\n")

cat("====================================\n")


# ==============================================================================
# 30. ELBOW PLOT:
# prior_scale vs. Anzahl Problemgebiete
# WICHTIG: Hochlärm-Punkte werden fix mitgezählt
# ==============================================================================

SCORE_munich <- readRDS("results_lin_disc/SCORE_munich_mit_laerm.rds")

data_joined <- data_munich_joined

data_eval <- data_joined %>%
  st_drop_geometry()

stopifnot(nrow(SCORE_munich) == nrow(data_eval))

if (!is.null(colnames(SCORE_munich))) {
  levels_c <- colnames(SCORE_munich)
} else {
  levels_c <- levels(data_eval$c)
}

data_eval$c <- factor(as.character(data_eval$c), levels = levels_c)

idx_area <- !is.na(data_eval$flaechen_id)

SCORE_area <- SCORE_munich[idx_area, , drop = FALSE]
data_area  <- data_eval[idx_area, ]

data_laerm_area <- data_munich_laerm_in_area %>%
  st_drop_geometry()

cat("\nElbow-Basis:\n")
cat("Normale Modellpunkte mit Fläche:", nrow(data_area), "\n")
cat("Hochlärm-Punkte mit Fläche:", nrow(data_laerm_area), "\n")


count_problemgebiete_for_prior <- function(
    SCORE,
    data,
    data_laerm,
    levels_c,
    prior_scale
) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  colnames(PROB_prior) <- levels_c
  
  pred <- levels_c[max.col(PROB_prior)]
  
  data_model_eval <- data %>%
    mutate(
      wohnlage_alt = as.character(c),
      wohnlage_neu = pred,
      changed = wohnlage_alt != wohnlage_neu,
      punkt_typ = "modellpunkt"
    )
  
  data_laerm_eval <- data_laerm %>%
    mutate(
      punkt_typ = "hochlaerm_direkt_abgewertet"
    )
  
  eval_all <- bind_rows(
    data_model_eval %>%
      select(flaechen_id, changed, punkt_typ),
    data_laerm_eval %>%
      select(flaechen_id, changed, punkt_typ)
  )
  
  gebiet_summary <- eval_all %>%
    group_by(flaechen_id) %>%
    summarise(
      n_wohnungen = n(),
      n_modellpunkte =
        sum(punkt_typ == "modellpunkt", na.rm = TRUE),
      n_hochlaerm_punkte =
        sum(punkt_typ == "hochlaerm_direkt_abgewertet", na.rm = TRUE),
      n_geaendert =
        sum(changed, na.rm = TRUE),
      anteil_geaendert =
        n_geaendert / n_wohnungen,
      .groups = "drop"
    )
  
  problemgebiete <- gebiet_summary %>%
    filter(
      n_wohnungen >= min_wohnungen_problemgebiet,
      anteil_geaendert >= min_anteil_geaendert_problemgebiet
    )
  
  data.frame(
    prior_scale = prior_scale,
    delta_equiv = log1p(prior_scale),
    n_problemgebiete = nrow(problemgebiete),
    n_geaenderte_punkte = sum(eval_all$changed, na.rm = TRUE),
    anteil_geaenderte_punkte = mean(eval_all$changed, na.rm = TRUE),
    n_hochlaerm_punkte = nrow(data_laerm_eval),
    n_geaenderte_hochlaerm_punkte =
      sum(data_laerm_eval$changed, na.rm = TRUE)
  )
}


prior_scales <- sort(unique(c(
  seq(0, 5, by = 0.25),
  seq(5, 30, by = 0.5),
  seq(30, 100, by = 2),
  seq(100, 300, by = 5),
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
        data_laerm = data_laerm_area,
        levels_c = levels_c,
        prior_scale = p
      )
    }
  )
)

print(elbow_df)


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

# ==============================================================================
# FEST GEWÄHLTER PRIOR_SCALE FÜR DIE DARSTELLUNG
# ==============================================================================

chosen_prior_scale <- 7.5

chosen_idx <- which.min(
  abs(elbow_df$prior_scale - chosen_prior_scale)
)

chosen_point <- elbow_df[chosen_idx, ]

cat("\n====================================\n")
cat("ELBOW-DARSTELLUNG\n")
cat("====================================\n")
cat("Angezeigter prior_scale:", chosen_prior_scale, "\n")
cat("Problemgebiete bei prior_scale = 7.5:",
    chosen_point$n_problemgebiete, "\n")
cat("====================================\n")

x_max_plot <- 50

p_elbow <- ggplot(
  elbow_df,
  aes(x = prior_scale, y = n_problemgebiete)
) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  
  geom_vline(
    xintercept = chosen_prior_scale,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  
  geom_point(
    data = chosen_point,
    aes(x = prior_scale, y = n_problemgebiete),
    size = 4,
    color = "black"
  ) +
  
  annotate(
    "label",
    x = chosen_prior_scale + 2,
    y = chosen_point$n_problemgebiete + 10,
    label = paste0(
      "gewählt\n",
      "prior_scale = ", chosen_prior_scale, "\n",
      "Problemgebiete = ", chosen_point$n_problemgebiete
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

write.csv(
  elbow_df,
  "results_lin_disc/elbow_prior_scale_problemgebiete_inkl_hochlaerm.csv",
  row.names = FALSE
)

ggsave(
  filename = "results_lin_disc/elbow_prior_scale_problemgebiete_inkl_hochlaerm.png",
  plot = p_elbow,
  width = 9,
  height = 5,
  dpi = 300
)

cat("\nGespeichert:\n")
cat("results_lin_disc/elbow_prior_scale_problemgebiete_inkl_hochlaerm.csv\n")
cat("results_lin_disc/elbow_prior_scale_problemgebiete_inkl_hochlaerm.png\n")