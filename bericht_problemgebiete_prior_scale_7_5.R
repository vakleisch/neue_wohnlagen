# ==============================================================================
# PDF-REPORT: Problemgebiete mit Leaflet-Kartenausschnitten
# Prior Scale 7.5
# Endformat: PDF
# - Problemgebiete sortiert nach Änderungsrate
# - Karte links, Tabelle rechts
# - Leaflet-Kartenausschnitte mit Basiskarte
# - näher an Problemgebiet herangezoomt
# - keine Mietspiegel-Punkte
# - keine Punkt-Legende unten links
# - Wohnlagen-Legende unten rechts bleibt erhalten
# - keine Hochlärm-Punkte in Karte oder Tabelle
# - kein Rang in Überschrift oder Tabelle
# ==============================================================================

# ==============================================================================
# 0. PAKETE
# ==============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(patchwork)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(png)

# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

suffix <- "prior_scale_7_5"

buffer_m <- 300

out_dir <- "reports/leaflet_ausschnitte"
pdf_file <- paste0("reports/problemgebiete_report_", suffix, "_leaflet.pdf")

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

if (dir.exists(out_dir)) {
  unlink(file.path(out_dir, "*"), recursive = TRUE)
} else {
  dir.create(out_dir, recursive = TRUE)
}

# ==============================================================================
# 2. DATEN LADEN
# ==============================================================================

problemgebiete_munich <- readRDS(
  paste0("results_lin_disc/problemgebiete_munich_", suffix, ".rds")
)

wohnlagen_munich_analyse <- readRDS(
  paste0("results_lin_disc/wohnlagen_munich_analyse_", suffix, ".rds")
)

data_munich_joined <- readRDS(
  paste0("results_lin_disc/data_munich_joined_", suffix, ".rds")
)

# ==============================================================================
# 3. CRS ANGLEICHEN
# ==============================================================================

target_crs <- st_crs(problemgebiete_munich)

wohnlagen_munich_analyse <- st_transform(wohnlagen_munich_analyse, target_crs)
data_munich_joined <- st_transform(data_munich_joined, target_crs)

# ==============================================================================
# 4. PROBLEMGEBIETE SORTIEREN
# ==============================================================================

problemgebiete_sorted <- problemgebiete_munich %>%
  st_drop_geometry() %>%
  arrange(
    desc(anteil_geaendert),
    desc(n_geaendert),
    desc(n_wohnungen)
  )

problem_ids <- problemgebiete_sorted$flaechen_id

# ==============================================================================
# 5. HILFSFUNKTIONEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

clean_wohnlage <- function(x) {
  trimws(gsub("zentrale", "", as.character(x), ignore.case = TRUE))
}

wrap_value <- function(x, width = 28) {
  vapply(
    as.character(x),
    function(z) paste(strwrap(z, width = width), collapse = "\n"),
    character(1)
  )
}

dominante_umklassifizierung <- function(df) {
  
  df_changed <- df %>%
    st_drop_geometry() %>%
    filter(changed == TRUE)
  
  if (nrow(df_changed) == 0) {
    return("keine Umklassifizierung")
  }
  
  tab <- df_changed %>%
    count(wohnlage_alt, wohnlage_neu, sort = TRUE)
  
  paste0(
    tab$wohnlage_alt[1],
    " -> ",
    tab$wohnlage_neu[1],
    " (n = ",
    tab$n[1],
    ")"
  )
}

# ==============================================================================
# 6. LEAFLET-KARTENAUSSCHNITT ALS PNG
# ==============================================================================

make_leaflet_problemgebiet_png <- function(
    flaechen_id_i,
    buffer_m = 300,
    out_dir = "reports/leaflet_ausschnitte"
) {
  
  png_file <- file.path(
    out_dir,
    paste0("problemgebiet_", flaechen_id_i, ".png")
  )
  
  html_file <- file.path(
    out_dir,
    paste0("problemgebiet_", flaechen_id_i, ".html")
  )
  
  pg <- problemgebiete_munich %>%
    filter(flaechen_id == flaechen_id_i)
  
  pg_utm <- st_transform(pg, 25832)
  bbox_geom_utm <- st_buffer(st_geometry(pg_utm), dist = buffer_m)
  bbox_geom <- st_transform(bbox_geom_utm, target_crs)
  
  wohnlagen_crop <- wohnlagen_munich_analyse[
    lengths(st_intersects(wohnlagen_munich_analyse, bbox_geom)) > 0,
  ] %>%
    mutate(
      Wohnlage_3cat = clean_wohnlage(Wohnlage),
      flaechenfarbe = unname(wohnlage_farben_3[Wohnlage_3cat])
    )
  
  punkte_crop <- data_munich_joined[
    lengths(st_intersects(data_munich_joined, bbox_geom)) > 0,
  ] %>%
    mutate(
      punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
    )
  
  punkte_changed <- punkte_crop %>%
    filter(changed == TRUE)
  
  punkte_unchanged <- punkte_crop %>%
    filter(changed == FALSE)
  
  pg_wgs <- st_transform(pg, 4326)
  bbox_wgs <- st_bbox(st_transform(bbox_geom, 4326))
  wohnlagen_crop_wgs <- st_transform(wohnlagen_crop, 4326)
  punkte_changed_wgs <- st_transform(punkte_changed, 4326)
  punkte_unchanged_wgs <- st_transform(punkte_unchanged, 4326)
  
  karte <- suppressWarnings({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = TRUE,
        attributionControl = TRUE
      )
    ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(
        data = wohnlagen_crop_wgs,
        fillColor = ~flaechenfarbe,
        fillOpacity = 0.45,
        color = "grey40",
        weight = 0.8
      ) %>%
      
      addCircleMarkers(
        data = punkte_unchanged_wgs,
        lng = ~s.long,
        lat = ~s.lat,
        radius = 3.2,
        fillColor = ~punktfarbe,
        fillOpacity = 0.35,
        color = "darkgreen",
        opacity = 0.45,
        stroke = TRUE,
        weight = 0.8
      ) %>%
      
      addCircleMarkers(
        data = punkte_changed_wgs,
        lng = ~s.long,
        lat = ~s.lat,
        radius = 4.8,
        fillColor = ~punktfarbe,
        fillOpacity = 0.90,
        color = "red",
        opacity = 1,
        stroke = TRUE,
        weight = 1.8
      ) %>%
      
      addPolygons(
        data = pg_wgs,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "red",
        weight = 4
      ) %>%
      
      addLegend(
        position = "bottomright",
        colors = unname(wohnlage_farben_3),
        labels = names(wohnlage_farben_3),
        title = "Wohnlage",
        opacity = 1
      ) %>%
      
      fitBounds(
        lng1 = unname(bbox_wgs["xmin"]),
        lat1 = unname(bbox_wgs["ymin"]),
        lng2 = unname(bbox_wgs["xmax"]),
        lat2 = unname(bbox_wgs["ymax"])
      )
  })
  
  suppressWarnings({
    htmlwidgets::saveWidget(
      karte,
      file = html_file,
      selfcontained = TRUE
    )
  })
  
  webshot2::webshot(
    url = normalizePath(html_file),
    file = png_file,
    vwidth = 1400,
    vheight = 860,
    delay = 8,
    zoom = 1
  )
  
  if (!file.exists(png_file)) {
    stop(paste0("PNG wurde nicht erzeugt: ", png_file))
  }
  
  if (file.info(png_file)$size < 10000) {
    warning(paste0("PNG ist sehr klein und eventuell leer: ", png_file))
  }
  
  png_file
}

# ==============================================================================
# 7. PNG ALS GGPLOT EINBINDEN
# ==============================================================================

make_leaflet_plot <- function(flaechen_id_i, buffer_m = 300) {
  
  png_file <- make_leaflet_problemgebiet_png(
    flaechen_id_i = flaechen_id_i,
    buffer_m = buffer_m,
    out_dir = out_dir
  )
  
  img <- png::readPNG(png_file)
  
  raster <- rasterGrob(
    img,
    x = 0.5,
    y = 0.5,
    width = unit(1, "npc"),
    height = unit(0.82, "npc"),
    just = "center",
    interpolate = TRUE
  )
  
  ggplot() +
    annotation_custom(
      raster,
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    labs(
      title = paste0("Problemgebiet ", flaechen_id_i)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.margin = margin(8, 8, 8, 8)
    )
}

# ==============================================================================
# 8. TABELLE JE PROBLEMGEBIET
# ==============================================================================

make_problemgebiet_table <- function(flaechen_id_i) {
  
  pg <- problemgebiete_munich %>%
    st_drop_geometry() %>%
    filter(flaechen_id == flaechen_id_i)
  
  punkte_pg <- data_munich_joined %>%
    filter(flaechen_id == flaechen_id_i)
  
  dominant_transition <- dominante_umklassifizierung(punkte_pg)
  
  tab_df <- data.frame(
    Kennzahl = c(
      "Flächen-ID",
      "Wohnlage der Fläche",
      "Wohnobjekte insgesamt",
      "Geänderte Punkte",
      "Änderungsrate",
      "Dominante Neulage",
      "Häufigste Umklassifizierung"
    ),
    Wert = c(
      pg$flaechen_id,
      clean_wohnlage(pg$Wohnlage),
      pg$n_wohnungen,
      pg$n_geaendert,
      paste0(round(pg$anteil_geaendert * 100, 1), " %"),
      pg$neue_lage_haeufig,
      dominant_transition
    ),
    stringsAsFactors = FALSE
  )
  
  tab_df$Kennzahl <- wrap_value(tab_df$Kennzahl, width = 24)
  tab_df$Wert <- wrap_value(tab_df$Wert, width = 30)
  
  tab_df
}

# ==============================================================================
# 9. PDF-REPORT ERSTELLEN
# ==============================================================================

pdf(
  file = pdf_file,
  width = 13.333,
  height = 7.5
)

for (fid in problem_ids) {
  
  cat("Erzeuge Seite für Problemgebiet", fid, "\n")
  
  map_plot <- make_leaflet_plot(
    flaechen_id_i = fid,
    buffer_m = buffer_m
  )
  
  tab_df <- make_problemgebiet_table(
    flaechen_id_i = fid
  )
  
  table_grob <- tableGrob(
    tab_df,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 11,
      core = list(
        fg_params = list(
          hjust = 0,
          x = 0.02,
          fontsize = 11
        )
      ),
      colhead = list(
        fg_params = list(
          fontface = "bold",
          fontsize = 12
        )
      )
    )
  )
  
  table_grob$widths <- unit(c(0.48, 0.52), "npc")
  
  table_panel <- wrap_elements(table_grob)
  
  page <- map_plot + table_panel +
    plot_layout(widths = c(2.15, 1))
  
  print(page)
}

dev.off()

cat("\nPDF gespeichert unter:\n")
cat(pdf_file, "\n")
cat("\nErzeugte Kartenausschnitte:\n")
print(list.files(out_dir, pattern = "\\.png$", full.names = TRUE))








# ==============================================================================
# TESTSKRIPT: PowerPoint-Testreport mit Leaflet-Kartenausschnitten
# Prior Scale 7.5
# Nur erste 3 Problemgebiete
# Ohne Mietspiegelpunkte
# ==============================================================================

# ==============================================================================
# 0. PAKETE
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(officer)
library(grid)
library(png)

# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

suffix <- "prior_scale_7_5"

anzahl_testfolien <- 3
buffer_m <- 700

out_dir <- "reports/test_leaflet_ausschnitte"
ppt_file <- paste0("reports/test_problemgebiete_leaflet_", suffix, ".pptx")

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

if (dir.exists(out_dir)) {
  unlink(file.path(out_dir, "*"), recursive = TRUE)
} else {
  dir.create(out_dir, recursive = TRUE)
}

# ==============================================================================
# 2. DATEN LADEN
# ==============================================================================

load_rds_first_existing <- function(paths) {
  path <- paths[file.exists(paths)][1]
  
  if (is.na(path)) {
    stop(
      paste0(
        "Keine der folgenden Dateien gefunden:\n",
        paste(paths, collapse = "\n")
      )
    )
  }
  
  readRDS(path)
}

problemgebiete_munich <- readRDS(
  paste0("results_lin_disc/problemgebiete_munich_", suffix, ".rds")
)

wohnlagen_munich_analyse <- readRDS(
  paste0("results_lin_disc/wohnlagen_munich_analyse_", suffix, ".rds")
)

data_munich_joined <- readRDS(
  paste0("results_lin_disc/data_munich_joined_", suffix, ".rds")
)

data_munich_laerm_knn <- load_rds_first_existing(
  c(
    paste0("results_lin_disc/data_munich_hochlaerm_knn_abgewertet_", suffix, ".rds"),
    "results_lin_disc/data_munich_hochlaerm_knn_abgewertet.rds"
  )
)

if (!inherits(data_munich_laerm_knn, "sf")) {
  data_munich_laerm_knn <- st_as_sf(
    data_munich_laerm_knn,
    coords = c("s.long", "s.lat"),
    crs = 4326,
    remove = FALSE
  )
}

# ==============================================================================
# 3. CRS ANGLEICHEN
# ==============================================================================

target_crs <- st_crs(problemgebiete_munich)

wohnlagen_munich_analyse <- st_transform(wohnlagen_munich_analyse, target_crs)
data_munich_joined <- st_transform(data_munich_joined, target_crs)
data_munich_laerm_knn <- st_transform(data_munich_laerm_knn, target_crs)

# ==============================================================================
# 4. PROBLEMGEBIETE SORTIEREN
# ==============================================================================

problemgebiete_sorted <- problemgebiete_munich %>%
  st_drop_geometry() %>%
  arrange(
    desc(anteil_geaendert),
    desc(n_geaendert),
    desc(n_wohnungen)
  ) %>%
  mutate(rang = row_number())

problem_ids <- problemgebiete_sorted$flaechen_id[seq_len(
  min(anzahl_testfolien, nrow(problemgebiete_sorted))
)]

# ==============================================================================
# 5. HILFSFUNKTIONEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

clean_wohnlage <- function(x) {
  trimws(gsub("zentrale", "", as.character(x), ignore.case = TRUE))
}

wrap_value <- function(x, width = 28) {
  vapply(
    as.character(x),
    function(z) paste(strwrap(z, width = width), collapse = "\n"),
    character(1)
  )
}

dominante_umklassifizierung <- function(df) {
  
  df_changed <- df %>%
    st_drop_geometry() %>%
    filter(changed == TRUE)
  
  if (nrow(df_changed) == 0) {
    return("keine Umklassifizierung")
  }
  
  tab <- df_changed %>%
    count(wohnlage_alt, wohnlage_neu, sort = TRUE)
  
  paste0(
    tab$wohnlage_alt[1],
    " -> ",
    tab$wohnlage_neu[1],
    " (n = ",
    tab$n[1],
    ")"
  )
}

# ==============================================================================
# 6. LEAFLET-KARTE ALS PNG ERZEUGEN
# ==============================================================================

make_leaflet_problemgebiet_png <- function(
    flaechen_id_i,
    rang_i,
    buffer_m = 700,
    out_dir = "reports/test_leaflet_ausschnitte"
) {
  
  png_file <- file.path(
    out_dir,
    paste0("problemgebiet_", rang_i, "_", flaechen_id_i, ".png")
  )
  
  html_file <- file.path(
    out_dir,
    paste0("problemgebiet_", rang_i, "_", flaechen_id_i, ".html")
  )
  
  pg <- problemgebiete_munich %>%
    filter(flaechen_id == flaechen_id_i)
  
  pg_utm <- st_transform(pg, 25832)
  bbox_geom_utm <- st_buffer(st_geometry(pg_utm), dist = buffer_m)
  bbox_geom <- st_transform(bbox_geom_utm, target_crs)
  
  wohnlagen_crop <- wohnlagen_munich_analyse[
    lengths(st_intersects(wohnlagen_munich_analyse, bbox_geom)) > 0,
  ] %>%
    mutate(
      Wohnlage_3cat = clean_wohnlage(Wohnlage),
      flaechenfarbe = unname(wohnlage_farben_3[Wohnlage_3cat])
    )
  
  punkte_crop <- data_munich_joined[
    lengths(st_intersects(data_munich_joined, bbox_geom)) > 0,
  ] %>%
    mutate(
      punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
    )
  
  punkte_changed <- punkte_crop %>%
    filter(changed == TRUE)
  
  punkte_unchanged <- punkte_crop %>%
    filter(changed == FALSE)
  
  laerm_crop <- data_munich_laerm_knn[
    lengths(st_intersects(data_munich_laerm_knn, bbox_geom)) > 0,
  ]
  
  pg_wgs <- st_transform(pg, 4326)
  bbox_wgs <- st_bbox(st_transform(bbox_geom, 4326))
  wohnlagen_crop_wgs <- st_transform(wohnlagen_crop, 4326)
  punkte_changed_wgs <- st_transform(punkte_changed, 4326)
  punkte_unchanged_wgs <- st_transform(punkte_unchanged, 4326)
  laerm_crop_wgs <- st_transform(laerm_crop, 4326)
  
  karte <- suppressWarnings({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = TRUE,
        attributionControl = TRUE
      )
    ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(
        data = wohnlagen_crop_wgs,
        fillColor = ~flaechenfarbe,
        fillOpacity = 0.45,
        color = "grey40",
        weight = 0.8,
        group = "Wohnlagenflächen"
      ) %>%
      
      addCircleMarkers(
        data = punkte_unchanged_wgs,
        lng = ~s.long,
        lat = ~s.lat,
        radius = 3,
        fillColor = ~punktfarbe,
        fillOpacity = 0.45,
        color = "darkgreen",
        opacity = 0.55,
        stroke = TRUE,
        weight = 0.8,
        group = "Modellpunkte unverändert"
      ) %>%
      
      addCircleMarkers(
        data = punkte_changed_wgs,
        lng = ~s.long,
        lat = ~s.lat,
        radius = 4.5,
        fillColor = ~punktfarbe,
        fillOpacity = 0.85,
        color = "red",
        opacity = 1,
        stroke = TRUE,
        weight = 1.8,
        group = "Modellpunkte umklassifiziert"
      ) %>%
      
      addCircleMarkers(
        data = laerm_crop_wgs,
        lng = ~s.long,
        lat = ~s.lat,
        radius = 4.5,
        fillColor = "darkred",
        fillOpacity = 0.95,
        color = "darkred",
        opacity = 1,
        stroke = TRUE,
        weight = 1,
        group = "Hochlärm-Punkte"
      ) %>%
      
      addPolygons(
        data = pg_wgs,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "red",
        weight = 4,
        group = "Problemgebiet"
      ) %>%
      
      addLegend(
        position = "bottomright",
        colors = unname(wohnlage_farben_3),
        labels = names(wohnlage_farben_3),
        title = "Wohnlage",
        opacity = 1
      ) %>%
      
      addLegend(
        position = "bottomleft",
        colors = c("darkgreen", "red", "darkred"),
        labels = c("unverändert", "umklassifiziert", "Hochlärm-Punkt"),
        title = "Punkte",
        opacity = 1
      ) %>%
      
      fitBounds(
        lng1 = unname(bbox_wgs["xmin"]),
        lat1 = unname(bbox_wgs["ymin"]),
        lng2 = unname(bbox_wgs["xmax"]),
        lat2 = unname(bbox_wgs["ymax"])
      )
  })
  
  suppressWarnings({
    htmlwidgets::saveWidget(
      karte,
      file = html_file,
      selfcontained = TRUE
    )
  })
  
  webshot2::webshot(
    url = normalizePath(html_file),
    file = png_file,
    vwidth = 1400,
    vheight = 860,
    delay = 8,
    zoom = 1
  )
  
  if (!file.exists(png_file)) {
    stop(paste0("PNG wurde nicht erzeugt: ", png_file))
  }
  
  if (file.info(png_file)$size < 10000) {
    warning(paste0("PNG ist sehr klein und eventuell leer: ", png_file))
  }
  
  png_file
}

# ==============================================================================
# 7. TABELLE FÜR POWERPOINT
# ==============================================================================

make_problemgebiet_table <- function(flaechen_id_i, rang_i) {
  
  pg <- problemgebiete_munich %>%
    st_drop_geometry() %>%
    filter(flaechen_id == flaechen_id_i)
  
  punkte_pg <- data_munich_joined %>%
    filter(flaechen_id == flaechen_id_i)
  
  dominant_transition <- dominante_umklassifizierung(punkte_pg)
  
  pg_geom <- problemgebiete_munich %>%
    filter(flaechen_id == flaechen_id_i)
  
  laerm_n <- sum(lengths(st_intersects(data_munich_laerm_knn, pg_geom)) > 0)
  
  tab_df <- data.frame(
    Kennzahl = c(
      "Rang",
      "Flächen-ID",
      "Wohnlage der Fläche",
      "Wohnobjekte insgesamt",
      "Geänderte Punkte",
      "Änderungsrate",
      "Dominante Ausgangslage",
      "Dominante Neulage",
      "Häufigste Umklassifizierung",
      "Hochlärm-Punkte"
    ),
    Wert = c(
      rang_i,
      pg$flaechen_id,
      clean_wohnlage(pg$Wohnlage),
      pg$n_wohnungen,
      pg$n_geaendert,
      paste0(round(pg$anteil_geaendert * 100, 1), " %"),
      pg$alte_lage_haeufig,
      pg$neue_lage_haeufig,
      dominant_transition,
      laerm_n
    ),
    stringsAsFactors = FALSE
  )
  
  tab_df$Kennzahl <- wrap_value(tab_df$Kennzahl, width = 24)
  tab_df$Wert <- wrap_value(tab_df$Wert, width = 30)
  
  tab_df
}

# ==============================================================================
# 8. TEST-POWERPOINT ERSTELLEN
# ==============================================================================

ppt <- read_pptx()

for (i in seq_along(problem_ids)) {
  
  fid <- problem_ids[i]
  
  cat("Erzeuge Testfolie", i, "für Problemgebiet", fid, "\n")
  
  png_file <- make_leaflet_problemgebiet_png(
    flaechen_id_i = fid,
    rang_i = i,
    buffer_m = buffer_m,
    out_dir = out_dir
  )
  
  tab_df <- make_problemgebiet_table(
    flaechen_id_i = fid,
    rang_i = i
  )
  
  ppt <- add_slide(
    ppt,
    layout = "Blank",
    master = "Office Theme"
  )
  
  ppt <- ph_with(
    ppt,
    value = fpar(
      ftext(
        paste0("Rang ", i, " | Problemgebiet ", fid),
        fp_text(font.size = 22, bold = TRUE)
      )
    ),
    location = ph_location(
      left = 0.35,
      top = 0.15,
      width = 12.5,
      height = 0.45
    )
  )
  
  ppt <- ph_with(
    ppt,
    external_img(png_file),
    location = ph_location(
      left = 0.35,
      top = 0.75,
      width = 8.5,
      height = 6.25
    )
  )
  
  ppt <- ph_with(
    ppt,
    value = tab_df,
    location = ph_location(
      left = 9.05,
      top = 0.95,
      width = 4.0,
      height = 5.8
    )
  )
}

print(ppt, target = ppt_file)

cat("\nTest-PowerPoint gespeichert unter:\n")
cat(ppt_file, "\n")
cat("\nErzeugte PNG-Testbilder:\n")
print(list.files(out_dir, pattern = "\\.png$", full.names = TRUE))