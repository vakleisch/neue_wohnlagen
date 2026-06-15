library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(patchwork)

suffix <- "prior_scale_7_5"

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

mietspiegel_geo <- load_rds_first_existing(
  c(
    paste0("results_lin_disc/mietspiegel_geo_problemgebiete_munich_", suffix, ".rds"),
    "results_lin_disc/mietspiegel_geo_problemgebiete_munich.rds"
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

target_crs <- st_crs(problemgebiete_munich)

wohnlagen_munich_analyse <- st_transform(wohnlagen_munich_analyse, target_crs)
data_munich_joined <- st_transform(data_munich_joined, target_crs)
data_munich_laerm_knn <- st_transform(data_munich_laerm_knn, target_crs)
mietspiegel_geo <- st_transform(mietspiegel_geo, target_crs)

problemgebiete_sorted <- problemgebiete_munich %>%
  st_drop_geometry() %>%
  arrange(
    desc(anteil_geaendert),
    desc(n_geaendert),
    desc(n_wohnungen)
  ) %>%
  mutate(rang = row_number())

problem_ids <- problemgebiete_sorted$flaechen_id

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

make_problemgebiet_map <- function(flaechen_id_i, rang_i, buffer_m = 250) {
  
  pg <- problemgebiete_munich %>%
    filter(flaechen_id == flaechen_id_i)
  
  pg_utm <- st_transform(pg, 25832)
  bbox_geom_utm <- st_buffer(st_geometry(pg_utm), dist = buffer_m)
  bbox_geom <- st_transform(bbox_geom_utm, target_crs)
  
  wohnlagen_crop <- wohnlagen_munich_analyse[
    lengths(st_intersects(wohnlagen_munich_analyse, bbox_geom)) > 0,
  ]
  
  punkte_crop <- data_munich_joined[
    lengths(st_intersects(data_munich_joined, bbox_geom)) > 0,
  ]
  
  laerm_crop <- data_munich_laerm_knn[
    lengths(st_intersects(data_munich_laerm_knn, bbox_geom)) > 0,
  ]
  
  miet_crop <- mietspiegel_geo[
    lengths(st_intersects(mietspiegel_geo, bbox_geom)) > 0,
  ]
  
  wohnlagen_crop <- wohnlagen_crop %>%
    mutate(
      Wohnlage_3cat = clean_wohnlage(Wohnlage)
    )
  
  ggplot() +
    geom_sf(
      data = wohnlagen_crop,
      aes(fill = Wohnlage_3cat),
      color = "grey45",
      linewidth = 0.25,
      alpha = 0.80
    ) +
    geom_sf(
      data = punkte_crop %>% filter(changed == FALSE),
      size = 0.65,
      alpha = 0.45,
      color = "grey25"
    ) +
    geom_sf(
      data = punkte_crop %>% filter(changed == TRUE),
      size = 1.15,
      alpha = 0.90,
      color = "red"
    ) +
    geom_sf(
      data = laerm_crop,
      size = 1.35,
      shape = 17,
      color = "darkred",
      alpha = 0.90
    ) +
    geom_sf(
      data = miet_crop,
      size = 1.35,
      shape = 21,
      fill = "blue",
      color = "white",
      stroke = 0.25,
      alpha = 0.95
    ) +
    geom_sf(
      data = pg,
      fill = NA,
      color = "red",
      linewidth = 1.35
    ) +
    scale_fill_manual(values = wohnlage_farben_3, drop = FALSE) +
    coord_sf(
      xlim = st_bbox(bbox_geom)[c("xmin", "xmax")],
      ylim = st_bbox(bbox_geom)[c("ymin", "ymax")],
      expand = FALSE
    ) +
    labs(
      title = paste0("Rang ", rang_i, " | Problemgebiet ", flaechen_id_i),
      fill = "Wohnlage"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.35, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

make_problemgebiet_table <- function(flaechen_id_i, rang_i) {
  
  pg <- problemgebiete_munich %>%
    st_drop_geometry() %>%
    filter(flaechen_id == flaechen_id_i)
  
  punkte_pg <- data_munich_joined %>%
    filter(flaechen_id == flaechen_id_i)
  
  dominant_transition <- dominante_umklassifizierung(punkte_pg)
  
  miet_n <- mietspiegel_geo %>%
    st_drop_geometry() %>%
    filter(problemgebiet_flaechen_id == flaechen_id_i) %>%
    nrow()
  
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
      "Mietspiegelpunkte",
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
      miet_n,
      laerm_n
    ),
    stringsAsFactors = FALSE
  )
  
  tab_df$Kennzahl <- wrap_value(tab_df$Kennzahl, width = 24)
  tab_df$Wert <- wrap_value(tab_df$Wert, width = 30)
  
  tab_df
}

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

pdf_file <- paste0(
  "reports/problemgebiete_report_",
  suffix,
  ".pdf"
)

pdf(
  file = pdf_file,
  width = 13.333,
  height = 7.5
)

for (i in seq_along(problem_ids)) {
  
  fid <- problem_ids[i]
  
  cat("Erzeuge Seite", i, "für Problemgebiet", fid, "\n")
  
  map_plot <- make_problemgebiet_map(
    flaechen_id_i = fid,
    rang_i = i,
    buffer_m = 250
  )
  
  tab_df <- make_problemgebiet_table(
    flaechen_id_i = fid,
    rang_i = i
  )
  
  table_grob <- tableGrob(
    tab_df,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 9,
      core = list(
        fg_params = list(
          hjust = 0,
          x = 0.02,
          fontsize = 9
        )
      ),
      colhead = list(
        fg_params = list(
          fontface = "bold",
          fontsize = 10
        )
      )
    )
  )
  
  table_grob$widths <- unit(c(0.45, 0.55), "npc")
  
  table_panel <- wrap_elements(table_grob)
  
  page <- map_plot + table_panel +
    plot_layout(widths = c(1.85, 1))
  
  print(page)
}

dev.off()

cat("PDF gespeichert unter:\n")
cat(pdf_file, "\n")