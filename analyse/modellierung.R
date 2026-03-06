library(mgcv)
library(dplyr)
source(here("daten_verarbeitung", "daten_bearbeitung.R"))
set.seed(476)

library(corrplot)
# Korrelation
# 1. Die neuen Prädiktoren aus deinem GAM-Modell definieren
variablen_neu <- c(
  "erreichbarkeit_gr10ha_in_metern_adr",
  "erreichbarkeit_innenstadt_in_minuten_adr",
  "erreichbarkeit_naechstehaltestelle_in_minuten_adr",
  "erreichbarkeit_u10ha_in_metern_adr",
  "flaeche_qm_sv",
  "wohnflaeche_je_ew_adr",
  "anteil_vf_sv",
  "anteil_gf_sv",
  "anteil_ve_sv",
  "anteil_beb_sv"
)

# 2. Nur diese numerischen Prädiktoren aus dem aktuellen Datensatz auswählen
# (Hinweis: Ich nutze hier den Datensatznamen aus deiner letzten Fehlermeldung)
df_korr_neu <- model_data_complete_zentral %>%
  select(all_of(variablen_neu)) %>% 
  st_drop_geometry() # Geometrien (sf-Objekt) droppen, falls vorhanden

# 3. Korrelationsmatrix berechnen
korr_matrix_neu <- cor(df_korr_neu, use = "complete.obs")

# 4. Heatmap-Plot erstellen
# Lade ggf. noch library(corrplot), falls nicht schon geschehen
corrplot(korr_matrix_neu, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)


formula_list1 <- list(wohnlage_ebene ~ 
                        #zentraler_bereich +
                                s(erreichbarkeit_gr10ha_in_metern_adr, k=10) +
                                s(erreichbarkeit_innenstadt_in_minuten_adr, k=10) +
                                s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=10) +
                             #   s(erreichbarkeit_u10ha_in_metern_adr, k=10) +
                                s(flaeche_qm_sv, k=10) +
                                s(wohnflaeche_je_ew_adr, k=10) +
                                s(anteil_vf_sv, k=10) +
                                s(anteil_gf_sv, k=10) +
                                s(anteil_ve_sv, k=10) #+
                               # s(anteil_beb_sv , k=10)
                      , 
                      ~ #zentraler_bereich +
                              s(erreichbarkeit_gr10ha_in_metern_adr, k=10) +
                                s(erreichbarkeit_innenstadt_in_minuten_adr, k=10) +
                                s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=10) +
                             #   s(erreichbarkeit_u10ha_in_metern_adr, k=10) +
                                s(flaeche_qm_sv, k=10) +
                                s(wohnflaeche_je_ew_adr, k=10) +
                                s(anteil_vf_sv, k=10) +
                                s(anteil_gf_sv, k=10) +
                                s(anteil_ve_sv, k=10)# +
                               # s(anteil_beb_sv , k=10)
                      )


gam_model_zentral <- gam(
  formula = formula_list1,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
  select = TRUE
)
