library(mgcv)
library(dplyr)
library(here)
source(here("daten_verarbeitung", "daten_bearbeitung.R"))
set.seed(476)
library(corrplot)






# Korrelation
# 1. Die neuen Prädiktoren aus GAM-Modell definieren
variablen_neu <- c(
  "erreichbarkeit_gr10ha_in_metern_adr",
  "erreichbarkeit_innenstadt_in_minuten_adr",
  "erreichbarkeit_naechstehaltestelle_in_minuten_adr",
  "brw_log",
  "anteil_vf_sv",
  "anteil_gf_sv",
  "grundschul_num",
  "spielplatz_num", 
  "ortszentru_num",
  "kitakigaho_num"
)

# Zentral
# 2. Nur diese numerischen Prädiktoren aus dem aktuellen Datensatz auswählen
df_korr_neu <- model_data_complete_zentral %>%
  select(all_of(variablen_neu)) %>% 
  st_drop_geometry() # Geometrien (sf-Objekt) droppen, falls vorhanden

# 3. Korrelationsmatrix berechnen
korr_matrix_neu <- cor(df_korr_neu, use = "complete.obs")

# 4. Heatmap-Plot erstellen
corrplot(korr_matrix_neu, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)

# Außerhalb
# 2.
df_korr_neu <- model_data_complete_ausserhalb %>%
  select(all_of(variablen_neu)) %>% 
  st_drop_geometry() # Geometrien (sf-Objekt) droppen, falls vorhanden

# 3. Korrelationsmatrix berechnen
korr_matrix_neu <- cor(df_korr_neu, use = "complete.obs")

# 4. Heatmap-Plot erstellen
corrplot(korr_matrix_neu, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)



# Gams

formula_list1b <- list(wohnlage_ebene ~ 
                        #zentraler_bereich +
                        s(erreichbarkeit_gr10ha_in_metern_adr, k=6, bs = "cr") +
                        s(erreichbarkeit_innenstadt_in_minuten_adr, k=6, bs = "cr") +
                        s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=6, bs = "cr") +
                       # s(wohnbere_1_log, k=6, bs = "cr") +
                        #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                       # s(flaeche_qm_sv, k=6, bs = "cr") +
                       # s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                         s(brw_log, k=6, bs = "cr")+
                        s(grundschul_num, k=6, bs = "cr") +
                        s(kitakigaho_num, k=6, bs = "cr") +
                        s(ortszentru_num, k=6, bs = "cr") +
                        s(spielplatz_num, k=6, bs = "cr") +
                        s(anteil_vf_sv, k=6, bs = "cr") +
                        s(anteil_gf_sv, k=6, bs = "cr") #+
                      #  s(anteil_ve_sv, k=6, bs = "cr") #+
                      # s(anteil_beb_sv , k=6)
                      , 
                      ~ #zentraler_bereich +
                        s(erreichbarkeit_gr10ha_in_metern_adr, k=6, bs = "cr") +
                        s(erreichbarkeit_innenstadt_in_minuten_adr, k=6, bs = "cr") +
                        s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=6, bs = "cr") +
                       # s(wohnbere_1_log, k=6, bs = "cr") +
                        #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                        #s(flaeche_qm_sv, k=6, bs = "cr") +
                        #s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                        s(brw_log, k=6, bs = "cr")+
                        s(grundschul_num, k=6, bs = "cr") +
                        s(kitakigaho_num, k=6, bs = "cr") +
                        s(ortszentru_num, k=6, bs = "cr") +
                        s(spielplatz_num, k=6, bs = "cr") +
                        s(anteil_vf_sv, k=6, bs = "cr") +
                        s(anteil_gf_sv, k=6, bs = "cr")# +
                      #  s(anteil_ve_sv, k=6, bs = "cr")# +
                      # s(anteil_beb_sv , k=6)
)



gam_model_ausserhalb_b <- gam(
  formula = formula_list1b,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_ausserhalb_b, file = "modelle/gam_model_ausserhalb_b.rds")


formula_list2 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=5, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=5, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=5, bs="cr") +
 #   s(wohnbere_1_log, k=5, bs = "cr") +
  #  s(flaeche_qm_sv, k=5, bs="cr") +
  #  s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
   s(brw_log, k=5, bs = "cr")+
    s(grundschul_num, k=5, bs = "cr") +
    s(kitakigaho_num, k=5, bs = "cr") +
    s(ortszentru_num, k=5, bs = "cr") +
    s(spielplatz_num, k=5, bs = "cr") +
    s(anteil_vf_sv, k=5, bs="cr") +
    s(anteil_gf_sv, k=5, bs="cr"),
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=5, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=5, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=5, bs="cr") +
  #  s(wohnbere_1_log, k=5, bs = "cr") +
  #  s(flaeche_qm_sv, k=5, bs="cr") +
   # s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
   s(brw_log, k=5, bs = "cr")+
    s(grundschul_num, k=5, bs = "cr") +
    s(kitakigaho_num, k=5, bs = "cr") +
    s(ortszentru_num, k=5, bs = "cr") +
    s(spielplatz_num, k=5, bs = "cr") +
    s(anteil_vf_sv, k=5, bs="cr") +
    s(anteil_gf_sv, k=5, bs="cr")
)


gam_model_zentral <- gam(
  formula = formula_list2,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_zentral, file = "modelle/gam_model_zentral.rds")





#-------------------------------------------------------------------------------

# Gams ohne Bodenrichtwert
formula_list_brw1 <- list(wohnlage_ebene ~ 
                         #zentraler_bereich +
                         s(erreichbarkeit_gr10ha_in_metern_adr, k=6, bs = "cr") +
                         s(erreichbarkeit_innenstadt_in_minuten_adr, k=6, bs = "cr") +
                         s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=6, bs = "cr") +
                         # s(wohnbere_1_log, k=6, bs = "cr") +
                         #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                         # s(flaeche_qm_sv, k=6, bs = "cr") +
                         # s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                         s(grundschul_num, k=6, bs = "cr") +
                         s(kitakigaho_num, k=6, bs = "cr") +
                         s(ortszentru_num, k=6, bs = "cr") +
                         s(spielplatz_num, k=6, bs = "cr") +
                         s(anteil_vf_sv, k=6, bs = "cr") +
                         s(anteil_gf_sv, k=6, bs = "cr") #+
                       #  s(anteil_ve_sv, k=6, bs = "cr") #+
                       # s(anteil_beb_sv , k=6)
                       , 
                       ~ #zentraler_bereich +
                         s(erreichbarkeit_gr10ha_in_metern_adr, k=6, bs = "cr") +
                         s(erreichbarkeit_innenstadt_in_minuten_adr, k=6, bs = "cr") +
                         s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=6, bs = "cr") +
                         # s(wohnbere_1_log, k=6, bs = "cr") +
                         #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                         #s(flaeche_qm_sv, k=6, bs = "cr") +
                         #s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                         s(grundschul_num, k=6, bs = "cr") +
                         s(kitakigaho_num, k=6, bs = "cr") +
                         s(ortszentru_num, k=6, bs = "cr") +
                         s(spielplatz_num, k=6, bs = "cr") +
                         s(anteil_vf_sv, k=6, bs = "cr") +
                         s(anteil_gf_sv, k=6, bs = "cr")# +
                       #  s(anteil_ve_sv, k=6, bs = "cr")# +
                       # s(anteil_beb_sv , k=6)
)



gam_model_ausserhalb_brw <- gam(
  formula = formula_list_brw1,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_ausserhalb_brw, file = "modelle/gam_model_ausserhalb_ohne_brw.rds")


formula_list_brw2 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=5, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=5, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=5, bs="cr") +
    #   s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    #  s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=5, bs = "cr") +
    s(kitakigaho_num, k=5, bs = "cr") +
    s(ortszentru_num, k=5, bs = "cr") +
    s(spielplatz_num, k=5, bs = "cr") +
    s(anteil_vf_sv, k=5, bs="cr") +
    s(anteil_gf_sv, k=5, bs="cr"),
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=5, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=5, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=5, bs="cr") +
    #  s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    # s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=5, bs = "cr") +
    s(kitakigaho_num, k=5, bs = "cr") +
    s(ortszentru_num, k=5, bs = "cr") +
    s(spielplatz_num, k=5, bs = "cr") +
    s(anteil_vf_sv, k=5, bs="cr") +
    s(anteil_gf_sv, k=5, bs="cr")
)


gam_model_zentral_brw <- gam(
  formula = formula_list_brw2,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_zentral_brw, file = "modelle/gam_model_zentral_ohne_brw.rds")

#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------

# Gams ohne Bodenrichtwert & mit weniger Flexibilität
formula_list_brw3 <- list(wohnlage_ebene ~ 
                            #zentraler_bereich +
                            s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
                            # s(wohnbere_1_log, k=6, bs = "cr") +
                            #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                            # s(flaeche_qm_sv, k=6, bs = "cr") +
                            # s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                            s(grundschul_num, k=3, bs = "cr") +
                            s(kitakigaho_num, k=3, bs = "cr") +
                            s(ortszentru_num, k=3, bs = "cr") +
                            s(spielplatz_num, k=3, bs = "cr") +
                            s(anteil_vf_sv, k=3, bs = "cr") +
                            s(anteil_gf_sv, k=3, bs = "cr") #+
                          #  s(anteil_ve_sv, k=6, bs = "cr") #+
                          # s(anteil_beb_sv , k=6)
                          , 
                          ~ #zentraler_bereich +
                            s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
                            # s(wohnbere_1_log, k=6, bs = "cr") +
                            #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                            #s(flaeche_qm_sv, k=6, bs = "cr") +
                            #s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                            s(grundschul_num, k=3, bs = "cr") +
                            s(kitakigaho_num, k=3, bs = "cr") +
                            s(ortszentru_num, k=3, bs = "cr") +
                            s(spielplatz_num, k=3, bs = "cr") +
                            s(anteil_vf_sv, k=3, bs = "cr") +
                            s(anteil_gf_sv, k=3, bs = "cr")# +
                          #  s(anteil_ve_sv, k=6, bs = "cr")# +
                          # s(anteil_beb_sv , k=6)
)



gam_model_ausserhalb_brw_less <- gam(
  formula = formula_list_brw3,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_ausserhalb_brw_less, file = "modelle/gam_model_ausserhalb_ohne_brw_less.rds")


formula_list_brw4 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    #   s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    #  s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr"),
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    #  s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    # s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr")
)


gam_model_zentral_brw_less <- gam(
  formula = formula_list_brw4,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_zentral_brw_less, file = "modelle/gam_model_zentral_ohne_brw_less.rds")

#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------

# Gams mit weniger Flexibilität
formula_list_less1 <- list(wohnlage_ebene ~ 
                            #zentraler_bereich +
                            s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
                            # s(wohnbere_1_log, k=6, bs = "cr") +
                            #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                            # s(flaeche_qm_sv, k=6, bs = "cr") +
                            # s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                            s(grundschul_num, k=3, bs = "cr") +
                            s(kitakigaho_num, k=3, bs = "cr") +
                            s(ortszentru_num, k=3, bs = "cr") +
                            s(spielplatz_num, k=3, bs = "cr") +
                            s(brw_log, k=3, bs = "cr")+
                            s(anteil_vf_sv, k=3, bs = "cr") +
                            s(anteil_gf_sv, k=3, bs = "cr") #+
                          #  s(anteil_ve_sv, k=6, bs = "cr") #+
                          # s(anteil_beb_sv , k=6)
                          , 
                          ~ #zentraler_bereich +
                            s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
                            s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
                            # s(wohnbere_1_log, k=6, bs = "cr") +
                            #   s(erreichbarkeit_u10ha_in_metern_adr, k=6) +
                            #s(flaeche_qm_sv, k=6, bs = "cr") +
                            #s(wohnflaeche_je_ew_adr_log, k=6, bs = "cr") +
                            s(grundschul_num, k=3, bs = "cr") +
                            s(kitakigaho_num, k=3, bs = "cr") +
                            s(ortszentru_num, k=3, bs = "cr") +
                            s(spielplatz_num, k=3, bs = "cr") +
                            s(brw_log, k=3, bs = "cr")+
                            s(anteil_vf_sv, k=3, bs = "cr") +
                            s(anteil_gf_sv, k=3, bs = "cr")# +
                          #  s(anteil_ve_sv, k=6, bs = "cr")# +
                          # s(anteil_beb_sv , k=6)
)



gam_model_ausserhalb_less <- gam(
  formula = formula_list_less1,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_ausserhalb_less, file = "modelle/gam_model_ausserhalb_less.rds")


formula_list_less2 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    #   s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    #  s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr")+
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr"),
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    #  s(wohnbere_1_log, k=5, bs = "cr") +
    #  s(flaeche_qm_sv, k=5, bs="cr") +
    # s(wohnflaeche_je_ew_adr_log, k=5, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr")+
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr")
)


gam_model_zentral_less <- gam(
  formula = formula_list_less2,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE), # reduziert Größe 
)

# Modell speichern
saveRDS(gam_model_zentral_less, file = "modelle/gam_model_zentral_less.rds")

#-------------------------------------------------------------------------------



# Lineare Modelle
# Lineare Formel für das Modell 'außerhalb'
formula_list1b_linear <- list(
  wohnlage_ebene ~ 
    erreichbarkeit_gr10ha_in_metern_adr +
    erreichbarkeit_innenstadt_in_minuten_adr +
    erreichbarkeit_naechstehaltestelle_in_minuten_adr +
    brw_log +
    grundschul_num +
    kitakigaho_num +
    ortszentru_num +
    spielplatz_num +
    anteil_vf_sv +
    anteil_gf_sv, 
  ~ 
    erreichbarkeit_gr10ha_in_metern_adr +
    erreichbarkeit_innenstadt_in_minuten_adr +
    erreichbarkeit_naechstehaltestelle_in_minuten_adr +
    brw_log +
    grundschul_num +
    kitakigaho_num +
    ortszentru_num +
    spielplatz_num +
    anteil_vf_sv +
    anteil_gf_sv
)

# Lineares Modell berechnen
linear_model_ausserhalb_b <- gam(
  formula = formula_list1b_linear,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), # für 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE)
)

# Modell speichern
saveRDS(linear_model_ausserhalb_b, file = "modelle/linear_model_ausserhalb_b.rds")


# Lineare Formel für das Modell 'zentral'
formula_list2_linear <- list(
  wohnlage_ebene ~ 
    erreichbarkeit_gr10ha_in_metern_adr +
    erreichbarkeit_innenstadt_in_minuten_adr +
    erreichbarkeit_naechstehaltestelle_in_minuten_adr +
    brw_log +
    grundschul_num +
    kitakigaho_num +
    ortszentru_num +
    spielplatz_num +
    anteil_vf_sv +
    anteil_gf_sv,
  ~ 
    erreichbarkeit_gr10ha_in_metern_adr +
    erreichbarkeit_innenstadt_in_minuten_adr +
    erreichbarkeit_naechstehaltestelle_in_minuten_adr +
    brw_log +
    grundschul_num +
    kitakigaho_num +
    ortszentru_num +
    spielplatz_num +
    anteil_vf_sv +
    anteil_gf_sv
)

# Lineares Modell berechnen
linear_model_zentral <- gam(
  formula = formula_list2_linear,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), # für 3 Kategorien
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE)
)

# Modell speichern
saveRDS(linear_model_zentral, file = "modelle/linear_model_zentral.rds")










# GAM mit spatial komponente mit brw und wneiger flexibiltät
# 1. Koordinaten für den Datensatz 'Ausserhalb' entpacken
model_data_complete_ausserhalb <- model_data_complete_ausserhalb %>%
  mutate(
    long = st_coordinates(.)[, 1], # Die X-Koordinate aus 'geom' ziehen
    lat  = st_coordinates(.)[, 2]  # Die Y-Koordinate aus 'geom' ziehen
  )

# 2. Koordinaten für den Datensatz 'Zentral' entpacken
model_data_complete_zentral <- model_data_complete_zentral %>%
  mutate(
    long = st_coordinates(.)[, 1],
    lat  = st_coordinates(.)[, 2]
  )


# --- 1. MODELL: AUSSERHALB ---
formula_list_less1 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs = "cr") +
    s(anteil_gf_sv, k=3, bs = "cr") +
    # NEU: 2D-Spline für die räumlichen Koordinaten (geringes k)
    s(long, lat, k=10, bs="tp"), 
  
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs = "cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs = "cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs = "cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs = "cr") +
    s(anteil_gf_sv, k=3, bs = "cr") +
    # NEU: 2D-Spline für die räumlichen Koordinaten (geringes k)
    s(long, lat, k=10, bs="tp")
)

gam_model_ausserhalb_spatial <- gam(
  formula = formula_list_less1,
  data = model_data_complete_ausserhalb,
  family = mgcv::multinom(K = 2), 
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) 
)

# Modell speichern
saveRDS(gam_model_ausserhalb_spatial, file = "modelle/gam_model_ausserhalb_spatial.rds")


# --- 2. MODELL: ZENTRAL ---
formula_list_less2 <- list(
  wohnlage_ebene ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr") +
    # NEU: 2D-Spline für die räumlichen Koordinaten
    s(long, lat, k=10, bs="tp"),
  
  ~ 
    s(erreichbarkeit_gr10ha_in_metern_adr, k=3, bs="cr") +
    s(erreichbarkeit_innenstadt_in_minuten_adr, k=3, bs="cr") +
    s(erreichbarkeit_naechstehaltestelle_in_minuten_adr, k=3, bs="cr") +
    s(grundschul_num, k=3, bs = "cr") +
    s(kitakigaho_num, k=3, bs = "cr") +
    s(ortszentru_num, k=3, bs = "cr") +
    s(spielplatz_num, k=3, bs = "cr") +
    s(brw_log, k=3, bs = "cr") +
    s(anteil_vf_sv, k=3, bs="cr") +
    s(anteil_gf_sv, k=3, bs="cr") +
    # NEU: 2D-Spline für die räumlichen Koordinaten
    s(long, lat, k=10, bs="tp")
)

gam_model_zentral_spatial <- gam(
  formula = formula_list_less2,
  data = model_data_complete_zentral,
  family = mgcv::multinom(K = 2), 
  method = "REML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) 
)

# Modell speichern
saveRDS(gam_model_zentral_spatial, file = "modelle/gam_model_zentral_spatial.rds")








# Random Forest Modell
# ==============================================================================
# RANDOM FOREST MODELLE (mlr3 + ranger)
# ==============================================================================

library(sf)
library(dplyr)
library(mlr3)
library(mlr3learners) 
library(ranger)       



# 1. Feature-Auswahl
features <- c(
  "erreichbarkeit_gr10ha_in_metern_adr",
  "erreichbarkeit_innenstadt_in_minuten_adr",
  "erreichbarkeit_naechstehaltestelle_in_minuten_adr",
 # "brw_log",
  "grundschul_num",
  "kitakigaho_num",
  "ortszentru_num",
  "spielplatz_num",
  "anteil_vf_sv",
  "anteil_gf_sv"
)

# 2. Datenvorbereitung
# WICHTIG: mlr3 (und ranger) können mit räumlichen Geometrien (sf-Objekten) 
# nichts anfangen. Wir müssen die Daten auf ein reines Dataframe reduzieren.


rf_data_ausserhalb <- model_data_complete_ausserhalb %>%
  st_drop_geometry() %>%
  select(wohnlage_ebene, all_of(features)) %>%
  # mlr3 verlangt, dass Klassifikations-Ziele Faktoren sind
  mutate(wohnlage_ebene = as.factor(wohnlage_ebene)) 

rf_data_zentral <- model_data_complete_zentral %>%
  st_drop_geometry() %>%
  select(wohnlage_ebene, all_of(features)) %>%
  mutate(wohnlage_ebene = as.factor(wohnlage_ebene))

# 3. Tasks in mlr3 erstellen (Die "Aufgabenbeschreibung" für den Algorithmus)
task_ausserhalb <- as_task_classif(rf_data_ausserhalb, target = "wohnlage_ebene", id = "rf_ausserhalb")
task_zentral <- as_task_classif(rf_data_zentral, target = "wohnlage_ebene", id = "rf_zentral")

# 4. Den Learner (Modell-Typ) definieren
# predict_type = "prob" ist essenziell, damit wir später Wahrscheinlichkeiten (0-100%) bekommen!
# importance = "permutation" berechnet später, wie wichtig jede Variable war.
learner_rf <- lrn("classif.ranger", 
                  predict_type = "prob", 
                  num.trees = 500,           # 500 Bäume sind der Standard
                  importance = "permutation") 

# ==============================================================================
# 5. MODELL 'AUSSERHALB' TRAINIEREN & SPEICHERN
# ==============================================================================


# Klonen des Learners stellt sicher, dass er komplett "leer" ins Training geht
model_rf_ausserhalb <- learner_rf$clone()$train(task_ausserhalb)

# Speichern
if (!dir.exists("modelle")) dir.create("modelle")
saveRDS(model_rf_ausserhalb, file = "modelle/rf_model_ausserhalb.rds")
cat("Random Forest 'Ausserhalb' gespeichert unter: modelle/rf_model_ausserhalb.rds\n")

# ==============================================================================
# 6. MODELL 'ZENTRAL' TRAINIEREN & SPEICHERN
# ==============================================================================

model_rf_zentral <- learner_rf$clone()$train(task_zentral)

# Speichern
saveRDS(model_rf_zentral, file = "modelle/rf_model_zentral.rds")
cat("Random Forest 'Zentral' gespeichert unter: modelle/rf_model_zentral.rds\n")





# ==============================================================================
# RANDOM FOREST MODELLE MIT SPATIAL CROSS-VALIDATION
# ==============================================================================

library(sf)
library(dplyr)
library(mlr3)
library(mlr3learners) 
library(ranger)
library(mlr3spatiotempcv) # Das Wundermittel für räumliche Splits!

cat("Starte räumliches Random Forest Training...\n")

# 1. Feature-Auswahl (OHNE den BRW, um den Zirkelschluss zu vermeiden!)
features <- c(
  "erreichbarkeit_gr10ha_in_metern_adr",
  "erreichbarkeit_innenstadt_in_minuten_adr",
  "erreichbarkeit_naechstehaltestelle_in_minuten_adr",
  # "brw_log", 
  "grundschul_num",
  "kitakigaho_num",
  "ortszentru_num",
  "spielplatz_num",
  "anteil_vf_sv",
  "anteil_gf_sv"
)

# ==============================================================================
# 2. DATENVORBEREITUNG (Koordinaten retten!)
# Wir müssen die X/Y-Koordinaten extrahieren, bevor wir die Geometrie löschen.
# ==============================================================================

# Für Modell 'Zentral'
coords_zentral <- st_coordinates(model_data_complete_zentral)
rf_data_zentral <- model_data_complete_zentral %>%
  st_drop_geometry() %>%
  select(wohnlage_ebene, all_of(features)) %>%
  mutate(
    wohnlage_ebene = as.factor(wohnlage_ebene),
    x = coords_zentral[, 1], # X-Koordinate hinzufügen
    y = coords_zentral[, 2]  # Y-Koordinate hinzufügen
  )

# Für Modell 'Ausserhalb'
coords_ausserhalb <- st_coordinates(model_data_complete_ausserhalb)
rf_data_ausserhalb <- model_data_complete_ausserhalb %>%
  st_drop_geometry() %>%
  select(wohnlage_ebene, all_of(features)) %>%
  mutate(
    wohnlage_ebene = as.factor(wohnlage_ebene),
    x = coords_ausserhalb[, 1],
    y = coords_ausserhalb[, 2]
  )

# ==============================================================================
# 3. RÄUMLICHE TASKS ERSTELLEN
# as_task_classif_st sagt mlr3, dass es sich um Geodaten handelt!
# ==============================================================================

task_zentral <- as_task_classif_st(
  rf_data_zentral, 
  target = "wohnlage_ebene", 
  id = "rf_zentral",
  coordinate_names = c("x", "y") # Hier sagen wir mlr3, wo die Koordinaten liegen
)

task_ausserhalb <- as_task_classif_st(
  rf_data_ausserhalb, 
  target = "wohnlage_ebene", 
  id = "rf_ausserhalb",
  coordinate_names = c("x", "y")
)

# Learner definieren (Ich habe den gestutzten Wald beibehalten, das schützt doppelt!)
learner_rf <- lrn("classif.ranger", 
                  predict_type = "prob", 
                  num.trees = 500,
                  max.depth = 10,       # Verhindert Überanpassung
                  min.node.size = 20,   # Mindestens 20 Häuser pro Blatt
                  importance = "permutation") 


# ==============================================================================
# 4. DIE RÄUMLICHE EVALUIERUNG (So testest du die echte Accuracy)
# ==============================================================================
cat("\nFühre räumliche Evaluierung durch (Das dauert kurz)...\n")

# Wir definieren eine räumliche Kreuzvalidierung mit 5 Folds (Stadt in 5 Blöcke geteilt)
spatial_cv <- rsmp("spcv_coords", folds = 5)

# Führe die Validierung für das zentrale Modell aus
rr_zentral <- resample(task_zentral, learner_rf, spatial_cv, store_models = TRUE)
acc_spatial_zentral <- rr_zentral$aggregate(msr("classif.acc"))

cat("=> Echte RÄUMLICHE Accuracy Zentral:", round(acc_spatial_zentral * 100, 2), "%\n")

# Führe die Validierung für das äußere Modell aus
rr_ausserhalb <- resample(task_ausserhalb, learner_rf, spatial_cv, store_models = TRUE)
acc_spatial_ausserhalb <- rr_ausserhalb$aggregate(msr("classif.acc"))

cat("=> Echte RÄUMLICHE Accuracy Ausserhalb:", round(acc_spatial_ausserhalb * 100, 2), "%\n\n")


# ==============================================================================
# 5. FINALE MODELLE TRAINIEREN UND SPEICHERN
# (Nachdem wir wissen, wie gut das Modell ist, trainieren wir es auf ALLEN Daten)
# ==============================================================================

if (!dir.exists("modelle")) dir.create("modelle")

# Modell Zentral
model_rf_zentral <- learner_rf$clone()$train(task_zentral)
saveRDS(model_rf_zentral, file = "modelle/rf_model_zentral_spatial.rds")
cat("✓ Finales Modell 'Zentral' gespeichert.\n")

# Modell Ausserhalb
model_rf_ausserhalb <- learner_rf$clone()$train(task_ausserhalb)
saveRDS(model_rf_ausserhalb, file = "modelle/rf_model_ausserhalb_spatial.rds")
cat("✓ Finales Modell 'Ausserhalb' gespeichert.\n")