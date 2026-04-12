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
