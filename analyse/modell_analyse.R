library(here)
source(here("analyse", "model_evaluation.R"))
source(here("analyse", "plot_funktionen.R"))
source(here("daten_verarbeitung", "daten_bearbeitung.R"))
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
library(rgl)
library(sf)
options(scipen = 999) # Verhindert wissenschaftliche Notation in ggplot2

# Modelle laden
model_gam_zentral <- readRDS("modelle/gam_model_zentral.rds") 

model_gam_ausserhalb_b <- readRDS("modelle/gam_model_ausserhalb_b.rds")

# Modelloutput ansehen
summary(model_gam_zentral)
summary(model_gam_ausserhalb_b)

# Modellgüte evaluieren
evaluate_confusion_matrix(model_gam_zentral, 
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene")

                      
evaluate_confusion_matrix(model_gam_ausserhalb_b, 
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene")

# Modellgüte evaluieren mit bereinigten Vorhersagen
evaluate_confusion_matrix_equal_priors(model_gam_zentral, 
                                       test_data = model_data_complete_zentral,
                                       y_col = "wohnlage_ebene")

evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb_b, 
                                       test_data = model_data_complete_ausserhalb,
                                       y_col = "wohnlage_ebene")

# Partielle Effekte:
# Wie verändert sich Vorhersage bei Änderung eines Prädiktors,
# wenn alle anderen konstant gehalten werden?
# y-Achse: log odds gegenüber referenzkategorie (durchschnittliche Lage)
visualize_part_effects(model_gam_zentral, "part_eff", subfolder_name = "part_effects_zent")
visualize_part_effects(model_gam_ausserhalb_b, "part_eff", subfolder_name = "part_effects_aus_b")










# Sind predict_labels und predict_labels_discr identisch? 

# 1. Beide Vorhersagen generieren lassen
vorhersage_normal <- predict_labels(model = model_gam_zentral, 
                                    test_data = model_data_complete_zentral)

vorhersage_korrigiert <- predict_labels_discr(model = model_gam_zentral, 
                                              test_data = model_data_complete_zentral)

# ==========================================
# 2. SCHNELLER CHECK: Sind sie identisch?
# ==========================================
sind_gleich <- identical(vorhersage_normal, vorhersage_korrigiert)
cat("Sind die Vorhersagen zu 100% identisch? ->", sind_gleich, "\n")

# Wie viele Beobachtungen unterscheiden sich?
anzahl_unterschiede <- sum(vorhersage_normal != vorhersage_korrigiert)
cat("Anzahl der unterschiedlichen Vorhersagen: ", anzahl_unterschiede, 
    " von ", length(vorhersage_normal), " Punkten.\n\n")

# ==========================================
# 3. DETAIL-CHECK: Kreuztabelle (Confusion Matrix der Methoden)
# ==========================================
cat("=== KREUZTABELLE DER VORHERSAGEN ===\n")
vergleichs_tabelle <- table(Normal = vorhersage_normal, 
                            Korrigiert = vorhersage_korrigiert)
print(vergleichs_tabelle)
