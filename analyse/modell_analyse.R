library(here)
source(here("analyse", "model_evaluation.R"))
source(here("analyse", "plot_funktionen.R"))
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
model_gam_ausserhalb <- readRDS("modelle/gam_model_ausserhalb.rds")

# Modelloutput ansehen
summary(model_gam_zentral)
summary(model_gam_ausserhalb)

# Modellgüte evaluieren
evaluate_confusion_matrix(model_gam_zentral, 
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene")
evaluate_confusion_matrix(model_gam_ausserhalb, 
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene")

# Modellgüte evaluieren mit bereinigten Vorhersagen
evaluate_confusion_matrix_equal_priors(model_gam_zentral, 
                                       test_data = model_data_complete_zentral,
                                       y_col = "wohnlage_ebene")
evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb, 
                                       test_data = model_data_complete_ausserhalb,
                                       y_col = "wohnlage_ebene")

# Partielle Effekte:
# Wie verändert sich Vorhersage bei Änderung eines Prädiktors,
# wenn alle anderen konstant gehalten werden?
# y-Achse: log odds gegenüber referenzkategorie (durchschnittliche Lage)
visualize_part_effects(model_gam_zentral, "part_eff", subfolder_name = "part_effects_zent")
visualize_part_effects(model_gam_ausserhalb, "part_eff", subfolder_name = "part_effects_aus")
