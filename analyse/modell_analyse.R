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

model_linear_zentral <- readRDS("modelle/linear_model_zentral.rds")

model_linear_ausserhalb_b <- readRDS("modelle/linear_model_ausserhalb_b.rds")

model_gam_zentral_brw <- readRDS("modelle/gam_model_zentral_ohne_brw.rds")

model_gam_ausserhalb_brw <- readRDS("modelle/gam_model_ausserhalb_ohne_brw.rds")

model_gam_zentral_brw_less <- readRDS("modelle/gam_model_zentral_ohne_brw_less.rds")

model_gam_ausserhalb_brw_less <- readRDS("modelle/gam_model_ausserhalb_ohne_brw_less.rds")

model_gam_zentral_less <- readRDS("modelle/gam_model_zentral_less.rds")

model_gam_ausserhalb_less <- readRDS("modelle/gam_model_ausserhalb_less.rds")

model_rf_ausserhalb <- readRDS("modelle/rf_model_ausserhalb_spatial.rds")

model_rf_zentral <- readRDS("modelle/rf_model_zentral_spatial.rds")

model_gam_zentral_spatial <- readRDS("modelle/gam_model_zentral_spatial.rds")

model_gam_ausserhalb_spatial <- readRDS("modelle/gam_model_ausserhalb_spatial.rds")

# Modelloutput ansehen
summary(model_gam_zentral)
summary(model_gam_ausserhalb_b)
summary(model_linear_zentral)
summary(model_linear_ausserhalb_b)

# Modellgüte evaluieren
evaluate_confusion_matrix(model_gam_zentral, 
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") # 82% accuracy

                      
evaluate_confusion_matrix(model_gam_ausserhalb_b, 
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 73% accuracy

evaluate_confusion_matrix(model_linear_zentral, 
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") #73% accuracy

evaluate_confusion_matrix(model_linear_ausserhalb_b, 
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 59% accuracy

evaluate_confusion_matrix(model_gam_ausserhalb_brw,
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 63% accuracy

evaluate_confusion_matrix(model_gam_zentral_brw,
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") # 78% accuracy

evaluate_confusion_matrix(model_gam_ausserhalb_brw_less,
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 60% accuracy

evaluate_confusion_matrix(model_gam_zentral_brw_less,
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") # 74% accuracy

evaluate_confusion_matrix(model_gam_ausserhalb_less,
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 66% accuracy

evaluate_confusion_matrix(model_gam_zentral_less,
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") # 77% accuracy

evaluate_confusion_matrix(model_gam_ausserhalb_spatial,
                          test_data = model_data_complete_ausserhalb,
                          y_col = "wohnlage_ebene") # 78% accuracy

evaluate_confusion_matrix(model_gam_zentral_spatial,
                          test_data = model_data_complete_zentral,
                          y_col = "wohnlage_ebene") # 89% accuracy

# Random Forest Modelle (accuracy extrem hoch!)
pred_rf_zentral <- model_rf_zentral$predict_newdata(model_data_complete_zentral)

print(pred_rf_zentral$confusion)
acc <- pred_rf_zentral$score(msr("classif.acc"))
cat("Accuracy:", acc, "\n")

pred_rf_ausserhalb <- model_rf_ausserhalb$predict_newdata(model_data_complete_ausserhalb)

print(pred_rf_zentral$confusion)
acc <- pred_rf_zentral$score(msr("classif.acc"))
cat("Accuracy:", acc, "\n")


# random Forest mit spatial korrektur
# Nutzt direkt den vorbereiteten Task von vorhin (inkl. x und y)
pred_rf_zentral <- model_rf_zentral$predict(task_zentral)

# Zeigt die Confusion Matrix
print(pred_rf_zentral$confusion)

# Zeigt die Accuracy
cat("Accuracy:", pred_rf_zentral$score(msr("classif.acc")), "\n")



# Importance für das zentrale Modell ziehen
importance_df <- as.data.frame(model_rf_zentral$importance())
colnames(importance_df) <- "Wichtigkeit"
importance_df$Feature <- rownames(importance_df)

# Plotten
ggplot(importance_df, aes(x = reorder(Feature, Wichtigkeit), y = Wichtigkeit)) +
  geom_col(fill = "#2c7bb6", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Was treibt den Random Forest an? (Zentral)",
    subtitle = "Permutation Feature Importance",
    x = "Variable",
    y = "Wichtigkeit (Abfall der Accuracy ohne dieses Feature)"
  )


# Importance für das Modell ausserhalb ziehen
importance_df <- as.data.frame(model_rf_ausserhalb$importance())
colnames(importance_df) <- "Wichtigkeit"
importance_df$Feature <- rownames(importance_df)

# Plotten
ggplot(importance_df, aes(x = reorder(Feature, Wichtigkeit), y = Wichtigkeit)) +
  geom_col(fill = "#2c7bb6", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Was treibt den Random Forest an? (Ausserhalb)",
    subtitle = "Permutation Feature Importance",
    x = "Variable",
    y = "Wichtigkeit (Abfall der Accuracy ohne dieses Feature)"
  )

# Modellgüte evaluieren mit bereinigten Vorhersagen
evaluate_confusion_matrix_equal_priors(model_gam_zentral, 
                                       test_data = model_data_complete_zentral,
                                       y_col = "wohnlage_ebene")

evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb_b, 
                                       test_data = model_data_complete_ausserhalb,
                                       y_col = "wohnlage_ebene")

# Partielle Effekte (ALT):
# Wie verändert sich Vorhersage bei Änderung eines Prädiktors,
# wenn alle anderen konstant gehalten werden?
# y-Achse: log odds gegenüber referenzkategorie (durchschnittliche Lage)
visualize_part_effects(model_gam_zentral, "part_eff", subfolder_name = "part_effects_zent")
visualize_part_effects(model_gam_ausserhalb_b, "part_eff", subfolder_name = "part_effects_aus_b")


# Effekte linear
visualize_linear_effects_sicher(
  model = model_linear_ausserhalb_b, 
  file_name_prefix = "part_eff_lin", 
  subfolder_name = "part_effs_lin_aus",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_linear_effects_sicher(
  model = model_linear_zentral, 
  file_name_prefix = "part_eff_lin", 
  subfolder_name = "part_effs_lin_zentral",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

visualize_logodds_effects(
  model = model_linear_ausserhalb_b, 
  file_name_prefix = "logodds_lin", 
  subfolder_name = "logodds_lin_aus",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_logodds_effects(
  model = model_linear_zentral, 
  file_name_prefix = "logodds_lin", 
  subfolder_name = "logodds_lin_zentral",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

# NEU: Nicht lineare Effekte schöner visualisiert
visualize_gam_probabilities(
  model = model_gam_zentral, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_probs",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

visualize_gam_probabilities(
  model = model_gam_ausserhalb_b, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_aus_probs",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_ausserhalb_b, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_aus_logodds",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_zentral, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_logodds",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)



# GAM ohne Bodenrichtwert:
visualize_gam_probabilities(
  model = model_gam_zentral_brw, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_brw_probs",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

visualize_gam_probabilities(
  model = model_gam_ausserhalb_brw, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_probs",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_ausserhalb_brw, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_logodds",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_zentral_brw, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_brw_logodds",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)


# GAM ohne Bodenrichtwert und weniger Knoten:
visualize_gam_probabilities(
  model = model_gam_zentral_brw_less, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_brw_less_probs",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

visualize_gam_probabilities(
  model = model_gam_ausserhalb_brw_less, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_less_probs",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_ausserhalb_brw_less, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_less_logodds",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_zentral_brw_less, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_brw_less_logodds",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)

# GAM mit weniger Knoten:
visualize_gam_logodds(
  model = model_gam_ausserhalb_spatial, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_spatial_probs",
  klassen_labels = c("durchschnittliche Lage", 
                     "gute Lage", "beste Lage")
)

visualize_gam_probabilities(
  model = model_gam_ausserhalb_less, 
  file_name_prefix = "prob_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_less_probs",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_ausserhalb_less, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_aus_brw_less_logodds",
  klassen_labels = c("durchschnittliche Lage", "gute Lage", "beste Lage")
)

visualize_gam_logodds(
  model = model_gam_zentral_less, 
  file_name_prefix = "logodds_gam", 
  subfolder_name = "neu_part_effs_gam_zentral_brw_less_logodds",
  klassen_labels = c("zentrale durchschnittliche Lage", 
                     "zentrale gute Lage", "zentrale beste Lage")
)


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







# Vergleiche accuracies
library(ggplot2)
library(dplyr)
library(scales)


berechne_accuracy_explizit <- function(modell, daten, ziel_var = "wohnlage_ebene") {
  
  # 1. Echte Vorhersagen auf dem übergebenen Datensatz machen
  preds <- predict(modell, newdata = daten, type = "response")
  
  # 2. Wahre Labels und Klassen-Level extrahieren
  wahre_werte <- as.character(daten[[ziel_var]])
  klassen_level <- levels(as.factor(daten[[ziel_var]]))
  
  # 3. Die Klasse mit der höchsten vorhergesagten Wahrscheinlichkeit ermitteln
  # max.col findet für jede Zeile die Spalte (Klasse) mit dem höchsten Wert
  pred_indices <- max.col(preds, ties.method = "first")
  vorhergesagte_werte <- klassen_level[pred_indices]
  
  # 4. Accuracy: (Anzahl Treffer) / (Anzahl aller Beobachtungen)
  accuracy <- sum(wahre_werte == vorhergesagte_werte) / length(wahre_werte)
  
  return(accuracy)
}


acc_zentral <- data.frame(
  Modell_Name = c(
    "GAM (Vollständig)", 
    "Linear (Vollständig)", 
    "GAM (ohne BRW)", 
    "GAM (reduziert)", 
    "GAM (ohne BRW & reduziert)"
  ),
  Accuracy = c(
    berechne_accuracy_explizit(model_gam_zentral, model_data_complete_zentral),
    berechne_accuracy_explizit(model_linear_zentral, model_data_complete_zentral),
    berechne_accuracy_explizit(model_gam_zentral_brw, model_data_complete_zentral),
    berechne_accuracy_explizit(model_gam_zentral_less, model_data_complete_zentral),
    berechne_accuracy_explizit(model_gam_zentral_brw_less, model_data_complete_zentral)
  ),
  Typ = c("GAM-Hauptmodell", "Lineares Modell", "GAM-Variante", "GAM-Variante", "GAM-Variante")
)


acc_ausserhalb <- data.frame(
  Modell_Name = c(
    "GAM (Vollständig)", 
    "Linear (Vollständig)", 
    "GAM (ohne BRW)", 
    "GAM (reduziert)", 
    "GAM (ohne BRW & reduziert)"
  ),
  Accuracy = c(
    berechne_accuracy_explizit(model_gam_ausserhalb_b, model_data_complete_ausserhalb),
    berechne_accuracy_explizit(model_linear_ausserhalb_b, model_data_complete_ausserhalb),
    berechne_accuracy_explizit(model_gam_ausserhalb_brw, model_data_complete_ausserhalb),
    berechne_accuracy_explizit(model_gam_ausserhalb_less, model_data_complete_ausserhalb),
    berechne_accuracy_explizit(model_gam_ausserhalb_brw_less, model_data_complete_ausserhalb)
  ),
  Typ = c("GAM-Hauptmodell", "Lineares Modell", "GAM-Variante", "GAM-Variante", "GAM-Variante")
)



# Farbcodierung 
farbe_typen <- c(
  "GAM-Hauptmodell" = "#2c7bb6",    # Dunkelblau für das Hauptmodell
  "Lineares Modell" = "#fdae61",    # Orange als lineare Baseline
  "GAM-Variante"    = "#abd9e9"     # Hellblau für Ablations-Modelle
)

if (!dir.exists("plots/evaluation")) dir.create("plots/evaluation", recursive = TRUE)

# --- PLOT ZENTRAL ---
plot_zentral <- ggplot(acc_zentral, aes(x = reorder(Modell_Name, Accuracy), y = Accuracy, fill = Typ)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  coord_flip() +  
  scale_fill_manual(values = farbe_typen) +
  geom_text(aes(label = percent(Accuracy, accuracy = 0.1)), hjust = -0.2, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1), expand = expansion(mult = c(0, 0.15))) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Modellvergleich: Region Zentral",
    x = NULL, 
    y = "Genauigkeit (Accuracy)",
    fill = "Modell-Typ"
  ) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )


ggsave("plots/evaluation/accuracy_vergleich_zentral.png", plot = plot_zentral, width = 9, height = 5)

# --- PLOT AUSSERHALB ---
plot_ausserhalb <- ggplot(acc_ausserhalb, aes(x = reorder(Modell_Name, Accuracy), y = Accuracy, fill = Typ)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = farbe_typen) +
  geom_text(aes(label = percent(Accuracy, accuracy = 0.1)), hjust = -0.2, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1), expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Modellvergleich: Region Ausserhalb",
    x = NULL, 
    y = "Genauigkeit (Accuracy)",
    fill = "Modell-Typ"
  ) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("plots/evaluation/accuracy_vergleich_ausserhalb.png", plot = plot_ausserhalb, width = 9, height = 5)
