# Funktionen zur Evaluierung der Modellgüte


# predict_labels_discr
# Input: Modell und Daten
# Ouput: Character Vektor mit der wahrscheinlichsten Kategorie pro Beobachtung 
predict_labels_discr <- function(model,
                                 test_data,
                                 y_col = "wohnlage_ebene",
                                 number_categories = 3) {
  # Matrix mit P(y=k|x)
  probs <- predict(model, newdata = test_data, type = "response")
  
  # Sicherstellen, dass die Spalten benannt sind
  all_levels <- as.character(0:(number_categories - 1))
  colnames(probs) <- all_levels  # WICHTIG
  
  # Empirische Klassenverteilung (sicherstellen, dass alle Levels enthalten sind)
  prior_emp <- table(factor(test_data[[y_col]], levels = all_levels)) / nrow(test_data)
  prior_emp <- as.numeric(prior_emp)
  
  # Modellklassenverteilung (mean prediction pro Klasse)
  prior_model <- colMeans(probs)
  
  # Korrigierte Wahrscheinlichkeiten
  adjusted <- sweep(probs, 2, prior_model, "/")
  adjusted <- sweep(adjusted, 2, prior_emp, "*")
  adjusted_probs <- adjusted / rowSums(adjusted)
  
  # Noch einmal sicherstellen, dass colnames stimmen
  colnames(adjusted_probs) <- all_levels
  
  # Vorhergesagte Klassen (als Strings, z.B. "0", "1", "2")
  predicted_classes <- apply(adjusted_probs, 1, function(x) all_levels[which.max(x)])
  
  return(predicted_classes)
}

# # predict_labels_priors
# Input: Modell und Daten
# Ouput: Character Vektor mit der wahrscheinlichsten Kategorie pro Beobachtung 
# Hinweis: Korrigiert beim Vorhersagen, die Klassen Imbalance
predict_labels_equal_priors <- function(model,
                                        test_data,
                                        number_categories = 3,
                                        y_col = "wohnlage_ebene") {
  # Matrix mit P(y = k | x) vom Modell
  probs <- predict(model, newdata = test_data, type = "response")
  
  # Levels sichern
  all_levels <- as.character(0:(number_categories - 1))
  colnames(probs) <- all_levels
  
  # Modellklassenverteilung (durchschnittliche Wahrscheinlichkeit je Klasse)
  prior_model <- colMeans(probs)
  
  # Gleichverteilung als neue Prior (z.B. 1/3 für jede Klasse bei 3 Klassen)
  prior_uniform <- rep(1 / number_categories, number_categories)
  
  # Wahrscheinlichkeiten anpassen: adjusted = probs / prior_model * prior_uniform
  adjusted <- sweep(probs, 2, prior_model, "/")
  adjusted <- sweep(adjusted, 2, prior_uniform, "*")
  adjusted_probs <- adjusted / rowSums(adjusted)
  
  # Noch einmal sicherstellen, dass colnames stimmen
  colnames(adjusted_probs) <- all_levels
  
  # Vorhergesagte Klassen (als Strings)
  predicted_classes <- apply(adjusted_probs, 1, function(x) all_levels[which.max(x)])
  
  return(predicted_classes)
}



# evaluate_multinom_micro_f1
# Input: Modell und Daten
# Ouput: Der Micro-Averaged F1-Score
evaluate_multinom_micro_f1 <- function(model,
                                       test_data = test_data, y_col = "y_numeric",
                                       number_categories = 3) {
  probs <- predict(model, newdata = test_data, type = "response")
  colnames(probs) <- as.character(0:(number_categories - 1))
  predicted_classes <- apply(probs, 1, function(x) colnames(probs)[which.max(x)])
  
  y_pred <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  y_true <- factor(test_data[[y_col]], levels = 0:(number_categories - 1))
  
  eval_df <- data.frame(truth = y_true, predicted = y_pred)
  yardstick::f_meas(eval_df, truth = truth, estimate = predicted, estimator = "micro")
}

# evaluate_multinom_accuracy
# Input: Modell und Daten
# Ouput: Die Accuracy
evaluate_multinom_accuracy <- function(model, 
                                       test_data = test_data, y_col = "y_numeric",
                                       number_categories = 3) {
  probs <- predict(model, newdata = test_data, type = "response")
  colnames(probs) <- as.character(0:(number_categories - 1))
  predicted_classes <- apply(probs, 1, function(x) colnames(probs)[which.max(x)])
  
  y_pred <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  y_true <- factor(test_data[[y_col]], levels = 0:(number_categories - 1))
  
  eval_df <- data.frame(truth = y_true, predicted = y_pred)
  yardstick::accuracy(eval_df, truth = truth, estimate = predicted)
}

# evaluate_confusion_matrix
# Input: Modell und Daten
# Ouput: Konfusionsmatrix sowie viele weitere Größen
evaluate_confusion_matrix <- function(model,
                                      test_data ,
                                      y_col = "wohnlage_ebene",
                                      number_categories = 3) {
  probs <- predict(model, newdata = test_data, type = "response")
  colnames(probs) <- as.character(0:(number_categories - 1))
  predicted_classes <- apply(probs, 1, function(x) colnames(probs)[which.max(x)])
  
  y_pred <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  y_true <- factor(test_data[[y_col]], levels = 0:(number_categories - 1))
  
  caret::confusionMatrix(y_pred, y_true)
}


# evaluate_confusion_matrix_equal_priors
# Input: Modell und Daten
# Ouput: Konfusionsmatrix sowie viele weitere Größen
# Hinweis: Korrigiert beim Vorhersagen, die Klassen Imbalance
evaluate_confusion_matrix_equal_priors <- function(model, test_data, y_col = "wohnlage_ebene", number_categories = 3) {
  # Modellvorhersage (P(y=k | x))
  probs <- predict(model, newdata = test_data, type = "response")
  
  # Modellklassenverteilung (Prior laut Modell)
  prior_model <- colMeans(probs)
  
  # Ziel: Gleichverteilung als "neuer Prior"
  prior_uniform <- rep(1 / number_categories, number_categories)
  
  # Korrektur: adjusted = probs / prior_model * prior_uniform
  adjusted <- sweep(probs, 2, prior_model, "/")
  adjusted <- sweep(adjusted, 2, prior_uniform, "*")
  adjusted_probs <- adjusted / rowSums(adjusted)
  
  # Vorhersageklasse basierend auf korrigierten Wahrscheinlichkeiten
  all_levels <- as.character(0:(number_categories - 1))
  colnames(adjusted_probs) <- all_levels
  predicted_classes <- apply(adjusted_probs, 1, function(x) all_levels[which.max(x)])
  
  y_pred <- factor(predicted_classes, levels = all_levels)
  y_true <- factor(test_data[[y_col]], levels = all_levels)
  
  caret::confusionMatrix(y_pred, y_true)
}


# missclassification_data
# Input: Modell und Daten
# Ouput: Data frame mit Daten der falsch vorhergesagten Wohnobjekte
missclassification_data <- function(model,
                                    data,
                                    y_col = "wohnlage_ebene",
                                    number_categories = 3,
                                    predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  
  # Sicherstellen, dass Länge passt
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  
  # Ziel + Vorhersage als Faktor
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  
  # Kombinieren
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  
  # Nur Fehlklassifikationen extrahieren
  fehlerhafte_klassifikationen <- data[data$Wohnlage_wahr != data$Wohnlage_vorhersage, ]
  # Umbennen und zu Faktor machen
  
  fehlerhafte_klassifikationen <- fehlerhafte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "gute Lage",
        Wohnlage_wahr == 2 ~ "beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "gute Lage",
        Wohnlage_vorhersage == 2 ~ "beste Lage"
      ),
      # Optional: gleich als faktor mit gewünschter Reihenfolge speichern
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage"))
    )
  
  return(fehlerhafte_klassifikationen)
}


# missclassification_data_zentral
# Input: Modell und Daten
# Ouput: Data frame mit Daten der falsch vorhergesagten Wohnobjekte
# Speziell für zentrale Wohnlagen
missclassification_data_zentral <- function(model,
                                            data = model_data_zentral_complete,
                                            y_col = "wohnlage_ebene",
                                            number_categories = 3,
                                            predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  # Sicherstellen, dass Länge passt
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  # Ziel + Vorhersage als Faktor
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  # Kombinieren
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  # Nur Fehlklassifikationen extrahieren
  fehlerhafte_klassifikationen <- data[
    as.character(data$Wohnlage_wahr) != as.character(data$Wohnlage_vorhersage), 
  ]
  # Umbennen und zu Faktor machen
  fehlerhafte_klassifikationen <- fehlerhafte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "zentrale durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "zentrale gute Lage",
        Wohnlage_wahr == 2 ~ "zentrale beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "zentrale durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "zentrale gute Lage",
        Wohnlage_vorhersage == 2 ~ "zentrale beste Lage"
      ),
      # Optional: gleich als faktor mit gewünschter Reihenfolge speichern
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
    )
}


# missclassification_data
# Input: Modell und Daten
# Ouput: Data frame mit Daten der falsch vorhergesagten Wohnobjekte
# Hinweis: Speziell für Wohnlagen außerhalb
missclassification_data_ausserhalb <- function(model,
                                               data = model_data_ausserhalb_complete,
                                               y_col = "wohnlage_ebene",
                                               number_categories = 3, 
                                               predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  # Sicherstellen, dass Länge passt
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  # Ziel + Vorhersage als Faktor
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  # Kombinieren
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  # Nur Fehlklassifikationen extrahieren
  fehlerhafte_klassifikationen <- data[data$Wohnlage_wahr != data$Wohnlage_vorhersage, ]
  # Umbennen und zu Faktor machen
  fehlerhafte_klassifikationen <- fehlerhafte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "gute Lage",
        Wohnlage_wahr == 2 ~ "beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "gute Lage",
        Wohnlage_vorhersage == 2 ~ "beste Lage"
      ),
      # Optional: gleich als faktor mit gewünschter Reihenfolge speichern
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage"))
    )
}


# korrekte_vorhersagen_zentral
# Input: Modell und Daten
# Ouput: Data frame mit Daten der richtig vorhergesagten Wohnobjekte
# Hinweis: Speziell für zentrale Wohnlagen
korrekte_vorhersagen_zentral <- function(model,
                                         data = model_data_zentral_complete,
                                         y_col = "wohnlage_ebene",
                                         number_categories = 3,
                                         predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  
  # Nur korrekt klassifizierte Zeilen behalten
  korrekte_klassifikationen <- data[
    as.character(data$Wohnlage_wahr) == as.character(data$Wohnlage_vorhersage), 
  ]
  
  korrekte_klassifikationen <- korrekte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "zentrale durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "zentrale gute Lage",
        Wohnlage_wahr == 2 ~ "zentrale beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "zentrale durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "zentrale gute Lage",
        Wohnlage_vorhersage == 2 ~ "zentrale beste Lage"
      ),
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("zentrale durchschnittliche Lage", "zentrale gute Lage", "zentrale beste Lage"))
    )
  
  return(korrekte_klassifikationen)
}

# korrekte_vorhersagen_ausserhalb
# Input: Modell und Daten
# Ouput: Data frame mit Daten der richtig vorhergesagten Wohnobjekte
# Hinweis: Speziell für Wohnlagen ausserhalb
korrekte_vorhersagen_ausserhalb <- function(model,
                                            data = model_data_ausserhalb_complete,
                                            y_col = "wohnlage_ebene",
                                            number_categories = 3,
                                            predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  
  # Nur korrekt klassifizierte Zeilen behalten
  korrekte_klassifikationen <- data[
    as.character(data$Wohnlage_wahr) == as.character(data$Wohnlage_vorhersage),
  ]
  
  korrekte_klassifikationen <- korrekte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "gute Lage",
        Wohnlage_wahr == 2 ~ "beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "gute Lage",
        Wohnlage_vorhersage == 2 ~ "beste Lage"
      ),
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage"))
    )
  
  return(korrekte_klassifikationen)
}


korrekte_vorhersagen <- function(model,
                                            data = model_data_ausserhalb_complete,
                                            y_col = "wohnlage_ebene",
                                            number_categories = 3,
                                            predict_fun = predict_labels_discr) {
  predicted_classes <- predict_fun(model = model, test_data = data,
                                   number_categories = number_categories, y_col = y_col)
  
  if (length(predicted_classes) != nrow(data)) {
    stop("Länge der Vorhersagen passt nicht zur Anzahl der Datenzeilen.")
  }
  
  Wohnlage_vorhersage <- factor(predicted_classes, levels = as.character(0:(number_categories - 1)))
  Wohnlage_wahr <- factor(data[[y_col]], levels = as.character(0:(number_categories - 1)))
  
  data <- data.frame(data, Wohnlage_wahr, Wohnlage_vorhersage)
  
  # Nur korrekt klassifizierte Zeilen behalten
  korrekte_klassifikationen <- data[
    as.character(data$Wohnlage_wahr) == as.character(data$Wohnlage_vorhersage),
  ]
  
  korrekte_klassifikationen <- korrekte_klassifikationen %>%
    mutate(
      Wohnlage_wahr = case_when(
        Wohnlage_wahr == 0 ~ "durchschnittliche Lage",
        Wohnlage_wahr == 1 ~ "gute Lage",
        Wohnlage_wahr == 2 ~ "beste Lage"
      ),
      Wohnlage_vorhersage = case_when(
        Wohnlage_vorhersage == 0 ~ "durchschnittliche Lage",
        Wohnlage_vorhersage == 1 ~ "gute Lage",
        Wohnlage_vorhersage == 2 ~ "beste Lage"
      ),
      Wohnlage_wahr = factor(Wohnlage_wahr, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage")),
      Wohnlage_vorhersage = factor(Wohnlage_vorhersage, levels = c("durchschnittliche Lage", "gute Lage", "beste Lage"))
    )
  
  return(korrekte_klassifikationen)
}
