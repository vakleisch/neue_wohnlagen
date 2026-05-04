library(mgcv)
library(brms)
data = read.csv("daten/pasing_data.csv")
source("daten_verarbeitung/daten_bearbeitung.R")
data <- data[complete.cases(data), ]
N = dim(data)[1]

# Wir definieren die Variablen, die uns interessieren
data$y1 = data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y2 = data$grundschul_num
data$y3 = data$kitakigaho_num
data$y4 = data$spielplatz_num
data$y5 = data$erreichbarkeit_gr10ha_in_metern_adr
data$y6 = data$ortszentru_num

hist(data$y1)
hist(data$y2)

d = 6  # Anzahl der Variablen, die inkludiert sind

# Wohnlage als Faktor
data$c = as.factor(data$wlg_numeric)

# Diskriminationsmodell mit räumlichen Komponenten
# Wir schätzen eine multivariate Normalverteilung mit
# einer räumlichen Komponente für JEDE Wohnlage
model <- gam(
  list(
    y1~s(s.long, s.lat, by = c)+c,
    y2~s(s.long, s.lat, by = c)+c,
    y3~s(s.long, s.lat, by = c)+c,
   y4~s(s.long, s.lat, by = c)+c,
   y5~s(s.long, s.lat, by = c)+c,
   y6~s(s.long, s.lat, by = c)+c)
,   # include main effect of factor
  family = mvn(d = d),  
  data = data,
)

# Varianzmatrix (HOMOGEN)
VAR = solve(crossprod(model$family$data$R)) 

# Berechnung der Diskrimination. 
# Zunächst brauchen wir einen Platzhalter, den nennen wir SCORE
SCORE = c()

# Matrix der Response Variablen
Y = cbind( data$y1, data$y2, data$y3, data$y4, data$y5, data$y6)

# Berechnung für Wohnlage 0
data.wlg = data
data.wlg$c = "0"
# Geschätze Mittelwerte für die Wohnlage 0 für JEDE Wohnung
fit.wlg = predict(model,newdata=data.wlg)
# Berechnung der quadratischen Form bei der multivariaten Normalverteilung
# Wir machen das etwas "haendischer" um Speicherplatz zu sparen
score = (Y-fit.wlg) %*% solve(VAR) 
score = (score * (Y - fit.wlg ) ) %*% matrix(1,d,1)
# Das ergibt den Score für die ERSTE Wohnlage, i.e. c=0
SCORE = cbind(SCORE,score)

# Berechnung für Wohnlage 1
data.wlg$c = "1"
fit.wlg = predict(model,newdata=data.wlg)
score = (Y-fit.wlg) %*% solve(VAR) 
score = (score * (Y - fit.wlg ) ) %*% matrix(1,d,1)
SCORE = cbind(SCORE,score)

# Berechnung für Wohnlage 2
data.wlg$c = "2"
fit.wlg = predict(model,newdata=data.wlg)
score = (Y-fit.wlg) %*% solve(VAR) 
score = (score * (Y - fit.wlg ) ) %*% matrix(1,d,1)
SCORE = cbind(SCORE,score)

# Aus den quadratischen Formen berechnen wir nun die Dichte
# der multivariaten Normalverteilung und normieren das.
PROB = exp(-SCORE)
PROB = PROB / c(PROB %*% matrix(1,3,1))

head(PROB)

# ANteil der Fehlklassifikatoren
mean(PROB[data$c==0,1]>0.5)
mean(PROB[data$c==1,2]>0.5)
mean(PROB[data$c==2,3]>0.5)

# Plotten der Daten und der Punkte mit Fehlklassifizierung
plot(data$s.long,data$s.lat, col="grey")
index = (1:N)[data$c==0 & (PROB[,1]<0.5)]
points(data$s.long[index],data$s.lat[index], col=2)
index = (1:N)[data$c==1 & (PROB[,2]<0.5)]
points(data$s.long[index],data$s.lat[index], col=3)
index = (1:N)[data$c==2 & (PROB[,3]<0.5)]
points(data$s.long[index],data$s.lat[index], col=4)


# Das ganze kann man auch berechnen, wenn man je Wohnung
# eine Prior nimmt, die mehr Wahrscheinlichkeitsmasse auf
# die bisherige Wohnlage legt. Das kann man gewichtet betrachten
# Wenn prior.scale = 0 so hat man das obige, bei prior.scale = 1e99
# bleibt man immer bei der bisherigen Wohnlage
prior.scale = 100
PRIOR = matrix(1 , N, 3 ) + prior.scale * 
  cbind( data$c==0, data$c==1,data$c==2)
PROB.prior = exp(-SCORE) * PRIOR
PROB.prior = PROB.prior / c(PROB.prior %*% matrix(1,3,1))

mean(PROB.prior[data$c==0,1]>0.5)
mean(PROB.prior[data$c==1,2]>0.5)
mean(PROB.prior[data$c==2,3]>0.5)

plot(data$s.long,data$s.lat, col="grey")
index = (1:N)[data$c==0 & (PROB.prior[,1]<0.5)]
points(data$s.long[index],data$s.lat[index], col=2)
index = (1:N)[data$c==1 & (PROB.prior[,2]<0.5)]
points(data$s.long[index],data$s.lat[index], col=3)
index = (1:N)[data$c==2 & (PROB.prior[,3]<0.5)]
points(data$s.long[index],data$s.lat[index], col=4)





# ==============================================================================
# INTERAKTIVE KARTE FÜR DAS DISKRIMINATIONSMODELL (MVN)
# ==============================================================================

library(leaflet)
library(dplyr)
library(htmlwidgets)

cat("Bereite Daten für die interaktive Karte vor...\n")




library(sf)

cat("Schneide Wohnlagen-Flächen auf Pasing zu...\n")

# 1. Wir machen aus deinen Pasing-Punkten kurz ein echtes Geometrie-Objekt
pasing_punkte_sf <- st_as_sf(map_data_mvn, coords = c("s.long", "s.lat"), crs = 4326)

# 2. Der magische räumliche Filter: 
# st_filter behält automatisch nur die Flächen aus München, die Pasing berühren!
wohnlagen_pasing_wgs <- st_filter(wohnlagen_muc_wgs, pasing_punkte_sf)

# 3. (Optional) Das Gleiche machen wir für die Umrisslinien, falls du sie nutzt:
wohnlage_grenzen_pasing_wgs <- st_filter(wohnlage_grenzen_wgs, pasing_punkte_sf)

# 1. Vorhersageklasse ermitteln (die Spalte mit der höchsten Wahrscheinlichkeit)
# max.col liefert 1, 2 oder 3. Wir ziehen 1 ab, um auf deine Klassen 0, 1, 2 zu kommen
predicted_class <- max.col(PROB.prior) - 1

# 2. Übersetzungs-Wörterbuch für die Klassen
klassen_namen <- c(
  "0" = "durchschnittliche Lage",
  "1" = "gute Lage",
  "2" = "beste Lage"
)

wohnlage_farben <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage" = "#afe391",
  "beste Lage" = "#7FCDBB"
)

# 3. Dataframe für die Karte zusammenstellen
map_data_mvn <- data %>%
  mutate(
    # Wahrscheinlichkeiten aus der PROB Matrix anhängen
    prob_0 = PROB.prior[, 1],
    prob_1 = PROB.prior[, 2],
    prob_2 = PROB.prior[, 3],
    
    # Klassen in Text übersetzen
    Wohnlage_wahr = klassen_namen[as.character(c)],
    Wohnlage_vorhersage = klassen_namen[as.character(predicted_class)],
    
    # War die Vorhersage korrekt?
    Korrekt = (c == predicted_class),
    
    # Farbe für die Karte zuweisen
    color = unname(wohnlage_farben[Wohnlage_vorhersage])
  )

# 4. HTML-Popups generieren
erstelle_popup_mvn <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, " (Klasse ", df$c, ")<br>",
    "<b>Vorhersage:</b> <span style='color:", ifelse(df$Korrekt, "black", "red"), ";'>", 
    df$Wohnlage_vorhersage, "</span><br>",
    "<hr>",
    "<b>Berechnete Dichte-Wahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_0 * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_1 * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_2 * 100, 1), " %<br>",
    "<hr>",
    "<i>Echte Infrastruktur-Werte:</i><br>",
    "Fahrtzeit Haltestelle: ", df$y1, " min<br>",
    "Grundschule Distanz: ", df$y2, " m<br>",
    "Kita Distanz: ", df$y3, " m<br>",
    "Spielplatz Distanz: ", df$y4, " m<br>",
    "Park (>10ha) Distanz: ", df$y5, " m<br>",
    "Ortszentrum Distanz: ", df$y6, " m<br>"
  )
}

map_data_mvn$popup_text <- erstelle_popup_mvn(map_data_mvn)

# 5. In Richtig und Falsch aufteilen
daten_korrekt_mvn <- map_data_mvn %>% filter(Korrekt == TRUE)
daten_fehler_mvn  <- map_data_mvn %>% filter(Korrekt == FALSE)

cat(paste("=>", nrow(daten_korrekt_mvn), "korrekte Vorhersagen,", nrow(daten_fehler_mvn), "Fehler.\n"))

# ==============================================================================
# 6. LEAFLET KARTE BAUEN
# ==============================================================================
cat("Zeichne Leaflet Karte...\n")

punkt_groesse <- 6

karte_mvn <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = wohnlagen_pasing_wgs,     # <--- HIER GEÄNDERT
    fillColor = ~color,      
    fillOpacity = 0.4,       
    color = "black",         
    weight = 0.5,
    label = ~as.character(Wohnlage),
    group = "Wohnlagen (Flächen)"
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_pasing_wgs, # <--- HIER GEÄNDERT
    color = "black", 
    weight = 0.5,
    group = "Wohnlagen (Flächen)"
  ) %>%
  # KORREKTE PUNKTE (Dünner schwarzer Rand)
  addCircleMarkers(
    data = daten_korrekt_mvn,
    lng = ~s.long,        # Bei puren Dataframes geben wir lng und lat explizit an!
    lat = ~s.lat,
    fillColor = ~color,
    fillOpacity = 0.9,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = punkt_groesse,
    popup = ~popup_text,
    group = "Korrekt"
  ) %>%
  
  # FEHLERHAFTE PUNKTE (Dicker roter Rand)
  addCircleMarkers(
    data = daten_fehler_mvn,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = ~color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 2.5,
    radius = punkt_groesse + 1,
    popup = ~popup_text,
    group = "Fehler"
  ) %>%
  
  # Legende
  # Legende (Auf die drei Pasinger Lagen reduziert)
  addLegend(
    position = "bottomright",
    # Wir übergeben hier direkt die drei relevanten Farben und Namen
    colors = c("#e8f5a4", "#afe391", "#7FCDBB"), 
    labels = c("durchschnittliche Lage", "gute Lage", "beste Lage"),
    title = "Vorhersage: starker Prior",
    opacity = 1
  ) %>%
  
  # Ebenen-Steuerung
  addLayersControl(
    overlayGroups = c("Fehler", "Korrekt"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Anzeigen
print(karte_mvn)

# Speichern
if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_mvn, file = "interaktive_karten/lda/karte_mvn_pasing_prior.html", selfcontained = TRUE)

cat("✓ Fertig! Karte erfolgreich als 'karte_mvn_pasing.html' gespeichert.\n")








# Auf ganz München loslassen
source("daten_verarbeitung/daten_bearbeitung.R")
library(mgcv)

# 1. Datenvorbereitung
data <- model_munich_data[complete.cases(model_munich_data), ]
N <- dim(data)[1]

# Wir definieren ALLE 10 relevanten Features
data$y1  <- data$erreichbarkeit_gr10ha_in_metern_adr
data$y2  <- data$erreichbarkeit_innenstadt_in_minuten_adr
data$y3  <- data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y4  <- data$grundschul_num
data$y5  <- data$spielplatz_num
data$y6  <- data$kitakigaho_num
data$y7  <- data$ortszentru_num
data$y8  <- data$brw_log
data$y9  <- data$anteil_vf_sv
data$y10 <- data$anteil_gf_sv

d <- 10  # Anzahl der Variablen

# Wohnlage als Faktor (hat nun 6 Level laut str())
data$c <- as.factor(data$wohnlage_bedeutung)
levels_c <- levels(data$c)
k <- length(levels_c) # Anzahl der Wohnlagen-Kategorien (6)

# 2. Diskriminationsmodell mit räumlichen Komponenten
model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c) + c,
    y2  ~ s(s.long, s.lat, by = c) + c,
    y3  ~ s(s.long, s.lat, by = c) + c,
    y4  ~ s(s.long, s.lat, by = c) + c,
    y5  ~ s(s.long, s.lat, by = c) + c,
    y6  ~ s(s.long, s.lat, by = c) + c,
    y7  ~ s(s.long, s.lat, by = c) + c,
    y8  ~ s(s.long, s.lat, by = c) + c,
    y9  ~ s(s.long, s.lat, by = c) + c,
    y10 ~ s(s.long, s.lat, by = c) + c
  ),
  family = mvn(d = d),  
  data = data
)

# Varianzmatrix (HOMOGEN)
VAR <- solve(crossprod(model$family$data$R)) 

# 3. Berechnung der Diskrimination
# Matrix der Response Variablen
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

# Platzhalter Matrix für alle Scores (N Zeilen, k Spalten)
SCORE <- matrix(0, nrow = N, ncol = k)

# Wir iterieren dynamisch über alle 6 Wohnlagen
for (i in 1:k) {
  data.wlg <- data
  data.wlg$c <- levels_c[i]
  
  # Geschätzte Mittelwerte für die jeweilige Wohnlage
  fit.wlg <- predict(model, newdata = data.wlg)
  
  # Berechnung der quadratischen Form
  score <- (Y - fit.wlg) %*% solve(VAR) 
  score <- (score * (Y - fit.wlg)) %*% matrix(1, d, 1)
  
  # Speichern in der passenden Spalte
  SCORE[, i] <- score
}

# 4. Wahrscheinlichkeiten berechnen (ohne Prior)
PROB <- exp(-SCORE)
# Normierung über alle k Spalten
PROB <- PROB / c(PROB %*% matrix(1, k, 1))

head(PROB)

# Anteil der Fehlklassifikatoren ausgeben
cat("\n--- Fehlklassifikatoren (Ohne Prior) ---\n")
for (i in 1:k) {
  fehl_quote <- mean(PROB[data$c == levels_c[i], i] > 0.5)
  cat("Wohnlage", levels_c[i], ":", fehl_quote, "\n")
}

# Plotten der Daten und der Punkte mit Fehlklassifizierung
plot(data$s.long, data$s.lat, col = "grey", main="Fehlklassifikationen (Ohne Prior)", pch=20, cex=0.5)
# Farbpalette für die 6 Level
farben <- c("red", "blue", "green3", "orange", "purple", "cyan")

for (i in 1:k) {
  index <- (1:N)[data$c == levels_c[i] & (PROB[, i] < 0.5)]
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.6)
}
legend("topright", legend=levels_c, col=farben, pch=20, cex=0.8, bty="n")


# 5. Berechnung mit Prior-Gewichtung
prior.scale <- 100
PRIOR <- matrix(1, N, k)

# Prior-Matrix dynamisch aufbauen
for (i in 1:k) {
  PRIOR[, i] <- PRIOR[, i] + prior.scale * (data$c == levels_c[i])
}

PROB.prior <- exp(-SCORE) * PRIOR
PROB.prior <- PROB.prior / c(PROB.prior %*% matrix(1, k, 1))

# Anteil der Fehlklassifikatoren (Mit Prior)
cat("\n--- Fehlklassifikatoren (Mit Prior) ---\n")
for (i in 1:k) {
  fehl_quote_prior <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
  cat("Wohnlage", levels_c[i], ":", fehl_quote_prior, "\n")
}

# Plot mit Prior
plot(data$s.long, data$s.lat, col = "grey", main="Fehlklassifikationen (Mit Prior)", pch=20, cex=0.5)
for (i in 1:k) {
  index <- (1:N)[data$c == levels_c[i] & (PROB.prior[, i] < 0.5)]
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.6)
}
legend("topright", legend=levels_c, col=farben, pch=20, cex=0.8, bty="n")







# Ohne zentrale Lage------------------------------------------------

data <- model_munich_data2[complete.cases(model_munich_data2), ]
N <- dim(data)[1]

# Wir definieren wieder ALLE 10 relevanten Features
data$y1  <- data$erreichbarkeit_gr10ha_in_metern_adr
data$y2  <- data$erreichbarkeit_innenstadt_in_minuten_adr
data$y3  <- data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y4  <- data$grundschul_num
data$y5  <- data$spielplatz_num
data$y6  <- data$kitakigaho_num
data$y7  <- data$ortszentru_num
data$y8  <- data$brw_log
data$y9  <- data$anteil_vf_sv
data$y10 <- data$anteil_gf_sv

d <- 10  # Anzahl der Variablen

# Wohnlage als Faktor (hat nun automatisch 3 Level!)
data$c <- data$wohnlage_bedeutung
levels_c <- levels(data$c)
k <- length(levels_c) # k ist jetzt 3

# --- 3. GAM MODELL ---
model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c) + c,
    y2  ~ s(s.long, s.lat, by = c) + c,
    y3  ~ s(s.long, s.lat, by = c) + c,
    y4  ~ s(s.long, s.lat, by = c) + c,
    y5  ~ s(s.long, s.lat, by = c) + c,
    y6  ~ s(s.long, s.lat, by = c) + c,
    y7  ~ s(s.long, s.lat, by = c) + c,
    y8  ~ s(s.long, s.lat, by = c) + c,
    y9  ~ s(s.long, s.lat, by = c) + c,
    y10 ~ s(s.long, s.lat, by = c) + c
  ),
  family = mvn(d = d),  
  data = data
)

# Varianzmatrix (HOMOGEN)
VAR <- solve(crossprod(model$family$data$R)) 

# --- 4. BERECHNUNG DER DISKRIMINATION ---
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

# Platzhalter Matrix (jetzt N Zeilen, 3 Spalten)
SCORE <- matrix(0, nrow = N, ncol = k)

for (i in 1:k) {
  data.wlg <- data
  data.wlg$c <- levels_c[i]
  
  fit.wlg <- predict(model, newdata = data.wlg)
  
  score <- (Y - fit.wlg) %*% solve(VAR) 
  score <- (score * (Y - fit.wlg)) %*% matrix(1, d, 1)
  
  SCORE[, i] <- score
}

PROB <- exp(-SCORE)
PROB <- PROB / c(PROB %*% matrix(1, k, 1))

# Fehlklassifikatoren ausgeben
cat("\n--- Fehlklassifikatoren (Ohne Prior) ---\n")
for (i in 1:k) {
  fehl_quote <- mean(PROB[data$c == levels_c[i], i] > 0.5)
  cat("Wohnlage", levels_c[i], ":", fehl_quote, "\n")
}

# Plot (Ohne Prior)
plot(data$s.long, data$s.lat, col = "grey", main="Fehlklassifikationen (Ohne Prior)", pch=20, cex=0.5)
farben <- c("red", "blue", "green3") # Nur noch 3 Farben nötig

for (i in 1:k) {
  index <- (1:N)[data$c == levels_c[i] & (PROB[, i] < 0.5)]
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.6)
}
legend("topright", legend=levels_c, col=farben, pch=20, cex=0.8, bty="n")

# --- 5. BERECHNUNG MIT PRIOR ---
prior.scale <- 100
PRIOR <- matrix(1, N, k)

for (i in 1:k) {
  PRIOR[, i] <- PRIOR[, i] + prior.scale * (data$c == levels_c[i])
}

PROB.prior <- exp(-SCORE) * PRIOR
PROB.prior <- PROB.prior / c(PROB.prior %*% matrix(1, k, 1))

cat("\n--- Fehlklassifikatoren (Mit Prior) ---\n")
for (i in 1:k) {
  fehl_quote_prior <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
  cat("Wohnlage", levels_c[i], ":", fehl_quote_prior, "\n")
}

# Plot (Mit Prior)
plot(data$s.long, data$s.lat, col = "grey", main="Fehlklassifikationen (Mit Prior)", pch=20, cex=0.5)
for (i in 1:k) {
  index <- (1:N)[data$c == levels_c[i] & (PROB.prior[, i] < 0.5)]
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.6)
}
legend("topright", legend=levels_c, col=farben, pch=20, cex=0.8, bty="n")










#--------------------------------------------------------------------------------
library(parallel)
library(mgcv)

# --- 1. DATEN VORBEREITEN (Alle Wohnlagen auf 3 Level reduzieren) ---

data <- model_munich_data[complete.cases(model_munich_data2), ]

# WICHTIG: Wir entfernen das Wort "zentrale" aus allen Einträgen. 
# Dadurch werden "zentrale gute Lage" und "gute Lage" zu einer einzigen Kategorie!
bereinigte_lage <- trimws(gsub("zentrale", "", data$wohnlage_bedeutung, ignore.case = TRUE))
data$c <- droplevels(as.factor(bereinigte_lage))

levels_c <- levels(data$c)
k <- length(levels_c) # Überprüfe dies: k MUSS jetzt exakt 3 sein!
N <- dim(data)[1]

# Alle 10 relevanten Features
data$y1  <- data$erreichbarkeit_gr10ha_in_metern_adr
data$y2  <- data$erreichbarkeit_innenstadt_in_minuten_adr
data$y3  <- data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y4  <- data$grundschul_num
data$y5  <- data$spielplatz_num
data$y6  <- data$kitakigaho_num
data$y7  <- data$ortszentru_num
data$y8  <- data$brw_log
data$y9  <- data$anteil_vf_sv
data$y10 <- data$anteil_gf_sv

d <- 10  # Anzahl der Variablen auf 10 gesetzt


# --- 2. GAM MODELL TRAINING ---
cat("\nStarte Modelltraining (Alle Lagen zusammengeführt auf 3 Kategorien)...\n")
start_zeit <- Sys.time()

model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y8  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c
  ),
  family = mvn(d = d),  
  data = data,
  optimizer = "efs",                        
  control = gam.control(trace = TRUE)
)

if (!dir.exists("modelle")) dir.create("modelle")
saveRDS(model, file = "modelle/lindisc_model_beides_3cat.rds")
cat("\nModell trainiert und gespeichert!\n")


# --- 3. BERECHNUNG DER DISKRIMINATION (OPTIMIERT) ---

VAR <- solve(crossprod(model$family$data$R)) 
# Invertierung VOR die Schleife ziehen
INV_VAR <- solve(VAR) 

# y11 wurde hier korrekt entfernt, damit die Dimensionen wieder stimmen!
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

# CPU-Kerne für die Vorhersage aktivieren
anzahl_kerne <- max(1, detectCores() - 1)
cl <- makeCluster(anzahl_kerne)

cat("\nStarte schnelle Vorhersage mit", anzahl_kerne, "Kernen...\n")

for (i in 1:k) {
  temp_data <- data
  temp_data$c <- levels_c[i]
  
  # Paralleles Predict
  fit.wlg <- predict(model, newdata = temp_data, cluster = cl)
  
  # C-optimierte schnelle Matrix-Berechnung
  diff_Y <- Y - fit.wlg
  score_temp <- (diff_Y %*% INV_VAR) * diff_Y
  SCORE[, i] <- rowSums(score_temp)
}

# Cluster sauber schließen
stopCluster(cl)


# --- 4. WAHRSCHEINLICHKEITEN (Numerisch stabilisiert) ---

# Den Shifted-Trick anwenden, um Rechenfehler (underflow) zu verhindern
SCORE_shifted <- SCORE - apply(SCORE, 1, min)

PROB <- exp(-SCORE_shifted)
PROB <- PROB / rowSums(PROB)

# Dauer ausgeben
end_zeit <- Sys.time()
cat("\nGesamte Berechnung (Modell + Scores) abgeschlossen!\n")
print(end_zeit - start_zeit)


# --- 5. OUTPUT ---
cat("\n--- Fehlklassifikatoren (Ohne Prior) ---\n")
for (i in 1:k) {
  fehl_quote <- mean(PROB[data$c == levels_c[i], i] > 0.5)
  cat("Wohnlage", levels_c[i], ":", round(fehl_quote * 100, 2), "%\n")
}

# Ergebnisse speichern
if (!dir.exists("results_lin_disc")) dir.create("results_lin_disc")
saveRDS(SCORE, "results_lin_disc/score_mat_beides_3cat.rds")
saveRDS(PROB, "results_lin_disc/prob_mat_beides_3cat.rds")
saveRDS(data, "results_lin_disc/data_beides_3cat.rds")








# NUR ZENTRALE LAGEN

# --- 1. DATENAUFBEREITUNG (Nur zentrale Lagen) ---
start_zeit_nur_zentrale_lagen <- Sys.time()

# Wir transformieren den spezifischen Datensatz für zentrale Lagen
model_munich_data_zentral <- st_transform(model_data_complete_zentral, crs = 4326)
coords <- st_coordinates(model_munich_data_zentral)

data <- model_munich_data_zentral %>%
  st_drop_geometry() %>% 
  mutate(
    s.long = coords[, 1],
    s.lat  = coords[, 2]
  ) %>%
  # Sicherheitshalber filtern wir hier nochmal auf alles, was "zentrale" im Namen hat
  filter(grepl("zentrale", wohnlage_bedeutung, ignore.case = TRUE)) %>%
  mutate(
    # Wir entfernen das Wort "zentrale", damit die Legenden und Plots sauberer sind
    c = as.factor(trimws(gsub("zentrale", "", wohnlage_bedeutung, ignore.case = TRUE)))
  )

# Falls Zeilen mit NA vorhanden sind, entfernen
data <- data[complete.cases(data), ]


N <- dim(data)[1]

# Wir definieren die 10 Features
data$y1  <- data$erreichbarkeit_gr10ha_in_metern_adr
data$y2  <- data$erreichbarkeit_innenstadt_in_minuten_adr
data$y3  <- data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y4  <- data$grundschul_num
data$y5  <- data$spielplatz_num
data$y6  <- data$kitakigaho_num
data$y7  <- data$ortszentru_num
data$y8  <- data$brw_log
data$y9  <- data$anteil_vf_sv
data$y10 <- data$anteil_gf_sv


# Speichern für später
saveRDS(data, "results_lin_disc/data_zent.rds")

d <- 10 
levels_c <- levels(data$c)
k <- length(levels_c) # Sollte 3 ergeben

# --- 2. GAM MODELL 
model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y8  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c
  ),
  family = mvn(d = d),  
  data = data,
  optimizer = "efs",                       
  control = gam.control(trace = TRUE)
)
saveRDS(model, file = "modelle/lindisc_model_zentral.rds")

# Varianzmatrix
VAR <- solve(crossprod(model$family$data$R)) 

# --- 3. DISKRIMINATION UND SCORES ---
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

for (i in 1:k) {
  temp_data <- data
  temp_data$c <- levels_c[i]
  fit.wlg <- predict(model, newdata = temp_data)
  
  score <- (Y - fit.wlg) %*% solve(VAR) 
  score <- (score * (Y - fit.wlg)) %*% matrix(1, d, 1)
  SCORE[, i] <- score
}

PROB <- exp(-SCORE)
PROB <- PROB / rowSums(PROB)

# --- 4. OUTPUT ---
cat("\n--- Fehlklassifikatoren in zentralen Lagen ---\n")
for (i in 1:k) {
  quote <- mean(PROB[data$c == levels_c[i], i] > 0.5)
  cat("Zentrale", levels_c[i], ":", quote, "\n")
}

# Plot
plot(data$s.long, data$s.lat, col = "lightgrey", 
     main="Fehlklassifikationen: Nur zentrale Lagen", pch=20, cex=0.7)
farben <- c("red", "blue", "green3") 

for (i in 1:k) {
  index <- which(data$c == levels_c[i] & (PROB[, i] < 0.5))
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.8)
}
legend("bottomleft", legend=paste("Zentral:", levels_c), col=farben, pch=20, bty="n")


end_zeit_nur_zentrale_lagen <- Sys.time()

# 3. Differenz berechnen und ausgeben
dauer <- end_zeit_nur_zentrale_lagen - start_zeit_nur_zentrale_lagen
dauer

saveDS(SCORE, "results_lin_disc/score_mat_zent.rds")
saveRDS(PROB, "results_lin_disc/prob_mat_zent.rds")



library(dplyr)
library(sf)
library(mgcv)

# --- 1. DATENAUFBEREITUNG (Außerhalb-Lagen) ---

# Wir transformieren den spezifischen Datensatz für Lagen außerhalb
model_munich_data_ausserhalb <- st_transform(model_data_complete_ausserhalb, crs = 4326)
coords <- st_coordinates(model_munich_data_ausserhalb)

data <- model_munich_data_ausserhalb %>%
  st_drop_geometry() %>% 
  mutate(
    s.long = coords[, 1],
    s.lat  = coords[, 2],
    # Da hier kein "zentrale" im Namen steht, wandeln wir es direkt in einen Faktor um
    c = droplevels(as.factor(wohnlage_bedeutung))
  )

# Falls Zeilen mit NA vorhanden sind, entfernen
data <- data[complete.cases(data), ]

N <- dim(data)[1]

# Wir definieren die 10 Features
data$y1  <- data$erreichbarkeit_gr10ha_in_metern_adr
data$y2  <- data$erreichbarkeit_innenstadt_in_minuten_adr
data$y3  <- data$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data$y4  <- data$grundschul_num
data$y5  <- data$spielplatz_num
data$y6  <- data$kitakigaho_num
data$y7  <- data$ortszentru_num
data$y8  <- data$brw_log
data$y9  <- data$anteil_vf_sv
data$y10 <- data$anteil_gf_sv

# Speichern für später
saveRDS(data, "results_lin_disc/data_aus.rds")

d <- 10 
levels_c <- levels(data$c)
k <- length(levels_c) # Sollte wieder 3 ergeben

# --- 2. GAM MODELL & BERECHNUNG (Mit Sys.time() Messung) ---

cat("Starte Modelltraining und Berechnung für Lagen außerhalb...\n")

# Startzeitpunkt erfassen
start_zeit <- Sys.time()

# Modelltraining
model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y8  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c
  ),
  family = mvn(d = d),  
  data = data,
  optimizer = "efs",                       # Schnellerer Optimizer
  control = gam.control(trace = TRUE)      # Fortschrittsanzeige in der Konsole
)

saveRDS(model, file = "modelle/lindisc_model_ausserhalb.rds")

# --- 3. DISKRIMINATION UND SCORES ---
VAR <- solve(crossprod(model$family$data$R)) 
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

for (i in 1:k) {
  temp_data <- data
  temp_data$c <- levels_c[i]
  fit.wlg <- predict(model, newdata = temp_data)
  
  score <- (Y - fit.wlg) %*% solve(VAR) 
  score <- (score * (Y - fit.wlg)) %*% matrix(1, d, 1)
  SCORE[, i] <- score
}

PROB <- exp(-SCORE)
PROB <- PROB / rowSums(PROB)

# Endzeitpunkt erfassen und Dauer berechnen
end_zeit <- Sys.time()
dauer <- end_zeit - start_zeit

cat("\nBerechnung abgeschlossen!\n")
cat("Gesamtdauer:\n")
print(dauer)

saveRDS(PROB, "prob_mat_aus.rds")
saveRDS(SCORE, "score_mat_aus.rds")


# --- 4. OUTPUT ---
cat("\n--- Fehlklassifikatoren (Außerhalb-Lagen) ---\n")
for (i in 1:k) {
  quote <- mean(PROB[data$c == levels_c[i], i] > 0.5)
  cat("Außerhalb:", levels_c[i], ":", quote, "\n")
}

# Plot
plot(data$s.long, data$s.lat, col = "lightgrey", 
     main="Fehlklassifikationen: Lagen außerhalb", pch=20, cex=0.5)
farben <- c("red", "blue", "green3") 

for (i in 1:k) {
  index <- which(data$c == levels_c[i] & (PROB[, i] < 0.5))
  points(data$s.long[index], data$s.lat[index], col = farben[i], pch=20, cex=0.6)
}
legend("topright", legend=paste("Außerhalb:", levels_c), col=farben, pch=20, bty="n")




# schnellere bercehnung wohnlagen ausserhalb
library(parallel)
model <- readRDS("modelle/lindisc_model_ausserhalb.rds")
data <- readRDS("results_lin_disc/data_aus.rds")

# --- VORBEREITUNG FÜR MAXIMALEN SPEED ---
start_zeit <- Sys.time()

VAR <- solve(crossprod(model$family$data$R)) 
# 1. Invertierung VOR die Schleife ziehen (spart wiederholte Rechenzeit)
INV_VAR <- solve(VAR) 

Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

# 2. CPU-Kerne für die Vorhersage aktivieren (nutzt alle Kerne bis auf einen)
anzahl_kerne <- max(1, detectCores() - 1)
cl <- makeCluster(anzahl_kerne)

cat("Starte Vorhersagen mit", anzahl_kerne, "Kernen...\n")

# --- SCHLEIFE ---
for (i in 1:k) {
  temp_data <- data
  temp_data$c <- levels_c[i]
  
  # predict() nutzt nun den Cluster und teilt die Arbeit auf!
  fit.wlg <- predict(model, newdata = temp_data, cluster = cl)
  
  # 3. Matrix-Mathe optimieren
  # Wir speichern die Differenz zwischen nach echten Werten und Vorhersage
  diff_Y <- Y - fit.wlg
  
  # Wir nutzen rowSums() anstelle von "%*% matrix(...)". 
  # Das ist intern in C geschrieben, extrem speicherschonend und rasend schnell.
  score_temp <- (diff_Y %*% INV_VAR) * diff_Y
  SCORE[, i] <- rowSums(score_temp)
}

# Cluster nach der Schleife wieder sauber schließen
stopCluster(cl) 

# --- WAHRSCHEINLICHKEITEN BERECHNEN ---
PROB <- exp(-SCORE)
PROB <- PROB / rowSums(PROB)

# Endzeitpunkt erfassen und Dauer berechnen
end_zeit <- Sys.time()
dauer <- end_zeit - start_zeit

cat("\nBerechnung abgeschlossen!\n")
cat("Gesamtdauer:\n")
print(dauer)

saveRDS(SCORE, "results_lin_disc/score_mat_aus.rds")
saveRDS(PROB, "results_lin_disc/prob_mat_aus.rds")




# schnellere Version zentral
library(parallel)
model <- readRDS("modelle/lindisc_model_zentral.rds")
data <- readRDS("results_lin_disc/data_zent.rds")

# --- VORBEREITUNG FÜR MAXIMALEN SPEED (ZENTRALE LAGEN) ---

# Startzeitpunkt erfassen (wichtig für die Dauer-Berechnung am Ende!)
start_zeit <- Sys.time()

VAR <- solve(crossprod(model$family$data$R)) 
# 1. Invertierung VOR die Schleife ziehen (spart wiederholte Rechenzeit)
INV_VAR <- solve(VAR) 

Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5, 
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

# 2. CPU-Kerne für die Vorhersage aktivieren (nutzt alle Kerne bis auf einen)
anzahl_kerne <- max(1, detectCores() - 1)
cl <- makeCluster(anzahl_kerne)

cat("Starte Vorhersagen für zentrale Lagen mit", anzahl_kerne, "Kernen...\n")

# --- SCHLEIFE ---
for (i in 1:k) {
  temp_data <- data
  temp_data$c <- levels_c[i]
  
  # predict() nutzt nun den Cluster und teilt die Arbeit auf!
  fit.wlg <- predict(model, newdata = temp_data, cluster = cl)
  
  # 3. Matrix-Mathe optimieren
  # Wir speichern die Differenz zwischen echten Werten und Vorhersage
  diff_Y <- Y - fit.wlg
  
  # Wir nutzen rowSums() anstelle von "%*% matrix(...)". 
  score_temp <- (diff_Y %*% INV_VAR) * diff_Y
  SCORE[, i] <- rowSums(score_temp)
}

# Cluster nach der Schleife wieder sauber schließen
stopCluster(cl) 

# --- WAHRSCHEINLICHKEITEN BERECHNEN ---
PROB <- exp(-SCORE)
PROB <- PROB / rowSums(PROB)

# Endzeitpunkt erfassen und Dauer berechnen
end_zeit <- Sys.time()
dauer <- end_zeit - start_zeit

cat("\nBerechnung abgeschlossen!\n")
cat("Gesamtdauer:\n")
print(dauer)

# --- ERGEBNISSE SPEICHERN ---


saveRDS(SCORE, "results_lin_disc/score_mat_zentral.rds")
saveRDS(PROB, "results_lin_disc/prob_mat_zentral.rds")

















# Mit anderem Prior (ZENTRAL UND AUSSERHALB)

# Ausserhalb
# Score matrix und data laden
data <- readRDS("results_lin_disc/data_aus.rds")
SCORE <- readRDS("results_lin_disc/score_mat_aus.rds")

prior.scale <- 100
levels_c <- levels(data$c)
k <- length(levels_c) # sollte 3 sein
N <- nrow(data)

# --- 1. PRIOR-MATRIX ERSTELLEN ---
# Startet mit einer Matrix aus 1en
PRIOR <- matrix(1, nrow = N, ncol = k)

# Für jede Spalte (Kategorie) erhöhen wir den Wert dort, 
# wo die Wohnung aktuell tatsächlich eingestuft ist
for (i in 1:k) {
  PRIOR[, i] <- PRIOR[, i] + prior.scale * (data$c == levels_c[i])
}

# --- 2. WAHRSCHEINLICHKEITEN MIT PRIOR BERECHNEN ---
# Wir nehmen die ursprünglichen Distanz-Scores (SCORE)
PROB.prior <- exp(-SCORE) * PRIOR

# Normierung, damit die Zeilensumme 1 ergibt (schnelle Variante)
PROB.prior <- PROB.prior / rowSums(PROB.prior)

# --- 3. AUSWERTUNG: ANTEIL DER KORREKTEN KLASSIFIZIERUNG ---
cat("\n--- Korrekt-Klassifizierungsrate (mit Prior-Scale", prior.scale, ") ---\n")
for (i in 1:k) {
  treffer_quote <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
  cat(levels_c[i], ": ", round(treffer_quote * 100, 2), "%\n", sep="")
}


# Zentral
# Score matrix  und data laden
data <- readRDS("results_lin_disc/data_zent.rds")
SCORE <- readRDS("results_lin_disc/score_mat_zentral.rds")

prior.scale <- 100
levels_c <- levels(data$c)
k <- length(levels_c) # sollte 3 sein
N <- nrow(data)

# --- 1. PRIOR-MATRIX ERSTELLEN ---
# Startet mit einer Matrix aus 1en
PRIOR <- matrix(1, nrow = N, ncol = k)

# Für jede Spalte (Kategorie) erhöhen wir den Wert dort, 
# wo die Wohnung aktuell tatsächlich eingestuft ist
for (i in 1:k) {
  PRIOR[, i] <- PRIOR[, i] + prior.scale * (data$c == levels_c[i])
}

# --- 2. WAHRSCHEINLICHKEITEN MIT PRIOR BERECHNEN ---
# Wir nehmen die ursprünglichen Distanz-Scores (SCORE)
PROB.prior <- exp(-SCORE) * PRIOR

# Normierung, damit die Zeilensumme 1 ergibt (schnelle Variante)
PROB.prior <- PROB.prior / rowSums(PROB.prior)

# --- 3. AUSWERTUNG: ANTEIL DER KORREKTEN KLASSIFIZIERUNG ---
cat("\n--- Korrekt-Klassifizierungsrate (mit Prior-Scale", prior.scale, ") ---\n")
for (i in 1:k) {
  treffer_quote <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
  cat(levels_c[i], ": ", round(treffer_quote * 100, 2), "%\n", sep="")
}




# VERSCHIEDENE PRIOR SCALE WERTE TESTEN
# --- SETUP ---
# Definiere hier die Prior-Werte, die du testen möchtest
prior_scales <- c(0,5, 10, 20, 30, 50, 70, 100, 200,  1000)

# Ein leerer Data Frame, der unsere Ergebnisse sammeln wird
results_df <- data.frame(
  Modell = character(),
  Prior_Scale = numeric(),
  Wohnlage = character(),
  Trefferquote_Prozent = numeric(),
  stringsAsFactors = FALSE
)

# --- HILFSFUNKTION FÜR DIE BERECHNUNG ---
evaluate_prior <- function(data, SCORE, model_name, p_scale) {
  levels_c <- levels(data$c)
  k <- length(levels_c)
  N <- nrow(data)
  
  # Numerische Stabilisierung
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  # Prior-Matrix erstellen
  PRIOR <- matrix(1, nrow = N, ncol = k)
  for (i in 1:k) {
    PRIOR[, i] <- PRIOR[, i] + p_scale * (data$c == levels_c[i])
  }
  
  # Wahrscheinlichkeiten berechnen
  PROB.prior <- exp(-SCORE_shifted) * PRIOR
  PROB.prior <- PROB.prior / rowSums(PROB.prior)
  
  # Ergebnisse für diesen Prior-Wert in einen temporären Data Frame schreiben
  temp_results <- data.frame(
    Modell = rep(model_name, k),
    Prior_Scale = rep(p_scale, k),
    Wohnlage = levels_c,
    Trefferquote_Prozent = numeric(k),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:k) {
    quote <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
    temp_results$Trefferquote_Prozent[i] <- round(quote * 100, 2)
  }
  
  return(temp_results)
}

# ==========================================
# 1. AUSWERTUNG: AUSSERHALB
# ==========================================
cat("Berechne Lagen AUSSERHALB...\n")
data_aus <- readRDS("results_lin_disc/data_aus.rds")
SCORE_aus <- readRDS("results_lin_disc/score_mat_aus.rds")

for (p in prior_scales) {
  res <- evaluate_prior(data_aus, SCORE_aus, "Ausserhalb", p)
  results_df <- rbind(results_df, res)
}

# RAM freimachen, bevor wir die nächsten großen Matrizen laden
rm(data_aus, SCORE_aus)
gc()

# ==========================================
# 2. AUSWERTUNG: ZENTRAL
# ==========================================
cat("Berechne Lagen ZENTRAL...\n")
data_zent <- readRDS("results_lin_disc/data_zent.rds")
SCORE_zent <- readRDS("results_lin_disc/score_mat_zentral.rds")

for (p in prior_scales) {
  res <- evaluate_prior(data_zent, SCORE_zent, "Zentral", p)
  results_df <- rbind(results_df, res)
}

# RAM wieder freimachen
rm(data_zent, SCORE_zent)
gc()

# ==========================================
# 3. ERGEBNISSE ANZEIGEN & SPEICHERN
# ==========================================
cat("\n--- FERTIG! Hier sind die Ergebnisse: ---\n")
print(results_df)

# Als RDS (für R) und als CSV (für Excel etc.) speichern
saveRDS(results_df, "results_lin_disc/prior_scale_vergleich.rds")
write.csv(results_df, "results_lin_disc/prior_scale_vergleich.csv", row.names = FALSE)
cat("\nErgebnisse gespeichert unter 'results_lin_disc/prior_scale_vergleich.csv'\n")






# Prior Klassifizierung zusammengelegte Wohnlagen
# --- SETUP ---
# Definiere hier die Prior-Werte, die du testen möchtest
prior_scales <- c(0, 5, 10, 20, 30, 50, 70, 100, 200, 1000)

# Ein leerer Data Frame, der unsere Ergebnisse sammeln wird
results_df <- data.frame(
  Modell = character(),
  Prior_Scale = numeric(),
  Wohnlage = character(),
  Trefferquote_Prozent = numeric(),
  stringsAsFactors = FALSE
)

# --- HILFSFUNKTION FÜR DIE BERECHNUNG (Bleibt identisch) ---
evaluate_prior <- function(data, SCORE, model_name, p_scale) {
  levels_c <- levels(data$c)
  k <- length(levels_c)
  N <- nrow(data)
  
  # Numerische Stabilisierung
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  # Prior-Matrix erstellen
  PRIOR <- matrix(1, nrow = N, ncol = k)
  for (i in 1:k) {
    PRIOR[, i] <- PRIOR[, i] + p_scale * (data$c == levels_c[i])
  }
  
  # Wahrscheinlichkeiten berechnen
  PROB.prior <- exp(-SCORE_shifted) * PRIOR
  PROB.prior <- PROB.prior / rowSums(PROB.prior)
  
  # Ergebnisse für diesen Prior-Wert in einen temporären Data Frame schreiben
  temp_results <- data.frame(
    Modell = rep(model_name, k),
    Prior_Scale = rep(p_scale, k),
    Wohnlage = levels_c,
    Trefferquote_Prozent = numeric(k),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:k) {
    quote <- mean(PROB.prior[data$c == levels_c[i], i] > 0.5)
    temp_results$Trefferquote_Prozent[i] <- round(quote * 100, 2)
  }
  
  return(temp_results)
}

# ==========================================
# AUSWERTUNG: GANZ MÜNCHEN (3 Kategorien)
# ==========================================
cat("Lese Daten für das zusammengeführte 3-Kategorien-Modell ein...\n")
data_gesamt <- readRDS("results_lin_disc/data_beides_3cat.rds")
SCORE_gesamt <- readRDS("results_lin_disc/score_mat_beides_3cat.rds")

cat("Berechne Raten für verschiedene Prior-Scales...\n")
for (p in prior_scales) {
  # Wir nennen das Modell im Data Frame "Gesamt_3Cat"
  res <- evaluate_prior(data_gesamt, SCORE_gesamt, "Gesamt_3Cat", p)
  results_df <- rbind(results_df, res)
}

# RAM freimachen
rm(data_gesamt, SCORE_gesamt)
gc()

# ==========================================
# ERGEBNISSE ANZEIGEN & SPEICHERN
# ==========================================
cat("\n--- FERTIG! Hier sind die Ergebnisse: ---\n")
print(results_df)

# Als RDS (für R) und als CSV (für Excel etc.) unter passendem Namen speichern
saveRDS(results_df, "results_lin_disc/prior_scale_vergleich_beides_3cat.rds")
write.csv(results_df, "results_lin_disc/prior_scale_vergleich_beides_3cat.csv", row.names = FALSE)
cat("\nErgebnisse gespeichert unter 'results_lin_disc/prior_scale_vergleich_beides_3cat.csv'\n")






# ==============================================================================
# BERECHNUNG UND SPEICHERUNG DER PROB.PRIOR MATRIZEN (Scale = 100)
# ==============================================================================

prior.scale <- 100

# --- 1. MODELL: AUSSERHALB ---
cat("Berechne und speichere Matrix für Lagen AUSSERHALB...\n")
data_aus <- readRDS("results_lin_disc/data_aus.rds")
SCORE_aus <- readRDS("results_lin_disc/score_mat_aus.rds")

k_aus <- length(levels(data_aus$c))
PRIOR_aus <- matrix(1, nrow = nrow(data_aus), ncol = k_aus)
for (i in 1:k_aus) {
  PRIOR_aus[, i] <- PRIOR_aus[, i] + prior.scale * (data_aus$c == levels(data_aus$c)[i])
}

SCORE_shifted_aus <- SCORE_aus - apply(SCORE_aus, 1, min)
PROB_aus <- exp(-SCORE_shifted_aus) * PRIOR_aus
PROB_aus <- PROB_aus / rowSums(PROB_aus)

saveRDS(PROB_aus, "results_lin_disc/PROB_prior_ausserhalb_scale_100.rds")
cat("✓ Gespeichert als 'PROB_prior_ausserhalb_scale_100.rds'\n\n")

rm(data_aus, SCORE_aus, PRIOR_aus, SCORE_shifted_aus, PROB_aus)
gc()


# --- 2. MODELL: ZENTRAL ---
cat("Berechne und speichere Matrix für Lagen ZENTRAL...\n")
data_zent <- readRDS("results_lin_disc/data_zent.rds")
SCORE_zent <- readRDS("results_lin_disc/score_mat_zentral.rds")

k_zent <- length(levels(data_zent$c))
PRIOR_zent <- matrix(1, nrow = nrow(data_zent), ncol = k_zent)
for (i in 1:k_zent) {
  PRIOR_zent[, i] <- PRIOR_zent[, i] + prior.scale * (data_zent$c == levels(data_zent$c)[i])
}

SCORE_shifted_zent <- SCORE_zent - apply(SCORE_zent, 1, min)
PROB_zent <- exp(-SCORE_shifted_zent) * PRIOR_zent
PROB_zent <- PROB_zent / rowSums(PROB_zent)

saveRDS(PROB_zent, "results_lin_disc/PROB_prior_zentral_scale_100.rds")
cat("✓ Gespeichert als 'PROB_prior_zentral_scale_100.rds'\n\n")

rm(data_zent, SCORE_zent, PRIOR_zent, SCORE_shifted_zent, PROB_zent)
gc()


# --- 3. MODELL: GESAMT (3 KATEGORIEN) ---
cat("Berechne und speichere Matrix für GESAMT MÜNCHEN (3 Kategorien)...\n")
data_gesamt <- readRDS("results_lin_disc/data_beides_3cat.rds")
SCORE_gesamt <- readRDS("results_lin_disc/score_mat_beides_3cat.rds")

k_gesamt <- length(levels(data_gesamt$c))
PRIOR_gesamt <- matrix(1, nrow = nrow(data_gesamt), ncol = k_gesamt)
for (i in 1:k_gesamt) {
  PRIOR_gesamt[, i] <- PRIOR_gesamt[, i] + prior.scale * (data_gesamt$c == levels(data_gesamt$c)[i])
}

SCORE_shifted_gesamt <- SCORE_gesamt - apply(SCORE_gesamt, 1, min)
PROB_gesamt <- exp(-SCORE_shifted_gesamt) * PRIOR_gesamt
PROB_gesamt <- PROB_gesamt / rowSums(PROB_gesamt)

saveRDS(PROB_gesamt, "results_lin_disc/PROB_prior_scale_100_beides_3cat.rds")
cat("✓ Gespeichert als 'PROB_prior_scale_100_beides_3cat.rds'\n\n")

rm(data_gesamt, SCORE_gesamt, PRIOR_gesamt, SCORE_shifted_gesamt, PROB_gesamt)
gc()




#'KARTEN'

library(leaflet)
library(dplyr)
library(htmlwidgets)
wohnlage_grenzen_wgs <- readRDS("daten/grenzen.rds")
wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds")

# 1. Daten und Matrix laden (passe den Dateinamen an deinen gewählten Prior an)
data_map1 <- readRDS("results_lin_disc/data_beides_3cat.rds")
PROB_map1 <- readRDS("results_lin_disc/PROB_prior_scale_100_beides_3cat.rds") # Beispiel!

levels_c <- levels(data_map1$c)

# 3. Dataframe für die Karte zusammenstellen (KORRIGIERT)
map_data_3cat <- data_map1 %>%
  mutate(
    # Wir holen uns die Wahrscheinlichkeiten jetzt DYNAMISCH über den Namen des Levels,
    # um alphabetische Sortierungsfehler von R zu vermeiden!
    prob_durchschnitt = PROB_map1[, which(levels_c == "durchschnittliche Lage")],
    prob_gute         = PROB_map1[, which(levels_c == "gute Lage")],
    prob_beste        = PROB_map1[, which(levels_c == "beste Lage")],
    
    # Echte Lage und Vorhersage direkt als Text
    Wohnlage_wahr = as.character(c),
    Wohnlage_vorhersage = levels_c[max.col(PROB_map1)],
    
    # War die Vorhersage korrekt?
    Korrekt = (Wohnlage_wahr == Wohnlage_vorhersage),
    
    # Farbe für die Karte zuweisen
    color = unname(wohnlage_farben_3[Wohnlage_vorhersage])
  )

# 4. HTML-Popups generieren (KORRIGIERT)
erstelle_popup <- function(df) {
  paste0(
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> <span style='color:", ifelse(df$Korrekt, "black", "red"), ";'>", 
    df$Wohnlage_vorhersage, "</span><br>",
    "<hr>",
    "<b>Berechnete Dichte-Wahrscheinlichkeiten:</b><br>",
    # Hier rufen wir jetzt die korrekt zugeordneten Spalten auf!
    "Durchschnittliche Lage: ", round(df$prob_durchschnitt * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gute * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    "<i>Infrastruktur-Werte:</i><br>",
    "Park (>10ha): ", df$y1, " m<br>",
    "Innenstadt: ", df$y2, " min<br>",
    "Haltestelle: ", df$y3, " min<br>",
    "Grundschule: ", df$y4, " m<br>",
    "Spielplatz: ", df$y5, " m<br>",
    "Kita: ", df$y6, " m<br>"
  )
}

map_data_3cat$popup_text <- erstelle_popup(map_data_3cat)


# 5. Aufteilen für den Layer-Switch
daten_korrekt <- map_data_3cat %>% filter(Korrekt == TRUE)
daten_fehler  <- map_data_3cat %>% filter(Korrekt == FALSE)

cat(paste("=>", nrow(daten_korrekt), "korrekte Vorhersagen,", nrow(daten_fehler), "Fehler.\n"))

# Hintergrund
#  Wir definieren unsere 3 Basis-Farben
wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

# 2. Flächen-Datensatz anpassen und Flächen zusammenlegen
wohnlagen_muc_wgs_3cat <- wohnlagen_muc_wgs %>%
  mutate(
    # "zentrale" aus dem Text entfernen und überflüssige Leerzeichen trimmen
    Wohnlage = trimws(gsub("zentrale", "", Wohnlage, ignore.case = TRUE)),
    
    # Neue Farbe basierend auf dem bereinigten Namen zuweisen
    color = unname(wohnlage_farben_3[Wohnlage])
  ) %>%
  # Flächen gleicher Kategorie zusammenfassen (löst innere Grenzen auf)
  group_by(Wohnlage, color) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cat("Flächen erfolgreich auf 3 Kategorien reduziert und zusammengeführt!\n")

# 6. LEAFLET KARTE (Ganz München)
karte_3cat <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Hintergrund-Polygone (Flächen) für das 3-Cat Modell
  addPolygons(
    data = wohnlagen_muc_wgs_3cat,  # <--- Hier das neue Objekt nutzen!
    fillColor = ~color,             # Zieht die soeben aktualisierten Farben
    fillOpacity = 0.6,
    color = "black",
    weight = 0.5,
    label = ~Wohnlage,
    group = "Wohnlagen (Flächen)"
  ) %>%
  # Umrisse
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5,
    group = "Wohnlagen (Flächen)"
  ) %>% 
  addCircleMarkers(
    data = daten_korrekt,
    lng = ~s.long, lat = ~s.lat,
    fillColor = ~color, fillOpacity = 0.9, color = "black", stroke = TRUE, weight = 1, radius = 6,
    popup = ~popup_text, group = "Korrekt"
  ) %>%
  addCircleMarkers(
    data = daten_fehler,
    lng = ~s.long, lat = ~s.lat,
    fillColor = ~color, fillOpacity = 1, color = "red", stroke = TRUE, weight = 2.5, radius = 7,
    popup = ~popup_text, group = "Fehler"
  ) %>%
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben_3), 
    labels = names(wohnlage_farben_3),
    title = "Vorhersage (3 Kategorien)", opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"), options = layersControlOptions(collapsed = FALSE))

if (!dir.exists("interaktive_karten")) dir.create("interaktive_karten")
saveWidget(karte_3cat, file = "results_lin_disc/karte_muenchen_3cat.html", selfcontained = TRUE)
cat("✓ Karte 1 erfolgreich gespeichert.\n")


# Karte getrennte modelle
# 1. Daten und Matrizen laden (Beispiel-Namen, passe sie an deine Prior-Files an)
data_aus <- readRDS("results_lin_disc/data_aus.rds")
PROB_aus <- readRDS("results_lin_disc/PROB_prior_ausserhalb_scale_100.rds") # Beispiel

data_zent <- readRDS("results_lin_disc/data_zent.rds")
PROB_zent <- readRDS("results_lin_disc/PROB_prior_zentral_scale_100.rds") # Beispiel

# 2. Farben für alle 6 Lagen definieren
wohnlage_farben_6 <- c(
  "durchschnittliche Lage"          = "#e8f5a4",
  "gute Lage"                       = "#afe391",
  "beste Lage"                      = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage"              = "#1f5a82",
  "zentrale beste Lage"             = "#271352"
)

levels_aus <- levels(data_aus$c)
levels_zent <- levels(data_zent$c)

# 3. Datensatz Außerhalb vorbereiten (KORRIGIERT)
map_data_aus <- data_aus %>%
  mutate(
    Wohnlage_wahr = as.character(c),
    Wohnlage_vorhersage = levels_aus[max.col(PROB_aus)],
    Korrekt = (Wohnlage_wahr == Wohnlage_vorhersage),
    
    # Dynamische Zuweisung statt harter Spalten-Nummern!
    prob_durchschnitt = PROB_aus[, which(levels_aus == "durchschnittliche Lage")],
    prob_gute         = PROB_aus[, which(levels_aus == "gute Lage")],
    prob_beste        = PROB_aus[, which(levels_aus == "beste Lage")]
  )

# 4. Datensatz Zentral vorbereiten (KORRIGIERT)
map_data_zent <- data_zent %>%
  mutate(
    # Für die Karte das "zentrale" wieder ankleben
    Wohnlage_wahr = paste("zentrale", as.character(c)),
    Wohnlage_vorhersage = paste("zentrale", levels_zent[max.col(PROB_zent)]),
    Korrekt = (Wohnlage_wahr == Wohnlage_vorhersage),
    
    # Dynamische Zuweisung!
    prob_durchschnitt = PROB_zent[, which(levels_zent == "durchschnittliche Lage")],
    prob_gute         = PROB_zent[, which(levels_zent == "gute Lage")],
    prob_beste        = PROB_zent[, which(levels_zent == "beste Lage")]
  )

# 5. Beide Datensätze für ganz München zusammenfügen
map_data_6cat <- bind_rows(map_data_aus, map_data_zent) %>%
  mutate(color = unname(wohnlage_farben_6[Wohnlage_vorhersage]))

# Popups generieren (nutzt die korrigierte Funktion erstelle_popup von eben!)
map_data_6cat$popup_text <- erstelle_popup(map_data_6cat)
# In Richtig und Falsch aufteilen
daten_korrekt_6 <- map_data_6cat %>% filter(Korrekt == TRUE)
daten_fehler_6  <- map_data_6cat %>% filter(Korrekt == FALSE)

cat(paste("=> (6 Kategorien)", nrow(daten_korrekt_6), "korrekte Vorhersagen,", nrow(daten_fehler_6), "Fehler.\n"))


# ==============================================================================
# 6. LEAFLET KARTE BAUEN
# ==============================================================================
cat("Zeichne Leaflet Karte...\n")

karte_6cat <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Hintergrund-Polygone (Flächen)
  addPolygons(
    data = wohnlagen_muc_wgs,
    fillColor = ~color,        # Besser nur ~color statt ~wohnlagen_muc_wgs$color nutzen
    fillOpacity = 0.6,
    color = "black",
    weight = 0.5,
    label = ~Wohnlage,
    group = "Wohnlagen (Flächen)"
  ) %>%
  
  # Umrisse
  addPolylines(
    data = wohnlage_grenzen_wgs, 
    color = "black", 
    weight = 0.5,
    group = "Wohnlagen (Flächen)"
  ) %>% 
  
  # --- HIER IST DAS HINZUGEFÜGTE %>% NACH DER LEGENDE ---
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben_6), # Geändert zu wohnlage_farben_6
    labels = names(wohnlage_farben_6),
    title = "Wohnlage (Hintergrund)",
    opacity = 1
  ) %>%
  
  # KORREKTE PUNKTE
  addCircleMarkers(
    data = daten_korrekt_6,
    lng = ~s.long, lat = ~s.lat,
    fillColor = ~color, 
    fillOpacity = 0.9, 
    color = "black", 
    stroke = TRUE, 
    weight = 1, 
    radius = 6,
    popup = ~popup_text, 
    group = "Korrekt"
  ) %>%
  
  # FEHLERHAFTE PUNKTE
  addCircleMarkers(
    data = daten_fehler_6,
    lng = ~s.long, lat = ~s.lat,
    fillColor = ~color, 
    fillOpacity = 1, 
    color = "red", 
    stroke = TRUE, 
    weight = 2.5, 
    radius = 7,
    popup = ~popup_text, 
    group = "Fehler"
  ) %>%
  
  # Ebenen-Steuerung (damit man die Punkte ein/ausschalten kann)
  addLayersControl(
    overlayGroups = c("Fehler", "Korrekt", "Wohnlagen (Flächen)"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Anzeigen und Speichern
print(karte_6cat)
saveWidget(karte_6cat, file = "results_lin_disc/karte_muenchen_6cat.html", selfcontained = TRUE)
cat("✓ Karte erfolgreich gespeichert.\n")









# Version mit regulariserung
library(mgcv)

# ==============================================================================
# 1. DATEN LADEN
# ==============================================================================
data <- readRDS("results_lin_disc/data_beides_3cat.rds")
data <- data[complete.cases(data), ]

# Zielvariable (bestehende Karte)
data$c <- as.factor(data$c)
levels_c <- levels(data$c)
k <- length(levels_c)

N <- nrow(data)
d <- 10

# ==============================================================================
# 2. MODELL SCHÄTZEN (LIKELIHOOD)
# ==============================================================================
model <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y8  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c
  ),
  family = mvn(d = d),
  data = data,
  optimizer = "efs",                       # Schnellerer Optimizer
  control = gam.control(trace = TRUE)      # Fortschrittsanzeige in der Konsole
)
saveRDS(model, "results_lin_disc/model_3cat_optimierung.rds")

# ==============================================================================
# 3. VARIANZSTRUKTUR
# ==============================================================================
VAR <- solve(crossprod(model$family$data$R))
INV_VAR <- solve(VAR)

# ==============================================================================
# 4. SCORE MATRIX (LIKELIHOOD)
# ==============================================================================
Y <- cbind(data$y1, data$y2, data$y3, data$y4, data$y5,
           data$y6, data$y7, data$y8, data$y9, data$y10)

SCORE <- matrix(0, nrow = N, ncol = k)

for (i in 1:k) {
  tmp <- data
  tmp$c <- levels_c[i]
  
  fit <- predict(model, newdata = tmp)
  
  diff <- Y - fit
  score <- (diff %*% INV_VAR) * diff
  SCORE[, i] <- rowSums(score)
}

# ==============================================================================
# 5. VORBERECHNUNG: AKTUELLE KLASSE UND DATENGETRIEBENE BESTKLASSE
# ==============================================================================

old_idx <- match(as.character(data$c), levels_c)

current_score <- SCORE[cbind(seq_len(N), old_idx)]

best_idx <- max.col(-SCORE)
best_score <- SCORE[cbind(seq_len(N), best_idx)]

potential_improvement <- current_score - best_score

cat("\nPotenzielle Verbesserung ohne Änderungskosten:\n")
print(summary(potential_improvement))

cat("\nÄnderungsrate ohne Änderungskosten:\n")
print(mean(best_idx != old_idx))


# ==============================================================================
# 6. ENTSCHEIDUNGSREGEL MIT ÄNDERUNGSKOSTEN
# ==============================================================================

apply_change_penalty_idx <- function(SCORE, old_idx, lambda) {
  
  SCORE_adj <- SCORE
  
  for (j in seq_len(ncol(SCORE))) {
    SCORE_adj[, j] <- SCORE[, j] + lambda * (j != old_idx)
  }
  
  pred_idx <- max.col(-SCORE_adj)
  
  return(pred_idx)
}


# ==============================================================================
# 7. LAMBDA-GITTER DEFINIEREN
# ==============================================================================

max_imp <- max(potential_improvement, na.rm = TRUE)

lambda_grid <- sort(unique(c(
  0,
  seq(0, max_imp, length.out = 300),
  seq(0, 10, by = 0.25),
  seq(10, 30, by = 1),
  seq(30, 100, by = 5)
)))

lambda_grid <- lambda_grid[lambda_grid >= 0]


# ==============================================================================
# 8. KANDIDATENKARTEN FÜR ALLE LAMBDA-WERTE BEWERTEN
# ==============================================================================

results_lambda <- data.frame()

for (lambda in lambda_grid) {
  
  pred_idx <- apply_change_penalty_idx(SCORE, old_idx, lambda)
  
  pred_score <- SCORE[cbind(seq_len(N), pred_idx)]
  
  change_rate <- mean(pred_idx != old_idx)
  
  realized_improvement <- mean(current_score - pred_score)
  
  results_lambda <- rbind(results_lambda, data.frame(
    lambda = lambda,
    change_rate = change_rate,
    realized_improvement = realized_improvement
  ))
}

results_lambda <- results_lambda %>%
  arrange(lambda)

print(results_lambda)


# ==============================================================================
# 9. ELBOW-PLOT RECHTER ANSATZ
# ==============================================================================

library(ggplot2)

elbow_plot <- ggplot(
  results_lambda,
  aes(
    x = change_rate * 100,
    y = realized_improvement,
    label = round(lambda, 2)
  )
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 1.25) +
  geom_text(
    vjust = -0.8,
    size = 3,
    check_overlap = TRUE
  ) +
  scale_x_reverse() +
  labs(
    title = "Rechter Ansatz: Datenverbesserung vs. Änderungsrate",
    x = "Änderungsrate gegenüber alter Karte (%)",
    y = "Realisierte SCORE Verbesserung",
    caption = "Labels zeigen lambda = Änderungskosten"
  ) +
  theme_minimal()

print(elbow_plot)


# ==============================================================================
# 10. AUTOMATISCHE ELBOW-AUSWAHL
# ==============================================================================

find_elbow <- function(df) {
  
  df <- df %>%
    arrange(change_rate)
  
  x <- df$change_rate
  y <- df$realized_improvement
  
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    stop("Elbow kann nicht bestimmt werden: change_rate oder improvement ist konstant.")
  }
  
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  x1 <- x_norm[1]
  y1 <- y_norm[1]
  x2 <- x_norm[length(x_norm)]
  y2 <- y_norm[length(y_norm)]
  
  distances <- abs((y2 - y1) * x_norm -
                     (x2 - x1) * y_norm +
                     x2 * y1 -
                     y2 * x1) /
    sqrt((y2 - y1)^2 + (x2 - x1)^2)
  
  df$elbow_distance <- distances
  
  df[which.max(df$elbow_distance), ]
}

elbow_point <- find_elbow(results_lambda)

cat("\n====================================\n")
cat("Automatisch gewähltes Elbow-Lambda:", elbow_point$lambda, "\n")
cat("Änderungsrate:", round(elbow_point$change_rate * 100, 2), "%\n")
cat("Realisierte Verbesserung:", round(elbow_point$realized_improvement, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 11. ALTERNATIVE AUSWAHL: MAXIMAL ERLAUBTE ÄNDERUNGSRATE
# ==============================================================================

target_max_change <- 0.10   # z.B. maximal 10% Änderungen

chosen_by_constraint <- results_lambda %>%
  filter(change_rate <= target_max_change) %>%
  slice_max(realized_improvement, n = 1, with_ties = FALSE)

cat("\n====================================\n")
cat("Auswahl mit maximal", target_max_change * 100, "% Änderungen\n")
cat("Gewähltes Lambda:", chosen_by_constraint$lambda, "\n")
cat("Änderungsrate:", round(chosen_by_constraint$change_rate * 100, 2), "%\n")
cat("Realisierte Verbesserung:", round(chosen_by_constraint$realized_improvement, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 12. FINALE KARTE
# ==============================================================================

# Variante A: automatische Elbow-Auswahl
# chosen_lambda <- elbow_point$lambda

# Variante B: maximale Änderungsrate
chosen_lambda <- chosen_by_constraint$lambda

final_idx <- apply_change_penalty_idx(SCORE, old_idx, chosen_lambda)

data$c_new <- levels_c[final_idx]
data$changed <- data$c_new != as.character(data$c)

data$score_current <- current_score
data$score_new <- SCORE[cbind(seq_len(N), final_idx)]
data$score_best <- best_score

data$potential_improvement <- potential_improvement
data$realized_improvement <- data$score_current - data$score_new

final_change_rate <- mean(data$changed)
final_improvement <- mean(data$realized_improvement)

cat("\n====================================\n")
cat("Final gewähltes Lambda:", chosen_lambda, "\n")
cat("Finale Änderungsrate:", round(final_change_rate * 100, 2), "%\n")
cat("Finale realisierte Verbesserung:", round(final_improvement, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 13. SPEICHERN
# ==============================================================================

saveRDS(data, "results_lin_disc/wohnlagenkarte_refined_ohne_alpha.rds")
saveRDS(results_lambda, "results_lin_disc/lambda_tradeoff_ohne_alpha.rds")
ggsave("results_lin_disc/elbow_plot_modifikation.png", plot = elbow_plot, width = 8, height = 5)





# Alternativer Ansatz links

# Anfang gleich

# ==============================================================================
# 4. GRUNDLEGENDE DIAGNOSE OHNE PRIOR
# ==============================================================================

data$c <- as.factor(data$c)

levels_c <- levels(data$c)
k <- length(levels_c)
N <- nrow(data)

old_idx <- match(as.character(data$c), levels_c)

current_score <- SCORE[cbind(seq_len(N), old_idx)]

best_idx <- max.col(-SCORE)
best_score <- SCORE[cbind(seq_len(N), best_idx)]

potential_improvement <- current_score - best_score

cat("\nPotenzielle Verbesserung ohne Prior:\n")
print(summary(potential_improvement))

cat("\nÄnderungsrate ohne Prior:\n")
cat(round(mean(best_idx != old_idx) * 100, 2), "%\n")


# ==============================================================================
# 5. PRIOR-ANSATZ MIT DELTA
# ==============================================================================

# Zusammenhang:
# delta = log(1 + prior_scale)
# prior_scale = exp(delta) - 1

apply_prior_delta <- function(SCORE, data, levels_c, delta) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  prior_scale <- exp(delta) - 1
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale * (as.character(data$c) == levels_c[j])
  }
  
  # Numerische Stabilisierung
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted) * PRIOR
  PROB <- PROB / rowSums(PROB)
  
  pred_idx <- max.col(PROB)
  pred <- levels_c[pred_idx]
  
  return(list(
    delta = delta,
    prior_scale = prior_scale,
    PROB = PROB,
    pred_idx = pred_idx,
    pred = pred
  ))
}


# ==============================================================================
# 6. DELTA-WERTE TESTEN
# ==============================================================================

delta_values <- c(
  0,
  0.25, 0.5, 0.75,
  1, 1.25, 1.5, 1.75,
  2, 2.25, 2.5, 2.75,
  3, 3.5, 4, 4.5,
  5, 6, 7.5, 10
)

results_prior_delta <- data.frame()

for (delta in delta_values) {
  
  res <- apply_prior_delta(
    SCORE = SCORE,
    data = data,
    levels_c = levels_c,
    delta = delta
  )
  
  pred_idx <- res$pred_idx
  pred_score <- SCORE[cbind(seq_len(N), pred_idx)]
  
  change_rate <- mean(pred_idx != old_idx)
  realized_improvement <- mean(current_score - pred_score)
  
  results_prior_delta <- rbind(results_prior_delta, data.frame(
    delta = delta,
    prior_scale = res$prior_scale,
    change_rate = change_rate,
    realized_improvement = realized_improvement
  ))
}

results_prior_delta <- results_prior_delta %>%
  arrange(delta)

cat("\nErgebnisse Prior-Ansatz mit delta:\n")
print(results_prior_delta)


# ==============================================================================
# 7. ELBOW-PLOT
# ==============================================================================

elbow_plot_delta <- ggplot(
  results_prior_delta,
  aes(
    x = change_rate * 100,
    y = realized_improvement,
    label = delta
  )
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 1.25)+
  geom_text(vjust = -0.8, size = 3) +
  scale_x_reverse() +
  labs(
    title = "Linker-Ansatz: Datenverbesserung vs. Änderungsrate",
    x = "Änderungsrate gegenüber alter Karte (%)",
    y = "Realisierte SCORE Verbesserung",
    caption = "Labels zeigen delta = log(1 + prior.scale)"
  ) +
  theme_minimal()

print(elbow_plot_delta)


# ==============================================================================
# 8. AUTOMATISCHE ELBOW-AUSWAHL
# ==============================================================================

find_elbow <- function(df) {
  
  df <- df %>%
    arrange(change_rate)
  
  x <- df$change_rate
  y <- df$realized_improvement
  
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    stop("Elbow kann nicht bestimmt werden: change_rate oder improvement ist konstant.")
  }
  
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  x1 <- x_norm[1]
  y1 <- y_norm[1]
  x2 <- x_norm[length(x_norm)]
  y2 <- y_norm[length(y_norm)]
  
  distances <- abs(
    (y2 - y1) * x_norm -
      (x2 - x1) * y_norm +
      x2 * y1 -
      y2 * x1
  ) / sqrt((y2 - y1)^2 + (x2 - x1)^2)
  
  df$elbow_distance <- distances
  
  df[which.max(df$elbow_distance), ]
}

elbow_point_delta <- find_elbow(results_prior_delta)

cat("\n====================================\n")
cat("Automatisch gewähltes Elbow-Delta:", elbow_point_delta$delta, "\n")
cat("Entsprechender Prior Scale:", round(elbow_point_delta$prior_scale, 4), "\n")
cat("Änderungsrate:", round(elbow_point_delta$change_rate * 100, 2), "%\n")
cat("Realisierte Verbesserung:", round(elbow_point_delta$realized_improvement, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 9. ALTERNATIVE AUSWAHL: MAXIMAL ERLAUBTE ÄNDERUNGSRATE
# ==============================================================================

# Beispiel: maximal 10% Änderungen erlauben
target_max_change <- 0.10

chosen_by_constraint_delta <- results_prior_delta %>%
  filter(change_rate <= target_max_change) %>%
  slice_max(realized_improvement, n = 1, with_ties = FALSE)

cat("\n====================================\n")
cat("Auswahl mit maximal", target_max_change * 100, "% Änderungen\n")
cat("Gewähltes Delta:", chosen_by_constraint_delta$delta, "\n")
cat("Entsprechender Prior Scale:", round(chosen_by_constraint_delta$prior_scale, 4), "\n")
cat("Änderungsrate:", round(chosen_by_constraint_delta$change_rate * 100, 2), "%\n")
cat("Realisierte Verbesserung:", round(chosen_by_constraint_delta$realized_improvement, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 10. FINALE AUSWAHL
# ==============================================================================

# Variante A: automatische Elbow-Auswahl
# chosen_delta <- elbow_point_delta$delta

# Variante B: fachliche Nebenbedingung, z.B. maximal 10% Änderungen
chosen_delta <- chosen_by_constraint_delta$delta

final_res_delta <- apply_prior_delta(
  SCORE = SCORE,
  data = data,
  levels_c = levels_c,
  delta = chosen_delta
)

PROB_final_delta <- final_res_delta$PROB
final_idx_delta <- final_res_delta$pred_idx

data$c_new_delta <- final_res_delta$pred
data$changed_delta <- data$c_new_delta != as.character(data$c)

data$score_current <- current_score
data$score_new_delta <- SCORE[cbind(seq_len(N), final_idx_delta)]
data$score_best <- best_score

data$potential_improvement <- potential_improvement
data$realized_improvement_delta <- data$score_current - data$score_new_delta

data$prob_max_delta <- apply(PROB_final_delta, 1, max)

# Wahrscheinlichkeiten je Klasse abspeichern
for (j in seq_len(k)) {
  prob_name <- paste0("prob_delta_", make.names(levels_c[j]))
  data[[prob_name]] <- PROB_final_delta[, j]
}

final_change_rate_delta <- mean(data$changed_delta)
final_improvement_delta <- mean(data$realized_improvement_delta)

cat("\n====================================\n")
cat("Final gewähltes Delta:", chosen_delta, "\n")
cat("Finaler Prior Scale:", round(final_res_delta$prior_scale, 4), "\n")
cat("Finale Änderungsrate:", round(final_change_rate_delta * 100, 2), "%\n")
cat("Finale realisierte Verbesserung:", round(final_improvement_delta, 4), "\n")
cat("====================================\n")


# ==============================================================================
# 11. SPEICHERN
# ==============================================================================

saveRDS(data, "results_lin_disc/wohnlagenkarte_prior_delta_refined.rds")
saveRDS(PROB_final_delta, "results_lin_disc/PROB_prior_delta_final.rds")
saveRDS(results_prior_delta, "results_lin_disc/delta_prior_tradeoff.rds")
ggsave("results_lin_disc/elbow_plot_modifikation_delta.png", plot = elbow_plot_delta, width = 8, height = 5)

cat("\nGespeichert:\n")
cat("- results_lin_disc/wohnlagenkarte_prior_delta_refined.rds\n")
cat("- results_lin_disc/PROB_prior_delta_final.rds\n")
cat("- results_lin_disc/delta_prior_tradeoff.rds\n")