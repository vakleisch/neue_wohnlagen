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

data <- model_munich_data2[complete.cases(model_munich_data2), ]

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
data$y11 <- data$laerm

d <- 11  # Anzahl der Variablen auf 10 gesetzt


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
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c,
    y11 ~ s(s.long, s.lat, by = c, k = 15) + c
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
           data$y6, data$y7, data$y9, data$y10,
           data$y11)

SCORE <- matrix(0, nrow = N, ncol = k)

for (i in 1:k) {
  tmp <- data
  tmp$c <- levels_c[i]
  
  fit <- predict(model, newdata = tmp)
  
  diff <- Y - fit
  score <- (diff %*% INV_VAR) * diff
  SCORE[, i] <- rowSums(score)
}

saveRDS(SCORE, "results_lin_disc/SCORE_matrix_final_model.rds")


# AB HIER AUSFÜHREN WENN MODELL GLEICH BLEIBEN SOLL
SCORE <- readRDS("results_lin_disc/SCORE_matrix_final_model.rds")

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

saveRDS(data, "results_lin_disc/wohnlagenkarte_refined_lamda.rds")
saveRDS(results_lambda, "results_lin_disc/lambda_tradeoff.rds")
ggsave("results_lin_disc/elbow_plot_modifikation_lamda.png", plot = elbow_plot, width = 8, height = 5)





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






# Vergleiche Modell mit und ohne lärm
model1 <- readRDS("results_lin_disc/model_3cat_optimierung.rds")
model2 <- readRDS("modelle/lindisc_model_beides_3cat.rds")
data <- readRDS("results_lin_disc/data_beides_3cat.rds")
library(dplyr)

# ==============================================================================
# HILFSFUNKTION: SCORE, PREDICTION, ACCURACY, CONFUSION MATRIX
# ==============================================================================

evaluate_model_accuracy <- function(model, data, y_vars, class_var = "c") {
  
  # Klassen
  levels_c <- levels(as.factor(data[[class_var]]))
  k <- length(levels_c)
  N <- nrow(data)
  d <- length(y_vars)
  
  # Response-Matrix
  Y <- as.matrix(data[, y_vars])
  
  # Kovarianzmatrix aus dem Modell
  VAR <- solve(crossprod(model$family$data$R))
  INV_VAR <- solve(VAR)
  
  # Score-Matrix
  SCORE <- matrix(0, nrow = N, ncol = k)
  colnames(SCORE) <- levels_c
  
  for (j in seq_len(k)) {
    
    tmp <- data
    tmp[[class_var]] <- levels_c[j]
    
    fit <- predict(model, newdata = tmp)
    
    diff <- Y - fit
    
    score_temp <- (diff %*% INV_VAR) * diff
    SCORE[, j] <- rowSums(score_temp)
  }
  
  # Wahrscheinlichkeit aus Scores
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted)
  PROB <- PROB / rowSums(PROB)
  
  colnames(PROB) <- levels_c
  
  # Vorhersage
  pred_idx <- max.col(PROB)
  pred <- factor(levels_c[pred_idx], levels = levels_c)
  
  truth <- factor(as.character(data[[class_var]]), levels = levels_c)
  
  # Accuracy
  accuracy <- mean(pred == truth)
  
  # Confusion Matrix
  conf_mat <- table(
    Wahrheit = truth,
    Vorhersage = pred
  )
  
  # Klassenweise Accuracy / Trefferquote
  class_accuracy <- diag(prop.table(conf_mat, margin = 1))
  
  class_accuracy_df <- data.frame(
    Wohnlage = names(class_accuracy),
    Trefferquote = as.numeric(class_accuracy)
  )
  
  return(list(
    SCORE = SCORE,
    PROB = PROB,
    prediction = pred,
    truth = truth,
    accuracy = accuracy,
    confusion_matrix = conf_mat,
    class_accuracy = class_accuracy_df
  ))
}


y_vars_model1 <- c(
  "y1", "y2", "y3", "y4", "y5",
  "y6", "y7", "y9", "y10",
  "y11"
)

eval_model1 <- evaluate_model_accuracy(
  model = model1,
  data = data,
  y_vars = y_vars_model1,
  class_var = "c"
)

cat("\n==============================\n")
cat("MODEL 1 MIT LÄRM\n")
cat("==============================\n")
cat("Training Accuracy:", round(eval_model1$accuracy * 100, 2), "%\n\n")

print(eval_model1$confusion_matrix)
print(eval_model1$class_accuracy)


#y_vars_model2 <- c(
#  "y1", "y2", "y3", "y4", "y5",
#  "y6", "y7", "y8", "y9", "y10"
#)

##eval_model2 <- evaluate_model_accuracy(
#  model = model2,
#  data = data,
#  y_vars = y_vars_model2,
#  class_var = "c"
#)

#cat("\n==============================\n")
#cat("MODEL 2 OHNE LÄRM\n")
#cat("==============================\n")
#cat("Training Accuracy:", round(eval_model2$accuracy * 100, 2), "%\n\n")

#print(eval_model2$confusion_matrix)
#print(eval_model2$class_accuracy)


#vergleich <- data.frame(
#  Modell = c("Model1_mit_Laerm", "Model2_ohne_Laerm"),
#  Accuracy = c(eval_model1$accuracy, eval_model2$accuracy),
#  Accuracy_Prozent = round(c(eval_model1$accuracy, eval_model2$accuracy) * 100, 2)
#)

#print(vergleich)


# Interaktive karten (mit und ohne lärm ohne prior)
library(mgcv)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)
library(parallel)

# ==============================================================================
# 1. BASISDATEN
# ==============================================================================

wohnlage_grenzen_wgs <- readRDS("daten/grenzen.rds")
wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds")

# Basisdatensatz ist weiterhin data
data$c <- as.factor(data$c)

levels_c <- levels(data$c)
k <- length(levels_c)
N <- nrow(data)

# ==============================================================================
# 2. VARIABLEN FÜR DIE BEIDEN MODELLE DEFINIEREN
# ==============================================================================

# Modell 1: MIT Lärm
# Bitte anpassen, falls deine Lärmvariable anders heißt, z.B. "y11" oder "laerm_eisenbahn"
y_vars_model1 <- c(
  "y1", "y2", "y3", "y4", "y5",
  "y6", "y7", "y9", "y10",
  "y11"   # <- falls Lärm als y11 im Modell ist
)

# Modell 2: OHNE Lärm
y_vars_model2 <- c(
  "y1", "y2", "y3", "y4", "y5",
  "y6", "y7", "y8", "y9", "y10"
)

# ==============================================================================
# 3. FARBEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

# ==============================================================================
# 4. FLÄCHEN AUF 3 KATEGORIEN REDUZIEREN
# ==============================================================================

wohnlagen_muc_wgs_3cat <- wohnlagen_muc_wgs %>%
  mutate(
    Wohnlage = trimws(gsub("zentrale", "", Wohnlage, ignore.case = TRUE)),
    color = unname(wohnlage_farben_3[Wohnlage])
  ) %>%
  group_by(Wohnlage, color) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cat("Flächen erfolgreich auf 3 Kategorien reduziert und zusammengeführt.\n")


# ==============================================================================
# 5. FUNKTION: SCORE UND PROB FÜR EIN MODELL BERECHNEN
# ==============================================================================

compute_prob_from_model <- function(model, data, y_vars, class_var = "c", cl = NULL) {
  
  levels_c <- levels(as.factor(data[[class_var]]))
  k <- length(levels_c)
  N <- nrow(data)
  
  Y <- as.matrix(data[, y_vars])
  
  VAR <- solve(crossprod(model$family$data$R))
  INV_VAR <- solve(VAR)
  
  SCORE <- matrix(0, nrow = N, ncol = k)
  colnames(SCORE) <- levels_c
  
  for (j in seq_len(k)) {
    
    cat("Berechne Scores für Klasse", j, "von", k, ":", levels_c[j], "\n")
    
    tmp <- data
    tmp[[class_var]] <- levels_c[j]
    
    if (is.null(cl)) {
      fit <- predict(model, newdata = tmp)
    } else {
      fit <- predict(model, newdata = tmp, cluster = cl)
    }
    
    diff <- Y - fit
    
    score_temp <- (diff %*% INV_VAR) * diff
    SCORE[, j] <- rowSums(score_temp)
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted)
  PROB <- PROB / rowSums(PROB)
  colnames(PROB) <- levels_c
  
  return(list(
    SCORE = SCORE,
    PROB = PROB
  ))
}

# ==============================================================================
# 6. FUNKTION: LEAFLET-KARTE FÜR EIN MODELL ERSTELLEN
# ==============================================================================

create_model_map <- function(data_map,
                             PROB,
                             model_label,
                             output_file) {
  
  levels_c <- levels(data_map$c)
  
  # Dynamische Wahrscheinlichkeiten
  data_map <- data_map %>%
    mutate(
      prob_durchschnitt = PROB[, which(levels_c == "durchschnittliche Lage")],
      prob_gute         = PROB[, which(levels_c == "gute Lage")],
      prob_beste        = PROB[, which(levels_c == "beste Lage")],
      
      Wohnlage_wahr = as.character(c),
      Wohnlage_vorhersage = levels_c[max.col(PROB)],
      
      Korrekt = Wohnlage_wahr == Wohnlage_vorhersage,
      
      prob_max = apply(PROB, 1, max),
      
      color = unname(wohnlage_farben_3[Wohnlage_vorhersage])
    )
  
  # Popups
  erstelle_popup <- function(df) {
    paste0(
      "<b>Modell:</b> ", model_label, "<br>",
      "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
      "<b>Vorhersage:</b> <span style='color:",
      ifelse(df$Korrekt, "black", "red"),
      ";'>", df$Wohnlage_vorhersage, "</span><br>",
      "<b>Max. Wahrscheinlichkeit:</b> ", round(df$prob_max * 100, 1), " %<br>",
      "<hr>",
      
      "<b>Berechnete Dichte-Wahrscheinlichkeiten:</b><br>",
      "Durchschnittliche Lage: ", round(df$prob_durchschnitt * 100, 1), " %<br>",
      "Gute Lage: ", round(df$prob_gute * 100, 1), " %<br>",
      "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
      "<hr>",
      
      "<i>Infrastruktur-Werte:</i><br>",
      "y1: ", df$y1, "<br>",
      "y2: ", df$y2, "<br>",
      "y3: ", df$y3, "<br>",
      "y4: ", df$y4, "<br>",
      "y5: ", df$y5, "<br>",
      "y6: ", df$y6, "<br>",
      "y7: ", df$y7, "<br>",
      "y8: ", df$y8, "<br>",
      "y9: ", df$y9, "<br>",
      "y10: ", df$y10, "<br>"
    )
  }
  
  data_map$popup_text <- erstelle_popup(data_map)
  
  daten_korrekt <- data_map %>% filter(Korrekt == TRUE)
  daten_fehler  <- data_map %>% filter(Korrekt == FALSE)
  
  accuracy <- mean(data_map$Korrekt)
  
  cat("\n", model_label, "\n")
  cat("Accuracy:", round(accuracy * 100, 2), "%\n")
  cat("=>", nrow(daten_korrekt), "korrekte Vorhersagen,",
      nrow(daten_fehler), "Fehler.\n")
  
  # Karte
  karte <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    
    addPolygons(
      data = wohnlagen_muc_wgs_3cat,
      fillColor = ~color,
      fillOpacity = 0.6,
      color = "black",
      weight = 0.5,
      label = ~Wohnlage,
      group = "Wohnlagen (Flächen)"
    ) %>%
    
    addPolylines(
      data = wohnlage_grenzen_wgs,
      color = "black",
      weight = 0.5,
      group = "Wohnlagen (Grenzen)"
    ) %>%
    
    addCircleMarkers(
      data = daten_korrekt,
      lng = ~s.long,
      lat = ~s.lat,
      fillColor = ~color,
      fillOpacity = 0.9,
      color = "black",
      stroke = TRUE,
      weight = 1,
      radius = 5,
      popup = ~popup_text,
      group = "Korrekt"
    ) %>%
    
    addCircleMarkers(
      data = daten_fehler,
      lng = ~s.long,
      lat = ~s.lat,
      fillColor = ~color,
      fillOpacity = 1,
      color = "red",
      stroke = TRUE,
      weight = 2.5,
      radius = 7,
      popup = ~popup_text,
      group = "Fehler"
    ) %>%
    
    addLegend(
      position = "bottomright",
      colors = unname(wohnlage_farben_3),
      labels = names(wohnlage_farben_3),
      title = paste0("Vorhersage<br>", model_label),
      opacity = 1
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Wohnlagen (Flächen)", "Wohnlagen (Grenzen)", "Fehler", "Korrekt"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  if (!dir.exists("results_lin_disc")) dir.create("results_lin_disc")
  
  saveWidget(karte, file = output_file, selfcontained = TRUE)
  
  cat("✓ Karte gespeichert unter:", output_file, "\n")
  
  return(list(
    map_data = data_map,
    map = karte,
    accuracy = accuracy
  ))
}

# ==============================================================================
# 7. SCORE UND PROB FÜR BEIDE MODELLE BERECHNEN
# ==============================================================================

cl <- makeCluster(max(1, detectCores() - 1))

cat("\nBerechne Wahrscheinlichkeiten für Model 1 mit Lärm...\n")

res_model1 <- compute_prob_from_model(
  model = model1,
  data = data,
  y_vars = y_vars_model1,
  class_var = "c",
  cl = cl
)

cat("\nBerechne Wahrscheinlichkeiten für Model 2 ohne Lärm...\n")

#res_model2 <- compute_prob_from_model(
#  model = model2,
#  data = data,
#  y_vars = y_vars_model2,
#  class_var = "c",
#  cl = cl
#)

stopCluster(cl)

# ==============================================================================
# 8. KARTEN ERSTELLEN
# ==============================================================================

karte_model1 <- create_model_map(
  data_map = data,
  PROB = res_model1$PROB,
  model_label = "Model 1 mit Lärm",
  output_file = "results_lin_disc/karte_model1_mit_laerm.html"
)

#karte_model2 <- create_model_map(
#  data_map = data,
#  PROB = res_model2$PROB,
#  model_label = "Model 2 ohne Lärm",
#  output_file = "results_lin_disc/karte_model2_ohne_laerm.html"
#)

# ==============================================================================
# 9. ERGEBNISSE SPEICHERN
# ==============================================================================

saveRDS(res_model1$SCORE, "results_lin_disc/SCORE_model1_mit_laerm.rds")
saveRDS(res_model1$PROB,  "results_lin_disc/PROB_model1_mit_laerm.rds")

#saveRDS(res_model2$SCORE, "results_lin_disc/SCORE_model2_ohne_laerm.rds")
#saveRDS(res_model2$PROB,  "results_lin_disc/PROB_model2_ohne_laerm.rds")

saveRDS(karte_model1$map_data, "results_lin_disc/map_data_model1_mit_laerm.rds")
#saveRDS(karte_model2$map_data, "results_lin_disc/map_data_model2_ohne_laerm.rds")

#vergleich_accuracy <- data.frame(
#  Modell = c("Model 1 mit Lärm", "Model 2 ohne Lärm"),
 # Accuracy = c(karte_model1$accuracy, karte_model2$accuracy),
#  Accuracy_Prozent = round(c(karte_model1$accuracy, karte_model2$accuracy) * 100, 2)
#)

#print(vergleich_accuracy)

#write.csv(
#  vergleich_accuracy,
#  "results_lin_disc/accuracy_vergleich_model1_model2.csv",
#  row.names = FALSE
#)



# Nochmla überarbeitet
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)

# ==============================================================================
# 1. RÄUMLICHE DATEN LADEN
# ==============================================================================

wohnlage_grenzen_wgs <- readRDS("daten/grenzen.rds")
wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds")

# Falls data$c noch kein Faktor ist
data$c <- as.factor(data$c)
levels_c <- levels(data$c)

# ==============================================================================
# 2. FARBEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

# ==============================================================================
# 3. FLÄCHEN AUF 3 KATEGORIEN REDUZIEREN
# ==============================================================================

wohnlagen_muc_wgs_3cat <- wohnlagen_muc_wgs %>%
  mutate(
    Wohnlage = trimws(gsub("zentrale", "", Wohnlage, ignore.case = TRUE)),
    color = unname(wohnlage_farben_3[Wohnlage])
  ) %>%
  group_by(Wohnlage, color) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# ==============================================================================
# 4. MAP-DATEN FÜR MODEL 1 AUFBAUEN
#    WICHTIG: hier wird nur res_model1$PROB verwendet, nichts neu berechnet
# ==============================================================================

PROB_map1 <- res_model1$PROB

map_data_model1 <- data %>%
  mutate(
    prob_durchschnitt = PROB_map1[, which(levels_c == "durchschnittliche Lage")],
    prob_gute         = PROB_map1[, which(levels_c == "gute Lage")],
    prob_beste        = PROB_map1[, which(levels_c == "beste Lage")],
    
    Wohnlage_wahr = as.character(c),
    Wohnlage_vorhersage = levels_c[max.col(PROB_map1)],
    
    Korrekt = (Wohnlage_wahr == Wohnlage_vorhersage),
    
    prob_max = apply(PROB_map1, 1, max),
    
    color = unname(wohnlage_farben_3[Wohnlage_vorhersage])
  )

# ==============================================================================
# 5. POPUPS MIT ANSCHAULICHEN LABELS + LÄRM
# ==============================================================================

erstelle_popup_model1 <- function(df) {
  paste0(
    "<b>Modell:</b> Model 1 mit Lärm<br>",
    "<b>Wahre Lage:</b> ", df$Wohnlage_wahr, "<br>",
    "<b>Vorhersage:</b> <span style='color:",
    ifelse(df$Korrekt, "black", "red"),
    ";'>", df$Wohnlage_vorhersage, "</span><br>",
    "<b>Max. Wahrscheinlichkeit:</b> ", round(df$prob_max * 100, 1), " %<br>",
    "<hr>",
    
    "<b>Berechnete Dichte-Wahrscheinlichkeiten:</b><br>",
    "Durchschnittliche Lage: ", round(df$prob_durchschnitt * 100, 1), " %<br>",
    "Gute Lage: ", round(df$prob_gute * 100, 1), " %<br>",
    "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
    "<hr>",
    
    "<i>Infrastruktur-/Lagewerte:</i><br>",
    "Park (&gt;10ha): ", df$y1, " m<br>",
    "Innenstadt: ", df$y2, " min<br>",
    "Haltestelle: ", df$y3, " min<br>",
    "Grundschule: ", df$y4, "<br>",
    "Spielplatz: ", df$y5, "<br>",
    "Kita: ", df$y6, "<br>",
    "Ortszentrum: ", df$y7, "<br>",
    "BRW (log): ", round(df$y8, 3), "<br>",
    "Anteil VF SV: ", round(df$y9, 3), "<br>",
    "Anteil GF SV: ", round(df$y10, 3), "<br>",
    "Lärm: ", df$y11, "<br>"
  )
}

map_data_model1$popup_text <- erstelle_popup_model1(map_data_model1)

# ==============================================================================
# 6. AUFTEILEN IN KORREKT / FEHLER
# ==============================================================================

daten_korrekt_model1 <- map_data_model1 %>% filter(Korrekt == TRUE)
daten_fehler_model1  <- map_data_model1 %>% filter(Korrekt == FALSE)

cat("Model 1 mit Lärm:\n")
cat("=>", nrow(daten_korrekt_model1), "korrekte Vorhersagen,",
    nrow(daten_fehler_model1), "Fehler.\n")

# ==============================================================================
# 7. LEAFLET-KARTE FÜR MODEL 1
# ==============================================================================

karte_model1_neu <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = wohnlagen_muc_wgs_3cat,
    fillColor = ~color,
    fillOpacity = 0.6,
    color = "black",
    weight = 0.5,
    label = ~Wohnlage,
    group = "Wohnlagen (Flächen)"
  ) %>%
  
  addPolylines(
    data = wohnlage_grenzen_wgs,
    color = "black",
    weight = 0.5,
    group = "Wohnlagen (Flächen)"
  ) %>%
  
  addCircleMarkers(
    data = daten_korrekt_model1,
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
  
  addCircleMarkers(
    data = daten_fehler_model1,
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
  
  addLegend(
    position = "bottomright",
    colors = unname(wohnlage_farben_3),
    labels = names(wohnlage_farben_3),
    title = "Vorhersage<br>Model 1 mit Lärm",
    opacity = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Wohnlagen (Flächen)", "Fehler", "Korrekt"),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(
  karte_model1_neu,
  file = "results_lin_disc/karte_model1_mit_laerm_popup_neu.html",
  selfcontained = TRUE
)

cat("✓ Neue Karte für Model 1 mit Lärm gespeichert.\n")












# Interaktive karte finales Modell mit prior 
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)

# ==============================================================================
# 0. EINSTELLUNGEN
# ==============================================================================

chosen_lambda <- 2.75

prior_scale <- exp(chosen_lambda) - 1

cat("Gewähltes lambda:", chosen_lambda, "\n")
cat("Daraus berechneter prior.scale:", round(prior_scale, 4), "\n")

# ==============================================================================
# 1. DATEN LADEN / VORBEREITEN
# ==============================================================================

data <- readRDS("results_lin_disc/data_beides_3cat.rds")
wohnlage_grenzen_wgs <- readRDS("daten/grenzen.rds")
wohnlagen_muc_wgs   <- readRDS("daten/wohnlagen_flächen.rds")

# Basisdatensatz ist weiterhin data
data$c <- as.factor(data$c)

levels_c <- levels(data$c)
k <- length(levels_c)
N <- nrow(data)

# ==============================================================================
# 2. SCORE-MATRIZEN LADEN
# ==============================================================================

# Passe Dateinamen an, falls sie bei dir anders heißen
SCORE_model1 <- readRDS("results_lin_disc/SCORE_model1_mit_laerm.rds")
SCORE_model2 <- readRDS("results_lin_disc/SCORE_model2_ohne_laerm.rds")

colnames(SCORE_model1) <- levels_c
colnames(SCORE_model2) <- levels_c

# ==============================================================================
# 3. FARBEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)

# ==============================================================================
# 4. FLÄCHEN AUF 3 KATEGORIEN REDUZIEREN
# ==============================================================================

wohnlagen_muc_wgs_3cat <- wohnlagen_muc_wgs %>%
  mutate(
    Wohnlage = trimws(gsub("zentrale", "", Wohnlage, ignore.case = TRUE)),
    color = unname(wohnlage_farben_3[Wohnlage])
  ) %>%
  group_by(Wohnlage, color) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cat("Flächen erfolgreich auf 3 Kategorien reduziert und zusammengeführt.\n")

# ==============================================================================
# 5. FUNKTION: PRIOR-TRANSFORMATION
# ==============================================================================

apply_prior_lambda <- function(SCORE, data, levels_c, lambda) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  prior_scale <- exp(lambda) - 1
  
  # Prior-Matrix
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale * (as.character(data$c) == levels_c[j])
  }
  
  # Numerische Stabilisierung
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  # Posterior-artige Wahrscheinlichkeiten
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  
  colnames(PROB_prior) <- levels_c
  
  pred_idx <- max.col(PROB_prior)
  pred <- levels_c[pred_idx]
  
  old_idx <- match(as.character(data$c), levels_c)
  
  current_score <- SCORE[cbind(seq_len(N), old_idx)]
  new_score     <- SCORE[cbind(seq_len(N), pred_idx)]
  best_score    <- apply(SCORE, 1, min)
  
  return(list(
    lambda = lambda,
    prior_scale = prior_scale,
    PROB = PROB_prior,
    pred_idx = pred_idx,
    pred = pred,
    current_score = current_score,
    new_score = new_score,
    best_score = best_score,
    realized_improvement = current_score - new_score,
    potential_improvement = current_score - best_score
  ))
}

# ==============================================================================
# 6. FUNKTION: LEAFLET-KARTE NACH PRIOR-TRANSFORMATION
# ==============================================================================

create_prior_map <- function(data_base,
                             prior_res,
                             model_label,
                             output_file,
                             include_laerm = FALSE) {
  
  PROB <- prior_res$PROB
  levels_c <- levels(data_base$c)
  
  map_data <- data_base %>%
    mutate(
      prob_durchschnitt = PROB[, which(levels_c == "durchschnittliche Lage")],
      prob_gute         = PROB[, which(levels_c == "gute Lage")],
      prob_beste        = PROB[, which(levels_c == "beste Lage")],
      
      Wohnlage_alt = as.character(c),
      Wohnlage_neu = prior_res$pred,
      
      Geaendert = Wohnlage_alt != Wohnlage_neu,
      
      prob_max = apply(PROB, 1, max),
      
      score_alt = prior_res$current_score,
      score_neu = prior_res$new_score,
      score_best = prior_res$best_score,
      realized_improvement = prior_res$realized_improvement,
      potential_improvement = prior_res$potential_improvement,
      
      color = unname(wohnlage_farben_3[Wohnlage_neu])
    )
  
  # Optional schöner Lärm-Text
  if (include_laerm && "y11" %in% names(map_data)) {
    map_data <- map_data %>%
      mutate(
        Laerm_Label = case_when(
          y11 == 1 ~ "1 - sehr gering",
          y11 == 2 ~ "2 - gering",
          y11 == 3 ~ "3 - mittel",
          y11 == 4 ~ "4 - hoch",
          y11 == 5 ~ "5 - sehr hoch",
          TRUE ~ as.character(y11)
        )
      )
  }
  
  # Popups
  erstelle_popup <- function(df) {
    
    laerm_text <- ""
    
    if (include_laerm && "y11" %in% names(df)) {
      laerm_text <- paste0(
        "Lärm: ", df$Laerm_Label, "<br>"
      )
    }
    
    paste0(
      "<b>Alte Lage:</b> ", df$Wohnlage_alt, "<br>",
      "<b>Neue Lage:</b> <span style='color:",
      ifelse(df$Geaendert, "red", "black"),
      ";'><b>", df$Wohnlage_neu, "</b></span><br>",
      "<b>Geändert:</b> ", ifelse(df$Geaendert, "JA", "nein"), "<br>",
      "<hr>",
      
      "<b>Prior-transformierte Wahrscheinlichkeiten:</b><br>",
      "Durchschnittliche Lage: ", round(df$prob_durchschnitt * 100, 1), " %<br>",
      "Gute Lage: ", round(df$prob_gute * 100, 1), " %<br>",
      "Beste Lage: ", round(df$prob_beste * 100, 1), " %<br>",
      
      "<i>Infrastruktur-/Lagewerte:</i><br>",
      "Distanz Park (&gt;10ha): ", df$y1, " m<br>",
      "Fahrzeit Innenstadt: ", df$y2, " min<br>",
      "Gehminuten Haltestelle: ", df$y3, " min<br>",
      "Fußweg Grundschule: ", df$y4, "<br>",
      "Fußweg Spielplatz: ", df$y5, "<br>",
      "Fußweg Kita: ", df$y6, "<br>",
      "Fußweg Ortszentrum: ", df$y7, "<br>",
      "Anteil Verkehrsfläche: ", round(df$y9, 3), "<br>",
      "Anteil Grünfläche: ", round(df$y10, 3), "<br>",
      laerm_text
    )
  }
  
  map_data$popup_text <- erstelle_popup(map_data)
  
  daten_unveraendert <- map_data %>% filter(Geaendert == FALSE)
  daten_geaendert    <- map_data %>% filter(Geaendert == TRUE)
  
  change_rate <- mean(map_data$Geaendert)
  realized_improvement_mean <- mean(map_data$realized_improvement)
  
  cat("\n", model_label, "\n")
  cat("Lambda:", prior_res$lambda, "\n")
  cat("Prior scale:", round(prior_res$prior_scale, 4), "\n")
  cat("Änderungsrate:", round(change_rate * 100, 2), "%\n")
  cat("Realisierte Verbesserung:", round(realized_improvement_mean, 4), "\n")
  cat("=>", nrow(daten_geaendert), "geänderte Punkte,",
      nrow(daten_unveraendert), "unveränderte Punkte.\n")
  
  karte <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    
    addPolygons(
      data = wohnlagen_muc_wgs_3cat,
      fillColor = ~color,
      fillOpacity = 0.6,
      color = "black",
      weight = 0.5,
      label = ~Wohnlage,
      group = "Wohnlagen (Flächen)"
    ) %>%
    
    addPolylines(
      data = wohnlage_grenzen_wgs,
      color = "black",
      weight = 0.5,
      group = "Wohnlagen (Grenzen)"
    ) %>%
    
    addCircleMarkers(
      data = daten_unveraendert,
      lng = ~s.long,
      lat = ~s.lat,
      fillColor = ~color,
      fillOpacity = 0.85,
      color = "black",
      stroke = TRUE,
      weight = 1,
      radius = 5,
      popup = ~popup_text,
      group = "Unverändert"
    ) %>%
    
    addCircleMarkers(
      data = daten_geaendert,
      lng = ~s.long,
      lat = ~s.lat,
      fillColor = ~color,
      fillOpacity = 1,
      color = "red",
      stroke = TRUE,
      weight = 2.5,
      radius = 7,
      popup = ~popup_text,
      group = "Geändert"
    ) %>%
    
    addLegend(
      position = "bottomright",
      colors = unname(wohnlage_farben_3),
      labels = names(wohnlage_farben_3),
      title = paste0("Neue Lage<br>", model_label),
      opacity = 1
    ) %>%
    
    addLayersControl(
      overlayGroups = c(
        "Wohnlagen (Flächen)",
        "Wohnlagen (Grenzen)",
        "Geändert",
        "Unverändert"
      ),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  saveWidget(karte, file = output_file, selfcontained = TRUE)
  
  cat("✓ Karte gespeichert unter:", output_file, "\n")
  
  return(list(
    map_data = map_data,
    map = karte,
    change_rate = change_rate,
    realized_improvement = realized_improvement_mean
  ))
}


# ==============================================================================
# 7. PRIOR-TRANSFORMATION FÜR BEIDE MODELLE  (das ausführen wenn anderes lambda)
# ==============================================================================

prior_model1 <- apply_prior_lambda(
  SCORE = SCORE_model1,
  data = data,
  levels_c = levels_c,
  lambda = chosen_lambda
)

#prior_model2 <- apply_prior_lambda(
#  SCORE = SCORE_model2,
#  data = data,
#  levels_c = levels_c,
#  lambda = chosen_lambda
#)

# ==============================================================================
# 8. KARTEN ERSTELLEN UND SPEICHERN
# ==============================================================================

karte_prior_model1 <- create_prior_map(
  data_base = data,
  prior_res = prior_model1,
  model_label = "Model 1 mit Lärm",
  output_file = paste0(
    "results_lin_disc/karte_prior_model1_mit_laerm_lambda_",
    gsub("\\.", "_", as.character(chosen_lambda)),
    ".html"
  ),
  include_laerm = TRUE
)

#karte_prior_model2 <- create_prior_map(
#  data_base = data,
#  prior_res = prior_model2,
#  model_label = "Model 2 ohne Lärm",
#  output_file = paste0(
#    "results_lin_disc/karte_prior_model2_ohne_laerm_lambda_",
#    gsub("\\.", "_", as.character(chosen_lambda)),
#    ".html"
#  ),
#  include_laerm = FALSE
#)

# ==============================================================================
# 9. OUTPUTS SPEICHERN
# ==============================================================================


saveRDS(
  karte_prior_model1$map_data,
  paste0(
    "results_lin_disc/map_data_prior_model1_mit_laerm_lambda_",
    gsub("\\.", "_", as.character(chosen_lambda)),
    ".rds"
  )
)

#saveRDS(
#  karte_prior_model2$map_data,
#  paste0(
#    "results_lin_disc/map_data_prior_model2_ohne_laerm_lambda_",
#    gsub("\\.", "_", as.character(chosen_lambda)),
#    ".rds"
#  )
#)

vergleich_prior <- data.frame(
  Modell = c("Model 1 mit Lärm", "Model 2 ohne Lärm"),
  Lambda = chosen_lambda,
  Prior_Scale = exp(chosen_lambda) - 1,
  Änderungsrate = c(
    karte_prior_model1$change_rate,
    karte_prior_model2$change_rate
  ),
  Änderungsrate_Prozent = round(c(
    karte_prior_model1$change_rate,
    karte_prior_model2$change_rate
  ) * 100, 2),
  Realisierte_Verbesserung = c(
    karte_prior_model1$realized_improvement,
    karte_prior_model2$realized_improvement
  )
)

print(vergleich_prior)







# NEU
library(sf)
library(dplyr)

# ==============================================================================
# 0. EINSTELLUNGEN
# ==============================================================================

delta <- 2.75
prior_scale <- exp(delta) - 1

cat("Delta:", delta, "\n")
cat("Entsprechender prior.scale:", round(prior_scale, 4), "\n")

# Welches Modell soll verwendet werden?
# Für Model 1 mit Lärm:
SCORE_use <- SCORE
model1_mit_laerm <- readRDS("results_lin_disc/model_3cat_optimierung.rds")
modell_name <- "model1_mit_laerm"





# ==============================================================================
# 1. WOHNLAGENFLÄCHEN LADEN UND FLÄCHEN-ID VERGEBEN
# ==============================================================================

wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds")

# Sicherstellen, dass es sf ist
wohnlagen_muc_wgs <- st_as_sf(wohnlagen_muc_wgs)

# Eindeutige Flächen-ID vergeben
wohnlagen_muc_wgs <- wohnlagen_muc_wgs %>%
  mutate(
    flaechen_id = row_number()
  )

cat("Anzahl Wohnlagenflächen:", nrow(wohnlagen_muc_wgs), "\n")


# ==============================================================================
# 2. DATA ALS SF-PUNKTE ERZEUGEN
# ==============================================================================

# data muss s.long und s.lat enthalten
data$c <- as.factor(data$c)

data_sf <- st_as_sf(
  data,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

# CRS angleichen, falls nötig
if (st_crs(data_sf) != st_crs(wohnlagen_muc_wgs)) {
  data_sf <- st_transform(data_sf, st_crs(wohnlagen_muc_wgs))
}


# ==============================================================================
# 3. DATA_SCORE ALS SF-PUNKTE UND SPATIAL JOIN
# ==============================================================================

data_sf <- st_as_sf(
  data_score,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

if (st_crs(data_sf) != st_crs(wohnlagen_muc_wgs)) {
  data_sf <- st_transform(data_sf, st_crs(wohnlagen_muc_wgs))
}

data_joined <- st_join(
  data_sf,
  wohnlagen_muc_wgs %>% select(flaechen_id, Wohnlage),
  join = st_within,
  left = TRUE
)

cat("Zeilen data_score:", nrow(data_score), "\n")
cat("Zeilen data_joined:", nrow(data_joined), "\n")
cat("Zeilen SCORE_use:", nrow(SCORE_use), "\n")
cat("Punkte ohne zugeordnete Fläche:", sum(is.na(data_joined$flaechen_id)), "\n")

stopifnot(nrow(data_joined) == nrow(SCORE_use))


# ==============================================================================
# 4. PRIOR-TRANSFORMATION MIT DELTA = 2.75
# ==============================================================================

levels_c <- levels(data_score$c)
k <- length(levels_c)
N <- nrow(data_score)

stopifnot(nrow(SCORE_use) == N)

prior_res <- apply_prior_scale_direct(
  SCORE = SCORE_use,
  data = data_score,
  levels_c = levels_c,
  prior_scale = prior_scale
)


# ==============================================================================
# 5. PRIOR-ERGEBNISSE AN DATA_JOINED ANHÄNGEN
# ==============================================================================

data_joined <- data_joined %>%
  mutate(
    wohnlage_alt = as.character(c),
    wohnlage_neu = prior_res$pred,
    changed = wohnlage_alt != wohnlage_neu,
    
    score_alt = prior_res$current_score,
    score_neu = prior_res$new_score,
    score_best = prior_res$best_score,
    realized_improvement = prior_res$realized_improvement,
    potential_improvement = prior_res$potential_improvement,
    
    prob_max = apply(prior_res$PROB, 1, max)
  )

cat("Gesamte Änderungsrate auf allen Punkten:", round(mean(data_joined$changed) * 100, 2), "%\n")


# ==============================================================================
# 6. PUNKTE OHNE FLÄCHE IGNORIEREN
# ==============================================================================

data_joined_in_area <- data_joined %>%
  filter(!is.na(flaechen_id))

cat("Punkte insgesamt:", nrow(data_joined), "\n")
cat("Punkte mit Fläche:", nrow(data_joined_in_area), "\n")
cat("Ignorierte Punkte ohne Fläche:", nrow(data_joined) - nrow(data_joined_in_area), "\n")

cat("Änderungsrate nur für Punkte mit Fläche:",
    round(mean(data_joined_in_area$changed) * 100, 2), "%\n")


# ==============================================================================
# 7. ANTEIL GEÄNDERTER WOHNUNGEN JE FLÄCHE
# ==============================================================================

gebiet_summary <- data_joined_in_area %>%
  st_drop_geometry() %>%
  group_by(flaechen_id) %>%
  summarise(
    n_wohnungen = n(),
    n_geaendert = sum(changed, na.rm = TRUE),
    anteil_geaendert = n_geaendert / n_wohnungen,
    anteil_geaendert_prozent = round(anteil_geaendert * 100, 2),
    
    mittlere_realisierte_verbesserung = mean(realized_improvement, na.rm = TRUE),
    mittlere_potenzielle_verbesserung = mean(potential_improvement, na.rm = TRUE),
    mittlere_max_prob = mean(prob_max, na.rm = TRUE),
    
    alte_lage_haeufig = names(sort(table(wohnlage_alt), decreasing = TRUE))[1],
    neue_lage_haeufig = names(sort(table(wohnlage_neu), decreasing = TRUE))[1],
    
    .groups = "drop"
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

print(head(gebiet_summary, 30))


# ==============================================================================
# 8. SUMMARY AN FLÄCHEN JOINEN
# ==============================================================================

wohnlagen_muc_wgs_analyse <- wohnlagen_muc_wgs %>%
  left_join(gebiet_summary, by = "flaechen_id") %>%
  mutate(
    n_wohnungen = ifelse(is.na(n_wohnungen), 0, n_wohnungen),
    n_geaendert = ifelse(is.na(n_geaendert), 0, n_geaendert),
    anteil_geaendert = ifelse(is.na(anteil_geaendert), 0, anteil_geaendert),
    anteil_geaendert_prozent = ifelse(is.na(anteil_geaendert_prozent), 0, anteil_geaendert_prozent)
  )


# ==============================================================================
# 9. GEBIETE MIT HOHEM ÄNDERUNGSANTEIL
# ==============================================================================

problemgebiete <- wohnlagen_muc_wgs_analyse %>%
  filter(
    n_wohnungen >= 1, # kann man ändern falls nur kleine gebiete relevant
    anteil_geaendert >= 0.20
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("Anzahl potenzieller Problemgebiete:", nrow(problemgebiete), "\n")

problemgebiete %>%
  st_drop_geometry() %>%
  select(
    flaechen_id,
    Wohnlage,
    n_wohnungen,
    n_geaendert,
    anteil_geaendert_prozent,
    mittlere_realisierte_verbesserung,
    alte_lage_haeufig,
    neue_lage_haeufig
  ) %>%
  print(n = 50)



library(sf)
library(dplyr)

# ==============================================================================
# 0. EINSTELLUNGEN
# ==============================================================================

delta <- 2.75
prior_scale <- exp(delta) - 1

SCORE_use <- readRDS("results_lin_disc/SCORE_matrix_final_model.rds")
model1 <- readRDS("results_lin_disc/model_3cat_optimierung.rds")

cat("Delta:", delta, "\n")
cat("Prior scale:", round(prior_scale, 4), "\n")
cat("Zeilen SCORE_use:", nrow(SCORE_use), "\n")


# ==============================================================================
# 1. EXAKTEN MODELLDATENSATZ VERWENDEN
# ==============================================================================

data_score <- model1$model

cat("Zeilen data_score:", nrow(data_score), "\n")
cat("Zeilen SCORE_use:", nrow(SCORE_use), "\n")

stopifnot(nrow(data_score) == nrow(SCORE_use))

data_score$c <- as.factor(data_score$c)

levels_c <- levels(data_score$c)
k <- length(levels_c)
N <- nrow(data_score)

colnames(SCORE_use) <- levels_c

cat("Klassen:", paste(levels_c, collapse = ", "), "\n")


# ==============================================================================
# 2. WOHNLAGENFLÄCHEN LADEN UND FLÄCHEN-ID VERGEBEN
# ==============================================================================

wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds") %>%
  st_as_sf() %>%
  mutate(flaechen_id = row_number())

cat("Anzahl Wohnlagenflächen:", nrow(wohnlagen_muc_wgs), "\n")


# ==============================================================================
# 3. DATA_SCORE ALS SF-PUNKTE
# ==============================================================================

data_sf <- st_as_sf(
  data_score,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

if (st_crs(data_sf) != st_crs(wohnlagen_muc_wgs)) {
  data_sf <- st_transform(data_sf, st_crs(wohnlagen_muc_wgs))
}


# ==============================================================================
# 4. PUNKTE EINDEUTIG EINER FLÄCHE ZUORDNEN
# ==============================================================================

idx_list <- st_intersects(data_sf, wohnlagen_muc_wgs)

n_matches <- lengths(idx_list)

cat("Punkte ohne Fläche:", sum(n_matches == 0), "\n")
cat("Punkte mit genau einer Fläche:", sum(n_matches == 1), "\n")
cat("Punkte mit mehreren Flächen:", sum(n_matches > 1), "\n")

polygon_idx <- sapply(idx_list, function(x) {
  if (length(x) == 0) {
    NA_integer_
  } else {
    x[1]
  }
})

data_joined <- data_sf %>%
  mutate(
    flaechen_id = wohnlagen_muc_wgs$flaechen_id[polygon_idx],
    Wohnlage_flaeche = wohnlagen_muc_wgs$Wohnlage[polygon_idx]
  )

cat("Zeilen data_joined:", nrow(data_joined), "\n")
cat("Zeilen SCORE_use:", nrow(SCORE_use), "\n")

stopifnot(nrow(data_joined) == nrow(SCORE_use))


# ==============================================================================
# 5. PRIOR-TRANSFORMATION
# ==============================================================================

apply_prior_scale_direct <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale * (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  colnames(PROB_prior) <- levels_c
  
  pred_idx <- max.col(PROB_prior)
  pred <- levels_c[pred_idx]
  
  old_idx <- match(as.character(data$c), levels_c)
  
  current_score <- SCORE[cbind(seq_len(N), old_idx)]
  new_score <- SCORE[cbind(seq_len(N), pred_idx)]
  best_score <- apply(SCORE, 1, min)
  
  list(
    PROB = PROB_prior,
    pred_idx = pred_idx,
    pred = pred,
    current_score = current_score,
    new_score = new_score,
    best_score = best_score,
    realized_improvement = current_score - new_score,
    potential_improvement = current_score - best_score
  )
}

prior_res <- apply_prior_scale_direct(
  SCORE = SCORE_use,
  data = data_score,
  levels_c = levels_c,
  prior_scale = prior_scale
)


# ==============================================================================
# 6. PRIOR-ERGEBNISSE AN DATA_JOINED ANHÄNGEN
# ==============================================================================

data_joined <- data_joined %>%
  mutate(
    wohnlage_alt = as.character(c),
    wohnlage_neu = prior_res$pred,
    changed = wohnlage_alt != wohnlage_neu,
    
    score_alt = prior_res$current_score,
    score_neu = prior_res$new_score,
    score_best = prior_res$best_score,
    realized_improvement = prior_res$realized_improvement,
    potential_improvement = prior_res$potential_improvement,
    
    prob_max = apply(prior_res$PROB, 1, max)
  )

cat("Gesamte Änderungsrate auf allen Punkten:",
    round(mean(data_joined$changed) * 100, 2), "%\n")


# ==============================================================================
# 7. PUNKTE OHNE FLÄCHE IGNORIEREN
# ==============================================================================

data_joined_in_area <- data_joined %>%
  filter(!is.na(flaechen_id))

cat("Punkte insgesamt:", nrow(data_joined), "\n")
cat("Punkte mit Fläche:", nrow(data_joined_in_area), "\n")
cat("Ignorierte Punkte ohne Fläche:",
    nrow(data_joined) - nrow(data_joined_in_area), "\n")

cat("Änderungsrate nur für Punkte mit Fläche:",
    round(mean(data_joined_in_area$changed) * 100, 2), "%\n")


# ==============================================================================
# 8. ANTEIL GEÄNDERTER WOHNUNGEN JE FLÄCHE
# ==============================================================================

gebiet_summary <- data_joined_in_area %>%
  st_drop_geometry() %>%
  group_by(flaechen_id) %>%
  summarise(
    n_wohnungen = n(),
    n_geaendert = sum(changed, na.rm = TRUE),
    anteil_geaendert = n_geaendert / n_wohnungen,
    anteil_geaendert_prozent = round(anteil_geaendert * 100, 2),
    
    mittlere_realisierte_verbesserung = mean(realized_improvement, na.rm = TRUE),
    mittlere_potenzielle_verbesserung = mean(potential_improvement, na.rm = TRUE),
    mittlere_max_prob = mean(prob_max, na.rm = TRUE),
    
    alte_lage_haeufig = names(sort(table(wohnlage_alt), decreasing = TRUE))[1],
    neue_lage_haeufig = names(sort(table(wohnlage_neu), decreasing = TRUE))[1],
    
    .groups = "drop"
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

print(head(gebiet_summary, 30))


# ==============================================================================
# 9. SUMMARY AN FLÄCHENOBJEKTE JOINEN
# ==============================================================================

wohnlagen_muc_wgs_analyse <- wohnlagen_muc_wgs %>%
  left_join(gebiet_summary, by = "flaechen_id") %>%
  mutate(
    n_wohnungen = ifelse(is.na(n_wohnungen), 0, n_wohnungen),
    n_geaendert = ifelse(is.na(n_geaendert), 0, n_geaendert),
    anteil_geaendert = ifelse(is.na(anteil_geaendert), 0, anteil_geaendert),
    anteil_geaendert_prozent = ifelse(is.na(anteil_geaendert_prozent), 0, anteil_geaendert_prozent)
  )


# ==============================================================================
# 10. GEBIETE MIT HOHEM ÄNDERUNGSANTEIL
# ==============================================================================

problemgebiete <- wohnlagen_muc_wgs_analyse %>%
  filter(
    n_wohnungen >= 20,
    anteil_geaendert >= 0.20
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

cat("Anzahl potenzieller Problemgebiete:", nrow(problemgebiete), "\n")

problemgebiete %>%
  st_drop_geometry() %>%
  select(
    flaechen_id,
    Wohnlage,
    n_wohnungen,
    n_geaendert,
    anteil_geaendert_prozent,
    mittlere_realisierte_verbesserung,
    alte_lage_haeufig,
    neue_lage_haeufig
  )


# ==============================================================================
# 11. SPEICHERN
# ==============================================================================

suffix <- paste0("delta_", gsub("\\.", "_", as.character(delta)), "_model1_mit_laerm")

saveRDS(
  data_joined,
  paste0("results_lin_disc/data_punkte_mit_flaechen_", suffix, ".rds")
)

saveRDS(
  data_joined_in_area,
  paste0("results_lin_disc/data_punkte_mit_flaechen_ohne_NA_", suffix, ".rds")
)

saveRDS(
  wohnlagen_muc_wgs_analyse,
  paste0("results_lin_disc/wohnlagen_flaechen_aenderungsanteil_", suffix, ".rds")
)

write.csv(
  st_drop_geometry(gebiet_summary),
  paste0("results_lin_disc/gebiet_summary_", suffix, ".csv"),
  row.names = FALSE
)

cat("✓ Ergebnisse gespeichert.\n")




# Neuer Ansatz von Göran um Lärm Problem zu beheben

library(sf)
library(dplyr)
library(mgcv)

# ==============================================================================
# 0. EINSTELLUNGEN
# ==============================================================================

delta <- 2.75
prior_scale <- exp(delta) - 1
cat("Delta:", delta, "\n")
cat("Entsprechender prior.scale:", round(prior_scale, 4), "\n")

data_pasing <- readRDS("daten/model_munich_data2_pasing.rds")
model_data_hoherlärm_pasing <- readRDS("daten/model_data_hoherlärm_pasing.rds")

# ==============================================================================
# WAHRE WOHNLAGE AUS wohnlage_ebene ABLEITEN
# ==============================================================================

# wohnlage_ebene ist in diesem Datensatz bereits 0 bis 5 kodiert:
# 0 = durchschnittliche Lage
# 1 = gute Lage
# 2 = beste Lage
# 3 = zentrale durchschnittliche Lage
# 4 = zentrale gute Lage
# 5 = zentrale beste Lage
#
# Für das 3-Kategorien-Modell werden zentrale und nicht-zentrale
# Varianten wieder zusammengelegt.

data_pasing$c <- case_when(
  data_pasing$wohnlage_ebene %in% c(0, 3) ~ "durchschnittliche Lage",
  data_pasing$wohnlage_ebene %in% c(1, 4) ~ "gute Lage",
  data_pasing$wohnlage_ebene %in% c(2, 5) ~ "beste Lage",
  TRUE ~ NA_character_
)

data_pasing$c <- factor(
  data_pasing$c,
  levels = c(
    "beste Lage",
    "durchschnittliche Lage",
    "gute Lage"
  )
)

cat("Verteilung der neuen wahren 3-Kategorien-Wohnlage:\n")
print(table(data_pasing$c, useNA = "ifany"))

levels_c <- levels(data_pasing$c)
k <- length(levels_c)

cat("Wohnlagenklassen:", paste(levels_c, collapse = ", "), "\n")


# ==============================================================================
# 2. Y-VARIABLEN ERZEUGEN
# ==============================================================================

data_pasing$y1  <- data_pasing$erreichbarkeit_gr10ha_in_metern_adr
data_pasing$y2  <- data_pasing$erreichbarkeit_innenstadt_in_minuten_adr
data_pasing$y3  <- data_pasing$erreichbarkeit_naechstehaltestelle_in_minuten_adr
data_pasing$y4  <- data_pasing$grundschul_num
data_pasing$y5  <- data_pasing$spielplatz_num
data_pasing$y6  <- data_pasing$kitakigaho_num
data_pasing$y7  <- data_pasing$ortszentru_num
data_pasing$y8  <- data_pasing$brw_log
data_pasing$y9  <- data_pasing$anteil_vf_sv
data_pasing$y10 <- data_pasing$anteil_gf_sv
data_pasing$y11 <- data_pasing$laerm


# ==============================================================================
# 3. VOLLSTÄNDIGE ZEILEN FÜR MODELL MIT LÄRM
# ==============================================================================

# Entspricht deinem bisherigen Modell mit Lärm:
# y8 wird NICHT als Response verwendet.
y_vars_model <- c(
  "y1", "y2", "y3", "y4", "y5",
  "y6", "y7", "y9", "y10", "y11"
)

vars_needed <- c(
  "c",
  "s.long",
  "s.lat",
  y_vars_model
)

data_pasing <- data_pasing %>%
  filter(complete.cases(across(all_of(vars_needed))))

N <- nrow(data_pasing)
d <- length(y_vars_model)

cat("Pasing-Punkte nach complete.cases:", N, "\n")
cat("Anzahl Modell-Responses d:", d, "\n")


# ==============================================================================
# 4. MODELL FÜR PASING NEU SCHÄTZEN
# ==============================================================================

cat("\nSchätze Pasing-Modell mit Lärm neu...\n")

start_zeit <- Sys.time()

model_pasing <- gam(
  list(
    y1  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y2  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y3  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y4  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y5  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y6  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y7  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y9  ~ s(s.long, s.lat, by = c, k = 15) + c,
    y10 ~ s(s.long, s.lat, by = c, k = 15) + c,
    y11 ~ s(s.long, s.lat, by = c, k = 15) + c
  ),
  family = mvn(d = d),
  data = data_pasing,
  optimizer = "efs",
  control = gam.control(trace = TRUE)
)

cat("\nModell geschätzt in:\n")
print(Sys.time() - start_zeit)

saveRDS(
  model_pasing,
  "results_lin_disc/model_pasing_mit_laerm.rds"
)


# ==============================================================================
# 5. VARIANZSTRUKTUR AUS PASING-MODELL
# ==============================================================================

VAR <- solve(crossprod(model_pasing$family$data$R))
INV_VAR <- solve(VAR)


# ==============================================================================
# 6. SCORE-MATRIX FÜR PASING BERECHNEN
# ==============================================================================

Y_pasing <- as.matrix(data_pasing[, y_vars_model])

SCORE_pasing <- matrix(
  0,
  nrow = N,
  ncol = k
)

colnames(SCORE_pasing) <- levels_c

for (j in seq_len(k)) {
  
  cat("Berechne Score für Klasse",
      j, "von", k, ":", levels_c[j], "\n")
  
  tmp <- data_pasing
  tmp$c <- levels_c[j]
  
  fit <- predict(model_pasing, newdata = tmp)
  
  diff <- Y_pasing - fit
  
  score_temp <- (diff %*% INV_VAR) * diff
  
  SCORE_pasing[, j] <- rowSums(score_temp)
}

saveRDS(
  SCORE_pasing,
  "results_lin_disc/SCORE_pasing_mit_laerm.rds"
)


# ==============================================================================
# 7. PRIOR-TRANSFORMATION MIT PRIOR.SCALE AUS DELTA = 2.75
# ==============================================================================

apply_prior_scale_direct <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB_prior <- exp(-SCORE_shifted) * PRIOR
  PROB_prior <- PROB_prior / rowSums(PROB_prior)
  
  colnames(PROB_prior) <- levels_c
  
  pred_idx <- max.col(PROB_prior)
  pred <- levels_c[pred_idx]
  
  old_idx <- match(as.character(data$c), levels_c)
  
  current_score <- SCORE[cbind(seq_len(N), old_idx)]
  new_score <- SCORE[cbind(seq_len(N), pred_idx)]
  best_score <- apply(SCORE, 1, min)
  
  return(list(
    PROB = PROB_prior,
    pred_idx = pred_idx,
    pred = pred,
    current_score = current_score,
    new_score = new_score,
    best_score = best_score,
    realized_improvement = current_score - new_score,
    potential_improvement = current_score - best_score
  ))
}

prior_res_pasing <- apply_prior_scale_direct(
  SCORE = SCORE_pasing,
  data = data_pasing,
  levels_c = levels_c,
  prior_scale = prior_scale
)


# ==============================================================================
# 8. PRIOR-ERGEBNISSE AN PASING-DATEN ANHÄNGEN
# ==============================================================================

data_pasing <- data_pasing %>%
  mutate(
    wohnlage_alt = as.character(c),
    wohnlage_neu = prior_res_pasing$pred,
    changed = wohnlage_alt != wohnlage_neu,
    
    score_alt = prior_res_pasing$current_score,
    score_neu = prior_res_pasing$new_score,
    score_best = prior_res_pasing$best_score,
    
    realized_improvement =
      prior_res_pasing$realized_improvement,
    
    potential_improvement =
      prior_res_pasing$potential_improvement,
    
    prob_max = apply(prior_res_pasing$PROB, 1, max)
  )

cat("\nÄnderungsrate in Pasing:",
    round(mean(data_pasing$changed) * 100, 2), "%\n")

cat("Realisierte mittlere Verbesserung:",
    round(mean(data_pasing$realized_improvement), 4), "\n")


# ==============================================================================
# 9. WOHNLAGENFLÄCHEN LADEN UND FLÄCHEN-ID VERGEBEN
# ==============================================================================

wohnlagen_muc_wgs <- readRDS("daten/wohnlagen_flächen.rds") %>%
  st_as_sf() %>%
  mutate(flaechen_id = row_number())

cat("Anzahl Wohnlagenflächen insgesamt:",
    nrow(wohnlagen_muc_wgs), "\n")


# ==============================================================================
# 10. PASING-PUNKTE EINDEUTIG EINER FLÄCHE ZUORDNEN
# ==============================================================================

data_pasing_sf <- st_as_sf(
  data_pasing,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

if (st_crs(data_pasing_sf) != st_crs(wohnlagen_muc_wgs)) {
  data_pasing_sf <- st_transform(
    data_pasing_sf,
    st_crs(wohnlagen_muc_wgs)
  )
}

idx_list <- st_intersects(
  data_pasing_sf,
  wohnlagen_muc_wgs
)

n_matches <- lengths(idx_list)

cat("Pasing-Punkte ohne Fläche:",
    sum(n_matches == 0), "\n")

cat("Pasing-Punkte mit genau einer Fläche:",
    sum(n_matches == 1), "\n")

cat("Pasing-Punkte mit mehreren Flächen:",
    sum(n_matches > 1), "\n")

polygon_idx <- sapply(idx_list, function(x) {
  if (length(x) == 0) {
    NA_integer_
  } else {
    x[1]
  }
})

data_pasing_joined <- data_pasing_sf %>%
  mutate(
    flaechen_id = wohnlagen_muc_wgs$flaechen_id[polygon_idx],
    Wohnlage_flaeche = wohnlagen_muc_wgs$Wohnlage[polygon_idx]
  )


# ==============================================================================
# 11. PUNKTE OHNE FLÄCHE IGNORIEREN
# ==============================================================================

data_pasing_in_area <- data_pasing_joined %>%
  filter(!is.na(flaechen_id))

cat("Pasing-Punkte insgesamt:",
    nrow(data_pasing_joined), "\n")

cat("Pasing-Punkte mit Fläche:",
    nrow(data_pasing_in_area), "\n")

cat("Ignorierte Punkte ohne Fläche:",
    nrow(data_pasing_joined) - nrow(data_pasing_in_area), "\n")


# ==============================================================================
# 12. ÄNDERUNGSANTEIL JE WOHNLAGENFLÄCHE IN PASING
# ==============================================================================

gebiet_summary_pasing <- data_pasing_in_area %>%
  st_drop_geometry() %>%
  group_by(flaechen_id) %>%
  summarise(
    n_wohnungen = n(),
    n_geaendert = sum(changed, na.rm = TRUE),
    
    anteil_geaendert =
      n_geaendert / n_wohnungen,
    
    anteil_geaendert_prozent =
      round(anteil_geaendert * 100, 2),
    
    mittlere_realisierte_verbesserung =
      mean(realized_improvement, na.rm = TRUE),
    
    mittlere_potenzielle_verbesserung =
      mean(potential_improvement, na.rm = TRUE),
    
    mittlere_max_prob =
      mean(prob_max, na.rm = TRUE),
    
    alte_lage_haeufig =
      names(sort(table(wohnlage_alt), decreasing = TRUE))[1],
    
    neue_lage_haeufig =
      names(sort(table(wohnlage_neu), decreasing = TRUE))[1],
    
    .groups = "drop"
  ) %>%
  arrange(desc(anteil_geaendert), desc(n_geaendert))

print(gebiet_summary_pasing, n = 50)


# ==============================================================================
# 13. SUMMARY AN FLÄCHENOBJEKTE JOINEN
# ==============================================================================

wohnlagen_pasing_analyse <- wohnlagen_muc_wgs %>%
  left_join(
    gebiet_summary_pasing,
    by = "flaechen_id"
  ) %>%
  filter(!is.na(n_wohnungen))

cat("Wohnlagenflächen mit mindestens einem Pasing-Punkt:",
    nrow(wohnlagen_pasing_analyse), "\n")


# ==============================================================================
# 14. POTENZIELLE PROBLEMGEBIETE IN PASING
# ==============================================================================

problemgebiete_pasing %>%
  st_drop_geometry() %>%
  select(
    flaechen_id,
    Wohnlage,
    n_wohnungen,
    n_geaendert,
    anteil_geaendert_prozent,
    mittlere_realisierte_verbesserung,
    alte_lage_haeufig,
    neue_lage_haeufig
  ) %>%
  head(50) %>%
  print()











# Validierungstest: Sind delta und prior scale äquivalent?
# ==============================================================================
# VERGLEICH:
# Delta-Ansatz vs. Prior-Scale-Ansatz
# Grundlage: zuvor berechnetes Pasing-Modell und SCORE_pasing
# ==============================================================================

library(dplyr)

# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

chosen_delta <- 2.75

# Mathematisch äquivalenter Prior-Scale-Wert
equivalent_prior_scale <- exp(chosen_delta) - 1

# Optionaler Vergleich: gleicher Zahlenwert, aber NICHT äquivalent
same_numeric_prior_scale <- 2.75

cat("\n====================================\n")
cat("Vergleich Delta vs. Prior Scale\n")
cat("====================================\n")
cat("Delta:", chosen_delta, "\n")
cat("Äquivalenter prior.scale = exp(delta)-1:",
    round(equivalent_prior_scale, 4), "\n")
cat("Nicht äquivalenter prior.scale mit gleichem Zahlenwert:",
    same_numeric_prior_scale, "\n")
cat("====================================\n")


# ==============================================================================
# 2. FUNKTION: DIREKTER PRIOR-SCALE-ANSATZ
# ==============================================================================

apply_prior_scale_compare <- function(SCORE, data, levels_c, prior_scale) {
  
  N <- nrow(data)
  k <- length(levels_c)
  
  PRIOR <- matrix(1, nrow = N, ncol = k)
  colnames(PRIOR) <- levels_c
  
  for (j in seq_len(k)) {
    PRIOR[, j] <- 1 + prior_scale *
      (as.character(data$c) == levels_c[j])
  }
  
  SCORE_shifted <- SCORE - apply(SCORE, 1, min)
  
  PROB <- exp(-SCORE_shifted) * PRIOR
  PROB <- PROB / rowSums(PROB)
  colnames(PROB) <- levels_c
  
  pred_idx <- max.col(PROB)
  pred <- levels_c[pred_idx]
  
  old_class <- as.character(data$c)
  changed <- pred != old_class
  
  return(list(
    prior_scale = prior_scale,
    delta_equivalent = log(1 + prior_scale),
    PROB = PROB,
    pred_idx = pred_idx,
    pred = pred,
    changed = changed
  ))
}


# ==============================================================================
# 3. FUNKTION: DELTA-ANSATZ
#    Intern wird prior.scale = exp(delta)-1 verwendet
# ==============================================================================

apply_prior_delta_compare <- function(SCORE, data, levels_c, delta) {
  
  prior_scale <- exp(delta) - 1
  
  res <- apply_prior_scale_compare(
    SCORE = SCORE,
    data = data,
    levels_c = levels_c,
    prior_scale = prior_scale
  )
  
  res$delta <- delta
  
  return(res)
}


# ==============================================================================
# 4. ERGEBNISSE BERECHNEN
# ==============================================================================

res_delta <- apply_prior_delta_compare(
  SCORE = SCORE_pasing,
  data = data_pasing,
  levels_c = levels_c,
  delta = chosen_delta
)

res_prior_equivalent <- apply_prior_scale_compare(
  SCORE = SCORE_pasing,
  data = data_pasing,
  levels_c = levels_c,
  prior_scale = equivalent_prior_scale
)

res_prior_same_numeric <- apply_prior_scale_compare(
  SCORE = SCORE_pasing,
  data = data_pasing,
  levels_c = levels_c,
  prior_scale = same_numeric_prior_scale
)


# ==============================================================================
# 5. VERGLEICH A:
#    Delta = 2.75
#    Prior Scale = exp(2.75)-1
#    Diese beiden sollten IDENTISCH sein.
# ==============================================================================

old_class <- as.character(data_pasing$c)

vergleich_equivalent <- data.frame(
  wohnlage_alt = old_class,
  
  wohnlage_neu_delta = res_delta$pred,
  wohnlage_neu_prior_equiv = res_prior_equivalent$pred,
  
  changed_delta = res_delta$changed,
  changed_prior_equiv = res_prior_equivalent$changed,
  
  gleiche_neue_klasse =
    res_delta$pred == res_prior_equivalent$pred,
  
  gleiche_aenderungsentscheidung =
    res_delta$changed == res_prior_equivalent$changed
)

cat("\n\n====================================\n")
cat("A) Delta = 2.75 vs. äquivalenter Prior Scale\n")
cat("====================================\n")

cat("Delta:", chosen_delta, "\n")
cat("Prior Scale:", round(equivalent_prior_scale, 4), "\n\n")

cat("Änderungsrate Delta:",
    round(mean(vergleich_equivalent$changed_delta) * 100, 2), "%\n")

cat("Änderungsrate äquivalenter Prior Scale:",
    round(mean(vergleich_equivalent$changed_prior_equiv) * 100, 2), "%\n\n")

cat("Alle neuen Klassen identisch:",
    all(vergleich_equivalent$gleiche_neue_klasse), "\n")

cat("Alle Änderungsentscheidungen identisch:",
    all(vergleich_equivalent$gleiche_aenderungsentscheidung), "\n\n")

cat("Anzahl abweichender neuer Klassen:",
    sum(!vergleich_equivalent$gleiche_neue_klasse), "\n")

cat("Anzahl abweichender Änderungsentscheidungen:",
    sum(!vergleich_equivalent$gleiche_aenderungsentscheidung), "\n")

cat("====================================\n")


# ==============================================================================
# 6. ÜBERGANGSMATRIZEN FÜR DEN ÄQUIVALENTEN VERGLEICH
# ==============================================================================

cat("\nÜbergangsmatrix Delta-Ansatz:\n")
print(table(
  Alt = vergleich_equivalent$wohnlage_alt,
  Neu = vergleich_equivalent$wohnlage_neu_delta
))

cat("\nÜbergangsmatrix äquivalenter Prior-Scale-Ansatz:\n")
print(table(
  Alt = vergleich_equivalent$wohnlage_alt,
  Neu = vergleich_equivalent$wohnlage_neu_prior_equiv
))


# ==============================================================================
# 7. VERGLEICH B:
#    Delta = 2.75
#    Prior Scale = 2.75
#    Diese beiden sind NICHT mathematisch äquivalent.
# ==============================================================================

vergleich_same_numeric <- data.frame(
  wohnlage_alt = old_class,
  
  wohnlage_neu_delta = res_delta$pred,
  wohnlage_neu_prior_2_75 = res_prior_same_numeric$pred,
  
  changed_delta = res_delta$changed,
  changed_prior_2_75 = res_prior_same_numeric$changed
) %>%
  mutate(
    geaendert_beide =
      changed_delta & changed_prior_2_75,
    
    nur_delta =
      changed_delta & !changed_prior_2_75,
    
    nur_prior_2_75 =
      !changed_delta & changed_prior_2_75,
    
    keiner =
      !changed_delta & !changed_prior_2_75,
    
    gleiche_neue_klasse =
      wohnlage_neu_delta == wohnlage_neu_prior_2_75
  )

cat("\n\n====================================\n")
cat("B) Delta = 2.75 vs. Prior Scale = 2.75\n")
cat("   NICHT äquivalent\n")
cat("====================================\n")

cat("Änderungsrate Delta = 2.75:",
    round(mean(vergleich_same_numeric$changed_delta) * 100, 2), "%\n")

cat("Änderungsrate Prior Scale = 2.75:",
    round(mean(vergleich_same_numeric$changed_prior_2_75) * 100, 2), "%\n\n")

cat("Anzahl unterschiedlicher neuer Klassen:",
    sum(!vergleich_same_numeric$gleiche_neue_klasse), "\n")

cat("Anzahl unterschiedlicher Änderungsentscheidungen:",
    sum(vergleich_same_numeric$changed_delta !=
          vergleich_same_numeric$changed_prior_2_75), "\n")

cat("====================================\n")


# ==============================================================================
# 8. WELCHE WOHNUNGEN WERDEN BEI VERGLEICH B GEÄNDERT?
# ==============================================================================

cat("\nVergleich der Änderungsmengen:\n")
print(table(
  Delta_geaendert = vergleich_same_numeric$changed_delta,
  PriorScale_2_75_geaendert = vergleich_same_numeric$changed_prior_2_75
))


# ==============================================================================
# 9. KLASSENSPEZIFISCHE ÄNDERUNGSRATEN:
#    Werden bestimmte Ausgangsklassen stärker geändert?
# ==============================================================================

vergleich_klassen <- vergleich_same_numeric %>%
  group_by(wohnlage_alt) %>%
  summarise(
    n = n(),
    
    aenderungsrate_delta =
      mean(changed_delta),
    
    aenderungsrate_prior_scale_2_75 =
      mean(changed_prior_2_75),
    
    anteil_nur_delta =
      mean(nur_delta),
    
    anteil_nur_prior_scale_2_75 =
      mean(nur_prior_2_75),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        aenderungsrate_delta,
        aenderungsrate_prior_scale_2_75,
        anteil_nur_delta,
        anteil_nur_prior_scale_2_75
      ),
      ~ round(.x * 100, 2)
    )
  )

cat("\nKlassenspezifischer Vergleich:\n")
print(vergleich_klassen)


# ==============================================================================
# 10. ÜBERGANGSMATRIZEN FÜR VERGLEICH B
# ==============================================================================

cat("\nÜbergangsmatrix Delta = 2.75:\n")
print(table(
  Alt = vergleich_same_numeric$wohnlage_alt,
  Neu = vergleich_same_numeric$wohnlage_neu_delta
))

cat("\nÜbergangsmatrix Prior Scale = 2.75:\n")
print(table(
  Alt = vergleich_same_numeric$wohnlage_alt,
  Neu = vergleich_same_numeric$wohnlage_neu_prior_2_75
))







# ==============================================================================
# K-NEAREST-NEIGHBOUR-ZUORDNUNG FÜR PASING-LÄRMPUNKTE
# ==============================================================================
#
# Idee:
# - Referenzpunkte: bereits modellierte Pasing-Punkte aus data_pasing
#   mit finaler prior-transformierter Klasse "wohnlage_neu"
# - Lärmpunkte: eigener Pasing-Lärm-Datensatz
# - Für jeden Lärmpunkt:
#     1. k nächste Referenzpunkte suchen
#     2. Mehrheitsklasse der Nachbarn bestimmen
#     3. Diese Klasse wegen Lärm um eine Wohnlagenstufe abwerten
#
# ==============================================================================
# 0. PAKETE
# ==============================================================================

library(dplyr)
library(sf)

if (!requireNamespace("FNN", quietly = TRUE)) {
  install.packages("FNN")
}

library(FNN)


# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

k_neighbors <- 10

cat("Verwendete Anzahl Nachbarn k:", k_neighbors, "\n")


# ==============================================================================
# 2. LÄRMDATENSATZ LADEN
# ==============================================================================

# HIER NUR DEN DATEINAMEN ANPASSEN, FALLS DEIN LÄRMDATENSATZ ANDERS HEISST
data_pasing_laerm <- readRDS("daten/model_data_hoherlärm_pasing.rds")

cat("Anzahl Pasing-Lärmpunkte:", nrow(data_pasing_laerm), "\n")


# ==============================================================================
# 3. SICHERHEITSCHECKS
# ==============================================================================

# data_pasing stammt aus dem vorherigen Workflow.
# Es muss bereits die prior-transformierte Wohnlage enthalten:
#   wohnlage_neu
# sowie Koordinaten:
#   s.long, s.lat

required_reference_vars <- c(
  "s.long",
  "s.lat",
  "wohnlage_neu"
)

missing_reference_vars <- setdiff(required_reference_vars, names(data_pasing))

if (length(missing_reference_vars) > 0) {
  stop(
    paste(
      "In data_pasing fehlen folgende Variablen:",
      paste(missing_reference_vars, collapse = ", ")
    )
  )
}

required_laerm_vars <- c(
  "s.long",
  "s.lat"
)

missing_laerm_vars <- setdiff(required_laerm_vars, names(data_pasing_laerm))

if (length(missing_laerm_vars) > 0) {
  stop(
    paste(
      "Im Pasing-Lärmdatensatz fehlen folgende Variablen:",
      paste(missing_laerm_vars, collapse = ", ")
    )
  )
}


# ==============================================================================
# 4. WAHRE ALTE 3-KATEGORIEN-WOHNLAGE AUS wohnlage_ebene ABLEITEN
#    Nur für spätere Diagnose / Vergleich
# ==============================================================================

map_wohnlage_3cat <- function(x) {
  
  vals <- sort(unique(na.omit(x)))
  
  # Fall A: wohnlage_ebene ist 0 bis 5 kodiert
  if (all(vals %in% 0:5)) {
    out <- case_when(
      x %in% c(0, 3) ~ "durchschnittliche Lage",
      x %in% c(1, 4) ~ "gute Lage",
      x %in% c(2, 5) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  }
  
  # Fall B: wohnlage_ebene ist 1 bis 6 kodiert
  else if (all(vals %in% 1:6)) {
    out <- case_when(
      x %in% c(1, 4) ~ "durchschnittliche Lage",
      x %in% c(2, 5) ~ "gute Lage",
      x %in% c(3, 6) ~ "beste Lage",
      TRUE ~ NA_character_
    )
  }
  
  else {
    stop("Unbekannte Kodierung von wohnlage_ebene.")
  }
  
  return(out)
}


if ("wohnlage_ebene" %in% names(data_pasing_laerm)) {
  
  data_pasing_laerm <- data_pasing_laerm %>%
    mutate(
      wohnlage_alt_3cat = map_wohnlage_3cat(wohnlage_ebene)
    )
  
  cat("Alte wahre 3-Kategorien-Wohnlage der Lärmpunkte:\n")
  print(table(data_pasing_laerm$wohnlage_alt_3cat, useNA = "ifany"))
}


# ==============================================================================
# 5. REFERENZ- UND LÄRMPUNKTE IN SF UMWANDELN
#    Ausgangspunkt: WGS84 Longitude/Latitude
# ==============================================================================

reference_sf <- st_as_sf(
  data_pasing,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)

laerm_sf <- st_as_sf(
  data_pasing_laerm,
  coords = c("s.long", "s.lat"),
  crs = 4326,
  remove = FALSE
)


# ==============================================================================
# 6. IN METRISCHE KOORDINATEN TRANSFORMIEREN
#    EPSG:25832 = ETRS89 / UTM Zone 32N
# ==============================================================================

reference_sf_utm <- st_transform(reference_sf, 25832)
laerm_sf_utm <- st_transform(laerm_sf, 25832)

reference_coords <- st_coordinates(reference_sf_utm)
laerm_coords <- st_coordinates(laerm_sf_utm)


# ==============================================================================
# 7. K-NEAREST-NEIGHBOURS SUCHEN
# ==============================================================================

knn_res <- FNN::get.knnx(
  data = reference_coords,
  query = laerm_coords,
  k = k_neighbors,
  algorithm = "kd_tree"
)

# Matrix: pro Lärmpunkt stehen hier die Zeilenindizes seiner k Nachbarn
neighbor_index_matrix <- knn_res$nn.index

# Matrix: Distanzen zu den k Nachbarn in Metern
neighbor_distance_matrix <- knn_res$nn.dist

cat("kNN-Suche abgeschlossen.\n")


# ==============================================================================
# 8. MEHRHEITSWOHNLAGE DER K NÄCHSTEN NACHBARN BESTIMMEN
# ==============================================================================

wohnlage_order <- c(
  "durchschnittliche Lage",
  "gute Lage",
  "beste Lage"
)

# Hilfsfunktion:
# - bestimmt die Mehrheitsklasse
# - falls Gleichstand: schlechtere Wohnlage wählen
majority_vote_conservative <- function(classes) {
  
  tab <- table(factor(classes, levels = wohnlage_order))
  
  max_count <- max(tab)
  
  winner_classes <- names(tab)[tab == max_count]
  
  # Bei Gleichstand wird die schlechtere Wohnlage genommen.
  # Reihenfolge ist: durchschnittlich < gut < beste
  winner_idx <- match(winner_classes, wohnlage_order)
  
  winner <- wohnlage_order[min(winner_idx)]
  
  return(winner)
}


wohnlage_knn_basis <- apply(
  neighbor_index_matrix,
  1,
  function(idx) {
    neighbor_classes <- data_pasing$wohnlage_neu[idx]
    majority_vote_conservative(neighbor_classes)
  }
)


# ==============================================================================
# 9. MEHRHEITSANTEIL DER K NACHBARN BERECHNEN
#    Das zeigt, wie eindeutig die kNN-Zuordnung ist.
# ==============================================================================

knn_vote_share <- apply(
  neighbor_index_matrix,
  1,
  function(idx) {
    
    neighbor_classes <- data_pasing$wohnlage_neu[idx]
    
    tab <- table(factor(neighbor_classes, levels = wohnlage_order))
    
    max(tab) / sum(tab)
  }
)


# ==============================================================================
# 10. WOHNLAGE WEGEN LÄRM UM EINE STUFE ABWERTEN
# ==============================================================================

downgrade_one_level <- function(x) {
  
  case_when(
    x == "beste Lage" ~ "gute Lage",
    x == "gute Lage" ~ "durchschnittliche Lage",
    x == "durchschnittliche Lage" ~ "durchschnittliche Lage",
    TRUE ~ NA_character_
  )
}

wohnlage_nach_laerm <- downgrade_one_level(wohnlage_knn_basis)


# ==============================================================================
# 11. ERGEBNISSE AN LÄRMDATENSATZ ANHÄNGEN
# ==============================================================================

data_pasing_laerm_knn <- data_pasing_laerm %>%
  mutate(
    knn_k = k_neighbors,
    
    wohnlage_knn_basis = wohnlage_knn_basis,
    wohnlage_nach_laerm = wohnlage_nach_laerm,
    
    knn_vote_share = knn_vote_share,
    
    distanz_naechster_nachbar_m =
      neighbor_distance_matrix[, 1],
    
    distanz_mittlere_k_nachbarn_m =
      rowMeans(neighbor_distance_matrix),
    
    abgewertet_durch_laerm =
      wohnlage_knn_basis != wohnlage_nach_laerm
  )


# ==============================================================================
# 12. KURZE AUSWERTUNG
# ==============================================================================

cat("\n====================================\n")
cat("kNN-Zuordnung der Lärmpunkte abgeschlossen\n")
cat("====================================\n")

cat("Anzahl Lärmpunkte:",
    nrow(data_pasing_laerm_knn), "\n")

cat("\nWohnlage aus kNN-Votum:\n")
print(table(data_pasing_laerm_knn$wohnlage_knn_basis, useNA = "ifany"))

cat("\nWohnlage nach Lärm-Abwertung:\n")
print(table(data_pasing_laerm_knn$wohnlage_nach_laerm, useNA = "ifany"))

cat("\nAnteil tatsächlich abgewertet:",
    round(mean(data_pasing_laerm_knn$abgewertet_durch_laerm) * 100, 2),
    "%\n")

cat("\nEindeutigkeit des kNN-Votums:\n")
print(summary(data_pasing_laerm_knn$knn_vote_share))

cat("\nDistanz zum nächsten Nachbarn in Metern:\n")
print(summary(data_pasing_laerm_knn$distanz_naechster_nachbar_m))

cat("\nMittlere Distanz zu den k Nachbarn in Metern:\n")
print(summary(data_pasing_laerm_knn$distanz_mittlere_k_nachbarn_m))


# ==============================================================================
# 13. OPTIONAL: VERGLEICH MIT ALTER WAHREN WOHNLAGE
# ==============================================================================
# Nur möglich, falls wohnlage_ebene vorhanden war.

if ("wohnlage_alt_3cat" %in% names(data_pasing_laerm_knn)) {
  
  cat("\nVergleich alte wahre Wohnlage vs. finale Lärm-Wohnlage:\n")
  
  print(table(
    Alt = data_pasing_laerm_knn$wohnlage_alt_3cat,
    Neu_nach_Laerm = data_pasing_laerm_knn$wohnlage_nach_laerm
  ))
}












# Mietspiegeldatensatz klasifizieren
mietspiegel  <- read.csv("daten/ADR_MSP27_20260513.csv", sep = ";",
                         colClasses = c(adressid = "character"))
# ==============================================================================
# MIETSPIEGEL-PUNKTE PRÜFEN:
# Liegen sie in einem problematischen Gebiet?
# Grundlage:
# - mietspiegel: Datensatz mit adressid
# - raeumliche_daten: Adresspunkte mit Geometrie und adressid
# - problemgebiete_pasing: problematische Wohnlagenflächen aus der Analyse
# ==============================================================================

library(sf)
library(dplyr)

# ==============================================================================
# 1. ADRESS-ID SAUBER FORMATIEREN
# ==============================================================================

mietspiegel <- mietspiegel %>%
  mutate(
    adressid = as.character(adressid)
  )

raeumliche_daten <- raeumliche_daten %>%
  mutate(
    adressid = as.character(adressid)
  )


# ==============================================================================
# 2. MIETSPIEGEL MIT RÄUMLICHEN ADRESSPUNKTEN MATCHEN
# ==============================================================================

mietspiegel_geo <- mietspiegel %>%
  left_join(
    raeumliche_daten %>%
      select(adressid, geom),
    by = "adressid"
  ) %>%
  st_as_sf(sf_column_name = "geom")

cat("Mietspiegel-Zeilen insgesamt:", nrow(mietspiegel_geo), "\n")
cat("Davon mit gefundener Geometrie:",
    sum(!st_is_empty(mietspiegel_geo$geom)), "\n")
cat("Davon ohne gefundene Geometrie:",
    sum(st_is_empty(mietspiegel_geo$geom)), "\n")


# ==============================================================================
# 3. CRS ANGLEICHEN
# ==============================================================================

if (st_crs(mietspiegel_geo) != st_crs(problemgebiete_pasing)) {
  mietspiegel_geo <- st_transform(
    mietspiegel_geo,
    st_crs(problemgebiete_pasing)
  )
}


# ==============================================================================
# 4. PRÜFEN, OB MIETSPIEGEL-PUNKTE IN PROBLEMGEBIETEN LIEGEN
# ==============================================================================

trefferliste <- st_intersects(
  mietspiegel_geo,
  problemgebiete_pasing
)

mietspiegel_geo$in_problemgebiet <- lengths(trefferliste) > 0

cat("\n====================================\n")
cat("ERGEBNIS\n")
cat("====================================\n")
cat("Mietspiegel-Punkte insgesamt:",
    nrow(mietspiegel_geo), "\n")
cat("Punkte in problematischem Gebiet:",
    sum(mietspiegel_geo$in_problemgebiet, na.rm = TRUE), "\n")
cat("Anteil in problematischem Gebiet:",
    round(mean(mietspiegel_geo$in_problemgebiet, na.rm = TRUE) * 100, 2),
    "%\n")
cat("====================================\n")


# ==============================================================================
# 5. PROBLEMGEBIETS-ID ANHÄNGEN
# ==============================================================================

mietspiegel_geo$problemgebiet_flaechen_id <- sapply(
  trefferliste,
  function(x) {
    if (length(x) == 0) {
      NA_integer_
    } else {
      problemgebiete_pasing$flaechen_id[x[1]]
    }
  }
)


# ==============================================================================
# 6. BETROFFENE MIETSPIEGEL-PUNKTE AUSGEBEN
# ==============================================================================

mietspiegel_in_problemgebieten <- mietspiegel_geo %>%
  filter(in_problemgebiet == TRUE)

cat("\nBetroffene Mietspiegel-Punkte:\n")

mietspiegel_in_problemgebieten %>%
  st_drop_geometry() %>%
  select(
    adressid,
    everything(),
    problemgebiet_flaechen_id
  ) %>%
  print()


# ==============================================================================
# 7. OPTIONAL: ÜBERSICHT NACH PROBLEMGEBIET
# ==============================================================================

mietspiegel_problemgebiet_summary <- mietspiegel_geo %>%
  st_drop_geometry() %>%
  filter(in_problemgebiet == TRUE) %>%
  count(
    problemgebiet_flaechen_id,
    name = "anzahl_mietspiegel_punkte"
  ) %>%
  arrange(desc(anzahl_mietspiegel_punkte))

print(mietspiegel_problemgebiet_summary)






# ==============================================================================
# INTERAKTIVE GESAMTKARTE PASING
# - Wohnlagenflächen in Wohnlagefarben
# - Problemgebiete mit rotem Rand
# - Modellpunkte: geändert / unverändert
# - Lärmpunkte: kNN + Abwertung
# - Mietspiegel-Punkte: Problemgebiet ja/nein
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)

# ==============================================================================
# 0. VORAUSSETZUNGEN PRÜFEN
# ==============================================================================

# Erwartete Objekte aus den vorherigen Schritten:
# - data_pasing_joined
# - wohnlagen_pasing_analyse
# - problemgebiete_pasing
# - data_pasing_laerm_knn
# - mietspiegel
# - raeumliche_daten

required_objects <- c(
  "data_pasing_joined",
  "wohnlagen_pasing_analyse",
  "data_pasing_laerm_knn",
  "mietspiegel",
  "raeumliche_daten"
)

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    paste(
      "Folgende Objekte fehlen im Workspace:",
      paste(missing_objects, collapse = ", ")
    )
  )
}

# Problemgebiete bei Bedarf neu erzeugen
if (!exists("problemgebiete_pasing")) {
  problemgebiete_pasing <- wohnlagen_pasing_analyse %>%
    filter(
      n_wohnungen >= 20,
      anteil_geaendert >= 0.20
    ) %>%
    arrange(desc(anteil_geaendert), desc(n_geaendert))
}


# ==============================================================================
# 1. EINSTELLUNGEN
# ==============================================================================

delta_used <- 2.75
prior_scale_used <- exp(delta_used) - 1

cat("Erstelle Pasing-Gesamtkarte...\n")
cat("Delta:", delta_used, "\n")
cat("Prior scale:", round(prior_scale_used, 4), "\n")


# ==============================================================================
# 2. WOHNLAGEFARBEN
# ==============================================================================

wohnlage_farben_3 <- c(
  "durchschnittliche Lage" = "#e8f5a4",
  "gute Lage"              = "#afe391",
  "beste Lage"             = "#7FCDBB"
)


# ==============================================================================
# 3. HILFSFUNKTIONEN
# ==============================================================================

clean_wohnlage <- function(x) {
  trimws(gsub("zentrale", "", as.character(x), ignore.case = TRUE))
}

safe_col_text <- function(df, colname, prefix = "") {
  if (colname %in% names(df)) {
    paste0(prefix, df[[colname]])
  } else {
    rep("", nrow(df))
  }
}


# ==============================================================================
# 4. GEOMETRIEN AUF WGS84 BRINGEN
# ==============================================================================

data_pasing_joined_wgs <- st_transform(data_pasing_joined, 4326)

wohnlagen_pasing_analyse_wgs <- st_transform(
  wohnlagen_pasing_analyse,
  4326
)

problemgebiete_pasing_wgs <- st_transform(
  problemgebiete_pasing,
  4326
)

# Lärmdaten können data.frame oder sf sein
if (inherits(data_pasing_laerm_knn, "sf")) {
  data_pasing_laerm_knn_wgs <- st_transform(data_pasing_laerm_knn, 4326)
} else {
  data_pasing_laerm_knn_wgs <- st_as_sf(
    data_pasing_laerm_knn,
    coords = c("s.long", "s.lat"),
    crs = 4326,
    remove = FALSE
  )
}


# ==============================================================================
# 5. WOHNLAGENFLÄCHEN AUFBEREITEN
# ==============================================================================

wohnlagen_pasing_analyse_wgs <- wohnlagen_pasing_analyse_wgs %>%
  mutate(
    Wohnlage_3cat = clean_wohnlage(Wohnlage),
    flaechenfarbe = unname(wohnlage_farben_3[Wohnlage_3cat])
  )

problemgebiete_pasing_wgs <- problemgebiete_pasing_wgs %>%
  mutate(
    Wohnlage_3cat = clean_wohnlage(Wohnlage)
  )


# ==============================================================================
# 6. MODELLPUNKTE AUFBEREITEN
# ==============================================================================

modellpunkte_changed <- data_pasing_joined_wgs %>%
  filter(changed == TRUE) %>%
  mutate(
    punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
  )

modellpunkte_unchanged <- data_pasing_joined_wgs %>%
  filter(changed == FALSE) %>%
  mutate(
    punktfarbe = unname(wohnlage_farben_3[wohnlage_neu])
  )


# ==============================================================================
# 7. LÄRMPUNKTE AUFBEREITEN
# ==============================================================================

data_pasing_laerm_knn_wgs <- data_pasing_laerm_knn_wgs %>%
  mutate(
    punktfarbe_laerm =
      unname(wohnlage_farben_3[wohnlage_nach_laerm])
  )


# ==============================================================================
# 8. MIETSPIEGEL-PUNKTE MIT GEOMETRIE MATCHEN
# ==============================================================================

mietspiegel <- mietspiegel %>%
  mutate(adressid = as.character(adressid))

raeumliche_daten <- raeumliche_daten %>%
  mutate(adressid = as.character(adressid))

mietspiegel_geo <- mietspiegel %>%
  left_join(
    raeumliche_daten %>%
      select(adressid, geom),
    by = "adressid"
  ) %>%
  st_as_sf(sf_column_name = "geom")

# Nur Punkte mit Geometrie behalten
mietspiegel_geo <- mietspiegel_geo %>%
  filter(!st_is_empty(geom))

cat("Mietspiegel-Punkte mit Geometrie:", nrow(mietspiegel_geo), "\n")


# ==============================================================================
# 9. MIETSPIEGEL-PUNKTE GEGEN PROBLEMGEBIETE PRÜFEN
# ==============================================================================

if (st_crs(mietspiegel_geo) != st_crs(problemgebiete_pasing)) {
  mietspiegel_geo <- st_transform(
    mietspiegel_geo,
    st_crs(problemgebiete_pasing)
  )
}

treffer_problemgebiete <- st_intersects(
  mietspiegel_geo,
  problemgebiete_pasing
)

mietspiegel_geo$in_problemgebiet <-
  lengths(treffer_problemgebiete) > 0

mietspiegel_geo$problemgebiet_flaechen_id <- sapply(
  treffer_problemgebiete,
  function(x) {
    if (length(x) == 0) {
      NA_integer_
    } else {
      problemgebiete_pasing$flaechen_id[x[1]]
    }
  }
)

cat(
  "Mietspiegel-Punkte in Problemgebieten:",
  sum(mietspiegel_geo$in_problemgebiet),
  "\n"
)

cat(
  "Mietspiegel-Punkte außerhalb von Problemgebieten:",
  sum(!mietspiegel_geo$in_problemgebiet),
  "\n"
)


# ==============================================================================
# 10. MIETSPIEGEL NACH WGS84 BRINGEN
# ==============================================================================

mietspiegel_geo_wgs <- st_transform(mietspiegel_geo, 4326)

coords_mietspiegel <- st_coordinates(mietspiegel_geo_wgs)

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    s.long = coords_mietspiegel[, 1],
    s.lat  = coords_mietspiegel[, 2]
  )

mietspiegel_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == TRUE)

mietspiegel_nicht_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == FALSE)


# ==============================================================================
# 11. POPUPS: MODELLPUNKTE
# ==============================================================================

popup_modellpunkte <- function(df) {
  paste0(
    "<b>Modellierter Pasing-Punkt</b><br>",
    "<hr>",
    "<b>Alte Wohnlage:</b> ", df$wohnlage_alt, "<br>",
    "<b>Neue Wohnlage:</b> ",
    ifelse(
      df$changed,
      paste0(
        "<span style='color:red; font-weight:bold;'>",
        df$wohnlage_neu,
        "</span>"
      ),
      df$wohnlage_neu
    ),
    "<br>",
    "<b>Umklassifiziert:</b> ",
    ifelse(df$changed, "Ja", "Nein"),
    "<br>",
    "<b>Lärmwert:</b> ",
    ifelse(is.na(df$laerm), "n. v.", df$laerm),
    "<br>",
    "<hr>",
    "<b>Score alt:</b> ",
    round(df$score_alt, 3),
    "<br>",
    "<b>Score neu:</b> ",
    round(df$score_neu, 3),
    "<br>",
    "<b>Realisierte Verbesserung:</b> ",
    round(df$realized_improvement, 3),
    "<br>",
    "<b>Potenzielle Verbesserung:</b> ",
    round(df$potential_improvement, 3),
    "<br>",
    "<b>Max. Prior-Wahrscheinlichkeit:</b> ",
    round(df$prob_max * 100, 1),
    " %<br>",
    "<hr>",
    "<b>Flächen-ID:</b> ",
    ifelse(is.na(df$flaechen_id), "keine Zuordnung", df$flaechen_id),
    "<br>"
  )
}

modellpunkte_changed$popup_text <-
  popup_modellpunkte(modellpunkte_changed)

modellpunkte_unchanged$popup_text <-
  popup_modellpunkte(modellpunkte_unchanged)


# ==============================================================================
# 12. POPUPS: LÄRMPUNKTE
# ==============================================================================

popup_laermpunkte <- function(df) {
  paste0(
    "<b>Pasing-Lärmpunkt</b><br>",
    "<hr>",
    if ("wohnlage_alt_3cat" %in% names(df)) {
      paste0(
        "<b>Alte Wohnlage:</b> ",
        df$wohnlage_alt_3cat,
        "<br>"
      )
    } else {
      ""
    },
    "<b>Lärmwert:</b> ",
    ifelse(is.na(df$laerm), "n. v.", df$laerm),
    "<br>",
    "<b>kNN-Basiswohnlage:</b> ",
    df$wohnlage_knn_basis,
    "<br>",
    "<b>Finale Wohnlage nach Lärm-Abwertung:</b> ",
    "<span style='color:red; font-weight:bold;'>",
    df$wohnlage_nach_laerm,
    "</span><br>",
    "<b>Abgewertet durch Lärm:</b> ",
    ifelse(df$abgewertet_durch_laerm, "Ja", "Nein"),
    "<br>",
    "<hr>",
    "<b>k:</b> ",
    df$knn_k,
    "<br>",
    "<b>Mehrheitsanteil kNN:</b> ",
    round(df$knn_vote_share * 100, 1),
    " %<br>",
    "<b>Distanz nächster Nachbar:</b> ",
    round(df$distanz_naechster_nachbar_m, 1),
    " m<br>",
    "<b>Mittlere Distanz der k Nachbarn:</b> ",
    round(df$distanz_mittlere_k_nachbarn_m, 1),
    " m<br>"
  )
}

data_pasing_laerm_knn_wgs$popup_text <-
  popup_laermpunkte(data_pasing_laerm_knn_wgs)


# ==============================================================================
# 13. POPUPS: WOHNLAGENFLÄCHEN
# ==============================================================================

wohnlagen_pasing_analyse_wgs <- wohnlagen_pasing_analyse_wgs %>%
  mutate(
    popup_flaeche = paste0(
      "<b>Wohnlagenfläche Pasing</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ", flaechen_id, "<br>",
      "<b>Wohnlage:</b> ", Wohnlage_3cat, "<br>",
      "<hr>",
      "<b>Anzahl Wohnungen/Punkte:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Anzahl geändert:</b> ",
      n_geaendert,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<hr>",
      "<b>Mittlere realisierte Verbesserung:</b> ",
      round(mittlere_realisierte_verbesserung, 3),
      "<br>",
      "<b>Mittlere potenzielle Verbesserung:</b> ",
      round(mittlere_potenzielle_verbesserung, 3),
      "<br>",
      "<b>Mittlere max. Wahrscheinlichkeit:</b> ",
      round(mittlere_max_prob * 100, 1),
      " %<br>",
      "<hr>",
      "<b>Häufigste alte Lage:</b> ",
      alte_lage_haeufig,
      "<br>",
      "<b>Häufigste neue Lage:</b> ",
      neue_lage_haeufig,
      "<br>"
    )
  )


# ==============================================================================
# 14. POPUPS: PROBLEMGEBIETE
# ==============================================================================

problemgebiete_pasing_wgs <- problemgebiete_pasing_wgs %>%
  mutate(
    popup_problemgebiet = paste0(
      "<b>Potenzielles Problemgebiet</b><br>",
      "<hr>",
      "<b>Flächen-ID:</b> ",
      flaechen_id,
      "<br>",
      "<b>Wohnlage:</b> ",
      Wohnlage_3cat,
      "<br>",
      "<b>Anzahl Wohnungen/Punkte:</b> ",
      n_wohnungen,
      "<br>",
      "<b>Anzahl geändert:</b> ",
      n_geaendert,
      "<br>",
      "<b>Anteil geändert:</b> ",
      round(anteil_geaendert_prozent, 2),
      " %<br>",
      "<b>Mittlere realisierte Verbesserung:</b> ",
      round(mittlere_realisierte_verbesserung, 3),
      "<br>"
    )
  )


# ==============================================================================
# 15. POPUPS: MIETSPIEGEL-PUNKTE
# ==============================================================================

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    popup_mietspiegel = paste0(
      "<b>Mietspiegel-Punkt</b><br>",
      "<hr>",
      "<b>Adress-ID:</b> ",
      adressid,
      "<br>",
      ifelse(
        "adresse_1" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Adresse:</b> ", adresse_1, "<br>"),
        ""
      ),
      ifelse(
        "adresse_2" %in% names(mietspiegel_geo_wgs),
        paste0("<b>PLZ:</b> ", adresse_2, "<br>"),
        ""
      ),
      ifelse(
        "adresse_3" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Ort:</b> ", adresse_3, "<br>"),
        ""
      ),
      ifelse(
        "adresse_4" %in% names(mietspiegel_geo_wgs),
        paste0("<b>Gebiet:</b> ", adresse_4, "<br>"),
        ""
      ),
      "<hr>",
      "<b>In Problemgebiet:</b> ",
      ifelse(in_problemgebiet, "Ja", "Nein"),
      "<br>",
      "<b>Problemgebiet-Flächen-ID:</b> ",
      ifelse(
        is.na(problemgebiet_flaechen_id),
        "-",
        problemgebiet_flaechen_id
      ),
      "<br>"
    )
  )

mietspiegel_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == TRUE)

mietspiegel_nicht_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == FALSE)


# ==============================================================================
# 16. KARTENMITTELPUNKT
# ==============================================================================

bbox_pasing <- st_bbox(wohnlagen_pasing_analyse_wgs)

map_center_lng <- mean(c(bbox_pasing["xmin"], bbox_pasing["xmax"]))
map_center_lat <- mean(c(bbox_pasing["ymin"], bbox_pasing["ymax"]))


# ==============================================================================
# 17. KARTE ERSTELLEN
# ==============================================================================

karte_pasing_gesamt <- leaflet(
  options = leafletOptions(preferCanvas = TRUE)
) %>%
  
  addProviderTiles(
    "CartoDB.Positron",
    group = "Basiskarte"
  ) %>%
  
  setView(
    lng = map_center_lng,
    lat = map_center_lat,
    zoom = 13
  ) %>%
  
  # --------------------------------------------------------------------------
# Wohnlagenflächen in Wohnlagefarben
# --------------------------------------------------------------------------
addPolygons(
  data = wohnlagen_pasing_analyse_wgs,
  fillColor = ~flaechenfarbe,
  fillOpacity = 0.60,
  color = "grey35",
  weight = 1,
  popup = ~popup_flaeche,
  label = ~paste0(
    "Fläche ",
    flaechen_id,
    " | ",
    Wohnlage_3cat,
    " | ",
    round(anteil_geaendert_prozent, 1),
    " % geändert"
  ),
  group = "Wohnlagenflächen"
) %>%
  
  # --------------------------------------------------------------------------
# Problemgebiete nur mit rotem Rand
# --------------------------------------------------------------------------
addPolygons(
  data = problemgebiete_pasing_wgs,
  fillColor = "transparent",
  fillOpacity = 0,
  color = "red",
  weight = 4,
  popup = ~popup_problemgebiet,
  label = ~paste0(
    "Problemgebiet Fläche ",
    flaechen_id,
    " | ",
    round(anteil_geaendert_prozent, 1),
    " % geändert"
  ),
  group = "Problemgebiete"
) %>%
  
  # --------------------------------------------------------------------------
# Modellpunkte: unverändert
# --------------------------------------------------------------------------
addCircleMarkers(
  data = modellpunkte_unchanged,
  lng = ~s.long,
  lat = ~s.lat,
  fillColor = ~punktfarbe,
  fillOpacity = 0.75,
  color = "black",
  stroke = TRUE,
  weight = 0.8,
  radius = 4,
  popup = ~popup_text,
  group = "Modellpunkte: unverändert"
) %>%
  
  # --------------------------------------------------------------------------
# Modellpunkte: umklassifiziert
# --------------------------------------------------------------------------
addCircleMarkers(
  data = modellpunkte_changed,
  lng = ~s.long,
  lat = ~s.lat,
  fillColor = ~punktfarbe,
  fillOpacity = 1,
  color = "red",
  stroke = TRUE,
  weight = 2,
  radius = 6,
  popup = ~popup_text,
  group = "Modellpunkte: umklassifiziert"
) %>%
  
  # --------------------------------------------------------------------------
# Lärmpunkte kNN + Abwertung
# --------------------------------------------------------------------------
addCircleMarkers(
  data = data_pasing_laerm_knn_wgs,
  lng = ~s.long,
  lat = ~s.lat,
  fillColor = ~punktfarbe_laerm,
  fillOpacity = 1,
  color = "#6a0000",
  stroke = TRUE,
  weight = 2.5,
  radius = 7,
  popup = ~popup_text,
  group = "Lärmpunkte: kNN + Abwertung"
) %>%
  
  # --------------------------------------------------------------------------
# Mietspiegel-Punkte in Problemgebieten
# --------------------------------------------------------------------------
addCircleMarkers(
  data = mietspiegel_problem,
  lng = ~s.long,
  lat = ~s.lat,
  fillColor = "red",
  fillOpacity = 1,
  color = "darkred",
  stroke = TRUE,
  weight = 2.5,
  radius = 9,
  popup = ~popup_mietspiegel,
  group = "Mietspiegel: in Problemgebiet"
) %>%
  
  # --------------------------------------------------------------------------
# Mietspiegel-Punkte außerhalb Problemgebieten
# --------------------------------------------------------------------------
addCircleMarkers(
  data = mietspiegel_nicht_problem,
  lng = ~s.long,
  lat = ~s.lat,
  fillColor = "blue",
  fillOpacity = 0.9,
  color = "darkblue",
  stroke = TRUE,
  weight = 2,
  radius = 7,
  popup = ~popup_mietspiegel,
  group = "Mietspiegel: nicht in Problemgebiet"
) %>%
  
  # --------------------------------------------------------------------------
# Wohnlagen-Legende
# --------------------------------------------------------------------------
addLegend(
  position = "bottomright",
  colors = unname(wohnlage_farben_3),
  labels = names(wohnlage_farben_3),
  title = "Wohnlage",
  opacity = 1
) %>%
  
  # --------------------------------------------------------------------------
# Layer Control
# --------------------------------------------------------------------------
addLayersControl(
  baseGroups = c("Basiskarte"),
  overlayGroups = c(
    "Wohnlagenflächen",
    "Problemgebiete",
    "Modellpunkte: unverändert",
    "Modellpunkte: umklassifiziert",
    "Lärmpunkte: kNN + Abwertung",
    "Mietspiegel: in Problemgebiet",
    "Mietspiegel: nicht in Problemgebiet"
  ),
  options = layersControlOptions(collapsed = FALSE)
) %>%
  
  hideGroup("Modellpunkte: unverändert")


# ==============================================================================
# 18. KARTE SPEICHERN
# ==============================================================================

if (!dir.exists("interaktive_karten")) {
  dir.create("interaktive_karten")
}

saveWidget(
  karte_pasing_gesamt,
  file = "interaktive_karten/karte_pasing_gesamtanalyse_mit_mietspiegel.html",
  selfcontained = FALSE
)

cat("\n✓ Interaktive Karte gespeichert:\n")
cat("interaktive_karten/karte_pasing_gesamtanalyse_mit_mietspiegel.html\n")


# ==============================================================================
# 19. ZUSAMMENFASSUNG IN DER KONSOLE
# ==============================================================================

cat("\n====================================\n")
cat("ZUSAMMENFASSUNG PASING-KARTE\n")
cat("====================================\n")

cat("Modellpunkte gesamt:",
    nrow(data_pasing_joined_wgs), "\n")

cat("Davon umklassifiziert:",
    nrow(modellpunkte_changed), "\n")

cat("Änderungsrate Modellpunkte:",
    round(mean(data_pasing_joined_wgs$changed) * 100, 2),
    "%\n")

cat("Wohnlagenflächen mit Pasing-Punkten:",
    nrow(wohnlagen_pasing_analyse_wgs), "\n")

cat("Problemgebiete:",
    nrow(problemgebiete_pasing_wgs), "\n")

cat("Lärmpunkte:",
    nrow(data_pasing_laerm_knn_wgs), "\n")

cat("Mietspiegel-Punkte gesamt:",
    nrow(mietspiegel_geo_wgs), "\n")

cat("Mietspiegel-Punkte in Problemgebieten:",
    nrow(mietspiegel_problem), "\n")

cat("Mietspiegel-Punkte nicht in Problemgebieten:",
    nrow(mietspiegel_nicht_problem), "\n")

cat("====================================\n")





# ==============================================================================
# MIETSPIEGEL-PUNKTE ZUR BESTEHENDEN PASING-KARTE HINZUFÜGEN
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)

# ==============================================================================
# 1. VORAUSSETZUNGEN PRÜFEN
# ==============================================================================

# Erwartet:
# - mietspiegel
# - raeumliche_daten
# - problemgebiete_pasing
# - karte_pasing_gesamt

if (!exists("mietspiegel")) {
  stop("Objekt 'mietspiegel' wurde nicht gefunden.")
}

if (!exists("raeumliche_daten")) {
  stop("Objekt 'raeumliche_daten' wurde nicht gefunden.")
}

if (!exists("problemgebiete_pasing")) {
  stop("Objekt 'problemgebiete_pasing' wurde nicht gefunden.")
}

if (!exists("karte_pasing_gesamt")) {
  stop("Objekt 'karte_pasing_gesamt' wurde nicht gefunden. Bitte zuerst die Pasing-Karte erzeugen.")
}


# ==============================================================================
# 2. ADRESS-ID SAUBER FORMATIEREN
# ==============================================================================

mietspiegel <- mietspiegel %>%
  mutate(
    adressid = as.character(adressid)
  )

raeumliche_daten <- raeumliche_daten %>%
  mutate(
    adressid = as.character(adressid)
  )


# ==============================================================================
# 3. MIETSPIEGEL MIT RÄUMLICHEN ADRESSPUNKTEN MATCHEN
# ==============================================================================

mietspiegel_geo <- mietspiegel %>%
  left_join(
    raeumliche_daten %>%
      select(adressid, geom),
    by = "adressid"
  ) %>%
  st_as_sf(sf_column_name = "geom")

cat("Mietspiegel-Zeilen insgesamt:", nrow(mietspiegel_geo), "\n")


# ==============================================================================
# 4. PUNKTE OHNE GEOMETRIE AUSSORTIEREN
# ==============================================================================

mietspiegel_geo <- mietspiegel_geo %>%
  filter(!st_is_empty(geom))

cat("Mietspiegel-Punkte mit Geometrie:", nrow(mietspiegel_geo), "\n")


# ==============================================================================
# 5. CRS AN PROBLEMGEBIETE ANGLEICHEN
# ==============================================================================

if (st_crs(mietspiegel_geo) != st_crs(problemgebiete_pasing)) {
  mietspiegel_geo <- st_transform(
    mietspiegel_geo,
    st_crs(problemgebiete_pasing)
  )
}


# ==============================================================================
# 6. PRÜFEN, OB MIETSPIEGEL-PUNKTE IN PROBLEMGEBIETEN LIEGEN
# ==============================================================================

trefferliste <- st_intersects(
  mietspiegel_geo,
  problemgebiete_pasing
)

mietspiegel_geo$in_problemgebiet <- lengths(trefferliste) > 0

mietspiegel_geo$problemgebiet_flaechen_id <- sapply(
  trefferliste,
  function(x) {
    if (length(x) == 0) {
      NA_integer_
    } else {
      problemgebiete_pasing$flaechen_id[x[1]]
    }
  }
)

cat("Mietspiegel-Punkte in Problemgebieten:",
    sum(mietspiegel_geo$in_problemgebiet), "\n")

cat("Mietspiegel-Punkte nicht in Problemgebieten:",
    sum(!mietspiegel_geo$in_problemgebiet), "\n")


# ==============================================================================
# 7. AUF WGS84 FÜR LEAFLET TRANSFORMIEREN
# ==============================================================================

mietspiegel_geo_wgs <- st_transform(mietspiegel_geo, 4326)

coords_mietspiegel <- st_coordinates(mietspiegel_geo_wgs)

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    s.long = coords_mietspiegel[, 1],
    s.lat  = coords_mietspiegel[, 2]
  )


# ==============================================================================
# 8. POPUPS ERSTELLEN
# ==============================================================================

mietspiegel_geo_wgs <- mietspiegel_geo_wgs %>%
  mutate(
    popup_mietspiegel = paste0(
      "<b>Mietspiegel-Punkt</b><br>",
      "<hr>",
      "<b>adressid:</b> ", adressid, "<br>",
      
      if ("adresse_1" %in% names(.)) {
        paste0("<b>Adresse:</b> ", adresse_1, "<br>")
      } else {
        ""
      },
      
      if ("adresse_2" %in% names(.)) {
        paste0("<b>PLZ:</b> ", adresse_2, "<br>")
      } else {
        ""
      },
      
      if ("adresse_3" %in% names(.)) {
        paste0("<b>Ort:</b> ", adresse_3, "<br>")
      } else {
        ""
      },
      
      if ("adresse_4" %in% names(.)) {
        paste0("<b>Stadtbezirk:</b> ", adresse_4, "<br>")
      } else {
        ""
      },
      
      "<hr>",
      "<b>In Problemgebiet:</b> ",
      ifelse(in_problemgebiet, "Ja", "Nein"),
      "<br>",
      "<b>Problemgebiet-Flächen-ID:</b> ",
      ifelse(
        is.na(problemgebiet_flaechen_id),
        "-",
        problemgebiet_flaechen_id
      ),
      "<br>"
    )
  )


# ==============================================================================
# 9. IN ZWEI LAYER AUFTEILEN
# ==============================================================================

mietspiegel_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == TRUE)

mietspiegel_nicht_problem <- mietspiegel_geo_wgs %>%
  filter(in_problemgebiet == FALSE)


# ==============================================================================
# 10. ZUR BESTEHENDEN KARTE HINZUFÜGEN
# ==============================================================================

karte_pasing_gesamt <- karte_pasing_gesamt %>%
  
  addCircleMarkers(
    data = mietspiegel_problem,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = "red",
    fillOpacity = 1,
    color = "darkred",
    stroke = TRUE,
    weight = 2.5,
    radius = 8,
    popup = ~popup_mietspiegel,
    group = "Mietspiegel: in Problemgebiet"
  ) %>%
  
  addCircleMarkers(
    data = mietspiegel_nicht_problem,
    lng = ~s.long,
    lat = ~s.lat,
    fillColor = "blue",
    fillOpacity = 0.85,
    color = "darkblue",
    stroke = TRUE,
    weight = 1.5,
    radius = 6,
    popup = ~popup_mietspiegel,
    group = "Mietspiegel: nicht in Problemgebiet"
  ) %>%
  
  addLayersControl(
    baseGroups = c("Basiskarte"),
    overlayGroups = c(
      "Wohnlagenflächen",
      "Problemgebiete",
      "Modellpunkte: unverändert",
      "Modellpunkte: umklassifiziert",
      "Lärmpunkte: kNN + Abwertung",
      "Mietspiegel: in Problemgebiet",
      "Mietspiegel: nicht in Problemgebiet"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )


# ==============================================================================
# 11. KARTE ERNEUT SPEICHERN
# ==============================================================================

saveWidget(
  karte_pasing_gesamt,
  file = "interaktive_karten/karte_pasing_gesamtanalyse_mit_mietspiegel.html",
  selfcontained = TRUE
)

cat("\n✓ Karte mit Mietspiegel-Punkten gespeichert:\n")
cat("interaktive_karten/karte_pasing_gesamtanalyse_mit_mietspiegel.html\n")