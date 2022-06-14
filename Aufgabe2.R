# Aufgabe2.R
# Bearbeitete Aufgaben zu der zweiten Abgabe in DataLiteracy
#
# Input: Erstsemesterbefragung_WiSe19-WiSe21.csv
# Output: /

######################################
# Vorbereitung
######################################
setwd("/Users/theoklautke/Repositories/DataLiteracy2-HdM-Stuttgart/")

# Lesen Sie den Datensatz `Erstsemesterbefragung_WiSe19-WiSe21.csv` als Data
# Frame namens `dat` ein.
dat <- read.table("Erstsemesterbefragung_WiSe19-WiSe21.csv", header = T, sep = ";")

# 3 Löschen Sie die Spalte `N_Manip1`.
dat$N_Manip1 <- NULL

# Fehlende Werte wurden in diesem Datensatz mit `-99` oder `-77` kodiert.
# Um besser in R damit weiterarbeiten zu können, kodieren sie jede vorkommende
# `-99` und `-77` zu `NA` um.
dat[dat == -99] <- NA
dat[dat == -77] <- NA

# Löschen Sie nun alle Zeilen welche fehlende Werte (`NA`s) enthalten.
# Wie viele Zeilen wurden gelöscht?
dat2 <- na.omit(dat)

nrow(dat) - nrow(dat2)
dat <- na.omit(dat)

######################################
# Aufgabenteil
######################################
# Aufgabe 1:    Erstellen Sie die folgenden zwei neuen Spalten im Datensatz:
#               -   N_gesamt, in der für jede Person der Mittelwert aus allen Spalten zur Narzissmus-Tendenz (Spalten
#                   50 bis 66) gespeichert wird
#               -   IKU, in der für jede Person der Mittelwert der Spalten IKU1 und IKU2 gespeichert wird.

dat$N_gesamt <- rowMeans(dat[, c("N_Angeb1", "N_Einzig1", "N_Angeb2", "N_Einzig2", "N_Ueberh1", "N_Manip2", "N_Anspr1", "N_Ueberh2", "N_Einzig3", "N_Anspr2", "N_Anspr3", "N_Aut1", "N_Aut2", "N_Aut3", "N_Anspr4", "N_Ueberh3")], na.rm = T)
dat$IKU <- rowMeans(dat[, c("IKU1", "IKU2")], na.rm = T)

# Aufgabe 2:    Sie haben die Hypothese, dass weibliche Studierende einen längeren Weg zur HdM haben als
#               männliche Studierende. Überprüfen Sie dies mit einem gerichteten Welch t-Test. Das Geschlecht
#               der Studierenden wird in der Spalte Geschlecht (1 = männlich, 2 = weiblich, 3 = divers) angegeben;
#               die Entfernung zur HdM (in km) steht in der Spalte Entfernung. Interpretieren Sie in einem
#               Kommentar in ihrem Skript das Ergebnis des Tests und gehen Sie hierbei auf den p-Wert ein.
#               Bestätigen die Daten die Hypothese?
testT <- t.test(dat$Entfernung[dat$Geschlecht == 2], dat$Entfernung[dat$Geschlecht == 1], alternative = "greater")

# 	Welch Two Sample t-test

# data:  dat$Entfernung[dat$Geschlecht == 2] and dat$Entfernung[dat$Geschlecht == 1]
# t = 2.4017, df = 496.58, p-value = 0.008342
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  3.233485      Inf
# sample estimates:
# mean of x mean of y
#  38.67813  28.37586

# Interpretation: Die Nullhypothese wird bestätigt, da der p-Wert unter 0,05 liegt (p-value = 0.008342).
# Somit kann man sagen, dass der Anfahrtsweg bei weiblichen Studenten länger ist.

# Aufgabe 3:    Visualisieren Sie das Ergebnis des in 2. durchgeführten t-Tests mit Hilfe eines Balkendiagramms.
#               Das Balkendiagramm soll zwei Balken enthalten (männlich und weiblich), die den Mittelwert der
#               Entfernung der jeweiligen Gruppe zur HdM anzeigen. Außerdem sollen die Fehlerbalken vorhanden
#               sein.

agg <- aggregate(Entfernung ~ Geschlecht, dat, mean)
agg$SD <- aggregate(Entfernung ~ Geschlecht, dat, sd)[, 2]
agg$n <- aggregate(Entfernung ~ Geschlecht, dat, length)[, 2]
agg$SE <- agg$SD / sqrt(agg$n)

agg <- agg[c(1, 2), ]

b <- barplot(agg$Entfernung,
    xlab = "Geschlecht", ylab = "Entfernung [km]",
    ylim = c(0, 65), col = c("#4fb0ff", "#005aa9")
)

axis(1, at = b[, 1], labels = agg$Geschlecht)
box()

arrows(
    x0 = b[, 1], y0 = agg$Entfernung - agg$SE, y1 = agg$Entfernung + agg$SE,
    code = 3, length = .15, angle = 90
)

# Aufgabe 4:    Betrachten Sie nun den Zusammenhang zwischen der Narzissmustendenz (N_gesamt) und der internalen Kontrollüberzeugung (IKU) der Teilnehmer.
#               Gibt es zwischen diesen beiden Variablen eine
#               Korrelation? Berechnen Sie zur Beantwortung dieser Frage den Pearson Korrelationskoeffizienten
#               mit Hilfe eines ungerichteten Korrelationstests, da hier keine gerichtete Hypothese aufgestellt wurde.
#               Interpretieren Sie das Ergebnis in einem Kommentar im Skript. Gehen Sie hierfür auf den p-Wert
#               und den Korrelationskoeffizienten ein.
pearson <- cor.test(dat$N_gesamt, dat$IKU,
    method = "pearson",
    alternative = "greater"
)

# 	Pearson's product-moment correlation

# data:  dat$N_gesamt and dat$IKU
# t = 7.0575, df = 611, p-value = 2.309e-12
# alternative hypothesis: true correlation is greater than 0
# 95 percent confidence interval:
#  0.211912 1.000000
# sample estimates:
#      cor
# 0.274543

# Interpretation: der P Wert liegt über 0,05. Der Korrelationskoeffizienten hat mit 0.274543 einen signifikaten positiven Zusammenhang zwischen
# der Narzissmustendenz und der internalen Kontrollüberzeugung.

# Aufgabe 5:    Visualisieren Sie den Zusammenhang zwischen dem Alter (Alter) der Versuchspersonen und ihrer
#               Narzissmusstendenz (N_gesamt) in einem Streudiagramm. Zeichnen Sie außerdem die Regressionslinie ein.

plot(N_gesamt ~ Alter, dat,
    cex = .7, col = "#4fb0ff",
    ylab = "Narzissmusstendenz", xlab = "Alter"
)

abline(lm(N_gesamt ~ Alter, dat), col = "#372d7b", lty = 2)