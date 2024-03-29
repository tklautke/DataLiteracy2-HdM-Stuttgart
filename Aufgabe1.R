# Aufgabe1.R
# Bearbeitete Aufgaben zu der ersten Abgabe in DataLiteracy
#
# Input: Erstsemesterbefragung_WiSe19-WiSe21.csv
# Output: /

######################################
# Aufgabenteil 1
######################################
# Aufgabe 1:    Einen Vektor Alter der die folgenden Alter der Versuchspersonen enthält:
#               21, 18, 24, 19, 20, 22, 27, 20, 19, 23, 18, 19, 23, 20, 18, 29, 20, 24, 21, 20.
ages <- c(
    21, 18, 24, 19, 20, 22, 27, 20, 19,
    23, 18, 19, 23, 20, 18, 29, 20, 24, 21, 20
)

# Aufgabe 2:    Einen Vektor Geschlecht der die Geschlechter der Versuchspersonen enthält. Die ersten 10 Ver-
#               suchspersonen waren männlich, die 10 danach weiblich. Verwenden Sie zum Erstellen dieses Vektors
#               die Funktion rep()
sex <- c("male", "female")
sex <- rep(sex, each = 5)


# DataFrame für weitere Aufgaben
dataFrame <- data.frame(ages, sex)

# Aufgabe 3:    Berechnen Sie den Mittelwert und die Standardabweichung des Alters Ihrer Versuchspersonen.
averageAge <- mean(ages)
standardDeviationAge <- sd(ages)

# Aufgabe 4:    Ist die 5. Versuchsperson älter als die 11. Versuchsperson? Überprüfen Sie dies durch Indizieren der
#               Werte und einen geeigneten Vergleichsoperator.
isPersonOlder <- ages[5] > ages[11]

# Aufgabe 5:    Wie viele Versuchspersonen sind unter 22 Jahre alt?
agesTable <- table(ages)
youngerThan22 <- agesTable[names(agesTable) < 22]


######################################
# Aufgabenteil 2
######################################
dat <- read.table("Erstsemesterbefragung_WiSe19-WiSe21.csv", header = T, sep = ";")

# Delete `N_Manip1`.
dat$N_Manip1 <- NULL

# Replace -99 und -77 to NA
dat[dat == -99] <- NA
dat[dat == -77] <- NA

# Delete all NAs
dat2 <- na.omit(dat)
nrow(dat) - nrow(dat2)
dat <- na.omit(dat)

# Aufgabe 6:    Wie viele Teilnehmer/innen waren männlich (m), wie viele waren weiblich (w)?
sexTable <- table(dat$Geschlecht)

maleSum <- sexTable[names(sexTable) == 1]
femaleSum <- sexTable[names(sexTable) == 2]
variousSum <- sexTable[names(sexTable) == 3]

# Aufgabe 7:    Lassen Sie sich die Nutzungshäufigkeiten für Smartphones und Tablet PCs anzeigen. Was waren die
#               jeweils am häufigsten angegebenen Werte für die Nutzungshäufigkeiten dieser Geräte für das Studium?
find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
}

modeSmartphone <- find_mode(dat$HaeufSmartphone) # Modus Rating = 3
modeTablet <- find_mode(dat$HaeufTabletPC) # Modus Rating = 1

# Aufgabe 8:    Erstellen Sie im Data Frame dat eine neue Spalte namens Geschwindigkeit, in der Sie für jede/n
#               Teilnehmer/in das Verhältnis von Entfernung und Fahrtdauer einspeichern.
dat$Geschwindigkeit <- c(dat$Entfernung / dat$Fahrtdauer)

# Aufgabe 9:    Berechnen Sie den Mittelwert der Geschwindigkeit Spalte getrennt für Frauen und Männer.
speedMan <- mean(dat[dat$Geschlecht == 1, "Geschwindigkeit"])
speedWoman <- mean(dat[dat$Geschlecht == 2, "Geschwindigkeit"])