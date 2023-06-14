getwd()
setwd("C:/RStudio/statistik_kurs_R")
getwd()

# Daten einlesen
data_sub_Amper <- read.table("C:/RStudio/statistik_kurs_R/Amper_bereinigt.csv", header = TRUE, sep = ";")
data_sub_Amper$Datum <- strptime(data_sub_Amper$Datum, format = "%F %H:%M:%S")
data_NO3 <- data_sub_Amper$NO3

dataNO3_TTest <- read.table("C:/RStudio/statistik_kurs_R/dataNO3_TTest.csv", header = T)

EDA_plots(data_NO3) # ?berpr?fen: Daten sehen normalverteilt aus
EDA_plots(dataNO3_TTest[1:154,2])
muGG <- mean(data_sub_Amper$NO3[!is.na(data_sub_Amper$NO3)])


# one sample t-Test
#stammen die Nitratdaten aus dataNO3_TTest aus einer Verteilung mit Mittelwert = muGG?
mu <- mean(dataNO3_TTest$NO3.mg.l.)      # Mittelwert der Stichprobe
sd <- sd(dataNO3_TTest$NO3.mg.l.)
n <- length(dataNO3_TTest$NO3.mg.l.)     # L?nge des Vektors
alpha <- 0.05
muGG <- 3.040393                         # Mittelwert der Grundgesamtheit

# Hypothesen   H0:mu = muGG , H1: mu =| muGG   # beidseitig   
      # oder:  H0: mu >= muGG, H1: mu < muGG   # einseitig, weil mu gr??er

# Testbeispiel mit 1 Stichprobe nicht funktional f?r diese Daten!!
# Teststatistik unter Annahme unbekannter Varianz der GG

Tstats <- ((mu-muGG)/sd)*sqrt(n)

# Ablehnbereich: Tstats: <= -t1-alpha/2(n-1) oder T >= t1-alpha/2(n-1)  # beidseitig
      # oder:  Tstats <= -t1-alpha(n-1)                               # einseitig, weil mu gr??er

# kritische Werte: beidseitig
Tquant_1 <- qt(alpha/2, df = n-1)
Tquant_r <- qt(alpha/2, df = n-1, lower.tail = F)
# kritische Werte: einseitig
Tquant <- qt(alpha, df = n-1, lower.tail = T)

# Testentscheidung beidseitig
Tstats    # eindeutig im Ablehnbereich, da viel gr??er als Tquant_1
Tquant_1
Tquant_r

Tstats <= Tquant_1 | Tstats >= Tquant_r  # als Abfrage dargestellt: True, also Tstats im Ablehnbereich
       # H0 wird also abgeleht
# p-Wert
(pt(Tstats, df = n-1, lower.tail = F))*2   

# Testentscheidung einseitig
Tstats
Tquant
Tstats <= Tquant    # False: also H0 bei einseitiger Fragestellung nicht abgelehnt

# p-Wert
(pt(Tstats, df = n-1, lower.tail = T))*2     # p-Wert also bei ca. 1%


# mit T-Test Funktion berechnen

t.test(dataNO3_TTest, mu = 3.040393, alternative = "two.sided", conf.level = 0.95)

res <- t.test(dataNO3_TTest, mu = 3.040393, alternative = "two.sided", conf.level = 0.95)    # speichert ergebnis unter variable res
attributes(res)    # zeigt struktur von res an
 
res$p.value       # gibt mir einzelne Werte aus
res$conf.int
res$method        # zeigt Methode an


# Mittels KI Berechnung

# Formel KI:
# mu +- t(1+alpha/2)(n-2)* (S1/sqrt(n1)+ S2/sqrt(n2))

ki1 <- mu +Tquant_1*(sd/sqrt(n))
ki2 <- mu +Tquant_r*(sd/sqrt(n))

ki1
ki2


# Teststatistik unter Annahme bekannter Varianz der GG
# Berechnung zu Fuss: gleiche Formel wie oben, nur wird sd mit der SD der GG ersetzt

install.packages("compositions")
library(compositions)

Gauss.test(dataNO3_TTest$NO3.mg.l., mean = 3.040393, sd = 1.415031, alternative = "two.sided")
 

# two sample t-Test: viel relevanter f?r diesen Datensatz: weil 2 Stichpunkte
# Stammen dataNO3_TTest und data_NO3 aus selber Grundgesamtheit? also kann data_NO3_TTest auch von Inkhofen stammen? --> selber Standord?

# ergleich dataNO3_Ttest und data_NO3
par(mfrow = c(1,1))
boxplot(dataNO3_TTest$NO3.mg.l., data_sub_Amper$NO3, names = c("neu", "subsatz_alt"))

# Fragestellung: Stammen beide Datens?tze aus derselben GG?

# Hypothesen: beidseitig: H0: mu1 = mu2, H1: mu1 =| mu2

# T-Test

mu1 <- mean(dataNO3_TTest$NO3.mg.l.)
mu2 <- mean(data_sub_Amper$NO3, na.rm = T)
sd1 <- sd(dataNO3_TTest$NO3.mg.l.)
sd2 <- sd(data_sub_Amper$NO3, na.rm = T)
n1 <- length(dataNO3_TTest$NO3.mg.l.)
n2 <- length(data_sub_Amper$NO3)
alpha <- 0.05
n <- n1+n2

# Teststatistik unter Annahme nicht gleicher Varianzen
Tstats1 <- (mu1-mu2) / (sqrt(sd1^2 / n1 + sd2^2 / n2))

# Teststatistik unter Annahme gleicher Varianzen
var.pooled <- weighted.mean(x=c(var(dataNO3_TTest$NO3.mg.l., na.rm = T), var(data_sub_Amper$NO3, na.rm = T), w=c(n1 -1, n2-1)))

Tstats2 <- (mu1 - mu2) / sqrt(var.pooled / n1 + var.pooled / n2)

# Ablehnbereich: beidseitig: H0 abgelehnt wenn: T <= -t1-alpha/2 oder T>= t1-alpha/2 (also T kleiner oder gr??er als Kritischer Wert)

Tquant_1 <- -qt(1-(alpha/2), df = n-2)
Tquant_r <- qt(1-(alpha/2), df = n-2)

Tstats1
Tquant_1
Tquant_r

Tstats1 <= Tquant_1 | Tstats1 >= Tquant_r    # ergibt True, also H0 kann abgelehnt werden, weil T ?ber kritischem Wert

# KI Berechnung: Formel: (mu1-mu2)+- t(1-alpha/)(n-2)* (sqrt(s1/n1 + s2/n2))

ki1 <- (mu1-mu2) + Tquant_1*(sqrt(sd1^2/n1+sd2^2/n2))
ki2 <- (mu1-mu2) + Tquant_r*(sqrt(sd1^2/n1+sd2^2/n2))

ki1    # Konfidenzinterval
ki2

# mit T-Test Funktion
res <- t.test(dataNO3_TTest$NO3.mg.l., data_sub_Amper$NO3)
res
# p-Wert sehr sehr klein, ~0 also Wahrscheinlichkeit praktisch 0, dass beobachteter Unterschied zw. Mittelwerten auftritt, unter Annahme dass sie nicht verschieden sind
# da hier P-Wert ann?hernd 0: H0 kann abgelehnt werden!


# Falls Daten nicht appr.normalverteilt sind --> U-Test
# nicht paramtetrischer Test, d.h. er nimmt keine Verteilung an
# untersucht Unterschiede in den Medianen

# Hypothesen: # kleiner/gr??er: H0: med0 >= med1, H1: med0 < med1
                              # H0: med0 <= med1, H1: med0 > med1
                              # H0: med0 = med1, H1: med0 =| med1

# Teststatistik W: Anzahl der Male, an denen med0 >, <, =| med1 ist
wilcox.test(dataNO3_TTest$NO3.mg.l., data_sub_Amper$NO3, alternative = c("two.sided"), conf.int = T)

# "location shift": Median der Unterschiede zwischen Wertepaaren aus beiden Verteilungen

# weiter Beispiele zur Formulierung
#t-Test nach Gruppen

install.packages("reshape2")
library(reshape2)

data(ChickWeight)
str(ChickWeight)

CWnew <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight") # f?r jedes Huhn Gewichtszunahme ?ber Zeit in einer Zeile
CWnew

CWnew$weightgain <- CWnew[,14]-CWnew[,3]  # Gewichtszunahme jedes Huhns ?ber Gesamtzeit

CWnew14 <- CWnew[CWnew$Diet == 1 | CWnew$Diet == 4,]  # nur H?hner mit Diet 1 oder 4 betrachtet

boxplot(weightgain~Diet, data = CWnew14)              # also H?hner mit Diet 4 sehr viel st?rkere Gewichtszunahme und geringere Streung

t.test(weightgain~Diet, data = CWnew14)    # signifikanter Unterschied: H0 abgelehnt



# Beta-Fehler und Power
# Trennsch?rfe bzw. Power ist die Wahrscheinlichkeit, H0 abzulehnen, wenn sie falsch ist.
# die Trennsch?rfe sollte m?glichst gro? sein, dann sinkt WSK einen Fehler 2. Art zu begehen, also keinen Effekt festzustellen obwohl es einen gibt
# Trennsch?rfe ist umso gr??er, je gr??er Stichprobenumfang n, je gr??er alpha und je gr??er Differenz zwischen Mittelwerten delta
# entspricht der Wahrscheinlichkeit: P(Tstats > t1-alpha|H1)

power.t.test(n = (n1+n2)/2, delta = mu2-mu1, sd = sqrt(sd1^2/n1+sd2^2/n2))

# wenn power bei 1 = 100%: Mittelwerte so weit voneinander entfernt, dass Wahrscheinlichkeit einen Effekt zu entdecken bei 100% liegt

# simulierte Daten
Diff <- seq(0,20, by = 0.25)   # Vektor der delta enth?lt 

# stat. Trennsch?rfe f?r verschiedene n
pow <- power.t.test(n = 10, delta = Diff, sd = 12)
pow1 <- power.t.test(n = 50, delta = Diff, sd = 12)
pow2 <- power.t.test(n = 100, delta = Diff, sd = 12)
pow3 <- power.t.test(n = 500, delta = Diff, sd = 12)                   
pow4 <- power.t.test(n = 1000, delta = Diff, sd = 12)  

plot(Diff, pow$power, type = "l", ylim =c(0,1), xlab = "Diff = ?1-?2", ylab = "Trennsch?rfe", main = "Einfluss von n und Diff auf die Trennsch?rfe")
 
lines(Diff, pow1$power, type = "l", col = "blue")
lines(Diff, pow2$power, type = "l", col = "green")
lines(Diff, pow3$power, type = "l", col = "orange")
lines(Diff, pow4$power, type = "l", col = "violet")
legend(15,0.4, fill = c("black", "blue", "green", "orange", "darkviolet"),
      legend = c("n=10", "n=50", "n=100", "n=500", "n=1000"), bty = "n")

# Trennsch?rfe umso h?her, je h?her delta, je h?her n, desto schneller Trennsch?rfe bei 1                    
   

# andere Anwendung der Funktion: Bestimmung von n
pow_n <- power.t.test(power = 0.5, delta = 2, sd = 12)
pow_n
# n= MindestUmfang den Stichprobe haben m?sste um signifikant zu sein, hier 278
# bei power kann ich anpassen: wenn statt 50%, 80%e Trennsch?rfe, muss n steigen: 566


# ANOVA
# Anwendung: mehrere Standorte vergleichen (wenn nur 2 Standorte entspricht Anova im wesentlichem dem T-test)

#Standortdaten einladen
data_main <- read.table("C:/RStudio/statistik_kurs_R/Main_Erlabrunn.csv", sep = ";", dec = ",", header = TRUE, na.strings = "", skip = 8)
main_ohnedatum <- data_main[,2:348]
main_ohnedatum[main_ohnedatum =="< BG"] <- 0
data_main <- data.frame(data_main[,1],main_ohnedatum)
main_dot <- as.data.frame(apply(data_main[,2:348], 1:2, function(x){gsub(",",".",x)}))
main_num <- apply(main_dot, 1:2, function(x){as.numeric(x)})
main_numeric <- data.frame(data_main[,1],main_num)
main_numeric$data_main...1. <- strptime(main_numeric$data_main...1., format = "%F %H:%M:%S")


data_regnitz <- read.table("C:/RStudio/statistik_kurs_R/Regnitz-Hausen.csv", sep = ";", dec = ",", header = TRUE, na.strings = "", skip = 8)
regnitz_ohnedatum <- data_regnitz[,2:338]
regnitz_ohnedatum[regnitz_ohnedatum =="< BG"] <- 0
data_regnitz <- data.frame(data_regnitz[,1],regnitz_ohnedatum)
regnitz_dot <- as.data.frame(apply(data_regnitz[,2:338], 1:2, function(x){gsub(",",".",x)}))
regnitz_num <- apply(regnitz_dot, 1:2, function(x){as.numeric(x)})
regnitz_numeric <- data.frame(data_regnitz[,1],regnitz_num)
regnitz_numeric$data_main...1. <- strptime(regnitz_numeric$data_regnitz...1., format = "%F %H:%M:%S")


boxplot(data_sub_Amper$NO3, main_numeric$NO3.N..mg.l....m.Tiefe., regnitz_numeric$NO3.N..mg.l....m.Tiefe.)

# k?nnen gleiche Varianzen angenommen werden? ja, mehr oder weniger
sd(data_sub_Amper$NO3, na.rm = T)
sd(main_numeric$NO3.N..mg.l....m.Tiefe., na.rm = T)
sd(regnitz_numeric$NO3.N..mg.l....m.Tiefe., na.rm = T)

# Hypothesen: H0: mu2 = mu3 = mu4, H1: mui =| mui f?r mindestens ein Paar
# Berechnung der F-Statistik

# 1. Standort eine Faktorvariable zuweisen
n2 <- length(data_sub_Amper$NO3)
n3 <- length(main_numeric$NO3.N..mg.l....m.Tiefe.)
n4 <- length(regnitz_numeric$NO3.N..mg.l....m.Tiefe.)

fac <- rep(LETTERS[1:3], c(n2,n3,n4))  #faktor-variable die die ersten 3 Buchstaben des Alphabets je in n2,n3,n4 wiederholt

# dataset aufbereiten, sodass alle Werte untereinander stehen
dataset <- c(data_sub_Amper$NO3, main_numeric$NO3.N..mg.l....m.Tiefe., regnitz_numeric$NO3.N..mg.l....m.Tiefe.)
dataset <- as.data.frame(cbind(dataset,fac))   # datenrahmen der in erster spalte die untereinanderstehenden Daten der drei Standorte und in 2. spalte die factorvariable
# also sieht man, dass A: Amper, B: Main, C: Regnitz
dataset <- dataset[,3]
str(dataset)  #noch nicht numerisch

dataset[,1] <- as.numeric(dataset[,1]) # numerische variable mit Messdaten
dataset[,2] <- factor(dataset[,2])     # in factorvariable umge?ndert
names(dataset) <- c("NO3", "Site")     # Namen angepasst
str(dataset)

# klassische ANOVa, setzt gleiche Varianz voraus
summary(aov(dataset$NO3~dataset$Site))  # p-wert sehr klein, also hochsignifikant: H0 kann abgelehnt werden: mindestens ein Paar unterscheidet sich signifikant

# welch Test, setzt nicht gleiche Varianz voraus
oneway.test(dataset$NO3~dataset$Site)
