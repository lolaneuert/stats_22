getwd()
setwd("C:/RStudio/statistik_kurs_R")
getwd()

data_sub_Amper$Datum <- strptime(data_sub_Amper$Datum, format = "%F %H:%M:%S")
str(data_sub_Amper)

# statistische Kennwerte: von kompletten data frames
colMeans(data_sub_Amper[,2:21], na.rm = T)   # Mittelwert der Spalten
rowMeans(data_sub_Amper[,2:21], na.rm = T)   # Mittelwert der Zeilen

sum(is.na.data.frame(data_sub_Amper))        # wie viele NAs im ganzen data frame
sum(is.na(data_sub_Amper$NO3))               # wie viele NAs in einem Vektor

# statistische Kennwerte: von Vektoren
mean(data_sub_Amper$NO3, na.rm = T)          # Mittelwert
median(data_sub_Amper$NO3, na.rm = T)        # Median

# wenn Median < Mittelwert ist Schiefer der Verteilung = rechtsschief

sd(data_sub_Amper$NO3, na.rm = T)            # Standardabweichung
sd(data_sub_Amper$NO3, na.rm = T)/mean(data_sub_Amper$NO3, na.rm = T)  # relative Standardabweichung (coeff. of variation)  

var(data_sub_Amper$NO3, na.rm = T)          # Varianz
range(data_sub_Amper$NO3, na.rm = T)        # Maximum und Minimum
quantile(data_sub_Amper$NO3, c(0.25,0.75), na.rm = T) # Quantile

summary(data_sub_Amper$NO3)                 # 5 Punkte Statistik: Min, Max, Median, Mean, NA's, 1st & 3rd Quantil


# Index plots: zum feststellen von Mustern/Variabilit?t und evtl. Ausrei?ern

plot(data_sub_Amper$NO3)                   # Plot von NO3 an Index
plot(data_sub_Amper$Datum, data_sub_Amper$NO3, pch = 19, cex = 0.5, col = "dark green") # Plot von NO3 an Datum

# zweite Variable hinzuf?gen
plot(data_sub_Amper$Datum, data_sub_Amper$NO3, pch = 19, cex = 0.5, col = "dark green")
points(data_sub_Amper$Datum, data_sub_Amper$TOC, col = "blue")  # Variable TOC zu NO3 hinzugef?gt

# mehrere plots untereinander/nebeneinander
par(mfrow = c(3,1))
plot(data_sub_Amper$NO3)
plot(data_sub_Amper$TOC)
plot(data_sub_Amper$P.ges)                # drei plots untereinander, wenn par(mfrow = c(1,3)) w?ren sie nebeneinander
 
# im zeitlichen Verlauf f?r mehrere Variablen
plot.ts(data_sub_Amper[,2:10])            # max 10

# x-y plots: zum feststellen von Zusammenh?ngen
par(mfrow = c(1,2))
plot(data_sub_Amper$NO3~data_sub_Amper$TOC)
plot(data_sub_Amper$P.ges~data_sub_Amper$O2.geloest)

# um alle Daten des Datensatzes gegeneinander zu plotten
pairs(data_sub_Amper[1:991,2:21])

# Histogramme: zum feststellen von Symmetrie und Verteilung
par(mfrow = c(1,1))
hist(data_sub_Amper$NO3)
hist(data_sub_Amper$NO3, breaks = 20, col = "red")

# mehrere Histogramme als png abspeichern auf einem Blatt
png("histogramme.png") # exportiert Histogramme als png

par(mfrow = c(2,3))
hist(data_sub_Amper$NO3)
hist(data_sub_Amper$TOC)
hist(data_sub_Amper$P.ges)
hist(data_sub_Amper$O2.geloest)
hist(data_sub_Amper$NH4.N)
hist(data_sub_Amper$ph.Wert)

dev.off()      #schlie?t png-Fenster wieder

# boxplots: zum feststellen der Lage von Median & Quantilen, Vergleich von Variablen/Gruppen
boxplot(data_sub_Amper$NO3)
boxplot(data_sub_Amper$NO3, data_sub_Amper$TOC)

data_sub_Amper$months <- format(data_sub_Amper$Datum, format = "%m")  # neue Monatsspalte erstellt

boxplot(data_sub_Amper$NO3~data_sub_Amper$months)                     # zum Vergleich der NO3 boxplots im Jahresverlauf, also pro Monat

# plot der Dichtefunktion und Kummulativen Verteilungsfunktion: um festzustellen ob normalverteilt
# Dichtefunktion: Masse unter der Gesamtfl?che des Integrals umfasst Gesamtmasse und damit = 1
plot(density(data_sub_Amper$NO3[!is.na(data_sub_Amper$NO3)]))         # klassische Darstellung der Normalbverteilung, Gesamtfl?che = 1
abline(v = mean(data_sub_Amper$NO3, na.rm = T), col = "red")          # Kennzeichnet Mittelwert als Linie in Dichteverteilungsplot

# kurzer Exkurs: plotten normalverteilter Daten
x <- seq(-4,4, length = 100)
y <- dnorm(x)
plot(x,y)

# kummulative Verteilungsfunktion
z_TA <- (data_sub_Amper$NO3-mean(data_sub_Amper$NO3, na.rm = T))/sd(data_sub_Amper$NO3, na.rm = T)   #daf?r muss ich zuerst alle Datenh standardisieren: also mean abziehen und durch Sd teilen
plot(ecdf(z_TA))      #empirical cummulative distribution function: plottet Verteilungsfunktion

# zum besseren Vergleich, Werte f?r normalverteilte Daten mit lines hinzuf?gen
Z <- seq(-4,4,0.1)      #generiere nomralverteilte Zahlen
lines(Z, pnorm(Z,0,1), col = "blue", lwd = 3)    # equivalent zu Funktion points, nur mit Linien

# QQ-plot (Quantil-Quantil-Diagramm): vergleicht Lage der Quantile zweier Variablen, um festzustellen ob normalverteilt
qqnorm(data_sub_Amper$NO3, cex = 0.5) # Daten der Quantile
qqline(data_sub_Amper$NO3)            # Linie stellt dar wie Quantile verteilt sein m?ssten, wenn normalverteilt

# EDA_Kennzahlen gesammelt
  mean(x, na.rm = T)         
  median(x, na.rm = T)     
  sd(x, na.rm = T)            
  sd(x, na.rm = T)/mean(data_sub_Amper$NO3, na.rm = T)  
  var(x, na.rm = T)         
  range(x, na.rm = T)        
  quantile(x, c(0.25,0.75), na.rm = T)
  summary(x)

# EDA_Plots gesammelt als Funktion
EDA_plots <- function(x){
 par(mfrow = c(3,2))
 plot(x)
 hist(x)
 boxplot(x)
 qqnorm(x)
 qqline(x)
 plot(density(x[!is.na(x)]))
  abline(v = mean(data_NO3, na.rm = T), col = "red")
  abline(v = median(data_NO3, na.rm = T), col = "green")
  abline(v = sd(data_NO3, na.rm = T), col = "blue")
 z_TA <- (x-mean(x, na.rm = T))/sd(x, na.rm = T)
 plot(ecdf(z_TA))
}

nitrat_analysis <- EDA_analysis(x = data_NO3)
nitrat_plots <- EDA_plots(x = data_NO3)


# Ausrei?er finden
data_NO3 <- data_sub_Amper$NO3  # subset

# Grenzwerte f?r Ausrei?er berechnen nach 1.5* IQR (Interquantil-Regel)
summary(data_NO3)
q1 <- summary(data_NO3)[2]       # speichere mir 1. Quantil als Skalar
q3 <- summary(data_NO3)[5]       # speichere mir 3. Quantil als Skalar

iqr <- q3-q1

outl_r_oben <- q3 + (1.5*iqr)   # f?r Ausrei?er dr?ber
outl_r_unten <- q1 - (1.5*iqr)  # f?r Ausrei?er drunter

# Ausrei?er entfernen
par(mfrow = c(1,1))

plot(data_NO3)
abline(h = outl_r_oben, col = "red")  # zeigt rote Linie ?ber welcher Ausrei?er liegen
abline(h = outl_r_unten, col = "red") # zeigt rote Linie unter welcher Ausrei?er liegen

otl <- identify(data_NO3) 
data_NO3_otl <- data_NO3[-otl]

boxplot(data_NO3)
boxplot(data_NO3_otl)
