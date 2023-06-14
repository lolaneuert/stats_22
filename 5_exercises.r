---
title: "Übung 4"
author: "Lola Neuert"
date: '2022-02-23'
output: html_document
---
# Inferenz
## Vergleich zweier Datensätze: könnten sie vom selben Standord (Inkhofen) stammen?
### 1. Daten einladen
```{r data}
data_sub_Amper <- read.table("C:/RStudio/statistik_kurs_R/Amper_bereinigt.csv", header = TRUE, sep = ";")
data_sub_Amper$Datum <- strptime(data_sub_Amper$Datum, format = "%F %H:%M:%S")
data_NO3 <- data_sub_Amper$NO3

dataNO3_TTest <- read.table("C:/RStudio/statistik_kurs_R/dataNO3_TTest.csv", header = T)
```

### 2. Normalverteilung beider Datensätze prüfen
für den ersten Datensatz kann Normalverteilung angenommen werden, für den zweiten weniger
```{r distribution}
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
EDA_plots(data_sub_Amper$NO3) 
EDA_plots(dataNO3_TTest$NO3.mg.l.)
```

### 3. Zwei Stichproben T-Test
Um zu überprüfen ob beide Datensätze vom selben Standort stammen könnten wird ein two sample T-Test durchgeführt, dabei wird vorallem die Frage beantwortet ob die Differenz der Mittelwerte statistisch möglich ist, unter der Annahme, dass die Mittelwerte eigentlich gleich sein sollten. Es wird für beide Datensätze eine appr. Normalverteilung angenomen und ein Signifikanzniveau von alpha = 5% festgelegt (Konfidenzintervall also 95%).
```{r t-test basics}
mu1 <- mean(dataNO3_TTest$NO3.mg.l.)
mu2 <- mean(data_sub_Amper$NO3, na.rm = T)
sd1 <- sd(dataNO3_TTest$NO3.mg.l.)
sd2 <- sd(data_sub_Amper$NO3, na.rm = T)
n1 <- length(dataNO3_TTest$NO3.mg.l.)
n2 <- length(data_sub_Amper$NO3)
alpha <- 0.05
n <- n1+n2
```
Die Hypothesen für diesen beidseitigen T-Test lauten also: H0: mu1 = mu2, H1: mu1 =| mu2. Die 0-Hypothese besagt also dass die beiden Mittelwerte gleich sind, während die 1-Hypothese besagt, dass die Mittelwerte nicht gleich sind.

#### 3.1 T-Statistik
```{r tstats}
Tstats1 <- (mu1-mu2) / (sqrt(sd1^2 / n1 + sd2^2 / n2))

Tquant_1 <- -qt(1-(alpha/2), df = n-2)
Tquant_r <- qt(1-(alpha/2), df = n-2)

Tstats1 <= Tquant_1 | Tstats1 >= Tquant_r   # Ergebnis = True
```
Die Formel für die T-Statistik lautet T = (x - y)/sqrt((sx²/n)+(sy²/m)), also in diesem Fall (mu1-mu2)/(sqrt(sd1²/n1 + sd2²/n2). Der Ablehnbereich entspricht einem T größer als (-t1-alpha/2) bzw. kleiner als (t1-alpha/2).

#### 3.2 Konfidenz Intervall und P-Wert
```{r confidence interval + p-value}
res <- t.test(dataNO3_TTest$NO3.mg.l., data_sub_Amper$NO3)
```
In diesem Fall ist der P-Wert mit 2.2e^-16 sehr klein, die Wahrscheinlichkeit also praktisch 0, das 95% Konfidenzinterwall liegt zwischen 1.5 und 2.0. H0 kann also abgelehnt werden weil die T-Statistik weit außerhalb des Konfidenzintervalls liegt (15.1) und signifikante Unterschiede zwischen den Datensätzen bestehen. Die Annahme, dass die Daten vom selben Standord stammen könnten wird also verworfen.

### 4.  Trennschärfe
da power=1, liegt die Trennschärfe bei 100%
```{r trennschärfe}
power.t.test(n = (n1+n2)/2, delta = mu2-mu1, sd = sqrt(sd1^2/n1+sd2^2/n2))
```

### 5. weiterer Standort
Voruntersuchung besagt, dass Nitratkonzentration ungefähr bei 3.5 mg/l liegt (Delta zu mu2 also 0.46). Stichprobenumfang muss 52.7 groß sein, damit auf dem 5% Signifikanzniveau eine Trennschärfe von min 80% zu den Nitratwerten der Amper vorliegt.
```{r samplesize}
st_fehler <- sd2/sqrt(n2)
pow_n <- power.t.test(power = 0.8, delta = 0.46, sd = sd2)
pow_n
```

### 6. ANOVA
Mithilfe einer Anova wird überprüft ob signifikante Unterschiede zwischen den Nitratdaten der Amper, des Mains und der Regnitz vorliegen. Die 0-Hypothese besagt, dass keine signifikanten Unterschiede zwischen den drei Standorten vorliegen (H0: mu2=mu3=mu4), während die 1-Hypothese besagt, dass zwischen mindestens einem der Paare ein signifikanter Unterschied vorliegt (H1: mui =| mui).

#### 6.1 Einladen und bereinigen der Daten
zuerst müssen die Daten für den Main und die Regnitz eingeladen und bereinigt werden, dafür wird das Datum angepasst, alle character values entfernt und alles in numerische Values umgerechnet
```{r data.1, warning = FALSE}
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
```


#### 6.2 boxplots und Varianzen berechnen
zum Vergleich wird ein boxplot der verschiedenen Standorte berechnet. Anschließend wird die Varianz der verschiedenen Standorte berechnet, die mehr oder weniger übereinstimmt, was die Vorraussetzung für eine ANOVA darstellt.
```{r boxplots}
boxplot(data_sub_Amper$NO3, main_numeric$NO3.N..mg.l....m.Tiefe., regnitz_numeric$NO3.N..mg.l....m.Tiefe.)

# können gleiche Varianzen angenommen werden? ja, mehr oder weniger
sd(data_sub_Amper$NO3, na.rm = T)
sd(main_numeric$NO3.N..mg.l....m.Tiefe., na.rm = T)
sd(regnitz_numeric$NO3.N..mg.l....m.Tiefe., na.rm = T)
```

#### 6.3 Standorten eine Faktorvariable zuweisen
```{r factor}
n2 <- length(data_sub_Amper$NO3)
n3 <- length(main_numeric$NO3.N..mg.l....m.Tiefe.)
n4 <- length(regnitz_numeric$NO3.N..mg.l....m.Tiefe.)

fac <- rep(LETTERS[1:3], c(n2,n3,n4))  # FAktor-Variable, die die ersten 3 Buchstaben des Alphabets je in n2,n3,n4 wiederholt

# dataset aufbereiten, sodass alle Werte untereinander stehen
dataset <- c(data_sub_Amper$NO3, main_numeric$NO3.N..mg.l....m.Tiefe., regnitz_numeric$NO3.N..mg.l....m.Tiefe.)
dataset <- as.data.frame(cbind(dataset,fac))   
# im Datenrahmen sieht man, dass A: Amper, B: Main, C: Regnitz

dataset[,1] <- as.numeric(dataset[,1]) # numerische Variable mit Messdaten
dataset[,2] <- factor(dataset[,2])     # in Factorvariable umgeändert
names(dataset) <- c("NO3", "Site")     # Namen angepasst
str(dataset)
```

#### 6.4 Berechnung der Anova
diese setzt gleiche Varianz voraus
```{r anova}
summary(aov(dataset$NO3~dataset$Site))  
```
p-Wert ist sehr klein, also hochsignifikant: H0 kann abgelehnt werden: mindestens ein Paar der Standorte Amper, Main und Regnitz unterscheidet sich signifikant

#### 6.5 Berechnung des Welch-Tests
dieser setzt nicht die gleiche Varianz aus, in diesem Fall stimmen die Ergebnisse aber überein, also ist die Annahme von oben, mit gleichen Varianzen zu rechnen, korrekt
```{r welch}
oneway.test(dataset$NO3~dataset$Site)
```
