setwd("C:/RStudio/statistik_kurs_R")

# Regression

library(MASS)
data_amp <- read.table("C:/RStudio/statistik_kurs_R/Amper_bereinigt.csv", header = TRUE, sep = ";")
data_amp$Datum <- strptime(data_amp$Datum, format = "%F %H:%M:%S")
NO3 <- data_amp$NO3
str(data_amp)

png("pairs_met_amp.png")
pairs(data_amp[,2:21])
dev.off()

with(data_amp, plot(NO3~Wassertemp))
with(data_amp, plot(NO3~O2.geloest))
with(data_amp, plot(NO3~NH3_N))

# bestes model laut meyer:
# modelle laut meyer: No2, O2, Natrium, Temp, mn und fe auch ok


# NAs entfernen?
data_sub <- data.frame(data_amp$Datum, data_amp$NO3, data_amp$O2.geloest, data_amp$Wassertemp, data_amp$NH3_N)

sum(is.na.data.frame(data_sub))  # z?hlt NAs: 732
data_amp_comp <- na.omit(data_sub)   # entfernt Zeilen mit NAs

with(data_amp_comp, plot(data_amp.NO3~data_amp.Wassertemp))
with(data_amp_comp, plot(data_amp.NO3~data_amp.O2.geloest))
with(data_amp_comp, plot(data_amp.NO3~data_amp.NH3_N))
names(data_amp_comp) <- c("Datum", "NO3", "O2.geloest", "Wassertemp", "NH3_N")

# Modelle erstellen

mod1 <- lm(NO3~Wassertemp, data = data_amp_comp)
mod2 <- update(mod1, NO3~Wassertemp+O2.geloest, data = data_amp_comp)
mod3 <- update(mod2, NO3~Wassertemp+O2.geloest+NH3_N, data = data_amp_comp)

# y <- beta0 + beta1 * Wassertemp
# y <- beta0 + beta1 * Wassertemp + beta2 * O2.geloest
# y <- beta0 + beta1 * Wassertemp + beta2 * O2.geloest + beta3 * NH3_N

# Modellg?te: Modelle bewerten: p-Wert, R2

summary(mod1)
summary(mod2)
summary(mod3)
# nach R2 enth?lt drittes Modell (R2 0.442) mit ca 42% die h?chste Erkl?rung f?r die Varianz von Nitrat
# Modellwahl nach AIC: Ergebnis: wie viel Information geht verloren? nur relativ sinnvoll: je kleiner desto besser
AIC(mod1)
AIC(mod2)
AIC(mod3)  # auch hier drittes Modell am niedrigsten, also am besten

# bottom-up oder top-down, add oder drop
# top-down:
drop1(mod3, ~Wassertemp, data = data_amp_comp)
drop1(mod3, ~., data = data_amp_comp)

drop1(mod1, ~., data= data_amp_comp)
drop1(mod2, ~., data = data_amp_comp)

# bottom-up
add1(mod1, ~.+O2.geloest, data = data_amp_comp)

stepAIC(mod3)    # aus MASS package
# nach Modellg?tekriterien ist drittes Modell am besten: kleinste AIC und h?chster R2


# Modelldiagnostik: ?berpr?fen von Residuen: normalverteilt mit sigma^2 und MW 0
hist(mod3$residuals)   # Normalverteilung ~ok
mean(mod3$residuals)   # Mittelwert der Residuen: -3.673
mean(resid(mod3))      # selbes Ergebnis wie oben -3.673

par(mfrow = c(2,2))
plot(mod3)             # Diagnostikplot

#rohe Residuen: oben links: sind Daten homoskidastisch? Residuen schwanken um 0 bis auf ein paar Ausnahmen oben und unten (markiert)
#standardisierte Residuen: unten links: sind Residuen korreliert? hier nicht der Fall (Korrelation w?re als Muster erkennbar)
#Quantils-Quantilsdiagramm: oben rechts: im oberen und unteren Bereich einige ungew?hnliche Beobachtungen aber sonst sehr sch?n auf gestrichelter Linie(=Standardnormalverteilung)
#Hebelwerte vs. stand. Residuen: unten links: wie gro? ist Effekt eines bestimmten Wertes auf Modell? wenn sich Modell nicht ?ndert, dann zwar ungw?hnliche Beobachtung aber nicht relevant: hier keine Wert mit gro?em Cook's-Abstand (Ma? f?r Effekt)

# Au?rei?er w?rde man idealerweise ?berpr?fen und entfernen

# Vorhersage der Daten

ydach <- predict(mod3, data = data_amp_comp[,c(3,4,5)])

par(mfrow = c(1,1))
plot(data_amp_comp$NO3, type = "l")
lines(ydach, col = "red")     # modellierte Daten: Schwankung wird nicht ?berall getroffen, vor allem f?r starke positive und negative Werte, es fehlen also noch erkl?rende Variablen: Modell ist nicht vollst?ndig


# Vorhersage f?r neuen Standort: Jochenstein(Donau)


# Daten einladen und bereinigen
data_joch <- read.table("C:/RStudio/statistik_kurs_R/Donau-Jochenstein.csv", sep = ";", dec = ",", header = TRUE, na.strings = "", skip = 8)
joch_ohnedatum <- data_joch[,2:341]
joch_ohnedatum[joch_ohnedatum =="< BG"] <- 0
data_joch <- data.frame(data_joch[,1],joch_ohnedatum)
joch_dot <- as.data.frame(apply(data_joch[,2:341], 1:2, function(x){gsub(",",".",x)}))
joch_num <- apply(joch_dot, 1:2, function(x){as.numeric(x)})
joch_numeric <- data.frame(data_joch[,1],joch_num)
joch_numeric$data_joch...1. <- strptime(joch_numeric$data_joch...1., format = "%F %H:%M:%S")

# Dataframe anpassen: NAs entfernen und nur relevante Variablen behalten und umbenennen

data_sub_joch <- data.frame(data_joch$data_joch...1., data_joch$NO3.N..mg.l....m.Tiefe., data_joch$O2.gel?.st..mg.l....m.Tiefe., data_joch$Wassertemp..vor.Ort...?.C....m.Tiefe., data_joch$NH3.N..µg.l....m.Tiefe.)

sum(is.na.data.frame(data_sub_joch))  # z?hlt NAs: 732
data_joch_comp <- na.omit(data_sub_joch)

names(data_joch_comp) <- c("Datum", "NO3", "O2.geloest", "Wassertemp", "NH3_N")

# Modelle f?r Jochstein definieren
mod4 <- lm(NO3~Wassertemp, data = data_joch_comp)
mod5 <- update(mod4, NO3~Wassertemp+O2.geloest, data = data_joch_comp)
mod6 <- update(mod5, NO3~Wassertemp+O2.geloest+NH3_N, data = data_joch_comp)

# Vorhersage f?r Jochstein
ydonau <- predict(mod6, data = data_joch_comp[,c(3,4,5)])

par(mfrow = c(1,1))
plot(data_joch_comp$NO3, type = "l")
lines(ydonau, col = "red") 
