# Basics R

# working directory
getwd()
setwd("C:/RStudio/statistik_kurs_R")
getwd()

# Hilfe: wenn Name der Funktion bekannt
?mean
help(mean)
help("linear models")

# Hilfe: wenn Name der Funktion nicht bekannt
help.search("linear models")
?? "linear models"

# Variablen
# Skalar definieren (<- = Zusweisungsoperator)
a <- 10

# Vektor definieren (c= concetinate, verketten)
b <- c(1,2,3,4)

# Berechnungen werden immer komponentenweise durchgef?hrt: Beispiel Vektoraddition
a+b

# in Variable speichern
c <- a+b

# einen data.frame definieren
df <- data.frame(a, b)
df                                       # um es in Konsole anzuzeigen
names(df) <- c("VariableA", "VariableB") # um Variablen umzunennen
d <- c("a", "b", "c", "d")               # neue Variable hinzuf?gen, d= Vektor aus a,b,c: also aus Charaktervalues
df$VariableD <- d                        # um d an df anzuh?ngen, generiere ich neue Variable, $ um auf einzelne Variable zuzugreifen

letters [1:5]
LETTERS [1:10]
LETTERS [24:20]


# Beispiele
b <- c(1:10)                             # : um alle Zahlen zwischen 1-10 in Variable aufzunehmen (b=Vektor der L?nge 10)
c <- c("red", "blue", "yellow")          # c= Vektor aus drei Charaktervalues
d <- rnorm(100,5,0.5)                    # d= Vektor der 100 normalverteilte Zufallszahlen, generiert mit rnorm
e <- matrix(1:10,2)                      # e= Matrix, mit Dimensionen 2*5
f <- data.frame("Name1" = rep(a,10), "Name2" = b) # f= Dataframe mit zwei Spalten

plot(f)
str(e)                                   # um mir Struktur einer Variablen anzeigen zu lassen
summary(d)                               # gibt mir 5-Werte Statistik an
table(d)                                 # gibt KOntingenz oder H?ufigkeitstabelle aus, hier nicht sinnvorll, weil jeder Wert nur einmal vorkommt


# Operationen sind für bestimmte Objekte definiert
hist(d)                                 # Input muss immer Vektor sein
hist(f$Name2)                           # funktioniert, weil nur Name 2 ausgew?hlt, ganz f w?rde nicht funktionieren

# Objekte haben Attribute
dim(f)                                  # Matrizen und data frames haben eine Dimension

dimnames(f)                             # gibt Namen der Zeilen und Spalten aus
names(f)                                # gibt nur Namen der Spalten aus

length(b)                               # gibt L?nge des Vektors an
class(e)                                # gibt Datentyp an

# Objekte im Workspace
ls()                                    # zeigt in Konsole Objekte an die im Workspace sind
rm(a)                                   # entfernt Objekte aus Workspace

# Funktion = Objekt
myfunc <- function(x){                  # um eigene Funktion anzulegen
  y <- x^2
  y
}

q <- myfunc(x = 5)

# Packages
search()                               # zeigt alle geladenen Pakete an
install.packages("dplyr")              # um zu installieren
library(dplyr)                         # muss ausgef?hrt werden damit auch benutzbar
