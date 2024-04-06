### Code tony ###

### Importation des séries ###

# Lecture des données, brutes et corrigées
library(readr)
df_brut <- read_delim("Projet/sorbet/valeurs_mensuelles.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_corr <- read_delim("Projet/corrected/valeurs_mensuelles.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Supprimer les 3 premières lignes et la dernière colonne inutile
df_brut <- df_brut[-(1:3), -3]
df_corr <- df_corr[-(1:3), -3]

# Renommer les colonnes
colnames(df_corr)[2] <- 'Prod_sorbet'
colnames(df_corr)[1] <- 'Date'

colnames(df_brut)[2] <- 'Prod_sorbet'
colnames(df_brut)[1] <- 'Date'

# Convertir la première colonne en une colonne de dates
df_brut$Date <- as.Date(paste(df_brut$Date, "-01", sep=""), format="%Y-%m-%d")
df_corr$Date <- as.Date(paste(df_corr$Date, "-01", sep=""), format="%Y-%m-%d")

# Mettre dans l'ordre les valeurs
df_brut <- df_brut[order(df_brut$Date), ]
df_corr <- df_corr[order(df_corr$Date), ]

# Conversion en valeurs numériques
df_brut$Prod_sorbet <- as.numeric(df_brut$Prod_sorbet)
df_corr$Prod_sorbet <- as.numeric(df_corr$Prod_sorbet)

### Visualisation ###

plot(df_brut$Date, df_brut$Prod_sorbet, type = "l", 
     xlab = "Date", ylab = "Prod_sorbet", 
     main = "Production de sorbet brute, au fil du temps")

plot(df_corr$Date, df_corr$Prod_sorbet, type = "l", 
     xlab = "Date", ylab = "Prod_sorbet", 
     main = "Production de sorbet corrigée, au fil du temps")

### REMARQUE ###

"""
Je ne sais pas comment ils ont corrigé les variations, 
mais la variabilité de la série explose à partir de 2009 (ce qui ne se voit pas sur la série brute).
Donc c'est sûr que la série corrigée ne sera pas stationnaire, pusiqu'il faut une constance 
de la variance au cours du temps. Le traitement de la série devra être différent sur la période 2009-2023
"""

#Observation détaillée de la période 2010-2011

install.packages("lubridate")
library(lubridate)

df_2010_2013 <- subset(df_brut, year(Date) >= 2010 & year(Date) <= 2011)

plot(df_2010_2013$Date, df_2010_2013$Prod_sorbet, type = "l", 
     xlab = "Date", ylab = "Prod_sorbet", 
     main = "Production de sorbet brute, sur 2010-2013",
     xaxt = "n")
axis.Date(1, at = df_2010_2013$Date, labels = format(df_2010_2013$Date, "%b-%Y"), las = 2)


### ACF et PACF de la série ###

#Tracer l'ACF
acf(df_corr$Prod_sorbet, main = "Fonction d'autocorrélation (ACF) de Prod_sorbet")

# Tracer le PACF
pacf(df_corr$Prod_sorbet, main = "Fonction d'autocorrélation partielle (PACF) de Prod_sorbet")




