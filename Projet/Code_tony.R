### Code tony ###

### Importation des séries ###

# Lecture des données, brutes et corrigées
library(readr)
df_brut <- read_delim("Projet/Eaux mineral/data/valeurs_mensuelles_brut.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_corr <- read_delim("Projet/Eaux mineral/data/valeurs_mensuelles_corr.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Supprimer les 3 premières lignes et la dernière colonne inutile
df_brut <- df_brut[-(1:3), -3]
df_corr <- df_corr[-(1:3), -3]

# Renommer les colonnes
colnames(df_corr)[2] <- 'Prod_water'
colnames(df_corr)[1] <- 'Date'

colnames(df_brut)[2] <- 'Prod_water'
colnames(df_brut)[1] <- 'Date'

# Convertir la première colonne en une colonne de dates
df_brut$Date <- as.Date(paste(df_brut$Date, "-01", sep=""), format="%Y-%m-%d")
df_corr$Date <- as.Date(paste(df_corr$Date, "-01", sep=""), format="%Y-%m-%d")

# Mettre dans l'ordre les valeurs
df_brut <- df_brut[order(df_brut$Date), ]
df_corr <- df_corr[order(df_corr$Date), ]

# Conversion en valeurs numériques
df_brut$Prod_water <- as.numeric(df_brut$Prod_water)
df_corr$Prod_water <- as.numeric(df_corr$Prod_water)


### Visualisation ###

#Figure 1
# Diviser la fenêtre graphique en 1 ligne et 2 colonnes
par(mfrow = c(1, 2))

# Premier graphique
plot(df_brut$Date, df_brut$Prod_water, type = "l", 
     xlab = "Date", ylab = "Prod_water", 
     main = "Raw Series")

# Deuxième graphique
plot(df_corr$Date, df_corr$Prod_water, type = "l", 
     xlab = "Date", ylab = "Prod_water", 
     main = "Corrected Series (CVS-CJO)")

#Figure 3
# Diviser la fenêtre graphique en 1 ligne et 2 colonnes
par(mfrow = c(1, 2))

# Premier graphique
plot(df_corr$Date, df_corr$Prod_water, type = "l", 
     xlab = "Date", ylab = "Prod_water", 
     main = "Corrected Series")

# Deuxième graphique
plot(data_diff$index, data_diff$diff_prod_water, type = "l", 
     xlab = "Index", ylab = "Prod_water_diff", 
     main = "Differentiated series")


#### STATIONNARITE ####

# Calculer la série différenciée
diff_prod_water <- diff(df_corr$Prod_water) %>% ts()
diff_prod_water

# Tracer la série différenciée
par(mfrow = c(1, 1))
plot(diff_prod_water, type = "l", 
     xlab = "Time index", ylab = "", 
     main = "Differentiated Series")


### TESTS ###

## Before making unit root test we test that there is not a linear trend

# Créer un objet data.frame avec la série différenciée
data_diff <- data.frame(diff_prod_water = diff_prod_water)

# Ajouter une colonne pour l'index numérique
data_diff$index <- 1:length(diff_prod_water)

# Appliquer le modèle de régression linéaire
lm_result_diff <- lm(diff_prod_water ~ index, data = data_diff)

# Afficher un résumé du modèle
summary(lm_result_diff)

#EXPORTER LES RESULTATS
library(stargazer)
stargazer(lm_result_diff, title="Simple regression results",
          single.row=TRUE, no.space=TRUE)


## TESTS ADF ##
library(fUnitRoots)

#exemple d'utilisation du test ADF
adf <- adfTest(diff_prod_water, lag=1, type="nc") #on prend "nc" car il n'y a ni tendance, ni niveau fixe
adf

#Comment choisir le lag ? il faut prendre le plus petit qui "marche", 
#c'est à dire le plus petit pour lequel on n'a plus d'autocorréltion des résidus du modèle

#fonction qui teste l'autocorrélation de résidus jusqu'à l'ordre k
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))

series <- diff_prod_water
kmax <- 24
adftype="nc"

adfTest_valid <- function(series, kmax, adftype){
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k," lags: residuals OK? "))
    adf <- adfTest(series, lags=k, type=adftype)
    pvals <- Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T)==0) {
      noautocorr <- 1; cat("OK \n")
    } else cat("nope \n")
    k <- k+1
  }
  return(adf)
}

adf <- adfTest_valid(diff_prod_water,24,adftype="nc")
adf

#test ADF final avec lag=7
adf <- adfTest(diff_prod_water, lag=7, type="nc")
adf

## TEST KPSS ##

#Ce test a pour hypothèse nulle la stationnarité de la série, donc on ne veut pas pouvoir rejeter H0
#Ce test est complémnetaire du test ADF

library("tseries")
?kpss.test

kpss_result <- kpss.test(diff_prod_water, null="Level")
kpss_result



#### MODELISATION ####

### ACF et PACF de la série ###

par(mfrow = c(2, 1), mar = c(2, 4, 4, 1))  # Diviser la fenêtre graphique en 2 lignes et 1 colonne

# Tracer l'ACF
acf(diff_prod_water, main = "Autocorrelation function (ACF)")

# Tracer le PACF
pacf(diff_prod_water, main = "Partial autocorrelation function (PACF)")


#### ESTIMATION ####

# Estimation d'un modèle ARMA avec p = 7 et q = 1
install.packages('astsa')
library('astsa')
?sarima

arma_model <- sarima(diff_prod_water, 7, 0, 1, details=FALSE, no.constant = TRUE)
arma_model$ttable

#test portemanteau ?
install.packages('portes')
library('portes')
LjungBox(arma_model$fit, lags=seq(1,24,1))
?LjungBox


## MODEL SELECTION

# Initialiser un vecteur pour stocker les résultats
results <- matrix(NA, nrow = 7, ncol = 2)
colnames(results) <- c("AIC", "BIC")

# Boucle pour estimer les modèles ARMA avec différentes valeurs de p
for (p in 1:7) {
  arma_model <- arima(diff_prod_water, order = c(p, 0, 1), include.mean = FALSE)
  results[p, ] <- c(AIC(arma_model), BIC(arma_model))
}

# Afficher les résultats
results


# Résidus gaussiens du modèle ?

arma_model <- sarima(diff_prod_water, 7, 0, 1, details=FALSE, no.constant = TRUE)

?checkresiduals
checkresiduals(arma_model$fit, lag=24)

autoplot(arma_model$fit)

##### PREVISION ######

prod_water = ts(df_corr$Prod_water, start = c(1990, 1), end = c(2024, 2), frequency = 12)
prod_water

?arima
arima_model <- arima(prod_water, order=c(7, 1, 1), include.mean = FALSE)
arima_model

forecast_res <- forecast(arima_model, level=c(95), h=2)
forecast_res

plot(forecast_res, include=50)




