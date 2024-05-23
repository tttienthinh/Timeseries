### Code tony ###


#### IMPORTATION ####

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

# Visualisation 

#Figure 1 : série brute - série corrigée

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


#### STATIONNARITE ####

# 1) DIFFERENCIATION

# Calculer la série différenciée
library(dplyr) 
diff_prod_water <- diff(df_corr$Prod_water) %>% ts()
diff_prod_water

# Tracer la série différenciée
par(mfrow = c(1, 1))
plot(diff_prod_water, type = "l", 
     xlab = "Time index", ylab = "", 
     main = "Differentiated Series")

# Série et série différenciée
x <- df_corr$Prod_water %>% ts()
x_diff <- diff(df_corr$Prod_water) %>% ts()

# 2) TESTS DE STATIONNARITE

## 2.1) On vérifie d'abord qu'il n'y a pas de tendance et que la série différenciée est centrée (moyenne nulle)

# Créer un objet data.frame avec la série différenciée
data_diff <- data.frame(diff_prod_water = diff_prod_water)
data_diff
# Ajouter une colonne pour l'index numérique
data_diff$index <- 1:length(diff_prod_water)

# Appliquer le modèle de régression linéaire
lm_result_diff <- lm(diff_prod_water ~ index, data = data_diff)

# Afficher un résumé du modèle
summary(lm_result_diff)

# Exporter la table des résultats
install.packages('stargazer')
library(stargazer)
stargazer(lm_result_diff, title="Simple regression results",
          single.row=TRUE, no.space=TRUE)


## 2.2) On procède au test de racine unitaire (ADF) pour le cas : sans tendance linéaire, sans niveau ##

install.packages('fUnitRoots')
library(fUnitRoots)

# On reprend la fonction du TD pour trouver le plus petit lag permettant d'éliminer l'autocorrélation

# Fonction qui teste l'autocorrélation de résidus jusqu'à l'ordre k
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

# Fonction qui trouve le plus petit lag à mettre dans le test adf, tel qu'il n'y a plus d'autocorrélation des résidus,
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

# Ici, la procédure conduit à choisir un lag de 7 (ce qui est logique vu le PACF plus bas), 
# Et le test correspondant rejette bien l'hypothèse nulle de présence d'une racine unitaire
adf <- adfTest_valid(x_diff,24,adftype="nc")
adf

## 2.3) Test KPSS complémentaire

# Ce test a pour hypothèse nulle la stationnarité de la série, donc on NE veut PAS pouvoir rejeter H0
# autrement dit, on souhaite une p-valeur supérieur aux seuils habituels de rejet

install.packages('tseries')
library("tseries")

kpss_result <- kpss.test(x_diff, null="Level")
kpss_result

#Figure 3 : série corrigée - série différenciée (stationnaire)
# Diviser la fenêtre graphique en 1 ligne et 2 colonnes
par(mfrow = c(1, 2))

# Premier graphique
plot(df_corr$Date, df_corr$Prod_water, type = "l", 
     xlab = "Date", ylab = "Prod_water", 
     main = "Corrected Series")

# Deuxième graphique
plot(diff_prod_water, type = "l", 
     xlab = "Time index", ylab = "", 
     main = "Differentiated Series")


#### MODELISATION ET ESTIMATION ####

## 1) ACF et PACF de la série

par(mfrow = c(2, 1), mar = c(2, 4, 4, 1))  # Diviser la fenêtre graphique en 2 lignes et 1 colonne

# Tracer l'ACF
acf(x_diff, main = "Autocorrelation function (ACF)")

# Tracer le PACF
pacf(x_diff, main = "Partial autocorrelation function (PACF)")

## 2) Estimation d'un modèle ARIMA avec p = 7, d=1 et q = 1
##    Recherche des autres modèles valides

install.packages('astsa')
library('astsa')

# the sarima function offers the advantge of printing automatically the p-values
model_7_1_1 <-sarima(x, 7, 1, 1, P = 0, D = 0, Q = 0, S = -1, 
                 details = TRUE, xreg = NULL, Model = TRUE,
                 fixed = NULL, tol = sqrt(.Machine$double.eps), 
                 no.constant = TRUE)
# everything is significant

# other choice of the parameters
p = 7
d = 1
q = 2

model_bis <-sarima(x, p, d, q, P = 0, D = 0, Q = 0, S = -1, 
                 details = TRUE, xreg = NULL, Model = TRUE,
                 fixed = NULL, tol = sqrt(.Machine$double.eps), 
                 no.constant = TRUE)

# p > 7 does not work
# q > 2 does not work

# Portemanteau test on the residuals
install.packages('LSTS')
library('LSTS')

Box.Ljung.Test(model_bis$fit$residuals, lag=24)

## 3) Sélection de modèle : BIC et AIC

# Créer une matrice pour stocker les résultats avec une colonne supplémentaire pour q
results <- matrix(NA, nrow = 3, ncol = 3)
colnames(results) <- c("q", "AIC", "BIC")

# Boucle pour estimer les modèles ARMA avec différentes valeurs de q
for (q in 0:2) {
  arma_model <- arima(x, order = c(7, 1, q), include.mean = FALSE)
  results[q + 1, ] <- c(q, AIC(arma_model), BIC(arma_model))
}

# Afficher les résultats
print(results)


##### PREVISION ######

install.packages("forecast")
library(forecast)

## 1) Observation des résidus

par(mfrow = c(1,1))
checkresiduals(residuals, lag=24)

#create Q-Q plot for residuals
qqnorm(residuals)

#add a straight diagonal line to the plot
qqline(residuals) 

## 2) ELLIPSE POUR LA SERIE INITIALE

model <- arima(x, order=c(7,1,1), include.mean = FALSE)
model

# Extraire les coefficients du modèle
ar1 <- coef(model)["ar1"]
ma1 <- coef(model)["ma1"]

# Définir les coefficients phi1, phi2 et theta1
phi1 <- 1 + ar1
phi2 <- ar1
theta1 <- ma1

# Afficher les coefficients
print(paste("phi1 =", phi1))
print(paste("phi2 =", phi2))
print(paste("theta1 =", theta1))

# Obtenir la variance des résidus (bruit blanc)
residuals <- residuals(model)

variance_residus <- var(residuals)

# Afficher la variance des résidus
print(paste("Variance du bruit blanc =", variance_residus))

# Calculer les éléments de la matrice
element11 <- variance_residus
element12 <- variance_residus * (phi1 - theta1)
element21 <- variance_residus * (phi1 - theta1)
element22 <- variance_residus * (1 + (phi1 - theta1)^2)

# Créer la matrice
Sigma <- matrix(c(element11, element12, element21, element22), nrow=2, ncol=2, byrow=TRUE)
print(Sigma)
install.packages("ellipse")
library(ellipse)

# Prédire les valeurs à T+1 et T+2 pour la série initiale ARIMA(7,1,1)
forecast_values <- predict(model, n.ahead=2)

# Extraire les prédictions
prediction_T1 <- forecast_values$pred[1]
prediction_T2 <- forecast_values$pred[2]

# Tracer l'ellipse de confiance
plot(ellipse(Sigma, centre=c(prediction_T1, prediction_T2), level=0.95),
     xlab="Prediction at T+1", ylab="Prediction at T+2",
     main="")

points(x=prediction_T1, y=prediction_T2, col="red", pch=19)
legend("topright", legend="Prédictions", pch=19, col="red", cex=0.8)
  
  
## 3) ELLIPSE POUR LA SERIE DIFFERENCIEE

model <- arima(x_diff, order=c(7,0,1), include.mean = FALSE)
model

# Extraire les coefficients du modèle
ar1 <- coef(model)["ar1"]
ma1 <- coef(model)["ma1"]

# Définir les coefficients phi1, phi2 et theta1
phi1 <- ar1
theta1 <- ma1

# Afficher les coefficients
print(paste("phi1 =", phi1))
print(paste("theta1 =", theta1))

# Obtenir la variance des résidus (bruit blanc)
residuals <- residuals(model)

variance_residus <- var(residuals)

# Afficher la variance des résidus
print(paste("Variance du bruit blanc =", variance_residus))

# Calculer les éléments de la matrice
element11 <- variance_residus
element12 <- variance_residus * (phi1 + theta1)
element21 <- variance_residus * (phi1 + theta1)
element22 <- variance_residus * (1 + (phi1 + theta1)^2)

# Créer la matrice
Sigma <- matrix(c(element11, element12, element21, element22), nrow=2, ncol=2, byrow=TRUE)
print(Sigma)

# Prédire les valeurs à T+1 et T+2 avec la série différenciée
forecast_values <- predict(model, n.ahead=2)

# Extraire les prédictions
prediction_T1 <- forecast_values$pred[1]
prediction_T2 <- forecast_values$pred[2]

# Tracer l'ellipse de confiance
plot(ellipse(Sigma, centre=c(prediction_T1, prediction_T2), level=0.95),
     xlab="Prediction at T+1", ylab="Prediction at T+2",
     main="")

points(x=prediction_T1, y=prediction_T2, col="red", pch=19)
legend("topright", legend="Prediction", pch=19, col="red", cex=0.8)

