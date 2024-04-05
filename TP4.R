require(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
require(tseries) #diverses fonctions sur les series temporelles

#### Q1 ####
# path <- "C:/Users/csaurel/Desktop/THESE/Cours_Ensae/STL"
# setwd(path) #definit l'espace de travail (working directory ou "wd")
# getwd() #affiche le wd
# list.files() #liste les elements du wd

datafile <- "Donnees1.csv" #definit le fichier de donnees

data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame
xm.source <- zoo(data[[1]]) #convertit le premier element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 dernieres valeurs

#### Q2 ####
plot(xm) #affiche la serie temporelle

plot(xm, xaxt="n") #
axis(side=1,at=seq(0,196,12)) #
acf(xm) #

# 

desaison <- xm-lag(xm,-12) #
par(mfrow=c(1,2)) #
plot(desaison)
acf(desaison)

#### Q3 ####
acf(desaison) #
pacf(desaison) #

# 
#### Q4 ####
dev.off() #reinitialise les parametres des representations graphiques
plot(desaison)

# 

pp.test(desaison) #
# 

#### Q5 ####
acf(desaison); pacf(desaison)
axis(side=1,at=seq(0,25))
# pmax = 3
# qmax = 2
y <- desaison - mean(desaison) #
par(mfrow=c(1,2)) 
acf(y,24);pacf(y,24) 

# 
arima(y,c(3,0,2)) #

arima302 <- arima(y,c(3,0,2)) # sauvegarde du modele


Box.test(arima302$residuals, lag=6, type="Ljung-Box", fitdf=5) # Residual autocorrelation test
#
# 
# 
# 
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(arima302$residuals, 24, 5) #tests de LB pour les ordres 1 a 24
round(Qtests(arima302$residuals,24,fitdf=5),3)
#
#



#### Q6 ####
# 
signif <- function(estim){ #fonction de test des significations individuelles des coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

signif(arima302) #

##
arimafit <- function(estim){
  adjust <- round(signif(estim),3)
  pvals <- Qtests(estim$residuals,24,length(estim$coef)-1)
  pvals <- matrix(apply(matrix(1:24,nrow=6),2,function(c) round(pvals[c,],3)),nrow=6)
  colnames(pvals) <- rep(c("lag", "pval"),4)
  cat("tests de nullite des coefficients :\n")
  print(adjust)
  cat("\n tests d'absence d'autocorrelation des residus : \n")
  print(pvals)
}

estim <- arima(y,c(1,0,0)); arimafit(estim)
# 

estim <- arima(y,c(2,0,0)); arimafit(estim)
# 

estim <- arima(y,c(3,0,0)); arimafit(estim)
# 
ar3 <- estim

estim <- arima(y,c(0,0,1)); arimafit(estim)
# 

estim <- arima(y,c(0,0,2)); arimafit(estim)
# 
ma2 <- estim

estim <- arima(y,c(1,0,1)); arimafit(estim)
# not good

estim <- arima(y,c(1,0,2)); arimafit(estim)
# AR fine but MA not good 

estim <- arima(y,c(2,0,1)); arimafit(estim)
# 
ar2ma1 <- estim

estim <- arima(y,c(2,0,2)); arimafit(estim)
# 


# 
models <- c("ar3","ma2","ar2ma1"); names(models) <- models
apply(as.matrix(models),1, function(m) c("AIC"=AIC(get(m)), "BIC"=BIC(get(m))))

#

#### Q7 ####

##
models <-  c("ar3","ma2","ar2ma1")
preds <- zoo(matrix(NA,ncol=3,nrow=4),order.by=tail(index(xm.source),4))
colnames(preds) <- models
desaisonp <- preds #
xmp <- preds #

##
for (m in models){
  pred1 <- mean(desaison) + zoo(predict(get(m),4)$pred, order.by=tail(index(xm.source),4))
  pred2 <- as.numeric(tail(xm,12))[1:4] + pred1
  desaisonp[,m] <- pred1
  xmp[,m] <- pred2
}

obs <- tail(xm.source,4) #
cbind(obs,xmp) #
apply(xmp,2, function(x) sqrt(sum((x-obs)^2)/4)/sd(xm.source)) #

#

#### Q8 ####
datafile <- "Donnees2.csv" #definit le fichier de donnees

data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame
xm.source <- zoo(data[[1]]) #convertit le premier element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 dernieres valeurs
dev.off() #reinitialise les parametre de graphique
plot(xm)
### 

trend <- 1:length(xm)
lt <- lm(xm ~ trend) #
summary(lt) #
r <- lt$residuals #
par(mfrow=c(1,2))
plot(r)
acf(r)
### 

pp.test(xm) 
### 

acf(r,24);pacf(r,24) 
### 
### 
pmax=4; qmax=21

### 



## fonction pour estimer un arima et en verifier l'ajustement et la validite
modelchoice <- function(p,q,data=r, k=24){
  estim <- try(arima(data, c(p,0,q),optim.control=list(maxit=20000)))
  if (class(estim)=="try-error") return(c("p"=p,"q"=q,"arsignif"=NA,"masignif"=NA,"resnocorr"=NA, "ok"=NA))
  arsignif <- if (p==0) NA else signif(estim)[3,p]<=0.05
  masignif <- if (q==0) NA else signif(estim)[3,p+q]<=0.05
  resnocorr <- sum(Qtests(estim$residuals,24,length(estim$coef)-1)[,2]<=0.05,na.rm=T)==0
  checks <- c(arsignif,masignif,resnocorr)
  ok <- as.numeric(sum(checks,na.rm=T)==(3-sum(is.na(checks))))
  return(c("p"=p,"q"=q,"arsignif"=arsignif,"masignif"=masignif,"resnocorr"=resnocorr,"ok"=ok))
}

## fonction pour estimer et verifier tous les arima(p,q) avec p<=pmax et q<=max
armamodelchoice <- function(pmax,qmax){
  pqs <- expand.grid(0:pmax,0:qmax)
  t(apply(matrix(1:dim(pqs)[1]),1,function(row) {
    p <- pqs[row,1]; q <- pqs[row,2]
    cat(paste0("Computing ARMA(",p,",",q,") \n"))
    modelchoice(p,q)
  }))
}

armamodels <- armamodelchoice(pmax,qmax) #estime tous les arima (patienter...)


selec <- armamodels[armamodels[,"ok"]==1&!is.na(armamodels[,"ok"]),] #modeles bien ajustes et valides
selec
### On a ? modeles bien ajustes et valides

pqs <- apply(selec,1,function(row) list("p"=as.numeric(row[1]),"q"=as.numeric(row[2]))) #cree une liste des ordres p et q des modeles candidats
names(pqs) <- paste0("arma(",selec[,1],",",selec[,2],")") #renomme les elements de la liste
models <- lapply(pqs, function(pq) arima(r,c(pq[["p"]],0,pq[["q"]]))) #cree une liste des modeles candidats estimes
vapply(models, FUN.VALUE=numeric(2), function(m) c("AIC"=AIC(m),"BIC"=BIC(m))) #calcule les AIC et BIC des modeles candidats
### L'ARMA(2,1) minimise les criteres d'information.

rps <- lapply(models, function(m) as.zoo(predict(m,4)$pred)) #previsions de r
xmps <- lapply(rps, function(rp) rp+cbind(1,c((T-3):T))%*%lt$coefficients) #previsions de xm
rmse <- vapply(xmps, FUN.VALUE=numeric(1), function(xmp) sqrt(sum((as.zoo(xmp)-tail(xm.source,4))^2))) #calcule les rmse out-of-sample
rmse
### L'ARMA(2,1) fait aussi la meilleure prevision

