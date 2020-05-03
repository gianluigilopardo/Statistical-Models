##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   ##### ##### 
#In Questo dataset ci sono dati bentonici marini (organismi  che vivono in stretto contatto 
#con il fondo o fissati ad un substrato solido), di 9 spiagge tedesche, osservati vicino alla 
#costa. 
#Per ogni spiaggia sono stati raccolti 5 campioni, e sono stati misurati Richness
#(Numero di specie diverse) il NAP (Differenza tra punto di campionamento e livello del mare) e 
#l'Esposure (un indice composto dall'azione delle onde, inclinazione, tipo di sabbia etc).

#L'interesse è capire come la richness dipende dalle altre variabili
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####   ##### ##### 


# 9 spiagge
# 5 campioni per spiaggia
# in ogni campione: richness, NAP, esposure.


rm(list=ls())

# LIBRARIES  -------------------------------------------------------------

library(lme4)
library(nlme)
library(lattice)
# Functions
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Settiamo la directory

DIR = "C:/Users/Gianluigi/Google Drive/PoliTO/PoliTO[#ajò_team]/Lezioni/Ingegneria Matematica/Modelli Statistici/Case study/Esame"
setwd(DIR)

# Carichiamo i dati -  Ricordatevi di controllare separatori e decimali
Data = read.csv("08 - dati Bentonici.txt", sep="", dec=".", header=T)
summary(Data)

# qualche plot
pairs(Data[,2:4], upper.panel=panel.cor,diag.panel=panel.hist)

par(mfrow=c(1,2))
scatter.smooth(x=Data[,3], y=Data[,2], pch=16, col="red", xlab=colnames(Data)[3], ylab=colnames(Data)[2])
scatter.smooth(x=Data[,4], y=Data[,2], pch=16, col="red", xlab=colnames(Data)[4], ylab=colnames(Data)[2])

# Fare attenzione a come le variabili vengono codificate str(Data)
Data$Beach = as.factor(Data$Beach)




##### ##### ##### ##### ##### ##### 
#####   Modello GLM semplice  #####
##### ##### ##### ##### ##### ##### 

# Modello Diviso per spiaggie

GLM3wrong2 = glm(Richness ~ Exposure+NAP+Beach+Exposure:Beach+NAP:Beach, data=Data, family=poisson)
summary(GLM3wrong2)
# 
table(Data$Exposure, Data$Beach)
# GLi NA sono dovuti al fatto che Exposure ha un solo valore per spiaggia
# Exposure è di fatto una caratteristica della spiaggia.
# Solo la spiaggia 2 ha Exposure 8. Spiaggia 1,5,8,9 hanno Exposure 10
# e 3,4,6,7 hanno Exposure 11.

# GLi NA sono dovuti al fatto che Exposure ha un solo valore per spiaggia.
# Quindi se uso la spiaggia, so già quale è exposure, 
# C'è una dipendenza perfetta, e le colonne X devono essere indipendeti.
# Conoscere beach è piu informativo, quindi eliminimo la variabile Exposure.
# 9 valori sono sono stati tolti

GLM3 = glm(Richness ~ Exposure+NAP+Beach+NAP:Beach, data=Data, family=poisson)
summary(GLM3)
# L'NA sulla spiaggia 7 è dovuto al fatto che Exposure ha 3 valori, 
# e quindi è molto facile che sia correlata con Spiaggia

# Calcoliamo residui e fitted values del modello 
resGLM3 = residuals(GLM3)
fitGLM3 = fitted(GLM3)

# plot residui colorati per spiaggia
par(mfrow=c(2,2))
plot(resGLM3, col=c(1:9)[Data$Beach], pch=20)
plot(resGLM3, fitGLM3, col=c(1:9)[Data$Beach], pch=20)
plot(Data$NAP, resGLM3, col=c(1:9)[Data$Beach], pch=20)




##### ##### ##### ##### ##### ##### 
##### Modello a effetti misti #####
##### ##### ##### ##### ##### ##### 
# introdurre spiaggia "costa" 8 gradi di libertà
# se non siamo interessati a sapere il valore di preciso per ogni spiaggia
# possiamo trattarla com un effetto random


# Modello a effetti misti con effetto random su spiaggia
# Aletorietà sull'intercetta: perché il valore atteso dipenderà dalla spiaggia.
# y_i|u_i ~ Poisson(lambda_i), generalmente u~N(0,Sigma)
# Attraverso la parte random introduco dipendenza tra le osservazioni
# condizionando ad U, ho le osservazioni indipendenti e ho il classico GLM
# SE NON CONDIZIONO ad U, le y (osservazioni) NON SONO PIU POISSON E NON SONO 
# INDIPENDENTI.

# Modello a effetti misti con effetto random su spiaggia (ogni spiaggia ha 
# la sua intercetta random). Essendoci 9 spiagge, il vettore dell'intercetta 
# sarà U=(u1,u2,...,u9)

GLMM1 = glmer(Richness ~ NAP+Exposure+(1|Beach), data=Data, family=poisson)
summary(GLMM1)

#IL MODELLO A EFFETTI MISTI SERVE A INTRODURRE DIPENDENZA TRA LE OSSERVAZIONI

# Il modello non stima l'intercetta random (i parametri u) ma la sua 
# variabilità (varianza=0.03485). Ogni spiaggia ha la sua intercetta 
# (generale 6.25) + quella random (ranef mi da le u, cioè solo i 
# coiefficienti random). Estraiamo i coefficienti random dell'intercetta: 
# mi dà i coiefficienti della parte fissa nap e exposure (che non 
# cambiano sulla spiaggia), mentre nap varia sulla spiaggia 
# (diversa in base alla spiaggia).


# estraiamo i coefficienti random dell'intercetta
coef(GLMM1)
# estraiamo i valori specifici della spiaggia
ranef(GLMM1)
# differenza tra i due effetti
coef(GLMM1)$Beach[,1]-ranef(GLMM1)$Beach 
# la differenza dà l'intercetta
summary(GLMM1)

# Nel caso di intercetta random, z_i è un vettore di uni
# z = #righe = numero di osservazioni
# colonne = numero di spiagge

# Plottiamo la matrice Z di y = X*beta+Z*b+epsilon, b = effetto random
Zmat1=getME(GLMM1,"Z")
?getME
image(Zmat1)

# Calcoliamo la matrice di covarianza degli effetti random.
# La varianza
Cov = as.numeric((summary(GLMM1))$varcor$Beach)
# MatCov1 = Cov(b)
MatCov1 = diag(Cov,9)
#MatCov = Cov(Z*b) = ZCov(b)Z'
MatCov = Zmat1%*%MatCov1%*%t(Zmat1) 
# cov tra le osservazioni (è a blocchi perchè c'è correlazione 
# solo nello stesso gruppo)
image(MatCov)


# Le prime 5 osservazioni sono correlate tra di loro, ma non sono correllate 
# con le secondo cinque. Allora la amtrice di covarianza è a blocchi 
# (c'è correlazione solo tra osservazioni della stessa spiaggia).

# Possiamo fare in due modi la previsione:
# utilizzare solo la parte fissa o parte fissa + random.



# Faccio predict per vedere la differenza tra i valori predetti dal modello
# che utilizza gli effetti casuali e quello che non li utilizza.
# Li plotto e vedo se c'è tanta differenza: assomiglia ad una retta, ma ci 
# sono delle oscillazioni.
# Noto che i valori predetti dal modello con effetti casuali anche se di poco
# si discostano dalla retta.


## Prediction  
?predict.merMod 
PredGLMM1_WithRandom = predict(GLMM1)
PredGLMM1_NoRandom   = predict(GLMM1,re.form=NA)
plot(PredGLMM1_WithRandom,PredGLMM1_NoRandom)

# residuals
resGLMM1_WithRandom = residuals(GLMM1)
par(mfrow=c(2,2))
plot(resGLMM1_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(resGLMM1_WithRandom, PredGLMM1_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(Data$NAP,resGLMM1_WithRandom, col=c(1:9)[Data$Beach], pch=20)
# Vedo che sono 'correlati' all'interno della stessa spiaggia, ma non al di fuori.
# Giusto così nel modello a effetti misti.

# Ogni spiaggia ha un nap diverso, toglie l'intercetta ma mette
# coefficiente regressivo su NAP.

# Testiamo una random slope su NAP, no effetto casuale su intercetta.
# nap ha sia coefficiente regressivo (generale), sia random (che 
# cambia da spiaggia a spiaggia)
GLMM2 = glmer(Richness ~ NAP+(-1+NAP|Beach), data=Data, family=poisson)
summary(GLMM2)

# estraiamo i coefficienti random dell'intercetta 
coef(GLMM2)
# estraiamo i valori specifici della spiaggia
ranef(GLMM2)
# differenza tra i due effetti
coef(GLMM2)$Beach[,2]-ranef(GLMM2)$Beach 
# la differenza da l'intercetta
summary(GLMM2)

# In questo caso la matrice non è di soli uni (perchè non è intercetta).

# Plottiamo la matrice Z di y = X*beta+Z*b+epsilon, b = effetto random
Zmat2 = getME(GLMM2,"Z")
image(Zmat2)

# La matrice di covarianza è a blocchi, perchè l'effetto random 
# mette dipendeza tra osservazioni della stessa spiaggia.
# In questo caso la correlazione tra la prima osservazione e la seconda 
# osservazione dipende dal valore di nap della prima e dal valore di nap
# della seconda. Quindi sono tutti diversi (è un po' diversa rispetto 
# a quella solo intercetta)

# Calcoliamo la matrice di covarianza degli effetti random
# la varianza
Cov = as.numeric((summary(GLMM2))$varcor$Beach)
# MatCov1 = Cov(b)
MatCov1 = diag(Cov,9)
#MatCov = Cov(Z*b) = ZCov(b)Z'
MatCov = Zmat2%*%MatCov1%*%t(Zmat2)


## Prediction  
PredGLMM2_WithRandom    = predict(GLMM2)
PredGLMM2_NoRandom      = predict(GLMM2,re.form=NA)
plot(PredGLMM2_WithRandom,PredGLMM2_NoRandom)

# residuals
resGLMM2_WithRandom     = residuals(GLMM2)
par(mfrow=c(2,2))
plot(resGLMM2_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(resGLMM2_WithRandom, PredGLMM2_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(Data$NAP,resGLMM2_WithRandom, col=c(1:9)[Data$Beach], pch=20)



# Modello con random slope e intercetta 
# (sia intercetta random che coefficiente per ogni spiaggia )
GLMM3    = glmer(Richness ~ NAP+(-1+NAP|Beach)+(1|Beach), data=Data, family=poisson)
summary(GLMM3)
coef(GLMM3)

Zmat3=getME(GLMM3,"Z")
image(Zmat3) #come se attaccase le due Z
# La prima parte sono i valori di nap nelle osservazioni

# calcoliamo la matrice di covarianza degli effetti random
# la varianza
CovNAP   = as.numeric((summary(GLMM3))$varcor[1])
CovBeach = as.numeric((summary(GLMM3))$varcor[2])
# MatCov1 = Cov(b)
MatCovNAP1       = diag(CovNAP,9)
MatCovBeach1     = diag(CovBeach,9)
#MatCov = Cov(Z*b) = ZCov(b)Z'
MatCovNAP   = Zmat3[,1:9]%*%MatCovNAP1%*%t(Zmat3[,1:9])
MatCovBeach = Zmat3[,9+1:9]%*%MatCovBeach1%*%t(Zmat3[,9+1:9])


## Prediction  
PredGLMM3_WithRandom    = predict(GLMM3)
PredGLMM3_NoRandom      = predict(GLMM3,re.form=NA)
plot(PredGLMM3_WithRandom,PredGLMM3_NoRandom)

# residuals
resGLMM3_WithRandom     = residuals(GLMM3)
par(mfrow=c(2,2))
plot(resGLMM3_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(resGLMM3_WithRandom, PredGLMM3_WithRandom, col=c(1:9)[Data$Beach], pch=20)
plot(Data$NAP,resGLMM3_WithRandom, col=c(1:9)[Data$Beach], pch=20)


# Scelta del modello
AIC(GLMM1,GLMM2,GLMM3)

# Il modello migliore è 1: solo intercetta random. I residui hanno il
# comportamento migliore. 
# Il modello peggio è 2: solo slope random. I residui hanno chiaramente delle
# strutture.

# Vantaggi:
# 1. se volessi stimare il modello con intercetta fissa, dovrei stimare 9 
# parametri: uno per spiaggia.
# Se invece faccio con intercetta random, devo stimare 2 parametri, cioè 
# l'intercetta generale più la varianza dell'effeto casuale.
# Soprattutto se ho poche osservazioni, è meglio stimare pochi parametri 
# per mantenere i gradi di libertà alti. 
# 2. Mi basta stimare la variabilità del parametro.
# 3. Meno parametri ho, più il modello è flessibile -> rasoio di hoccam 




