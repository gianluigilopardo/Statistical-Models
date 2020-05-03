##########################################################################
##########################################################################
##########   Codice Studio Binomiale Negativa e Zero inflated   ##########
##########################################################################
##########################################################################


rm(list=ls())
## librerie
library(pscl)
library(sandwich)
library(lmtest)
library(MASS)


#####
# 1       ofp     Numero di visite in studio medico (risultato intero)
# 2      ofnp
# 3       opp
# 4      opnp
# 5      emer
# 6      hosp     Numero di ricoveri in ospedale (intero)
# 7    health     stato di salute percepito da sé (poor, average, excellent)
# 8  numchron     Numero di condizioni croniche (numero intero)
# 9   adldiff
# 10   region
# 11      age
# 12    black
# 13   gender     Sesso (female, male)
# 14  married
# 15   school     Numero di anni di istruzione (numero intero)
# 16   faminc
# 17 employed
# 18  privins     Indicatore di assicurazione privata (no, yes)
# 19 medicaid

## Load
data = geturl('https://github.com/gianluigilopardo/statistical-models/blob/master/07%20-%20dati%20NB.Rdata?raw=true')
Data = data
# Il dataset contiene 19 variabili per 4406 pazienti. Per ognuno sono riportati il 
# numero di ricoveri in ospedale, il numero di visite dal medico e alcuni 
# indicatori sulla persona (sesso, istruzione)
summary(Data)
head(Data)


### Plot descrittivi:
plot(sort(Data$ofp)) # vedo la distribuzione ordinata dei valori:
# abbiamo molti valori estremi, e per fare plot utilizziamo il log della variabile
# clog per plottare in base logaritmica

clog <- function(x) log(x + 0.5)
# e creaiamo una funzione che trasformi una variabile numerica in un fattore
cfac <- function(x, breaks = NULL) {
   if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
   x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
   levels(x) <- paste(breaks[-length(breaks)], 
                      ifelse(diff(breaks) > 1,
                      c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                        sep = "")
   return(x)
}


par(mfrow=c(3,2))
plot(clog(ofp) ~ health, data = Data, varwidth = TRUE)
plot(clog(ofp) ~ cfac(numchron), data = Data)
plot(clog(ofp) ~ privins, data = Data, varwidth = TRUE)
plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = Data)
plot(clog(ofp) ~ gender, data = Data, varwidth = TRUE)
plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = Data, breaks = 9)
par(mfrow=c(1,1))
# tutti i boxplot si intersecano, quindi una sola variabile non puo essere sufficiente 
# a spiegare il modello 


## ## ## ## ## ## ## ## ##
## Definiamo la formula ##
## ## ## ## ## ## ## ## ##
formula <- ofp ~ hosp + health + numchron + gender + school + privins
## e stimiamo il modello
modelPoisson <- glm(formula = formula,
                    family  = poisson(link = "log"),
                    data    = Data)
summary(modelPoisson)
# Poor health compared to average health status increases the average office visits 
# by 1.28 times, whereas excellent health decreases the average office visits by 
# 0.69 times.

# Per GLM Poisson: la funzione link canonica è g(?i) = log(?i)
# teta_i = log(lambda_i)
# Quindi E(yi)=Var(yi)=exp(teta_i)=lambda_i

## I coefficienti nella scala della media
exp(coef(modelPoisson))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  Possiamo anche fittare un modello BN  ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Dato un processo di Bernoulli, ovvero una serie di variabili aleatorie indipendenti 
# X_{1},X_{2},... di uguale distribuzione di Bernoulli B(p), la distribuzione di Pascal 
# (detta anche Binomiale Negativa) NB(p,n) descrive la variabile aleatoria T_n che conta 
# il numero di fallimenti precedenti il successo numero n (ovvero il numero di prove 
# necessarie ad ottenerlo, meno n):
# T_{n} = min{t: X_{1}+...+X_{t+n}=n}
# T_{n}+n = min{t: X_{1}+...+X_{t}=n}

modelNB <- glm.nb(formula = formula,
                  data    = Data)
# glm.nb serve per passare da Poisson a Binomiale Negativa, 
# dà anche il valore di dispersione 
summary(modelNB)

## coefficienti
exp(coef(modelNB))

# e vedere i coefficienti dei due modelli insieme
rbind(exp(coef(modelPoisson)),exp(coef(modelNB)))
# qui noto poca differenza

# e il loro intervalli di confidenza
cbind(confint.default((modelPoisson)),confint.default((modelNB)))
# qui la differenza si vede di più

# Ricorda che sto usando E(Y)=Var(Y)=lambda
# se però questo non vale, ad esempio se Var(Y)>E(Y), potrei avere
# intervalli di confidenza più piccoli, ma non è detto che sia una cosa positiva

# Più gli intervalli di confidenza sono stretti, più è sicuro il modello,
# quindi in questo caso dovrebbe essere più sicuro il modello di Poisson.
# Però nel modello di Poisson non distinguo tra varianza e media, quindi 
# non posso stimare bene la varianza. Se però la varianza non è uguale 
# alla media allora per calcolare gli intervalli di confidenza utilizza quel lambda, 
# quindi se trovo intervalli di confidenza piu stretti, ma magari sto usando 
# una varianza sbagliata.

# meglio accettare, che rifiutare H0 se è vero 
# rifiutare H0 = dire che c'è l'effetto delle variabili.

# Ampiezza intervalli
cbind(confint.default((modelPoisson))[,2]-confint.default((modelPoisson))[,1],
confint.default((modelNB))[,2]-confint.default((modelNB))[,1])




## ## ## ## ## ## ## 
##  Fit zeroinfl  ##
## ## ## ## ## ## ##

# ZERO INFLATED (su R):
# esiste Z_i = {1 -> Y_i = 0
#              {0 -> [Y_i ~ Poiss(lambda_i) oppure Y_i ~ BN(lambda_i) o altra distrib.

# -> qui uso Y_i ~ Poiss(lambda_i)
# Il problema è che non conosciamo Z_i, ma dobbiamo stimarla. 
# Dico che Z_i ~ Binomiale(pi_i)
# Quindi logit(pi_i) = X_z*beta_z
# mentre log(lambda_i) = X_y*beta_y 
# Nota i coefficienti beta_z e beta_y sono generalmente diversi 


# stesso modello per le due componenti
modelZeroInfl <- zeroinfl(formula = formula,
                          dist    = "negbin",
                          data    = Data)
# dist è la distribuzione di Y_i
# se non passo nessun altro modello,
# allora assume che il modello di Z è uguale al modello di Y
# ->stesso modello per le due componenti
# Stima due modelli, quello di Z e quello di Y.
# count model: chi ha una percezione positiva della salute sta meno giorni in ospedale
# Zero Inflation tenderà ad aver Z=1 quindi Y=0 quindi non va in ospedale.


# Devo trovare Z_i (non è noto). Assumo che Z_i ~ B(pi_i); 
# logit(pi_i) = X_z*Beta_z; log(lambda_i)=X_y*Beta_y
summary(modelZeroInfl)
# Nota i coefficienti Beta_z e Beta_y sono generalmente diversi 

## E i corrispettivi coefficienti
expCoef <- exp(coef((modelZeroInfl)))
expCoef <- matrix(expCoef, ncol = 2)
# La prima colonna è per Z, la seconda per Y


## Possiamo facilemnte settare differenti modelli per le due parti:
# La parte di conteggio e la parte di Z hanno diverse coviariate
modelZeroInflSimpler <- zeroinfl(formula = ofp ~ hosp + health + numchron + gender + school + privins | hosp + numchron + gender + school + privins,
                                 dist    = "negbin",
                                 data    = DebTrivedi)
summary(modelZeroInflSimpler)

# Test sulle devianze tra i due modelli Y e Z: si vede che non c'è molta differenza
## E fare un test delle verosimiglianza (Devianza)
lmtest::lrtest(modelZeroInfl, modelZeroInflSimpler)
# lmtest è solo un modo per fare test sulle devianze

## possiamo fare diversi predict  
?predict.zeroinfl
plot(predict(modelZeroInfl,type="zero"))
# type = 'zero', cioè predict della Bernoulli, infatti predict sono delle probabilità

# decidiamo con l'AIC quale sia il modello migliore
AIC(modelPoisson,modelNB,modelZeroInfl,modelZeroInflSimpler)
# modelZeroInfl e modelZeroInflSimpler sono praticamente equivalenti
# (intorno a +-5 di AIC due modelli si possono considerare equivalenti)
# modelPoisson è il peggiore perchè è l'unico che non tiene conto della sovradispersione

