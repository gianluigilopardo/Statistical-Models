
#####################################################
# MODELLI STATISTICI
#
# Regressione lineare - Bayesiana
#
# Dataset: Adverstising
#
# Il dataset contiene le vendite, di migliaia di unit?, di un prodotto in 200 mercati,
# insieme al budget speso per la pubblicit?, in migliaia di dollari, su 3 media: Tv,
# radio e giornali.
# Lo scopo ? determinare quale strategia pubblicitaria ? migliore e prevedere
# le vendita in base al budget speso per ogni media
#####################################################


# distribuzione a priori di beta: normale
# distribuzione a priori di sigma^2: Gamma inversa
# Sono entrambe congiugate


# Puliamo il workspace ----------------------------------------------

rm(list=ls())

# libraries
library(coda)
# carichiamo il modello bayesiano
source("RegressioneLineare.R")
# Carichiamo i dati  --------------------------------------------------------------

# Leggiamo il file csv
data = getURL("https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/01%20-%20dati%20Advertising.csv?token=ACNKBBEH5JHNCHLTM7OK4J26V2JNI")
advertising = read.csv(text = data, header = T)

# fare attenzione a separatori, decimali e header

# leggiamo le prime righe del dataset
head(advertising)

# facciamo un summary
summary(advertising)

# facciamo attach del dataset, per richiamare le variabili pi? facilmente
attach(advertising)
# con il comando detach(advertising) si elimina l'effetto di attach

TV

### ### ### ### ### ### ### ### ### ### ###
### ### ## Anailisi descrittive  ## ### ### 
### ### ### ### ### ### ### ### ### ### ###

# Dimensioni del dataset
length(advertising) # Numero di variabili:
# questo comando funziona solo con dataframe, ma non con matrici.
dim(advertising)        # Righe e colonne
names(advertising)  # nome variabili, potete usare anche colnames(advertising)
length(X)                       # numero di osservazioni
# altre alternative
nrow(advertising)
ncol(advertising)

# plot generale
plot(advertising)




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ###   Modello regressivo multivariato - beta N e sigma2 IG  ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
set.seed(1)
# regressioni
formula = Sales ~ TV+Radio+Newspaper
#beta provengono da una normale, sigma da una gamma inversa
MultiReg1 = ModLin(
    formula = formula,  
    beta.mean = c(-10,-10,0,50),  #vettore di media a priori
    beta.variance = diag(100,4),  #varianza alta -> poca importanza alla a priori
    sigma2.a = 1,                 #parametri della gamma inversa
    sigma2.b = 1, 
    start.beta = rep(0,4),        #b_0=(0,0,0,0)
    start.sigma2 = 1, 
    iter = 2000,                  #faccio duemila iterazioni
    burnin = 100,                 #trascuro le 100 osservazioni iniziali
    thin = 1,                     #li prendo tutti
    Data=advertising
    )
#salvo 1900 campioni

str(MultiReg1)


## trasformiamo i risutati in "mcmc"
#questi sono i risultati della funzione che ho chiamato. 
betaOUT = as.mcmc(MultiReg1$Beta)
sigma2OUT = as.mcmc(MultiReg1$sigma2) 
# ? una serie temporale (il tempo rappresenta l'iterazione)
# in tutto le iterazioni sono 1900

# Parametri ignoti: b_0, b_1, b_2, b_3, sigma^2
# Le iterazioni (cio? i campioni dei paramtri ignoti) sono 1900 e sono salvati 
# in beta0UT. Vorrei convergenza dei campioni.


# facciamo dei plot: campioni della distribuzione a posteriori  
plot(betaOUT, type="l") #ho 4 parametri b_0,...,b_3. Non ho convergenza
plot(sigma2OUT, type="l")  #valore piu ottenuto ? circa 2.7
# I campioni che sto ottenenedo non sono campioni della distribuzione che cerco
# perch? ? troppo oscillante


# Non ? detto che l'aposteriori sia gamma se so che la condizionata ? gamma.
# Confronto il vettore con s? stesso, ma di un'altra iterazione
# Al lag 2 non c'? correlazione, quindi va bene thin=1
# plot dell'auto-correlation function
acf(betaOUT)
acf(sigma2OUT) 

# e vediamo qualche risultato
summary(betaOUT) #simile a SE^2
# Posso vedere i quantili della distribuzione che ho trovato, 
# cio? dell'ipotetica distribuzione a posteriori di beta.
# Siccome gli intervalli di credibilit? di beta (il corrispettivo 
# bayesiano degli intervalli di confidenza) non contengono lo zero
# possiamo dire che sono significativi nel senso bayesiano.

summary(sigma2OUT)

# Possiamo vedere i risultati dello stesso modello frequentista.
MultiReg1 = lm(Sales~TV+Radio+Newspaper)
summary.lm(MultiReg1)
(summary(MultiReg1)$sigma)^2 #SE^2 del modello

# Le stime tra modello frequentista e bayesiano sono praticamente uguali, perch? 
# V=100I, allora quando ne faccio l'inversa V^-1 diventa qualcosa di molto piccolo.
# Assumiamo sia quasi zero, allora la parte scompare (cio? ho utilizzato una varianza 
# molto alta)
# V_p =((X'X)/sigma^2 + V^-1)^-1
# mu_p = V_p()
# La media della condizionata ? uguale alla stima di massima versomiglianza, 
# se metto varianza elevata vuol dire che d? poca importanza alla distribuzione
# a priori e la media della condizionata ? la stima di massima verosimgilianza.




# calcoliamo le distribuzioni a posteriori dei residui
resmatrix = matrix(NA, ncol=nrow(betaOUT), nrow= nrow(advertising))
# per ogni campione a posteriori, calcolimao i residui di tutte le osservazioni
Cov = model.matrix(formula,advertising)
for(j in 1:nrow(betaOUT))
{
  Betaj = betaOUT[j,, drop=F] # un campione
  sigma2j = sigma2OUT[j,, drop=T]
  Xbetaj = Cov%*%t(Betaj)
  resmatrix[,j] = advertising$Sales-Xbetaj
}
# Posso calcolare la media dei residui
# vediamone qualcuna
par(mfrow=c(3,2))
plot(resmatrix[1,], type="l") #residui primo dato
plot(density(resmatrix[1,]))
plot(resmatrix[10,], type="l")
plot(density(resmatrix[10,]))
plot(resmatrix[100,], type="l")
plot(density(resmatrix[100,]))
par(mfrow=c(1,1))




# possiamo calcolarne il valore medio  
# e la varianza a posteriori
meanRes = apply(resmatrix,1,mean)
varRes = apply(resmatrix,1,var)

# e li plottiamo rispetto alle y
par(mfrow=c(2,1))
plot(advertising$Sales, meanRes, cex=1)
plot(advertising$Sales, varRes, cex=1)
par(mfrow=c(1,1))
# media dei residui non buona e anche la varianza dipende da y 
# l'ipotesi di varianza costante non ? giusta
# la varianza dei residui ? alta all'inizio, poi si abbassa, 
# e poi si alza rispetto all y
# se guardo solo una colonna ? un campione della marginale

# e poi rispetto alle covariate
par(mfrow=c(3,2))
plot(advertising$TV, meanRes, cex=1)
plot(advertising$TV, varRes, cex=1)
plot(advertising$Radio, meanRes, cex=1)
plot(advertising$Radio, varRes, cex=1)
plot(advertising$Newspaper, meanRes, cex=1)
plot(advertising$Newspaper, varRes, cex=1)
par(mfrow=c(1,1))


## possiamo calcolare anche le a posteriori dei predetti 
predmatrix = matrix(NA, ncol=nrow(betaOUT), nrow=nrow(advertising))

for(j in 1:nrow(betaOUT))
{
  predmatrix[,j] = advertising$Sales-resmatrix[,j]
}


# calcoliamo medie e varianza a posteriori
meanPred = apply(predmatrix,1,mean)
varPred = apply(predmatrix,1,var)

# e facciamo qualche plot
par(mfrow=c(2,1))
plot(advertising$Sales, meanPred, cex=1)
plot(advertising$Sales, varPred, cex=1)
plot(meanPred,meanRes, cex=1)
plot(varPred,varRes, cex=1)
par(mfrow=c(1,1))

# possiamo anche calcolare la distribuzione dell'AIC
AICvec = matrix(NA, ncol=nrow(betaOUT), nrow= 1)

Cov = model.matrix(formula ,advertising)
for(j in 1:nrow(betaOUT))
{
  Betaj = betaOUT[j,, drop=F]
  sigma2j = sigma2OUT[j,, drop=T]
  Xbetaj = Cov%*%t(Betaj)
  AICvec[j] = -2*sum(dnorm(advertising$Sales,Xbetaj,sigma2j^0.5,log=T))+2*(ncol(Betaj)+1)
}
# e si considera come valore AIC del modello il suo
# minimo (quello che ha la log-verosimiglianza maggiore)
AICbayes = AICvec[which.min(AICvec)]



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
###   Modello regressivo multivariato - beta N e sigma2 G   ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# [Non viene chiesto all'esame]

MultiReg_Gamma = ModLin_Vers2(
  formula = formula,  
  beta.mean = c(-10,-10,0,50), 
  beta.variance = diag(100,4),
  sigma2.a = 1, 
  sigma2.b = 1, 
  start.beta = rep(0,4), 
  start.sigma2 = 1, 
  iter = 2000, 
  burnin = 100, 
  thin = 1, 
  Data=advertising,
  sd.prop = 1 #  
)



## trasformiamo i risutati in "mcmc"
betaOUT_Gamma = as.mcmc(MultiReg_Gamma$Beta)
sigma2OUT_Gamma = as.mcmc(MultiReg_Gamma$sigma2)


# facciamo dei plot
plot(betaOUT_Gamma, type="l")
plot(sigma2OUT_Gamma, type="l") # provate con sd.prop = 1

# plot dell'auto-correlation function
acf(betaOUT_Gamma)
acf(sigma2OUT_Gamma)



