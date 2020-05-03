### ### ### ### ### ### ### ### ### 
### Si vuole studiare come il numero delle rotture di
### fili di lana dipendano dal tipo di
### lana e la tensione 
### ### ### ### ### ### ### ### ### 

#  Modello poisson, usato per dati di conteggio
# 1) se X ~ Poisson(lambda), allora E(X)=Var(X)=lambda
# 2) Usato per contare eventi che accadono randomly a un particolare rate

# La distribuzione multinomiale può essere considerata della famiglia esponenziale perchè
# è la distribuzione della somma di variabili Poisson, condizionatamente alla loro somma.
# Ipotizziamo di avere J variabili, ognuna da Yj ~ Pois(Lambda_j). 
# La loro densità congiunta è:
#     f(y) = prod_{j=1}^n frac{lambda_j^y_j exp(-lambda_j)}{y_j!}
# Siamo interessati a trovare:
#     f(y|n) = f(y,n)/f(n)=f(y)/f(n)
#     -> ho f(y), mi serve f(n).


# La multinomiale si modella come sequenza di binomiali.
# Considero la logistica nominale, in cui si modella 
#         logit(pi_j) = log(pi_j/(1-pi_j)) = X_j*B_j.


# OFFSET si usa quando ad esempio si vuole tener conto del tempo che passa 
# (6 eventi in un anno diverso da 6 eventi in un secolo)
# Poisson -> modelizzo i numeri
# Multinomiali -> modelizzo le proporzioni

# Y_ij = volte in cui si verifica l'i-esimo tipo della prima variabile e 
# il j-esimo tipo della seconda variabile
## 
rm(list =ls())
## 

## librerie
library(datasets)
library(contrast)
library(MASS)
##

# carichiamo i dati
Data = warpbreaks
summary(Data)

# i dati si possono rappresentare come tabelle a doppia entrata
table(Data[,-1])

mytable= xtabs(~Data[,2]+Data[,3]) #Create a contingency table
ftable(mytable)



### ### ### ### ### ### ### 
### Statistiche descrittive
### ### ### ### ### ### ### 

mean(Data$breaks)
var(Data$breaks)

tapply(Data$breaks,Data$tension,mean) 
# Per ogni valore di tensione, calcolo la media dei sottogruppi, 
# dove breaks è la variabile di interessa, divisa in gruppi rispetto 
# alla tensione e fanne la media.
tapply(Data$breaks,Data$wool,mean)

tapply(Data$breaks,Data$tension,var)
tapply(Data$breaks,Data$wool,var) 
# problemi perchè la media è diversa dalla varianza
# magari se metto una covariata il problema sparisce perchè la variabilità viene spiegata

tapply(Data$breaks,paste(Data$tension,Data$wool),mean) 
# la nuova variabile è la concatenazione delle due variabili
tapply(Data$breaks,paste(Data$tension,Data$wool),var)



## ## ## ## ## ## ## ##  
## Modello di Poisson
## ## ## ## ## ## ## ##
#funzione link canonica g(mu_i) = log(mu_i)
Mod1 = glm(breaks ~ (tension+wool), data=Data, family=poisson(link = "log"))
summary(Mod1)
# (H_0: beta_j=0)
# test significativo -> p-value minore di 0.05 -> rifiuto l'ipotesi H_0


# Il valore dell'intercetta è log(lambda_{A,L})
X= model.matrix(Mod1) 
Data[1:9,] 
# Infatti dalla model matrix vedo che le prime nove osservazioni hanno solo il valore 
# dell'intercetta.
# Esempio: tension M = -0.3213 è l'effetto che ho sull'intercetta quando passo da L a M.
summary(Mod1)

# lambda_ij=exp(X*beta)
# LE VARIABILI SONO FATTORI!!!

## L'offeset si setta come
## Mod1 = glm(breaks ~ (tension+wool)^2+offset(variabile offset), data=Data, family=poisson(link = "log"))

# Possiamo fare dei contrasti.
# Primo contrasto per lana A e secondo contrasto per lana B)
# Il test è H_0: b1=b2 (cioè la media della tensione M e della tensione H sono 
# uguali della lana A)

# CONTRASTO individuale (cioè distinzione a seconda della lana)
# H0: b1=b2
Cont1 = contrast(Mod1, 
                 list(tension="M", wool = levels(Data$wool)),
                 list(tension="H", wool = levels(Data$wool)), type="individual" )
print(Cont1, X=T) # vedo i valori per i due tipi di lana: sono uguali.

# contrasto marginale (no distinzione di lana)
Cont2 = contrast(Mod1, 
                 list(tension="M", wool = levels(Data$wool)),
                 list(tension="H", wool = levels(Data$wool)), type="average" )
print(Cont2, X=T)

## Calcoliamo gli intervalli di confidenza per i parametri della regressione
confint.default(Mod1) #tensione M non contiene lo zero
# o in termini di log-ratio 
exp(confint.default(Mod1)) #(exp(b_j))
# tensione M non contiene l'uno, ma è minore di uno,
# infatti abbiamo visto che il suo coefficiente nella Mod1 è negativo, 
# quindi fa diminuire le medie, perchè exp(b) è un fattore moltiplicativo




## ## ## ## ## ## ## ## ## 
## TESTS
## ## ## ## ## ## ## ## ## 

# Test sulla DEVIANZA:
# D = 2(L(y,B^max)-l(y,B^))
# L(B,y)-L(B^,y) = -1/2 (B-B^)'*J*(B-B^)
# 2(L(B^,y)-L(B,y)) = (B-B^)'*J*(B-B^) ~ Chisq_p

# Testo quindi l'ipotesi H0: Beta=Beta_0 (contro H1: Beta=Beta_1)
# D0 = 2(L(B_max,y)-L(B0,y)): devianza del modello nullo M0 (solo Beta_0)
# D1 = 2(L(B_max,y)-L(B1,y)): devianza del modello in esame M1 
# deltaD = D0-D1 = 2(L(y,B1)-L(y,B0)) ~ Chisq_(p-q)_(v0-v1)
# p = parametri M1, v0 = D0
# q = parametri M0, v1 = D1
# v0 = v1 -> modelli equivalenti
# v0 > v1 -> M0 peggiore di M1
# Testo deltaD su una Chisq_(p-q).
# Se deltaD appartiene alla regione critica (p-value<alpha) -> rifiuto H0 e quindi uso M1
# Altrimenti scelgo il modello più parsimonioso.


str(summary(Mod1))
D0 = summary(Mod1)$null.deviance 
# devianza residua, distanza dal modello ottimale a quello nullo (solo intercetta) 
gdl0 = summary(Mod1)$df.null 
# 53 gradi di liberta (54 osservazioni- 1 parametro stimato)
D1 = summary(Mod1)$deviance 
# devianza residua, distanza del mio modello da quello ottimale
gdl1 = summary(Mod1)$df.residual  
# (50 = 54 osservazioni -4 parametri stimati)

deltaD = D0-D1 #D_0 modello peggiore, e D_1 è il mio modello 
# vorrei che questa sia più diversa da 0 
# Sotto H_0, deltaD è una chiquadro con parametro di centralità=0
pchisq(deltaD, gdl0-gdl1, lower.tail=T) #area di sinistra CON T 
# SOPRA è il pvalue SE FOSSE lower.tail=F

# H_0 l'aggiunta di p-1 parametri è necessaria? Cioè il mio modello è migliore rispetto
# a quello nullo con un solo parametro


## vediamo se il modello è buono come il saturo
pchisq(D1, gdl1, lower.tail=T) 
# Se metto lower.tail=F ho il p.value, ma è piccolo e neanche si vede
# rappresentazione grafica
xseq = seq(0,210, length.out=100)
plot(xseq,dchisq(xseq,gdl1), type="l")
abline(v=D1,col=2)
# rifiuto l'ipotesi che il modello sia buono come il saturo



## ## ## ## ## ## ## ##  
## Misure di influenza
## ## ## ## ## ## ## ## 


# Togliendo un'osservazione, vorrei che i parametri non cambiassero molto,
# perchè se cambiano quell'osservazione era molto influente

InfMeas1 = influence.measures(Mod1)
str(InfMeas1)
InfMeas1Mat = InfMeas1$infmat
ObsInf = InfMeas1$is.inf
InfMeas1Mat[1:10,]
# Prima colonna riga i-esima, come cambia l'intercetta se tolgo la iesima osservazione
# Seconda colonna riga i-esima, come cambia il coefficiente regressivo della prima covariata 
# (b_1) se tolgo la i-esima osservazione.

# i risultati dipendono dalla parametrizzazione ANOVA

plot(InfMeas1Mat[,1])
plot(InfMeas1Mat[,2])
plot(InfMeas1Mat[,3])
# Intercetta contiene tensione L e lana A, il terzo coefficiente come passa tensione 
# H ad L, con lana A. Da 11 a 19 non vengono toccate se cambio il coefficiente terzo, 
# relativo ad H. 
# TALE Coefficiente (relativo a tensione H) non viene influenzato da quelli che 
# hanno tensione M 
Data[22:25,] # dati relativi ad H: sono influenti per H (coefficiente 3)
Data[12:18,] # dati relativi a M: non influenti
Data[37:45,] # dati relativi a M: non influenti

table(ObsInf) 
# Mi dice quale osservazione è influente per il particolare indice che ho calcolato.
# In questo caso nessuna osservazione è influente per l'intercetta, 
# nessuna per b1 nessuna per b2 etc
# n*p=54*8=432




### ### ### ### ### ### ### 
### Riparametrizziamo usando il
### modello per dati in tabella
### ### ### ### ### ### ### 

# calcoliamo i lambda per i possibili
# valori delle covariate
UnVal = unique(Data[,-1])

# facciamo previsione
Pred = predict(Mod1, type="link", newdata = UnVal) # lambda_ij

MuThetaA = sum(Pred[1:3]) #muTheta per lana A (primi tre valori di Pred)
MuThetaB =  sum(Pred[4:6]) #muTheta per lana B (ultimi tre valori di Pred)

Mu1    = MuThetaA+MuThetaB #mu
ThetaA = MuThetaA/Mu1 
ThetaB = MuThetaB/Mu1 

# verifichiamo che sommino a uno
ThetaA+ThetaB


MuPsiL = sum(Pred[c(1,4)])
MuPsiM = sum(Pred[c(1,4)+1])
MuPsiH = sum(Pred[c(1,4)+2])
Mu2    = MuPsiL+MuPsiM+MuPsiH #Mu1


PsiL = MuPsiL/Mu2 
PsiM = MuPsiM/Mu2
PsiH = MuPsiH/Mu2

# verifichiamo che sommino a uno
PsiL+PsiM+PsiH

ThetaA
ThetaB
PsiL
PsiM
PsiH
# -> Sono le marginali nella tabella


## ## ## ## ## ## ## ##  
## introduciamo l'interazione
## ## ## ## ## ## ## ## 

Mod2 = glm(breaks ~ tension+wool+tension:wool, data=Data, family=poisson(link = "log"))
summary(Mod2)


# AIC
AIC(Mod1,Mod2)
# TEST SULLE DEVIANZE
anova(Mod1,Mod2,test="Chisq")

# -> Il modello con l'interazione è superiore


## ## ## ## ## ## ## ##  
## Proviamo il modello overdispersed, BINOMIALE NEGATIVA
## ## ## ## ## ## ## ## 
# stimiamo lo stesso modello

Mod1OD = glm.nb(breaks ~ (tension+wool)^2, data=Data)
summary(Mod1OD)

## confrontiamo i risultati
coefficients(Mod1)
coefficients(Mod1OD)

summary(Mod1)
summary(Mod1OD)

# testiamo il modelli con l'AIC, AIC VA MINIMIZZATO
AIC(Mod1,Mod1OD)
# e con le devianza

DeltaD = deviance(Mod1)-deviance(Mod1OD)
pchisq(DeltaD, 1, lower.tail=F) # F -> mi dà il p-value
# FALSE, CALCOLA LA PROBABILITA A DESTRA DI DELTAD, il pvalue è basso e rifiuto l'ipotesi
# che le due devianze siano uguali



