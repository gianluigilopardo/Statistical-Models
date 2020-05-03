### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Si vuole studiare come gli studenti decidano    ###
### il tipo di programma da seguire                 ###
### tra generale, accademico e tecnico (vocation)   ###
###                                                 ###
### ses indica lo stato economico                   ###
### schtyp il tipo di scuola                        ###
### read, write, math e science sono                ###
### valutazione nelle rispettive materie            ###
### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Null Deviance D0 = 2(LL(Saturated Model) - LL(Null Model)) 
# Residual Deviance D1 = 2(LL(Saturated Model) - LL(Proposed Model)) 
# df = df_Sat - df_Proposed
# The Saturated Model is a model that assumes each data point has its own 
# parameters (which means you have n parameters to estimate.)

# The Null Model assumes the exact "opposite", in that is assumes one parameter 
# for all of the data points, which means you only estimate 1 parameter.



# REGRESSIONE LOGISTICA, MULTINOMIALE!
## 
rm(list =ls())
## 

## librerie
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
##

data = getURL("https://github.com/gianluigilopardo/statistical-models/blob/master/05%20-%20hsbdemo.dta")
Data = read.dta(text = data)
summary(Data)
head(Data)

# modello = multinomiale che puo assumere tre valori


## ## ## ## ## ## ## ##  
## Descrittive
## ## ## ## ## ## ## ## 
?with
with(Data, table(ses, prog))
table(Data$ses, Data$prog) #frequenze

?do.call
?with
?tapply
with(Data, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
# Per ogni x, la funzione ha come output un vettore con media e deviazione standard.
# Fa la media per ogni programma scelto dei voti di scrittura.

## Qualche boxplot
boxplot(Data$write~ Data$prog) 
boxplot(Data$science~ Data$prog)
boxplot(Data$math~ Data$prog)

# TEST T-STUDENT
t.test(Data$write[Data$prog=="general"],Data$write[Data$prog=="academic"],var.equal=T)
# H0: medie uguali. Riufiuto: 0 non appartiene all'intervallo di confidenza, p-value<alpha. 

# scrittura = b_0+b_1*prog 
# La differenza tra le media ? b_1, quindi se b_1 ? uguale a zero allora le 
# medie sono uguali. 
# Il p-value ? uguale, perch? se b_1=0 allora i due gruppi hanno la stessa media 
# ed era lo stesso test del T TEST.
# test sono equivalenti -> p value uguale (0.001256)
summary(lm(write~ prog, data =Data[Data$prog!="vocation",] )) 
# nel dataset non prendo quelli 'vocation'
summary(aov(write ~ prog, data=Data[Data$prog!="vocation",]))



### ### ### ### ### ### ### 
### testiamo i modelli nominali
### ### ### ### ### ### ### 

# attenzione che le x devono essere indipendenti
plot(Data$math,Data$science) #c'? una corrispondenza molto forte

Data$V1V3 = ifelse(Data$prog=="vocation",1,0) # vocation=1, general=0
# Data$V1V3 = ifelse(Data$prog!="vocation",1,0)
# sto modelizzando vocation vs generale quindi ? modello binomiale 
Mod13 = glm(V1V3~ ses+schtyp+read+write+math, data =Data[Data$prog!="academic",], family="binomial" )
summary(Mod13) 
# Mod13 non  buono perch? la differenza tra il modello migliore e quello nullo 
# (devianza nulla) ? simile alla differenza tra il modello migliore e 
# il mio modello (devianza residua)



# confronto accademico vs generale (sto modelizzando log(pi_3/pi_1)), con pi_1+pi_3=1
Data$V1V2 = ifelse(Data$prog=="academic",1,0) #1 se accademico, 0 se non lo ? 
Mod12 = glm(V1V2~ ses+schtyp+read+write+math, data =Data[Data$prog!="vocation",], family="binomial" )
summary(Mod12) # academic=1, general = 0

# Ho le variabili che non sono significative, quindi andrebbero tolte
# ma puo accadere che se elimino alcune variabili, altre diventano significative

### Vediamo i parametri
cbind(summary(Mod13)$coefficients[,c(1,4)],summary(Mod12)$coefficients[,c(1,4)])

### Scegliamo i subset dei parametri con step
?step 
# Calcola i modelli togliendo una variabile alla volta e decide di togliere quella 
# che comporta la massima riduzione di aic.
# step calcola AIC, e inizia a togliere variabili, fra tutti questi li mette
# in ordine e sceglie quello con AIC piu basso, e poi ripete questo procedimento
# con il modello trovato, fino a quando arriva all'ultimo modello che ? quello che 
# fa vedere
StepMod13 = step(Mod13)
summary(StepMod13)

# prima matematica non era significativa, ma dopo step math ? significativa

StepMod12 = step(Mod12)
summary(StepMod12)

# anche in questo caso matematica ? diventata significativa

# I modelli non stanno funzionando bene:
# - i modelli glm funzionano molto bene se n ? grande e in questo caso n ? piccolo (200)
# - inoltre in questo caso sto togliendo alcune osservazioni, 
# quindi era meglio confrontare una classe rispetto al totale log((pi_1/(pi_2+pi_3))),
# in questo modo non elimino osservazioni ma uso tutti i soggetti.
# Modelizzo chi sceglie scuole accademiche, rispetto a chi NON sceglie accamedico


# dal predittore al rapporto delle probabilit?
Exp13 = exp(predict(StepMod13, type="link",newdata=Data))
Exp12 = exp(predict(StepMod12, type="link",newdata=Data))


pigeneral = 1/(1+Exp13+Exp12) #probabilit? di scegliere generale vs altre due
pivocation = Exp13/(1+Exp13+Exp12)
piacademic = Exp12/(1+Exp13+Exp12)

Prev = cbind(pigeneral,pivocation,piacademic)
rowSums(Prev) 

## Qualche interpretazione grafica
# plot delle probabilita (sull'asse y) vs i voti dei test (asse x)
plot(0,0, xlim=c(min(Data$write), max(Data$write)), ylim=c(0,1))
points(Data$write,Prev[,1], col=2, pch=20) #Prev[,1]= probab di andare in generale [rosso]
points(Data$write,Prev[,2], col=3, pch=20) #Prev[,2]= probab di andare in vocation [verde]
points(Data$write,Prev[,3], col=4, pch=20) #Prev[,3]= probab di andare in academic [blu]

# Pi? alto valore di test in matematica, pi? aumenta la probabilit? di andare nella 
# classe blu:
plot(0,0, xlim=c(min(Data$math), max(Data$math)), ylim=c(0,1))
points(Data$math,Prev[,1], col=2, pch=20) #Prev[,1]= probab di andare in generale [rosso]
points(Data$math,Prev[,2], col=3, pch=20) #Prev[,2]= probab di andare in vocation [verde]
points(Data$math,Prev[,3], col=4, pch=20) #Prev[,3]= probab di andare in academic [blu]

# box plot delle probabilit? vs stato eonomico
par(mfrow=c(1,3))
boxplot(Prev[,1] ~ Data$ses, col=2:4) #generale
boxplot(Prev[,2] ~ Data$ses, col=2:4) #vocation
boxplot(Prev[,3] ~ Data$ses, col=2:4) #academic
par(mfrow=c(1,1))
# -> i poveri tendono a fare general, i medi a fare vocation, i ricchi academic


### ### ### ### ### ### ### ### ### ### ### ### 
###  Possiamo testare altri tipi di modelli ###
### ### ### ### ### ### ### ### ### ### ### ### 

Data$V1 = ifelse(Data$prog=="general",1,0) #1 se generale, 0 se non lo ?
#Data$V1V3 = ifelse(Data$prog!="vocation",1,0)
# Cio? la probabilit? di scegliere generale vs prob di NON scegliere generale 
# (logit(pi_1/(pi_2+pi_3)))
Mod = glm(V1 ~ ses + schtyp +read+ write+ math, data =Data, family="binomial" )
summary(Mod) 

