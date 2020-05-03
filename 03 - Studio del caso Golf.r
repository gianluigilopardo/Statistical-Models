##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
# golf2.txt  - by Gianluca Mastrantonio
#              DSMS 2019 Esempio Golfers 2 tratto da  
# McClave JT., Benson PG. e Sincich T. (2014). Statistics for Business and
# Economics., Pearson Education Limited.   

# viene usato un robot per provare
# due mazze diverse, chiamate DRIVER e 5IRON, su 4 marche di palle da
# golf con 4 repliche in un piano sperimentale fattoriale completo.
# Viene poi misurata la distanza ottenuta con un tiro standard con le
# diverse combinazioni.
# I dati in formato largo sono 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Puliamo il workspace 

rm(list=ls())

# LIBRARIES  
library(reshape2)
library(contrast)
# Directory

# Leggiamo il file -  formato table
data_golf = getURL("https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/03%20-%20dati%20golf.txt?token=ACNKBBENNGKR2X4VWKVZLTK6V2LEU")
Data_wide = read.table(text=data_golf, header=T, sep="", dec=".")
summary(Data_wide)
Data_wide

Data = melt(Data_wide, id.vars="CLUB") #melt=fusione
summary(Data)
Data
replications(Data[,c(1,2)])
# Returns a vector or a list of the number of replicates for each term in the formula
# per capire meglio il funzionamento di replications
# provate replications(Data[1:9,c(1,2)])


# Ordiniamo in base a Club
Data = Data[order(Data$CLUB),]

# rappresentazione grafica, effetti singoli e interattivi:
#   -CLUB: tipo di mazza
#   -variable: marca di palla

par(mfrow=c(1,2))
# C'è effetto della variabile club eperchè i boxplot non sono sovrapposti
# sembra che non ci sia un grande effetto della pallina perchè i boxplot sono sovrapposti.
boxplot(value ~ CLUB, data=Data, col=1:8+1, main="Boxplot")
# la linea è la mediana, il rettangolo va dal primo al terzo quantile 
# (rappresenta il 50% dei dati attorno alla mediana) e vediamo anche 
# i due estremi sono il massimo e il minimo
boxplot(value ~ variable, data=Data, col=1:8+1, main="Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
boxplot(value ~ CLUB*variable, data=Data, col=1:8+1, main="Boxplot")
# C'è interazione perchè conoscendo iron o driver posso dire qualcosa sulla pallina,
# poichè non ritrovo lo stesso andamento nel primo grafico vuol dire che c'è interazione
# posso dire qualcosa sul comportamento delle palline in base a quale mazza utilizzo
# -> c'è interazione
boxplot(value ~ CLUB, data=Data, col=1:8+1, main="Boxplot")
boxplot(value ~ variable, data=Data, col=1:8+1, main="Boxplot")
par(mfrow=c(1,1))

# collega la media di 5IRON A CON LA MEDIA DI DRIVER A, per ogni A,B,C,D
# interaction plot per la media: factor=categoria
interaction.plot(x.factor=Data$CLUB, trace.factor=Data$variable, response=Data$value,fun=mean, pch=20, type="b", main="Interaction plot",ylab="Value")
interaction.plot(x.factor=Data$variable, trace.factor=Data$CLUB, response=Data$value,fun=mean, pch=20, type="b", main="Interaction plot",ylab="Value")

#i valori x.factor sull'asse delle x, response sull'asse y, trace.factor cosa collegare

# interaction plot per la varianza
interaction.plot(x.factor=Data$CLUB, trace.factor=Data$variable, response=Data$value,fun=var, pch=20, type="b", main="Interaction plot",ylab="Value")
interaction.plot(x.factor=Data$variable, trace.factor=Data$CLUB, response=Data$value,fun=var, pch=20, type="b", main="Interaction plot",ylab="Value")

# medie per gruppi
tapply(Data$value, list(Data$CLUB,Data$variable), mean)


# H0: non c'è interazione
# test è significativo (stars), reject H0.
# test non è significativo, accept H0.
## Anova
# analisi con la statististica Fisher 
aov.out = aov(value ~ CLUB*variable, data=Data)
summary(aov.out)
model.tables(aov.out, type="means")
#test 1: le medie tra i club sono uguali
#test 2: le medie tra le palline sono uguali
#test 3: le medie tra un gruppo determinato da pallina e club sono uguali


# testiamo le varianze 
# H0: la varianza in ogni gruppo (campione) è la stessa.
bartlett.test(value ~ interaction(CLUB, variable), data=Data)
bartlett.test(value ~ CLUB, data=Data)
bartlett.test(value ~ variable, data=Data)
# -> test non significativo, accettiamo l'ipotesi nulla quindi le varianze sono le stesse 
# e possiamo usare il modello lineare e anova. 


# vediamo più attentamente il  modello 
summary(aov.out)
# una differente parametrizzazione
reg = lm(value ~ CLUB*variable, data=Data) #intercetta è la media di gruppo IRON e pallina A
summary(reg) #oppure summary.lm(aov.out)
# se voglio pallina c e club driver, allora devo sommare intercetta clubdrive, 
# variable c  e interazione.

# vediamo come è formalizzato il modello:
# MATRICE X sperimentale (gli uni corrispondono ad avere pallina A 
# e iron, gli altri valori mi diranno l'incremento rispetto a pallina 
# A e iron  (incremento o decremento che ho usando altre palline)
ModelMatrix = model.matrix(~ CLUB*variable, data=Data)

reg2 = lm(value ~-1+ CLUB*variable, data=Data)
# il primo valore è media iron-a, il secondo driver-a, per ottenere gli 
# altri li sommo alle rispettive medie
summary(reg2)
# vediamo come è formalizzato il modello
ModelMatrix2 = model.matrix(~ -1+CLUB*variable, data=Data)
#non ho più l'intercetta, ma divido i due club


# p-value negli i.c.: accettiamo l'ipotesi nulla se il valore
# ipotizzato è compreso nell'intervallo costruito, 

TukeyHSD(aov.out) # Fa gli intervalli di confidenza della differenza 
# delle medie tra due gruppi (considera tutti i possibili confronti) 
# (28 confronti) (c(c-1))/2 8 gruppi, (8*7)/2= 28
# p_value adj tiene conto del numero delle variabili usate


aov.out1 =aov(value ~ 1, data=Data)
aov.out2 =aov(value ~ CLUB*variable, data=Data)
aov.out3 =aov(value ~ CLUB:variable, data=Data)
aov.out4 =aov(value ~ CLUB, data=Data)
aov.out5 =aov(value ~ variable, data=Data)

# confronta i due modelli 
# aov anova normale
# anova confronta i modelli
anova(aov.out1,aov.out2)
anova(aov.out1,aov.out3)
anova(aov.out1,aov.out4)
anova(aov.out1,aov.out5)
anova(aov.out2,aov.out4)



## ## ## ## ## ## ## ## ## 
## ##    Contrasti   ## ## 
## ## ## ## ## ## ## ## ## 
# settiamo i livelli base
#Data$CLUB = relevel(Data$CLUB, ref="5IRON")
#Data$variable = relevel(Data$variable, ref="A")
reg = lm(value ~ CLUB*variable, data=Data)
summary(reg)
ModelMatrix = model.matrix(~ CLUB*variable, data=Data)
# interpretazione effetti 
regCoef = reg$coefficients
names(regCoef) = paste(names(regCoef),"_b",0:7,sep="")
regCoef
# z è una statistica per fare i confronti
# z=(5IRON,B)-(5IRON,C)=b0+b2-(b0+b3)=b2-b3 e mi chiedo se z=0 
# e io conosco la distribuzione di z 
# z= b2-b3 = (0,0,1,-1,0)'*(b0,b1,b2,b3,b4)
# io conosco la distribuzione di (0,0,1,-1,0)'*(b0,b1,b2,b3,b3) 
# è una normale (vedi trasformazione di una normale con il vettore (0,0,1,-1,0))
# Gli effetti possono essere calcolati come:
# (5IRON,A)  = b0
# (5IRON,B)  = b0+b2
# (5IRON,C)  = b0+b3
# (5IRON,D)  = b0+b4
# (DRIVER,A) = b0+b1
# (DRIVER,B) = b0+b1+b2+b5
# (DRIVER,C) = b0+b1+b3+b6
# (DRIVER,D) = b0+b1+b4+b7


## Calcoliamo i contrasti:

# Voglio vedere se c'è differenza tra C e B in 5IRON. è SIGNIFICATIVA, cioè c'è differenza
# test: H0: z=p'b=0, con p=(0,0,-1,1,0,0,0,0) e b=(b0,b1,b2,b3,b3,b5,b6,b7)
# quindi il test diventa H0: (-b2+b3)=0 <-> b3=b2
# il p-value è piu piccolo di alplha=0.05 quindi posso rifiutare l'ipotesi H0, 
# quindi b2 è diverso da b3.

# C1 Ipotizza che la media del sottogruppo (Variabile C e 5iron) sia uguale 
# alla media del sottogruppo (variabile B e 5iron)
C1 = contrast(reg, 
list(variable="C", CLUB="5IRON"),
list(variable="B", CLUB="5IRON")
)
print(C1, X=T)

# Calcoliamolo manualmente, cioè trovo la distribuzione di z
coefs = matrix(coef(reg),ncol=1) #vettore beta
# (XX')^{-1}sigma^2
covmat=matrix(summary(reg)$cov.unscaled*(summary.lm(reg)$sigma^2), ncol=nrow(coefs))
# cov.unscaled = inv(X*X')
# è una t-studente perchè la varianza non è certa ma è stimata, 
# se fosse varianza nota allora sarebbe una normale
VecConf = matrix(0, nrow=1, ncol=nrow(coefs)) #alloco
VecConf[c(3,4)] = c(-1,1) 

Diff = VecConf%*%coefs #numeratore della statistica
Var  = VecConf%*%covmat%*%t(VecConf)

Tvalue = Diff/Var^0.5 #valore della statistica, RITROVO IL VALORE DELLA STATISTICA t in C1
pt(Tvalue,24,0,lower.tail=F) #p-value t student
# 0 = parametro di non centralità
# 24 gradi di libertà (32-8)


# (5IRON,A) - (5IRON,B) = b0-(b0+b2) = -b2
C2 = contrast(reg, 
              list(variable="B", CLUB="DRIVER"),
              list(variable="C", CLUB="DRIVER")
)
print(C2, X=T)
# (DRIVER,B)-(DRIVER,C) = b0+b1+b2+b5- (b0+b1+b3+b6)= b2+b5-b3-b6

# CONTRASTO MARGINALE, non voglio un particolare club, ma in generale 
# vorrei sapere se c'è differenza tra b e c
C3 = contrast(reg, 
              list(variable="B" ,CLUB = levels(Data$CLUB)),
              list(variable="C", CLUB = levels(Data$CLUB)),
              type="average") # type="individual"
print(C3, X=T)
# (DRIVER,B)+(5IRON,B)-((DRIVER,C)+(5IRON,C)) = b0+b2+b0+b1+b2+b5-(b0+b3+b0+b1+b3+b6) = 
# = b2+b2+b5-(b3+b3+b6) = 2b2-2b3+b5-b6
# In questo caso il vettore dei contrasti è (0,0,1,-1,0,0.5,-0.5,0)

