### ### ### ### ### ### ### ### ### ###
### Modello regressivo multivariato ###
### ### ### ### ### ### ### ### ### ###
library(RCurl)

rm(list=ls())
data = getURL("https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/01%20-%20dati%20Advertising.csv?token=ACNKBBEH5JHNCHLTM7OK4J26V2JNI")
advertising = read.csv(text = data, header = T)

# fare attenzione a separatori, decimali e header
# i predittori sono i soldi spesi in pubblicit? in TV, RADIO, GIORNALE (budget$), 
# la variabile risposta (y) sono i ricavi
# leggiamo le prime righe del dataset
attach(advertising)


# regressioni multiple
attach(advertising)
head(advertising)
# regressioni multiple
MultiReg1 = lm(Sales~TV+Radio+Newspaper)
summary.lm(MultiReg1)
advertising$TV2 = I(TV^(0.5))
attach(advertising)
MultiReg2 = lm(Sales~TV2+Radio+Newspaper)
summary.lm(MultiReg2)
# In tutti e due i modelli la variabile Newspaper non ? significativa.
# pvalue alto -> accetto ipotesi (H0: b=0) -> il coefficiente ? zero e dunque la variabile 
# non ha una relazione con la variabile risposta.
# e va eliminata dal modello
MultiReg3 = lm(Sales~TV+Radio)
summary.lm(MultiReg3)
MultiReg4 = lm(Sales~TV2+Radio)
summary.lm(MultiReg4)

## model choice
AIC(MultiReg3,MultiReg4) # il quarto ? il migliore
#intervalli di confidenza per b0,b1,b2
# CIs
paste(round(coef(MultiReg4),3), " (95%CI: [",round(confint(MultiReg4, level=0.95),3)[,1], " ; ",round(confint(MultiReg4, level=0.95),3)[,2], "])", sep="")
# standard errors
paste(round(coef(MultiReg4),3), " (SE: ",round(summary.lm(MultiReg4)$coefficients[,2],3), ")",sep="")

# Matrice di correlazione dei beta dei regressori (sigma^2 inv(X'X))
covmat=summary(MultiReg4)$cov.unscaled*(summary.lm(MultiReg4)$sigma^2) # covariance matrix
round(covmat,2)


# salviamo i residui
res2=MultiReg4$residuals
# oppure
res = rstandard(MultiReg4)
# e facciamone un plot
plot(fitted(MultiReg4),res)
abline(h=0)
# c'? una struttura nei residui che va eliminata. Per adesso ignoriamola

# plot dei residui rispetto alle variabili usate nel modello
par(mfrow=c(1,3))
plot(fitted(MultiReg4),res)
abline(h=0)
plot(TV2, res)
plot(Radio, res)
# anche qui ci sono strutture nei residui

# facciamo un'analisi di influenza
meas.inf = influence.measures(MultiReg4)$infmat
colnames(meas.inf)
par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dffit"])
#plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])
##
which(meas.inf[,"dfb.1_"]< -0.4)
which(meas.inf[,"dfb.TV2"]>0.6)

which(meas.inf[,"cook.d"]>0.2)

# l'osservazione 131 sembra essere anomala
summary.lm(MultiReg4)
summary(advertising)
advertising[131,]
res[131]
summary(res)
# L'osservazione ha residuo alto in modulo
# ha valori alti per radio e bassi per TV2
# e valore di vendita medio basso
# Vediamo il valore predetto
predict(MultiReg4)[131]



# alcune previsioni
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=30, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=100), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=30, Newspaper=100), interval="confidence")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###   Modello regressivo multivariato con interazione   ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# con tre variabili e abbia molti modelli da testare
# effetti singoli
# effeti singoli più interazioni
# Non si dovrebbe mettere un'interazione se non sono presenti anche gli effetti singoli,
# diventa difficile l'interpretazione
#The '*' operator denotes factor crossing: 'a*b' interpreted as 'a+b+a:b
# ci sono differenti modi per specificare l'interazione
Reg1 = lm(Sales~TV2:Radio)       #due parametri da stimare
Reg2 = lm(Sales~TV2*Radio)       #quattro parametri da stimare
Reg3 = lm(Sales~(TV2+Radio)^2)   #quattro parametri da stimare
summary(Reg1)
summary(Reg2)
summary(Reg3)
#reg3 e Reg2 sono lo stesso modello


# Scelta automatica del miglior modello
RegAll = lm(Sales~(TV2+Radio+Newspaper)^2)
summary.lm(RegAll)
# notate come newspaper ha un effetto interattivo significativo

# Utilizziamo la funzione step
# Stepwise regression is a method of fitting regression models in which the choice of 
# predictive variables is carried out by an automatic procedure.
?step
RegStep = step(RegAll)
summary(RegStep)
# nel modello migliore, newspaper ha un effetto singolo significativo,
# anche se nel modello senza interazione non era significativa
# In generale (non in questo caso) step potrebbe scegliere un modello
# con alcune variabili non significative, che vanno poi eliminate

# Intervalli di confidenza per i parametri del modello migliore
# text manipulation for a useful output: estimate and its 95%CI
paste(round(coef(RegStep),3), " (95%CI: [",round(confint(RegStep, level=0.95),3)[,1], " ; ",round(confint(RegStep, level=0.95),3)[,2], "])", sep="")
# text manipulation for a useful output: estimate and its standard error
paste(round(coef(RegStep),3), " (SE: ",round(summary.lm(RegStep)$coefficients[,2],3), ")",sep="")


# get the residuals
res=RegStep$residuals
# plot of the residuals
plot(fitted(RegStep),res)
abline(h=0)
# We still have some structure on the residuals (inverse U-shape)
# but it is better than before






# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep), res)
plot(TV2, res)
plot(Radio, res)
plot(Newspaper, res)
plot(TV2*Newspaper, res)
plot(TV2*Radio, res)


# facciamo un'analisi di influenza
meas.inf = influence.measures(RegStep)$infmat
colnames(meas.inf)
par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dfb.Nwsp"])
plot(meas.inf[,"dfb.TV2:R"])
plot(meas.inf[,"dfb.TV2:N"])


##  le osservazioni 131 e 156 sono influenti
par(mfrow=c(3,2))
plot(meas.inf[,"dffit"])
#plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])
##
which(meas.inf[,"dfb.TV2"]>0.4)
which(meas.inf[,"dfb.Radi"]< -1)
which(meas.inf[,"dfb.Nwsp"]>0.4)
which(meas.inf[,"dfb.TV2:R"]> 1)

which(meas.inf[,"dffit"]< -0.7)
which(meas.inf[,"cook.d"]>0.2)



# ripetiamo queste ultime analisi senza queste osservazioni

# Scelta automatica del miglior modello
RegAll2 = lm(Sales~(TV2+Radio+Newspaper)^2, data = advertising[-c(131,156),])
summary.lm(RegAll2)
# notate come newspaper ha un effetto interattivo significativo

# Utilizziamo la funzione step
?step
RegStep2 = step(RegAll2)
summary(RegStep2)


# get the residuals
res=RegStep2$residuals
# plot of the residuals
plot(fitted(RegStep2),res)
abline(h=0)
# We still have some structure on the residuals (inverse U-shape) - but it is better than before


# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep2), res)
plot(advertising[-c(131,156),]$TV2, res)
plot(advertising[-c(131,156),]$Radio, res)
plot(advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Radio, res)
# I risultati sono migliori

