#######################################################################################
# STATISTICAL MODELS                                                                  #
# Linear regression                                                                   #      
#                                                                                     #
# Dataset: Adverstising                                                               #
#                                                                                     #  
# The dataset contains the sales, of thousands of units, of a product in 200 markets, # 
# together with the budget spent on advertising, in thousands of dollars, on 3 media: #
# TV, radio and newspapers.                                                           #
# The aim is to determine which advertising strategy is best and to predict the sale  #
# based on the budget spent on each media                                             #
#######################################################################################

rm(list=ls())
library(RCurl)

# Functions
?pairs
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

data = getURL("https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/01%20-%20dati%20Advertising.csv?token=ACNKBBEH5JHNCHLTM7OK4J26V2JNI")
advertising = read.csv(text = data, header = T)

# Pay attention to separators, decimals and headers.
# Predictors are the money spent on advertising on TV, RADIO, NEWSPAPER (budget $),
# the response variable (y) is the revenue.
# Let's read the first few lines of the dataset
head(advertising)

summary(advertising)

attach(advertising)

TV
hist(TV)

### ### ### ### ### ### ### ### ### ### ###
### ### ### Descriptive analysis ## ### ### 
### ### ### ### ### ### ### ### ### ### ###

length(advertising) # Number of variables
                    # this works with dataframe, not matrix.
dim(advertising)    # rows and columns
names(advertising)  # names of the attributes -> colnames(advertising)
length(X)           # number of records
nrow(advertising)
ncol(advertising)

# plot 
plot(advertising)
?pairs
pairs(advertising[,2:5]) # standard plot
pairs(advertising[,2:5], upper.panel=panel.cor) # correlation
pairs(advertising[,2:5], upper.panel=panel.cor,diag.panel=panel.hist) # histogram
# Pay attention to the correlation among predictors

# Plots
par(mfrow=c(nrows=1,ncols=3)) # par(mfrow=c(1,3))
plot(x=TV, y=Sales, pch=16, col="red")
plot(x=Radio, y=Sales, pch=16, col="red")
plot(x=Newspaper, y=Sales, pch=16, col="red")

# smooth estimate of the relationship
par(mfrow=c(nrows=1,ncols=3)) # par(mfrow=c(1,3))
scatter.smooth(x=TV, y=Sales, pch=16, col="red")
scatter.smooth(x=Radio, y=Sales, pch=16, col="red")
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red")

# increase the smooth: span is the parameter for "smoothness".
?scatter.smooth 
par(mfrow=c(1,3))
scatter.smooth(x=TV, y=Sales, pch=16, col="red", span=0.1)
scatter.smooth(x=Radio, y=Sales, pch=16, col="red", span=0.1)
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red", span=0.1)


### ### ### ### ### ### ### ### ### ### ###
### # Linear regression: one predictor  ###
### ### ### ### ### ### ### ### ### ### ###

### TV: Sales=b_0+b_1*TV
# 200 observations and two parameters to estimate (b_0 e b_1) 
# -> 198 degree of freedom for the model. 
# Residual average is zero -> mean(SimpleReg1$residuals) ~0 

SimpleReg1 = lm(Sales~TV)
summary(SimpleReg1)
# b_0 = expectation of Sales (revenue) without advertising in TV.
# estimate of the variance from MSE (quadratic sum of residuals/(n-p))

# b_1: if I increase of 1 money spent in advertising for TV, then Sales increase of b_1
# low p-value -> I decline H0.

### Radio
SimpleReg2 = lm(Sales~Radio)
summary(SimpleReg2)

### Newspaper
SimpleReg3 = lm(Sales~Newspaper)
summary(SimpleReg3)

# Choosing the model:
# R2 = SSreg*SStot=1-SSres/SStot
# R2adj = 1-(1-R2)*(n-1)/(n-p-1) -> take in account of the number of parameters
# -> R2 e R2adj to maximize.
# AIC = -2k+2log(L(Beta,sigma2;y)) -> to minimize
# k = #(model parameters) = (p+1)+1
RsquaredAdj = c(
    summary(SimpleReg1)$adj.r.squared,
    summary(SimpleReg2)$adj.r.squared,
    summary(SimpleReg3)$adj.r.squared
    )
Rsquared = c(
    summary(SimpleReg1)$r.squared,
    summary(SimpleReg2)$r.squared,
    summary(SimpleReg3)$r.squared
    )
Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3)[,2]

# All three prefer the first model
ModelChoice = data.frame(
    Mod=c("SimpleReg1","SimpleReg2","SimpleReg3"),
    RsquaredAdj=RsquaredAdj,
    Rsquared=Rsquared,
    Aic = Aic
    )
ModelChoice

# Let's see the results of the first model
summary(SimpleReg1)
names(SimpleReg1)
str(SimpleReg1)

# Stime e 95%CIs: crea intevalli di confidenza per b_0 e b_1
paste(round(coef(SimpleReg1),3), " (95%CI: [",round(confint(SimpleReg1, level=0.95),3)[,1], " ; ",round(confint(SimpleReg1, level=0.95),3)[,2], "])", sep="")
# Stime e standard errors = square of variance fitted (MSE*inv((X'X))
paste(round(coef(SimpleReg1),3), " (SE: ",round(summary.lm(SimpleReg1)$coefficients[,2],3), ")",sep="")


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV, y=Sales, pch=16, col="red")
abline(SimpleReg1, col="blue", lwd=3)

#
segments(x0=TV,y0=Sales,x1=TV,y1=coef(SimpleReg1)[1] + coef(SimpleReg1)[2]*TV)

## Verifica delle ipotesi alla base del modello

res = SimpleReg1$residuals
# o res = summary.lm(SimpleReg1)$residuals...

# plottiamo i residui
# Ipotesi: se conosco X (TV), non so nulla sull'errore (epsilon). 
# -> Var(e|X) = var(e) in questo caso non è rispettata
plot(TV,res)
abline(h=0)
# C'è eteroschedasticità: le ipotesi alla base del modello non sono soddisfatte
# C'è una componente "curvilinea" residua

# Verifichiamo la normalità dei residui:
# I residui, essendo lineari in y, sono normali.
# Per controllare se i residui sono normali, possiamo usare Q-Q plot, 
# un plot dei residui del modello contro i residui di una statitista 
# distribuita come Normale(0,1)
qqnorm(res/sd(res))  # plot quantile-quantile (residui standardizzati)
# La standardizzazione si ottiene sottraendo al valore osservato 
# la media e dividendo per la deviazione standard 
# media dei residui è zero
abline(a=0,b=1,col=2) #Se i residui sono normali, da qqplot devo 
# ottenere i dati sulla bisettrice


# Il coefficiente di leverage (leva, influenza), che varia tra 0 ed 1, è una 
# misura di quanto un dato valore della variabile indipendente si discosta 
# dalla sua media. I valori di Y relativi ad X con elevati valori di leva 
# (h's outliers) hanno maggior peso nel determinare l'andamento della linea
# di regressione.
# Cook's distance combina leverage e residui standardizatti
plot(SimpleReg1)

# oppure
shapiro.test(res) #  H0: i dati sono distribuiti normalmente

# Valori di distanze di Cook bassi per un punto indicano che la
# rimozione della rispettiva osservazione ha poco effetto sui risultati
# della regressione, ovvero l'osservazione in particolare non ha valori
# devianti dalla tendenza. Invece valori di distanze di Cook
# superiori a 1 sono sospetti ed indicano la presenza di un
# possibile outlier o di un modello povero.
# Leverage is a measure of how far away the independent variable 
# values of an observation are from those of the other observations.

# Per alti valori di leverage (h_ii ~ 1), Var(mu_i_hat)~Var(y_i), e questo implica
# Var(e_i)=0 -> y_i ha una grande influenza su mu_i_hat.

# La distanza di Cook si basa sul cambiamento di b_hat quando l'osservazione
# è rimossa dal dataset. Un punto con un'alto Leverage è un punto d'influenza 
# se è un outlier rispetto alla retta di regressione.
# That points for which Cook's distance is higher than 1 are to be considered as influential.


# Intervalli di confidenza dei parametri
confint(SimpleReg1) #Calcola intervalli di confidenza per i parametri del modello. 
# Nel caso di regressione lineare, la statistica è una t student
Lower_bounds=confint(SimpleReg1)[,1] #2.5 = (1-alpha)/2, 97.5 = 1-(1-alpha)/2
Upper_bounds=confint(SimpleReg1)[,2]

# banda di previsione
plot(x=TV, y=Sales, pch=16, col="red")
abline(SimpleReg1, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)
# Se ho problemi di media, posso fare una trasformazione
# Se ho problemi di varianza, posso aggiungere una variabile

# Proviamo a risolvere i problemi nei residui con una trasformazione
# è impossibile risolvere il problema dell'eteroschedasticità con una trasformazione,
# ma possiamo provare ad eliminare, o alleviare, la componente curva residua.

# Il modello non funziona perchè non sono rispettate le ipotesi, infatti Var(e|x) 
# è diversa da Var(e). Questo è causato dal fatto che tra Sales e TV non c'è 
# relazione lineare.


# trasformazione
advertising$TV2 = TV^0.5
pairs(advertising[,2:6], upper.panel=panel.cor,diag.panel=panel.hist)
scatter.smooth(x=TV2, y=Sales, pch=16, col="red")

SimpleReg4 = lm(Sales~I(TV^0.5))
# oppure
SimpleReg4 = lm(Sales~TV2) # da errore, bisogna rifare attach del dataset
attach(advertising)
SimpleReg4 = lm(Sales~TV2)


# Scelta del modello
RsquaredAdj = c(
    summary(SimpleReg1)$adj.r.squared,
    summary(SimpleReg2)$adj.r.squared,
    summary(SimpleReg3)$adj.r.squared,
    summary(SimpleReg4)$adj.r.squared
    )
Rsquared = c(
    summary(SimpleReg1)$r.squared,
    summary(SimpleReg2)$r.squared,
    summary(SimpleReg3)$r.squared,
    summary(SimpleReg4)$r.squared
    )
Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3,SimpleReg4)[,2]

ModelChoice = data.frame(
    Mod=c("SimpleReg1","SimpleReg2","SimpleReg3","SimpleReg4"),
    RsquaredAdj=RsquaredAdj,
    Rsquared=Rsquared,
    Aic = Aic
    )
ModelChoice # il nuovo modello è il migliore


# Vediamo i risultati
summary(SimpleReg4)
names(SimpleReg4)
str(SimpleReg4)


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV2, y=Sales, pch=16, col="red") # fate attenzione: usare TV2
abline(SimpleReg4, col="blue", lwd=3)

# plot the least squares lines
segments(x0=TV2,y0=Sales,x1=TV2,y1=coef(SimpleReg4)[1] + coef(SimpleReg4)[2]*TV2)

## Verifica delle ipotesi alla base del modello
res = SimpleReg4$residuals
# o res = summary.lm(SimpleReg4)$residuals...

# plottiamo i residui
plot(TV2,res)
abline(h=0)
# C'è ancora eteroschedasticità. Proviamo a risolverla utilizzando più variabili


# Verifichiamo la normalità dei residui
qqnorm(res/sd(res))         # plot quantile-quantile
abline(a=0,b=1,col=2)
# oppure
plot(SimpleReg4)


# Intervalli di confidenza dei parametri
confint(SimpleReg4)
Lower_bounds=confint(SimpleReg4)[,1]
Upper_bounds=confint(SimpleReg4)[,2]

# banda di previsione
plot(x=TV2, y=Sales, pch=16, col="red")
abline(SimpleReg4, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)


# predizione
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="confidence")
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="prediction")



### ### ### ### ### ### ### ### ### ### ### ### 
###   Misure di influenza - Quarto Modello  ###
### ### ### ### ### ### ### ### ### ### ### ###

# L'idea è di eliminare un'osservazione e vedere come cambia il modello
meas.inf = influence.measures(SimpleReg4)$infmat
colnames(meas.inf)
#"dfb.1_"       variazioni nel valore dell'intercetta
#"dfb.TV2"      variazioni nel valore del coefficiente regressivo
#"dffit"        variazioni nella previsione i-esima
#"cov.r"        variazioni nella matrice di covarianza
#"cook.d"       variazione nella previsione generale
#"hat"          matrice di influenza
par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dffit"])
plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])
# non sembrano esserci osservazioni particolarmente influenti

## possiamo trovare le osservazioni con valori "estremi" tramite
which(meas.inf[,"hat"]>0.035)



