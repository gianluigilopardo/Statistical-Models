### ### ### ### ### ### ### ### ### 
### Si vuole determinare le variabili che influenzano 
### il salario
### ### ### ### ### ### ### ### ### 

# La variabile di interesse è income, voglio vedere come cambia l'età in base a income.
# Per farlo vedere uso il boxplot.

# Sotto glm, l'ipotesi della distribuzione vale SOLO asintoticamente, ma asintoticamente 
# la t diventa normale, quindi usiamo direttamente la normale.
# Inoltre la t tende ad una normale se n cresce asintoticamente, quindi non ha senso 
# usare una t di student. Approssimiamo a una normale, quindi usiamo la normale perchè 
# è piu facile.

# Avere un master è equivalente ad avere un pcd, perchè il test non è significativo 
# per il master (quindi accetto l'ipotesi che siano uguali).
# La razza che manca è quella eschimesi, se sei bianco guadagni di piu rispetto 
# agli eschimesi (test significativo).
# Se sei maschio quadagni di più.

# Differenza con gl: z value e il relativo p-value sono di una normale.
# Nel modello lineare la statistica è t. Questi risultati sono validi se n è grande 
# (in modo asintotico), e la t student è asintoticamente una normale.
# Vediamo i residui di devianza:
# devianza nulla (differenza tra il modello ottimale e quello con solo l'intercetta 
# (CHE è IL PEGGIORE CHE POTREI FARE) e devianza residua (devianza per noi, 
# cioè 2 per (differenza tra modello ottimale e quello che stiamo usando))

# NOI VOGLIAMO CHE LA DEVIANZA RESIDUA E QUELLA NULLA SIANO MOLTO DIVERSE TRA DI LORO. 
# devianza residua, devianza tra il mio modello e quello ottimale 
# è SIMILE A R^2 (indice che serve a dire quanto il modello è buono)

# I gradi sono N (numero di osservazioni) - p ( numero di parametri del modello usato) = 
# = gradi di libertà della chi quadro nella devianza

# La devianza è quanta informazione rimane nel mio modello 
# devianza nulla = quanto sono distante dal mio modello ideale 
# posso fare il numero 1-D_res/D_null = R^2_pseudo (indice che mi dice quanto io spiego)

# devianza residua=(quella che ho chiamato devianza nella teoria)=(modello migliore - modello che sto studiando) 
# -> (la differenza che io non ho spiegato)
# devianza nulla=(devianza dove la previsione è la media delle y_i) 
# (cioe media campionaria), cioè il modello peggiore (perchè ho assegnato a tutti 
# la media perchè uso come coefficiente solo b_0).
# Il modello nullo si ottiene se uso come predittore XB=b_0.
# Per avere un modello buono devo avere che null deviance sia piu grande di residual 
# deviance (altrimenti il mio modello non è migliore di quello nullo)

# educationCommunity = variabile qualitativa 
# i gradi della libertà sono quelli della chi quadro.

## 
rm(list=ls())
## 


## librerie
library(contrast)
library(deldir)
library(tidyverse)
library(aod)
##
data = getURL("https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/04%20-%20dati%20guadagno.csv?token=ACNKBBCL32U2KGP2PVNMEMC6V2L2I")

# carichiamo i dati
Data = read.csv(text = data,sep="")
summary(Data)
head(Data)


## puliamo e sistemiamo il dataset 
# CONTROLLARE SE UN VALORE è NUMERICO O FATTORIALE

recast_Data <- Data %>% select(-x)%>%select(-workclass) %>%
  mutate(education = factor(ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | education == "9th", "dropout", ifelse(education == "HS-grad", "HighGrad", ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community", ifelse(education == "Bachelors", "Bachelors", ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))

recast_data_2 <-  recast_Data %>% mutate(marital.status = factor(ifelse(marital.status == "Never-married" | marital.status == "Married-spouse-absent", "Not_married", ifelse(marital.status == "Married-AF-spouse" | marital.status == "Married-civ-spouse", "Married", ifelse(marital.status == "Separated" | marital.status == "Divorced", "Separated", "Widow")))))

Data = recast_data_2 
# Ci viene detto se uno guadagna + o - di 40k,
# quindi ho variabile binaria.


## ## ## ## ## ##  
## Descrittive ##
## ## ## ## ## ## 


###  Numeriche

plot(Data$age ~ Data$income, col=2:3)
plot(Data$hours.per.week ~ Data$income, col=2:3)
plot(Data$educational.num ~ Data$income, col=2:3)
plot(Data$educational.num~Data$education)


# riscaliamo le variabili
# (solo quelle numeriche non quelle fattoriali)
Data_rescale <- Data  # Bisognerebbe sempre riscalare le varaibili e centrarle.
# x* = (x-E(x)/sd)
for(i in 1:ncol(Data))
{
  if(is.numeric(Data_rescale[,i]))
  {
    Data_rescale[,i] = scale(Data_rescale[,i]) 
    # riscala le variabili numeriche (centrarla rispetto alla media) 
    # nuova variabile = (variabile-media(variabile))/(deviaz standard((variabile)))
  }
}
# -> Alla fine devo riportarmi al modello 
# B0 + B1 X = (B0* - B1* mu_i/sigma) + (B1*/sigma)X


###  Fattori

attributes(Data$gender)
table(Data$gender)
table(Data$gender, Data$income)/nrow(Data)
table(Data$gender, Data$income)/matrix(table(Data$income), ncol=2, nrow=2, byrow=T)
table(Data$gender, Data$income)/matrix(table(Data$gender), ncol=2, nrow=2, byrow=F)
# non abbiamo lo stesso numero di uomini e donne nel dataset, 
# quindi bisogna tenere conto che il numero è diverso.

attributes(Data$ marital.status)
table(Data$marital.status, Data$income)/matrix(table(Data$income), ncol=2, nrow=4, byrow=T)
table(Data$marital.status, Data$income)/matrix(table(Data$marital.status), ncol=2, nrow=4, byrow=F)


attributes(Data$race)
table(Data$race, Data$income)/matrix(table(Data$income), ncol=2, nrow=5, byrow=T)
table(Data$race, Data$income)/matrix(table(Data$race), ncol=2, nrow=5, byrow=F)

attributes(Data$education )
table(Data$education, Data$income)/matrix(table(Data$income), ncol=2, nrow=6, byrow=T)
table(Data$education, Data$income)/matrix(table(Data$education), ncol=2, nrow=6, byrow=F)



## ## ## ## ## ## ## ##  
## Modello logistico ##
## ## ## ## ## ## ## ## 

# y_i = #(successi)
# E(y_i) = n_i*pi_i = mu_i
# Modellizzo pi_i usando la funzione logistica
# logit(pi_i) = X*Beta = eta
# Interpretazione di Beta:
# quando pos., pi_i cresce al crescere di X
# quando è neg., pi_i decresce al crescere di X
# Ricordando che B^ ~ N(B,J^-1),
# J = X'WX, W_ii = 1/Var(yi)/*(dmu_i/deta_i)^2
# -> J^ = X'*diag(n_i*pi_i*(1-pi_i))*X

?glm
?family
?make.link
utils::str(make.link("logit"))

# La variabile risposta è successo se income è maggiore di 50.
# pi_i = P(y_i = 1)=P(income>50)
# Il modello è logit(pi_i), cioè logit(pi_i) = b_0 + b_1*x_{i,1} + b_2*x_{2,i}

# Se si voglino cambiare i reference dei fattori
# usare relevel
Data$education = relevel(Data$education,ref="PhD")
Mod1 = glm(income ~ age+education+educational.num+marital.status+race+gender+hours.per.week, data=Data, family=binomial(link = "logit"))

# In questo caso la statistica è una normale Z
# (nel caso di modello regressivo il test è t student, perchè uso una sigma 
# stimata in quanto non la conosco)

summary(Mod1)
# Nel summary manca la prima variabile, perchè i coefficienti che ci sono 
# sono fatti in confronto alla variabile che manca. è simile al modello lineare. 


# Studiamo la differenza tra due set di parametri.
# Possiamo fare dei contrasti

# Contrasto: B^ ~ N(B,J^-1)
# Potrei definire B3 = Bachelor, B4 = Phd
# B3-B4 = Phd-Bachelor
# -> So che:  C*B^ ~ N(C*B, C*J^-1*C')
# c = [0,0,-1,1]
# Beta = [B1,B2,B3,B4]
# -> c*Beta = B4-b3 ~ N(C*B, C*J^-1*C')

Cont1 = contrast(Mod1 , 
              list(education="Community" ,marital.status = "Not_married", race="Black", gender="Male",age=26, educational.num= 7, hours.per.week=40),
              list(education="Community" ,marital.status = "Not_married", race="White", gender="Male",age=21, educational.num= 7, hours.per.week=40)) # type="individual"
print(Cont1, X=T) # non è significativo 
print(Cont1)

# e possiamo calcolarli manualmente:

# Beta^ ~ N(Beta,J^-1)
# J = X'WX, W_ii = 1/Var(y_i)*dmu_i/deta_i
# in questo caso: eta_i=g(mu_i)=logit(pi_i)=logit(mu_i/n_i)=log(mu_i/n_i/(1-mu_i/n_i))
# -> deta_i/dmu_i = 1/(n_u*pi_i(q-pi_i))
# -> J = X'*diag(n_i*mu_i*(1-mu_i))*X

BetaPar = matrix(coef(Mod1),ncol=1)
#(X^tX)^{-1}sigma^2
JpowerLess1= summary(Mod1)$cov.unscaled 
# Trovo la matrice J^-1, varcov di beta stimato.
# Il comando precedente funziona solo se il modello di poisson o binomiale, 
# in questo caso è binomiale.

VecConf = Cont1$X

Diff = VecConf%*%BetaPar  
Var  = VecConf%*%JpowerLess1%*%t(VecConf) 

zvalue = Diff/(Var)^0.5 #valore della statistica osservata
# calcolo il p-value
pnorm(zvalue,0,1,lower.tail=T)*2 
# cumulata (densità) di media 0 e varianza 0, (per 2 perchè l'ipotesi è bilateriale)

# calcoliamo J manualmente e vedo che sia uguale a quella trovata con summary
pred = predict(Mod1,type="response")
X = model.matrix(Mod1)
Xt = t(X)
Wapp = matrix((pred*((1-pred))),ncol=nrow(Data), nrow=ncol(X),byrow=T) #W= 
Jcal = (Xt*Wapp)%*%X #J= x'WX

plot( c(Jcal),c(solve(JpowerLess1)))
abline(a=0,b=1,col=2)

## Calcoliamo gli intervalli di confidenza
confint.default(Mod1) #intervalli di confidenza dei singoli parametri 
#o in termini di log-ratio (OO)
exp(confint.default(Mod1))

# Usiamo la statistica di Wald su opportuni parametri:
# Ipotesi sull'intero modello
# H0: Beta=b; H1: Beta!=b
# possono essere testate utilizzando la statistica di Wald
# (beta-b)'*J*(beta-b)~chiquadro_p
wald.test(b = coef(Mod1), Sigma = vcov(Mod1), Terms = 2:5) 
# H_0: b_2=b_3=b_4=b_5=0 -> reject

# o su combinazioni lineari
wald.test(b = coef(Mod1), Sigma = vcov(Mod1), L = VecConf)
#nota: ritrovo il p-value di prima (0.085)

# possiamo anche calcolare i p-adjusted se abbiamo ipotesi multiple
p.adjust(summary(Mod1)$coefficients[,4], c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")[1])


ydic = as.numeric(Data$income)-1 #1 se è maggiore di 50mila, 0 altrimenti
pred = rep(mean(ydic), length(ydic)) #replico per la lunghezza di ydic #modello nullo

W1 = which(ydic==1) 
W0 = which(ydic==0)
## null deviance (distanza modello saturo  dal modello nullo)
# D0 = 2*L(y;B^_0)
D0 = 2*(
  sum(log(ydic/pred)[W1])+sum(log((1-ydic)/(1-pred))[W0]) 
)

gdl0 = length(ydic)-1 #gradi di libertà (1= parametri stimati, solo b_0)

## Model DEviance (residuals)
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

pred = predict(Mod1,type="response") #previsione in termine di mu)
str(pred) 
?predict.glm

W1 = which(ydic==1)
W0 = which(ydic==0)

# D1 = # D0 = 2*L(y;B^_1)
D1 = 2*(
  sum(log(ydic/pred)[W1])+sum(log((1-ydic)/(1-pred))[W0])
) #devianza tra il modello migliore e il mio modello
gdl1 = length(ydic)-length(BetaPar) #betapar quanti parametri ho nel modello


## testiamo se il modello è meglio del nullo
deltaD = D0-D1 #vorrei che questa sia più diversa da 0
# deltaD = 2*(L(y;B^_1)-L(y;B^_0))
pchisq(deltaD, gdl0-gdl1, lower.tail=F) #H_0: D1=D0, reject



## Vediamo se il modello è buono come il saturo H0: v=0 (v parametroo di non centralità)
pchisq(D1, gdl1, lower.tail=T)  #area a sinistra della linea rossa
# per fare un test sulla variazione di devianza.
# rappresentazione grafica
xseq = seq(35000,50000, length.out=100)
plot(xseq,dchisq(xseq,gdl1), type="l")
abline(v=D1,col=2) #statistica osservata
#per vedere dove si posiziona il mio modello

# La statistica osservata cade nella regione di accettazione, il test è facile da accettare,
# cioè ci vorrebbe tanta evidenza statistica per rifiutare.
# Valori alti della statistica fanno rifiutare l'ipotesi perchè sono simbolo di una 
# distribuzione non centrata.
# Il mio test è parametro di centralità v=0

## analisi dei residui
?residuals.glm

# Residui di Pearson: 
# D_i = 1/2 * frac{(y_i-n*pi_i)^2}{n*pi_i(1-pi_i)}
# Z^2 = sum_{i=1}^n D_i ~ Chisq_{n-p}

p.res   = residuals.glm(Mod1, type="pearson")
dev.res =  residuals.glm(Mod1, type="deviance")
# I predittori devono essere indipendenti.
# Possiamo vedere come si relazionano alle covariate
plot(Data$age, p.res)
# o ai valori predetti - attenzione che le indipendenze non sono valide qui
plot(pred, p.res)

# oppur possiamo usare il predittore lineare
plot(predict(Mod1,type="link"), p.res)


## Possiamo vedere anche le misure di influenza
# attenzione che sono time-consuming
#InfMeas1 = influence.measures(Mod1)


## con il test ANOVA possiamo verificare se le componenti del modello sono significative
# ATTENZIONE che le variabili sono testate nell'ordine dato dal modello.
# Aggiungo variabili se è significativa (cioè se il p-value del test: 
# H0:parametro=0 è molto basso)


## ## ## ## ## ## ## ## ## ## ## ## 
## ##    Scelta del modello   ## ## 
## ## ## ## ## ## ## ## ## ## ## ## 
# Partiamo dal modello nullo, aggiungiamo una variabile e ci chiediamo se la 
# differenza di varianze è significativa da un passaggio all'altro:
anova(Mod1,test="Chisq") #fa test sulle devianze
# L'aggiunga di age è significativ e ho fatto bene, poi aggiungo education poi etc. 
# Aggiunge secondo l'ordine del modello. 
# When given a sequence of objects, anova tests the models against one another 
# in the order specified.

Mod2 = update(Mod1,update(Mod1$formula, ~ . - age)) 
# Il modello 2 = modello 1 senza la variabile age
anova(Mod2,Mod1,test="Chisq") 
Dconf = deviance(Mod2)-deviance(Mod1) #statistica osservata 

pchisq(Dconf, 1, lower.tail=F) #p-value
xseq = seq(0,600, length.out=100)
plot(xseq,dchisq(xseq,1), type="l")
abline(v=Dconf,col=2)

# H0 in questo caso (contro filosofia) è cio che mi piace, cioè lo vorrei accettare. 
# è molto facile rimanere sotto l'ipotesi H0.
# Dovrei avere moltissime evidenza per rifuitare H0, H0 è cio che io vorrei (la potenza 
# del test è bassa). Ma in generale H0 dovrei riufitarlo



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  Step function per la scelta del modello  ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# step rimuove una variabile alla volta e calcola l'AIC: vediamo come varia.

# Partiamo da un modello con più coefficienti di quanti pensiamo siano utili:
ModStep = step(glm(income ~ age+education+educational.num+marital.status+race+gender+hours.per.week+age:education+age:gender+marital.status:race, data=Data, family=binomial(link = "logit")))




