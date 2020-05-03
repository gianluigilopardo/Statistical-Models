###########################################################################
#
# labsurv.R - by MG, 06/11/2019
#             the survival() library in R
#             
###########################################################################
rm(list=ls())
### Simulating survival data 
set.seed(2020)  ### setting simulation seed

# simulo 10 tempi morte come exp(0.2) (media 5):
lifetimes <- rexp( 10, rate = 0.2)  ### death (event) times
# simulo 10 valori censurati uniformi:
censtimes <- 10*runif(10)        ### censoring times
# i tempi osservabili saranno quindi i minimi compoente per componente: 
time <- pmin(lifetimes, censtimes)  ### observable times
# voglio conoscere lo stato: 1 morte genuina, 0 valore censurato:
status <- as.numeric(censtimes > lifetimes) ### indicators of true deaths
x <- rnorm(10)                      ### a covariate
cbind(lifetimes,censtimes, time, status, x) # matrice avente i vettori in input come colonne
dati <- as.data.frame(cbind(time,status, x))
dati  ### display observable data (the actual sample)

### using package survival 
#library() # see all packages
library(survival) # load the survival package
### construct the Surv object
# Surv è la classe necessaria per costruire un modello di regressione:
mysurv <- Surv(time,status)

############# PARAMETRIC APPROACH ###########################################
### fitting an exponential distribution ...
# fitto il modello nullo:
fitpar <- survreg( mysurv ~ 1, dist="exponential")
summary(fitpar)
predict(fitpar)
### parametric regression assuming an exponential distribution ...

# Nella regressione con un modello parametrico:
# 1) si usa L=logY invece di Y~Weib(lambda,alpha).
#     Si dimostra che L ha funzione di sopravvivenza SL(x)=exp(-lambda*exp(alpha*x))
#     e ponendo lambda=exp(-mu/sigma), alpha = a/sigma, si dimostra che L=logY=mu+sigma*W,
#     dove W ha densità fw(x)=exp(x-exp(x)), chiamata "extreme value distribution".
# 2) In presenza di covariate X1,...,Xp-1 scriviamo µ=beta0+beta1*X1+...+betap-1*Xp-1

fitpar <- survreg( mysurv ~ x, dist="exponential") # x è una covariata: la uso per predirre Y.
# test Z per vederese il coefficiente di X è nullo.
summary(fitpar) # is x significant? why or why not?
# p>0.05 -> Qui non rifiuto H0: Il coefficiente di x è nullo (x non è significativa).
# Ha senso: ho generato x come normale, Y come esponenziale.

### ... or a Weibull distribution
# ripeto quanto sopra, usando la Weibul:
summary(fitpar <- survreg( mysurv ~ 1, dist="weibull"))
predict(fitpar)
summary(fitpar <- survreg(mysurv ~ x, dist="weibull"))

############# NONPARAMETRIC APPROACH #########################################
### nonparametric estimator (Kaplan-Meier, no regression) 
fit <- survfit(mysurv~1) # survfit invece di survreg
summary(fit)
plot(fit)

### Nelson-Aalen (Fleming e Hurrington) alternatives
summary(survfit(Surv(time, status)~1, type="fh", conf.type="log-log"))

############# real leukemia data ############################################## 
data(leukemia) ### may be data(aml)
plot(survfit(Surv(leukemia$time[1:11], leukemia$status[1:11])~1, 
             data = leukemia))

### analysis according to treatment x
fit <- survfit(Surv(time, status) ~ x, data = leukemia)
plot(fit, lty = 2:3)   
legend(100, .8, c("Maintained", "Nonmaintained"), lty = 2:3)
summary(fit)        ### cosa si nota?

### logrank test for the two groups
survdiff(Surv(time, status) ~ x, data = leukemia)

### logrank is a special case of Cox regression when the predictor is binary
summary( coxph(Surv(time, status) ~ x, data = leukemia) )

### Modeling Survival Data: Extending the Cox Model.
### by Therneau, Terry M. e Grambsch, Patricia M. (2000). Springer.


