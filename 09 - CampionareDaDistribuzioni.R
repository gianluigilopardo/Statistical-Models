## ## ## ## ## ## ## ## ## 
## Mostriamo vari modi 
## per ottenere campioni
## da una normale bivariata
## (per trovare la distribuzione condizionata, 
## https://en.wikipedia.org/wiki/Multivariate_normal_distribution)
## ## ## ## ## ## ## ## ## ## ## ## ## 

rm(list=ls())

# Definiamo la funzione che ci permette di simulare da una 
# normale multivariata e calcolarne la densit?.
# Non siamo interessati a calcolare la distribuzione, ma alcune statistiche 
# di essa (come la media).
# N = norm_d(0,I)
# Y = A*N+mean
# Y = norm_d(mean,A'A)

rmnorm = function(n = 1, mean = rep(0,d), varcov) 
{
  d <- if (is.matrix(varcov)) 
    ncol(varcov)
  else 1
  z <- matrix(rnorm(n*d),n,d)%*%chol(varcov) #normale di-variata (0,1)
  y <- t(mean + t(z))
  return(y)
}
# ripartizione (dnorm funzione di densit? calcolata nel punto)
dmnorm=function (x, mean = rep(0,d), varcov, log = FALSE) 
{
  d <- if (is.matrix(varcov)) 
    ncol(varcov)
  else 1
  if (d > 1 & is.vector(x)) 
    x <- matrix(x, 1, d)
  n <- if (d == 1) 
    length(x)
  else nrow(x)
  X <- t(matrix(x, nrow = n, ncol = d)) - mean
  Q <- apply((solve(varcov) %*% X) * X, 2, sum)
  logDet <- sum(logb(abs(diag(qr(varcov)[[1]]))))
  logPDF <- as.vector(Q + d * logb(2 * pi) + logDet)/(-2)
  if (log) 
    logPDF
  else exp(logPDF)
}



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ##   Metodo Monte Carlo - Normale Bivariata  ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# Campiono direttamente della congiunta
# Calcoliamo la media assumendo di saper campionare dalla normale bivariata
nb = 10000
mu = c(10,20)
Sigma = matrix(c(1,0.5,0.5,1), ncol=2)
z = rmnorm(nb,mu,Sigma) #simulazione

# facciamone il plot per vedere la congiunta
smoothScatter(z,asp=1)
# calcoliamo la matrice di covarianza
cov(z) #simulazione vicino alla teorica
# vediamo la distribuzione marginali stimate e vere
par(mfrow=c(1,2))
plot(density(z[,1]))
lines(seq(0,50,by=0.2), dnorm(seq(0,50,by=0.2), mu[1],Sigma[1,1]), col=2)
plot(density(z[,2]))
lines(seq(0,50,by=0.2), dnorm(seq(0,50,by=0.2), mu[2],Sigma[2,2]), col=2)
par(mfrow=c(1,1))
# The bandwidth is a measure of how closely you want the 
# density to match the distribution


# vediamo il traceplot della simulazioni e l'autocorrelazione
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

# Possiamo ottenere gli stessi risultati
# campionando Z_1 dalla marginale e Z_2 dalla condizionata
z1 = rnorm(nb, mu[1], Sigma[1,1]^0.5)
z2 = rnorm(nb, mu[2]+Sigma[2,1]*(z1-mu[1])/Sigma[1,1], (Sigma[2,2]-Sigma[2,1]^2/Sigma[1,1])^0.5)
zstar = cbind(z1,z2)

par(mfrow=c(1,2))
smoothScatter(z,asp=1)
smoothScatter(zstar,asp=1)
par(mfrow=c(1,1))



## ## ## ## ## ##
## Metropolis  ##
## ## ## ## ## ## 
# L'algoritmo Metropolis ? meno efficiente rispetto al precedente, perch? i 
# campioni non sono indipendenti, perch? il valore che propongo dipende dal 
# valore accettato precedentemente e inotre se io rifiuto il valore, 
# allora il valore nuovo sar? identico a quello precedente.

# immaginiamo di non saper campionare dalla normale bivariata
# ma sappiamo calcolare la densit?.

z = matrix(NA, ncol=2,nrow=nb)

# Vogliamo campionare dalla bivariata
# Inizializziamo l'algoritmo 
z[1, ] = c(0,0)
sd_prop = 1 # testate diversi valori e vedete cosa succede
for(b in 2:nb)
{
  # come proposa utilizziamo una normale bivariate con sd sd_prop,
  # correlazione 0 e media z^{b-1}
  z_prop = c(rnorm(1,z[b-1,1],sd_prop),rnorm(1,z[b-1,2],sd_prop))
  
  # calcoliamo alpha
  LogNum = dmnorm(z_prop,mu,Sigma,log=T)+dnorm(z[b-1,1],z_prop[1],sd_prop^0.5,log=T)+dnorm(z[b-1,2],z_prop[2],sd_prop^0.5 ,log=T)
  LogDen = dmnorm(z[b-1,],mu,Sigma,log=T)+dnorm(z_prop[1],z[b-1,1],sd_prop^0.5,log=T)+dnorm(z_prop[2],z[b-1,2],sd_prop^0.5 ,log=T)
  Alpha = min(1,exp(LogNum - LogDen))
  
  # vediamo se accettare
  u = runif(1,0.0,1.0)
  if(u<Alpha)
  {
    z[b,] = z_prop
  }else{
    z[b,] = z[b-1,]
  }
}
# modificheremo i risultati, quindi salviamoli in un oggetto
zorig = z

# vediamo il traceplot della simulazioni e l'autocorrelazione
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

# proviamo a eliminare i campioni iniziali (burnin)
z = z[-c(1:100),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))
# Possiamo utilizzare questi valori oppure fare del thin 
# (problema del thin ? che diminuisco il campione)
# per renderli indipendenti
z  = z[seq(1,nrow(z), by=20),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

smoothScatter(z,asp=1)
dim(z)


## Un pacchetto utile per analizzare outpt MCMC
library(coda)

zmcmc = as.mcmc(z)
summary(zmcmc)

zorigmcmc = as.mcmc(zorig)
summary(zorigmcmc)



## ## ## ## ## ##
## ## Gibbs ## ## 
## ## ## ## ## ## 

# usiamo un gibbs ipotizzado di essere in grado di campionare dalle condizionate
z = matrix(NA, ncol=2,nrow=nb)

# inizializziamo l'algoritmo 
z[1, ] = c(0,0)

for(b in 2:nb)
{
  z[b,1] = rnorm(1, mu[1]+Sigma[1,2]*(z[b-1,2]-mu[2])/Sigma[2,2], (Sigma[1,1]-Sigma[1,2]^2/Sigma[2,2])^0.5) # condizionata di z_1|z_2
  z[b,2] = rnorm(1, mu[2]+Sigma[2,1]*(z[b,1]-mu[1])/Sigma[1,1], (Sigma[2,2]-Sigma[2,1]^2/Sigma[1,1])^0.5) # condizionata di z_2|z_1
}

# vediamo il traceplot della simulazioni e l'autocorrelazione
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

# anche qui va tolto il burnin e possibilemnte fatto del thin
# proviamo a eliminare i campioni iniziali (burnin)
z = z[-c(1:100),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))
# possiamo utilizzare questi valori oppure fare del thin
# per renderli indipendenti
z  = z[seq(1,nrow(z), by=3),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

smoothScatter(z, asp=1)
dim(z)



## ## ## ## ## ## ## ## ## ## ## 
##   Metropolis within Gibbs  ##
## ## ## ## ## ## ## ## ## ## ##  

# usiamo un gibbs ipotizzado di non essere in grado di campionare
# dalle condizionate ma di conoscerne la densit?


z = matrix(NA, ncol=2,nrow=nb)

# inizializziamo l'algoritmo 
z[1, ] = c(0,0)
sd_prop = 1
for(b in 2:nb)
{
  ###### z_1 #######
  # come proposa utilizziamo una normale  con sd sd_prop,
  # correlazione 0 e media z_1^{b-1}
  z_prop = rnorm(1,z[b-1,1],sd_prop)
  
  # calcoliamo alpha
  LogNum = dmnorm(c(z_prop,z[b-1,2]), mu,Sigma, log=T)  + dnorm(z[b-1,1], z_prop,sd_prop^0.5 ,log=T)
  
  LogDen = dmnorm(c(z[b-1,1],z[b-1,2]), mu,Sigma, log=T)  + dnorm(z_prop,z[b-1,1],sd_prop^0.5 ,log=T)
  
  Alpha = min(1,exp(LogNum - LogDen))
  
  # vediamo se accettare
  u = runif(1,0.0,1.0)
  if(u<Alpha)
  {
    z[b,1] = z_prop
  }else{
    z[b,1] = z[b-1,1]
  }
  
  ###### z_2 #######
  # come proposa utilizziamo una normale con sd sd_prop,
  # correlazione 0 e media z_2^{b-1}
  z_prop = rnorm(1,z[b-1,2],sd_prop)
  
  # calcoliamo alpha
  LogNum = dmnorm(c(z[b,1],z_prop), mu,Sigma, log=T)  + dnorm(z[b-1,2], z_prop,sd_prop^0.5 ,log=T)
  LogDen = dmnorm(c(z[b,1],z[b-1,2]), mu,Sigma, log=T)  + dnorm(z_prop,z[b-1,2],sd_prop^0.5 ,log=T)
  Alpha = min(1,exp(LogNum - LogDen))
  
  # vediamo se accettare
  u = runif(1,0.0,1.0)
  if(u<Alpha)
  {
    z[b,2] = z_prop
  }else{
    z[b,2] = z[b-1,2]
  }
}

# vediamo il traceplot della simulazioni e l'autocorrelazione
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))


# anche qui va tolto il burnin e possibilemnte fatto del thin
# proviamo a eliminare i campioni iniziali (burnin)
z = z[-c(1:100),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))
# possiamo utilizzare questi valori oppure fare del thin
# per renderli indipendenti
z  = z[seq(1,nrow(z), by=3),]
par(mfrow=c(2,2))
plot(z[,1], type="l")
acf(z[,1])
plot(z[,2], type="l")
acf(z[,2])
par(mfrow=c(1,1))

smoothScatter(z, asp=1)
dim(z)

