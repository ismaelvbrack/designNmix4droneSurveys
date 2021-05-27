
#######################################################################
##* Simulate spatiotemporally replicated count data (e.g. in drone images)
##*   and analyze with N-mixture model using JAGS (Bayesian approach)
######################################################
#* Examples with different protocols in image review:
#    (1)single observer counts
#    (2)independent double observers mark
#    (3)mixed single and double protocols

library(jagsUI)

# * Defining biological scenario and sampling -----------------------------

lambda <- 0.3 # local abundance in sites

phi <- 0.4 # availability of individuals for detection

p <- 0.8 # perception probability by each observer

S <- 60 # number of sites

J <- 5 # number of visits per site

propObs <- 0.4 # proportion of site-visits reviewed by a 2nd observer (only for ex.3)

# (1) Single observer counts ----------------------------------------------

#* Simulating...
# Sample abundance in sites from a Poisson distribution
M <- rpois(n = S, lambda = lambda)

# Array structure to receive simulated count data
Y <- array(dim = c(S,J))

# Loop over visits...
for(j in 1:J){
  Y[,j] <- rbinom(S, M, (phi*p)) # sample counts for all sites in each visit
}

#* Analyzing...

# Model in BUGS
sink("model_1obs.txt") 
cat(" 
    model { 
    # Priors
    p~dunif(0,1)
    lambda~dunif(0,100)
    
    # Likelihood 
    
    # Biological model for true abundance 
    for (i in 1:S) { 
      M[i] ~ dpois(lambda)
      
      # Observation model for replicated counts 
      for (j in 1:J) { 
        Y[i,j] ~ dbin(p , M[i])
      }#j
    } #i
    
    # Derived quantities
    totalM <- sum(M[])
    
    } #model
    ",fill = TRUE) 
sink()

# Bundle data
data1 <- list(Y = Y, S = S, J = J)

# Initial values
inits <- function() list(p=runif(1,0,1),M=apply(Y,1,max)+1)
# Parameters monitored
params <- c("totalM", "lambda","p")
# MCMC settings
ni <- 50000
nt <- 1
nb <- 10000
nc <- 3
na <- 10000


out1 <- jags(data1, inits, params, model.file="model_1obs.txt",
            n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

#* see results
out1
traceplot(out1)

# Get max counts
max.count <- apply(Y,1,max)

# Plot posteriors
par(mfrow=c(2,2),mar=c(4,4,1,1),omi=c(.3,.3,.1,.1))

## p
hist(out1$sims.list$p0, col = "gray", main = "", xlab = "detection prob.",breaks=60,freq=F,
     las = 1, xlim = c(0,1))
abline(v = p*phi, lwd = 2, col = "red")
abline(v=out1$mean$p0,lwd=3,col="blue")
abline(v=c(out1$q2.5$p0,out1$q97.5$p0),lwd=2,col="gray",lty=3)

## lambda
hist(out1$sims.list$lambda, col = "gray", main = "", xlab = "local abundance",breaks=60
     ,freq=F,las = 1)
abline(v = lambda, lwd = 3, col = "red")
abline(v=out1$mean$lambda,lwd=3,col="blue")
abline(v=c(out1$q2.5$lambda,out1$q97.5$lambda),lwd=2,col="gray",lty=3)

## total M
hist(out1$sims.list$totalM, col = "gray", main = "", xlab = "total N",breaks=60,freq=F,
     las = 1)
abline(v = sum(M), lwd = 3, col = "red")
abline(v=out1$mean$totalM,lwd=3,col="blue")
abline(v=c(out1$q2.5$totalM,out1$q97.5$totalM),lwd=2,col="gray",lty=3)
abline(v=sum(max.count),lwd=3)
legend("topright",c("Real","Estimate","95% CI","Counts"),lty=c(1,1,3,1),lwd=2,col=c(2,4,"gray",1),bty="n",cex=.6)



# (2) Double observers ----------------------------------------------------

#* Simulating...
# Sample abundance from a Poisson distribution
M <- rpois(n = S, lambda = lambda)

# Multinomial cells probabilities
cellprobs <- c(
  (1-p) * p,    # 01
  p * (1-p),    # 10
  p * p,        # 11
  (1-p) * (1-p) # 00
)

# Create structure to contain available individuals 
avails <- array(dim = c(S, J))

# Create structure to contain frequencies of capture histories
Y <- array(dim=c(S,4,J)) # 01, 10, 11, 00

## Loop over sites and occasions to generate data
for (i in 1:length(M)){
  avails[i,] <- rbinom(n = J, size = M[i], prob = phi) # Sample available individuals from a Binomial(N, phi) 
  for (j in 1:ncol(avails)){
    Y[i,,j] <- rmultinom(1, avails[i,j], cellprobs)
  }#j
}#i

Y <- Y[,-4,]  # excluding "00"

#* Analyzing...

# Model in BUGS
sink("model_2obs.txt") 
cat(" 
    model { 
    # Priors
    phi~dunif(0,1)
    p0~dunif(0,1)
    lambda~dunif(0,100)
    
    # Likelihood 
    
    # Biological model for true abundance 
    for (i in 1:S) { 
      M[i] ~ dpois(lambda)
      
      # Observation model for replicated counts 
      for (j in 1:J) { 
        avails[i,j] ~ dbin(phi , M[i])
        
        # Define multinomial cell probabilities
        p[i,j] <- p0
        mu[i,1,j] <- p[i,j] * p[i,j]         # 11
        mu[i,2,j] <- p[i,j] * (1-p[i,j])     # 10     
        mu[i,3,j] <- (1-p[i,j]) * p[i,j]     # 01
        pi0[i,j] <- 1 - mu[i,1,j] - mu[i,2,j] - mu[i,3,j]
        pcap[i,j] <- 1 - pi0[i,j] #Probability of sighting by at least one observer
        for (k in 1:3) {
          muc[i,k,j] <- mu[i,k,j] / pcap[i,j]
        } #k
      
        # Observation model for multiple observers - conditional multinomial
        ncap[i,j] ~ dbin(pcap[i,j] , avails[i,j])
        Y[i,1:3,j] ~ dmulti(muc[i,1:3,j] , ncap[i,j])
      } #j
    } #i
    
    # Derived quantities
    totalM <- sum(M[])
    
    } #model
    ",fill = TRUE) 
sink()

# Bundle data
ncap <- array(dim=c(S,J))
for (j in 1:J) {
  for (i in 1:nrow(Y[,,j])) {
    ncap[i,j] <- sum(Y[i,,j])
  }}
nmax <- apply(ncap,1,max)

data1 <- list(Y = Y, ncap = ncap , S = S, J = J)

# Initial values
inits <- function() list(p0=runif(1,0,1),phi=runif(1,0,1),M=nmax+2,avails=ncap+1)
# Parameters monitored
params <- c("totalM", "lambda","p0","phi")
# MCMC settings
ni <- 50000
nt <- 1
nb <- 10000
nc <- 3
na <- 10000


out2 <- jags(data1, inits, params, model.file="model_2obs.txt",
            n.chains=nc,n.thin=nt, n.iter=ni, n.burnin=nb, n.adapt=na,parallel=T)

#* see results
out2
traceplot(out2)

# Get max counts
max.count <- numeric(0)
for(i in 1:dim(Y)[3]){
  max.count[i] <- sum(rowSums(Y[,,i]))
}

# Plot posteriors
par(mfrow=c(2,2),mar=c(4,4,1,1),omi=c(.3,.3,.1,.1))

## phi
hist(out2$sims.list$phi, col = "gray", main = "", xlab = "avaliabilty",breaks=60,freq=F,
     las = 1, xlim = c(0,1))
abline(v = phi, lwd = 3, col = "red")
abline(v=out2$mean$phi,lwd=3,col="blue")
abline(v=c(out2$q2.5$phi,out2$q97.5$phi),lwd=2,col="gray",lty=3)

## p
hist(out2$sims.list$p0, col = "gray", main = "", xlab = "perception prob.",breaks=60,freq=F,
     las = 1, xlim = c(0,1))
abline(v = p, lwd = 2, col = "red")
abline(v=out2$mean$p0,lwd=3,col="blue")
abline(v=c(out2$q2.5$p0,out2$q97.5$p0),lwd=2,col="gray",lty=3)

## lambda
hist(out2$sims.list$lambda, col = "gray", main = "", xlab = "local abundance",breaks=60
     ,freq=F,las = 1)
abline(v = lambda, lwd = 3, col = "red")
abline(v=out2$mean$lambda,lwd=3,col="blue")
abline(v=c(out2$q2.5$lambda,out2$q97.5$lambda),lwd=2,col="gray",lty=3)

## total M
hist(out2$sims.list$totalM, col = "gray", main = "", xlab = "total N",breaks=60,freq=F,
     las = 1)
abline(v = sum(M), lwd = 3, col = "red")
abline(v=out2$mean$totalM,lwd=3,col="blue")
abline(v=c(out2$q2.5$totalM,out2$q97.5$totalM),lwd=2,col="gray",lty=3)
abline(v=max(max.count),lwd=3)
legend("topright",c("Real","Estimate","95% CI","Counts"),lty=c(1,1,3,1),lwd=2,col=c(2,4,"gray",1),bty="n",cex=.6)


# (3) Mixed-protocols -----------------------------------------------------

#* Simulating...
# Sample abundance from a Poisson distribution
M <- rpois(n = S, lambda = lambda)

# Data type matrix in mixed protocols
dataty <- matrix(sample(rep(1:2, c( round(S*J*(1-propObs)) , round(S*J*propObs) ))),
                 nrow=S,ncol=J)

cellprobs <- c(
  (1-p) * p,    # 01
  p * (1-p),    # 10
  p * p,        # 11
  (1-p) * (1-p) # 00
)

# Create structure to contain available individuals 
avails <- array(dim = c(S, J))

# Create structure to contain frequencies of capture histories
Y <- array(dim=c(S,5,J)) # 01, 10, 11, 00, 1x

## Loop over sites and occasions to generate data
for (i in 1:length(M)){
  avails[i,] <- rbinom(n = J, size = M[i], prob = phi) # Sample available individuals from a Binomial(N, phi) 
  for (j in 1:ncol(avails)){
    if(dataty[i,j]==2){     # if it's a double observer protocol
      Y[i,1:4,j] <- rmultinom(1, avails[i,j], cellprobs)
    }
    if(dataty[i,j]==1){     # if it's a single observer protocol
      Y[i,5,j] <- rbinom(1, avails[i,j], p)
    }
  }#j
}#i

Y <- Y[,-4,]  # excluding "00"

#* Analyzing...

# Model in BUGS
sink("model_mixed-protocols.txt") 
cat(" 
    model { 
    # Priors
    phi~dunif(0,1)
    p~dunif(0,1)
    lambda~dunif(0,40)
    
    # Likelihood 
    # Biological model for true abundance 
    for (i in 1:S) { 
      M[i] ~ dpois(lambda)
    # Model for available individuals in replicated counts
      for (j in 1:J) { 
         avails[i,j] ~ dbin(phi , M[i])
      } #j
    } #i
    
    # Define multinomial cell probabilities
      mu[1] <- (1-p) * p    # 01
      mu[2] <- p * (1-p)     # 10
      mu[3] <- p * p         # 11
      pi0 <- 1 - mu[1] - mu[2] - mu[3]
      pcap <- 1 - pi0 #Probability of detection by at least one observer
      #Conditional probabilities:
      for (k in 1:3) {
         muc[k] <- mu[k] / pcap
      } #k
          
    # Observation model for single observer
    for(j in 1:length(Y1)) {
      Y1[j] ~ dbin(p, avails[siteID1[j] , visits1[j]])
    } #j
    
    # Observation model for multiple observers - conditional multinomial
    for(j in 1:length(ncap)) {
      ncap[j] ~ dbin(pcap , avails[siteID2[j] , visits2[j]])
      Y2[j,1:3] ~ dmulti(muc[1:3] , ncap[j])
    } #j
    
    # Derived quantities
    totalM <- sum(M[])
    
    } #model
    ",fill = TRUE) 
sink()

##* Data for single observer
Y1 <- as.vector(Y[,4,])
siteID1 <- rep(1:S,J)
visits1 <- rep(1:J,each=S)

##* Data for double observers
Y2 <- apply(Y[,-4,],2,rbind)
Y2 <- Y2[as.vector(dataty)==2,]
siteID2 <- rep(1:S,J)
siteID2 <- siteID2[as.vector(dataty)==2]
visits2 <- rep(1:J,each=S)
visits2 <- visits2[as.vector(dataty)==2]

# Bundle data
data <- list(Y1=Y1,visits1=visits1,siteID1=siteID1,
             Y2=Y2,visits2=visits2,siteID2=siteID2,ncap=rowSums(Y2), 
             S = S, J = J)

# Initial values
inits <- function() list(p=runif(1),phi=runif(1),M=rep(3,S),avails=matrix(2,S,J))
# Parameters monitored
params <- c("lambda","phi","p","totalM")
# MCMC settings
ni <- 50000
nt <- 1
nb <- 10000
nc <- 3
na <- 10000

# Running!
out3 <- jags(data, inits, params, "model_mixed-protocols.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb, n.adapt=na,parallel=T)

#* see results
out3
traceplot(out3)

# Get max counts
max.count <- numeric(0)
for(i in 1:dim(Y)[3]){
  max.count[i] <- sum(rowSums(Y[,,i],na.rm=T))
}

# Plot posteriors
par(mfrow=c(2,2),mar=c(4,4,1,1),omi=c(.3,.3,.1,.1))

## phi
hist(out3$sims.list$phi, col = "gray", main = "", xlab = "avaliabilty",breaks=60,freq=F,
     las = 1, xlim = c(0,1))
abline(v = phi, lwd = 3, col = "red")
abline(v=out3$mean$phi,lwd=3,col="blue")
abline(v=c(out3$q2.5$phi,out3$q97.5$phi),lwd=2,col="gray",lty=3)

## p
hist(out3$sims.list$p, col = "gray", main = "", xlab = "perception prob.",breaks=60,freq=F,
     las = 1, xlim = c(0,1))
abline(v = p, lwd = 2, col = "red")
abline(v=out3$mean$p,lwd=3,col="blue")
abline(v=c(out3$q2.5$p,out3$q97.5$p),lwd=2,col="gray",lty=3)

## lambda
hist(out3$sims.list$lambda, col = "gray", main = "", xlab = "local abundance",breaks=60
     ,freq=F,las = 1)
abline(v = lambda, lwd = 3, col = "red")
abline(v=out3$mean$lambda,lwd=3,col="blue")
abline(v=c(out3$q2.5$lambda,out3$q97.5$lambda),lwd=2,col="gray",lty=3)

## total M
hist(out3$sims.list$totalM, col = "gray", main = "", xlab = "total N",breaks=60,freq=F,
     las = 1)
abline(v = sum(M), lwd = 3, col = "red")
abline(v=out3$mean$totalM,lwd=3,col="blue")
abline(v=c(out3$q2.5$totalM,out3$q97.5$totalM),lwd=2,col="gray",lty=3)
abline(v=max(max.count),lwd=3)
legend("topright",c("Real","Estimate","95% CI","Counts"),lty=c(1,1,3,1),lwd=2,col=c("red","blue","gray",1),bty="n",cex=.6)

