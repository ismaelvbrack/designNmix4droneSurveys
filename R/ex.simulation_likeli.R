
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Simulate spatiotemporally replicated count data (e.g. in drone images)
##*   and analyze with N-mixture model using unmarked (maximum likelihood estimation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* Examples with different protocols in image review:
#    (1)single observer counts
#    (2)independent double observers mark
#    (3)mixed single and double protocols
#* Only constant models for illustration

library(unmarked)


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
#create unmarked frame
umf <- unmarkedFramePCount(y=Y)

# fitting models... lambda, phi, p
m0 <- pcount(~1 ~1,data=umf,mixture="P",K=200)

# see results
data.frame(real=c(lambda,(phi*p)),
           estimate=c(backTransform(m0,"state")@estimate,backTransform(m0,"det")@estimate),
           se=c(sqrt(backTransform(m0,"state")@covMat),sqrt(backTransform(m0,"det")@covMat)),
           lcl=c(exp(confint(m0,type="state")[1]),plogis(confint(m0,type="det")[1])),
           ucl=c(exp(confint(m0,type="state")[2]),plogis(confint(m0,type="det")[2]))
)


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

enc.hist <- t(apply(Y,1,cbind)) # matrix S*(J*3)

# Pi functions
doubleObsFun <- function(p){
  p1 <- p[,1]
  p2 <- p[,2]
  cbind("01" = (1-p1) * p2,
        "10" = p1 * (1-p2),
        "11" = p1 * p2)
}#doubleObs

o2y <- matrix(1,2,3)

#create unmarked frame
umf <- unmarkedFrameGMM(y=enc.hist, numPrimary=J,piFun="doubleObsFun",obsToY=o2y)

# fitting models... lambda, phi, p
m0 <- gmultmix(~1,~1,~1,data=umf,mixture="P",K=200)

# see results
data.frame(real=c(lambda,phi,p),
           estimate=c(backTransform(m0,"lambda")@estimate,backTransform(m0,"phi")@estimate,backTransform(m0,"det")@estimate),
           se=c(sqrt(backTransform(m0,"lambda")@covMat),sqrt(backTransform(m0,"phi")@covMat),sqrt(backTransform(m0,"det")@covMat)),
           lcl=c(exp(confint(m0,type="lambda")[1]),plogis(confint(m0,type="phi")[1]),plogis(confint(m0,type="det")[1])),
           ucl=c(exp(confint(m0,type="lambda")[2]),plogis(confint(m0,type="phi")[2]),plogis(confint(m0,type="det")[2]))
)


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
enc.hist <- t(apply(Y,1,cbind)) # matrix S*(J*4)

# Pi functions for mixed protocols
doubleObsFun <- function(p){
  p1 <- p[,1]
  p2 <- p[,2]
  cbind("01" = (1-p1) * p2,
        "10" = p1 * (1-p2),
        "11" = p1 * p2,
        "1x" = p1)
}#doubleObs
assign("doubleObsFun",doubleObsFun,envir=.GlobalEnv)
o2y <- matrix(1,2,4)

#create unmarked frame
umf <- unmarkedFrameGMM(y=enc.hist, numPrimary=J,piFun="doubleObsFun",obsToY=o2y)

# fitting models... lambda, phi, p
m0 <- gmultmix(~1,~1,~1,data=umf,mixture="P",K=200)

# saving results...
data.frame(real=c(lambda,phi,p),
                   estimate=c(backTransform(m0,"lambda")@estimate,backTransform(m0,"phi")@estimate,backTransform(m0,"det")@estimate),
                   se=c(sqrt(backTransform(m0,"lambda")@covMat),sqrt(backTransform(m0,"phi")@covMat),sqrt(backTransform(m0,"det")@covMat)),
                   lcl=c(exp(confint(m0,type="lambda")[1]),plogis(confint(m0,type="phi")[1]),plogis(confint(m0,type="det")[1])),
                   ucl=c(exp(confint(m0,type="lambda")[2]),plogis(confint(m0,type="phi")[2]),plogis(confint(m0,type="det")[2]))
)
