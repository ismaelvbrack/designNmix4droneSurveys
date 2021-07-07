
#####################################################################
############~~~~~~~~~~~************************~~~~~~~~~~############
###**~            Function to simulate count data
##    in multiple sites, with multiple visits and 1,2 or 3 observers
##               and analyze with N-mixture models
############~~~~~~~~~~~************************~~~~~~~~~~############
#####################################################################
# Created by Ismael V. Brack - Jul/2021 to support shiny app "simulNmix_example"

simul_Nmix <- function(S,J,obs2,lambda,phi,p){
  #* S: sites
  #* J: visits
  #* obs2: proportion of SxJ with 2 observers.
  ##      0 for 1 observer, 1 for 2 observers and 2 for 3 observers and ]0,1[ for mixed 1 and 2 observers
  #* lambda: local abundance (Poisson dist.)
  #* phi: availability probability (Binomial dist.)
  #* p: perception probability by an observer (Binomial or Multinomial dist.)
  
  if(obs2<0 | (obs2>1 & obs2!=2)){stop("obs2 protocol must be 0 for Binomial N-mix, 1 or 2 for Multinomial N-mix
            and between 0 and 1 for mixed 1 and 2 observer protocols")}
  
  
  #####------------------------------------------
  ##*~ Single observer: Binomial N-mix (obs2==0)
  #####------------------------------------------
  if(obs2==0){
    # Sample abundance from a Poisson distribution
    M <- rpois(n = S, lambda = lambda)
    
    # Array structure to receive simulated data
    Y <- array(dim = c(S,J))
    
    # Loop over visits...
    for(j in 1:J){
      Y[,j] <- rbinom(S, M, (phi*p)) # sample counts for all sites in each visit
    }
    
    #create unmarked frame
    umf <- unmarkedFramePCount(y=Y)
    
    # fitting models... lambda, phi, p
    m0 <- pcount(~1 ~1,data=umf,mixture="P",starts=c(qlogis(phi*p),log(lambda)),K=200+lambda)
    
    # saving results...
    resu <- data.frame(true=c(lambda,(phi*p)),
                       beta=coef(m0),
                       beta.se=SE(m0),
                       estimate=c(backTransform(m0,"state")@estimate,backTransform(m0,"det")@estimate),
                       se=c(sqrt(backTransform(m0,"state")@covMat),sqrt(backTransform(m0,"det")@covMat)),
                       lcl=c(exp(confint(m0,type="state")[1]),plogis(confint(m0,type="det")[1])),
                       ucl=c(exp(confint(m0,type="state")[2]),plogis(confint(m0,type="det")[2]))
    )
    rownames(resu) <- c("lambda","p*")
    
  }# Single Observer
  
  #####--------------------------------------------------------------
  ##*~ Mixed 1 and 2 observers: Binomial/Multinomial N-mix (0<obs2<1)
  #####--------------------------------------------------------------
  if(obs2>0 & obs2<1){
    
    # Sample abundance from a Poisson distribution
    M <- rpois(n = S, lambda = lambda)
    
    # Data type matrix in mixed protocols
    dataty <- matrix(sample(rep(1:2, c( round(S*J*(1-obs2)) , round(S*J*obs2) ))),
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
    
    enc.hist <- t(apply(Y,1,cbind)) # matrix R*(T*4)
    
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
    m0 <- gmultmix(~1,~1,~1,data=umf,mixture="P",starts=c(log(lambda),qlogis(phi),qlogis(p)),K=200+lambda)
    
    # saving results...
    resu <- data.frame(true=c(lambda,phi,p),
                       beta=coef(m0),
                       beta.se=SE(m0),
                       estimate=c(backTransform(m0,"lambda")@estimate,backTransform(m0,"phi")@estimate,backTransform(m0,"det")@estimate),
                       se=c(sqrt(backTransform(m0,"lambda")@covMat),sqrt(backTransform(m0,"phi")@covMat),sqrt(backTransform(m0,"det")@covMat)),
                       lcl=c(exp(confint(m0,type="lambda")[1]),plogis(confint(m0,type="phi")[1]),plogis(confint(m0,type="det")[1])),
                       ucl=c(exp(confint(m0,type="lambda")[2]),plogis(confint(m0,type="phi")[2]),plogis(confint(m0,type="det")[2]))
    )
    rownames(resu) <- c("lambda","phi","p")
    
    rm("doubleObsFun")
  }# Mixed 1 and 2 observers
  
  #####------------------------------------------
  ##*~ 2 observers: Multinomial N-mix (obs2==1)
  #####------------------------------------------
  if(obs2==1){
    
    # Sample abundance from a Poisson distribution
    M <- rpois(n = S, lambda = lambda)
    
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
    
    enc.hist <- t(apply(Y,1,cbind)) # matrix S*(J*4)
    
    # Pi functions for mixed protocols
    doubleObsFun <- function(p){
      p1 <- p[,1]
      p2 <- p[,2]
      cbind("01" = (1-p1) * p2,
            "10" = p1 * (1-p2),
            "11" = p1 * p2)
    }#doubleObs
    assign("doubleObsFun",doubleObsFun,envir=.GlobalEnv)
    o2y <- matrix(1,2,3)
    
    #create unmarked frame
    umf <- unmarkedFrameGMM(y=enc.hist, numPrimary=J,piFun="doubleObsFun",obsToY=o2y)
    
    # fitting models... lambda, phi, p
    m0 <- gmultmix(~1,~1,~1,data=umf,mixture="P",starts=c(log(lambda),qlogis(phi),qlogis(p)),K=200+lambda)
    
    # saving results...
    resu <- data.frame(true=c(lambda,phi,p),
                       beta=coef(m0),
                       beta.se=SE(m0),
                       estimate=c(backTransform(m0,"lambda")@estimate,backTransform(m0,"phi")@estimate,backTransform(m0,"det")@estimate),
                       se=c(sqrt(backTransform(m0,"lambda")@covMat),sqrt(backTransform(m0,"phi")@covMat),sqrt(backTransform(m0,"det")@covMat)),
                       lcl=c(exp(confint(m0,type="lambda")[1]),plogis(confint(m0,type="phi")[1]),plogis(confint(m0,type="det")[1])),
                       ucl=c(exp(confint(m0,type="lambda")[2]),plogis(confint(m0,type="phi")[2]),plogis(confint(m0,type="det")[2]))
    )
    rownames(resu) <- c("lambda","phi","p")
    
    rm("doubleObsFun")
  }# 
  
  #####------------------------------------------
  ##*~ 3 observers: Multinomial N-mix (obs2==2)
  #####------------------------------------------
  if(obs2==2){
    
    # Sample abundance from a Poisson distribution
    M <- rpois(n = S, lambda = lambda)
    
    cellprobs <- c(
      p * p * p,         # 111
      p * p * (1-p),     # 110
      p * (1-p) * p,     # 101
      p * (1-p) * (1-p), # 100
      (1-p) * p * p,     # 011
      (1-p) * p * (1-p), # 010
      (1-p) * (1-p) * p, # 001
      (1-p) * (1-p) * (1-p) # 000
    )
    
    # Create structure to contain available individuals 
    avails <- array(dim = c(S, J))
    
    # Create structure to contain frequencies of capture histories
    Y <- array(dim=c(S,8,J)) #"111","110","101","100","011","010","001","000"
    
    ## Loop over sites and occasions to generate data
    for (i in 1:length(M)){
      avails[i,] <- rbinom(n = J, size = M[i], prob = phi) # Sample available individuals from a Binomial(N, phi) 
      for (j in 1:ncol(avails)){
        Y[i,,j] <- rmultinom(1, avails[i,j], cellprobs)
      }#j
    }#i
    
    Y <- Y[,-8,]  # excluding "000"
    
    enc.hist <- t(apply(Y,1,cbind)) # matrix S*(J*8)
    
    # Pi functions for mixed protocols
    tripleObsFun <- function(p){
      p1 <- p[,1]
      p2 <- p[,2]
      p3 <- p[,3]
      cbind("111" = p1 * p2 * p3,
            "110" = p1 * p2 * (1-p3),
            "101" = p1 * (1-p2) * p3,
            "100" = p1 * (1-p2) * (1-p3),
            "011" = (1-p1) * p2 * p3,
            "010" = (1-p1) * p2 * (1-p3),
            "001" = (1-p1) * (1-p2) * p3)
    }#tripleObs
    assign("tripleObsFun",tripleObsFun,envir=.GlobalEnv)
    o2y <- matrix(1,3,7)
    
    #create unmarked frame
    umf <- unmarkedFrameGMM(y=enc.hist, numPrimary=J,piFun="tripleObsFun",obsToY=o2y)
    
    # fitting models... lambda, phi, p
    m0 <- gmultmix(~1,~1,~1,data=umf,mixture="P",starts=c(log(lambda),qlogis(phi),qlogis(p)),K=200+lambda)
    
    
    # saving results...
    resu <- data.frame(true=c(lambda,phi,p),
                       beta=coef(m0),
                       beta.se=SE(m0),
                       estimate=c(backTransform(m0,"lambda")@estimate,backTransform(m0,"phi")@estimate,backTransform(m0,"det")@estimate),
                       se=c(sqrt(backTransform(m0,"lambda")@covMat),sqrt(backTransform(m0,"phi")@covMat),sqrt(backTransform(m0,"det")@covMat)),
                       lcl=c(exp(confint(m0,type="lambda")[1]),plogis(confint(m0,type="phi")[1]),plogis(confint(m0,type="det")[1])),
                       ucl=c(exp(confint(m0,type="lambda")[2]),plogis(confint(m0,type="phi")[2]),plogis(confint(m0,type="det")[2]))
    )
    rownames(resu) <- c("lambda","phi","p")
    
    rm("tripleObsFun")
  }# Triple Observer
  
  return(resu)
  
}#function  
