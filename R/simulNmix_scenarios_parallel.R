
######################################################
#####---------------------------------------------####
##*~ Simulating data and fitting N-mixture models ~*##
#####----  for many scenarios in parallel  -------####
######################################################
#* using maximum likelihood approach with package unmarked

library(foreach)
library(doParallel)

# get function to create and fit data given a set of parameters and sample design
source(here::here("R","func_simul_Nmix.R"))

# examples... testing function
simul_Nmix(S=50,J=4,obs2=.7)

simul_Nmix(S=69,J=29,obs2=0,lambda=.1,phi=.1,p=.8)

###-------------------------###
##*    Defining scenarios
###-------------------------###
# availability of individuals
phi.seq <- 0.14 #seq(0.1,0.8,0.1)

# local abundance per site
lambda.seq <- 0.33 #c(0.1,0.2,0.3,0.5,1,2,4,8)

scenarios <- data.frame(lambda=rep(lambda.seq,length(phi.seq)),phi=rep(phi.seq,each=length(lambda.seq)))

# proportion of double obsevers
obs2.seq <- c(0,0.2,0.5,1) #obs2.seq <- c(0.1,0.2,0.4,0.6,0.8)

# total effort = budget
effort <- 812 #2000 # sites x visits

# number of iterations
simul <- 2000

# number of maximum visits per scenario
scenarios$Jmax <- c(30,rep(28,7),rep(20,16),rep(10,40)) #rep(c(16,8), c(2,4)) or  rep(c(23,15,15),each=3)

#* Object to contains final results
resu <- list()

##** Running in parallel
registerDoParallel(makeCluster(8))

###-------------------------###
##*   Running simulations
###-------------------------###
scen = 0
#for(fi in seq_along(phi.seq)){
#for(la in seq_along(lambda.seq)){

for(i in 1:nrow(scenarios)){    
  temp.out <- list() # object to contain all iterations of one scenario
  j.seq <- 4:26 #2:(scenarios$Jmax[i])
  for(j in seq_along(j.seq)){
    
    S <- round(effort/j.seq[j]) # number of sites is effort / J
    temp.mid <- list() # object to contain iterations for one J in each scenario
    for(ob in seq_along(obs2.seq)){
      cat("\n Simulating...", "obs2:",obs2.seq[ob],"; lambda:",scenarios$lambda[i],"; phi:",scenarios$phi[i],"; J:",j.seq[j])
      cat("\n       ",as.character(Sys.time()))
      
      temp.inn <- list() # object to contain iterations for one model-type (dbl or sgl observer) in each J in each scenario
      
      # parallel running of iterations...
      temp.inn <- foreach(w=1:simul, .errorhandling = 'remove') %dopar% {
        simul_Nmix(S=S,J=j.seq[j],obs2=obs2.seq[ob],lambda=scenarios$lambda[i],phi=scenarios$phi[i],p=.8)
      } #parallel simuls
      
      # transform list of iterations to data.frame
      temp.inn <- do.call(rbind,temp.inn)
      temp.inn$lambda <- scenarios$lambda[i]
      temp.inn$phi <- scenarios$phi[i]
      temp.inn$J <- j.seq[j]
      temp.inn$obs2 <- obs2.seq[ob]
      temp.mid[[ob]] <- temp.inn
    }#obs2
    temp.mid <- do.call(rbind,temp.mid) # join model-types into one data.frame
    temp.out[[j]] <- temp.mid
  }#j.seq
  temp.out <- do.call(rbind,temp.out) # join all J into one data.frame
  
  scen = scen+1
  resu[[scen]] <- temp.out # final results data.frame for the scenario
  names(resu)[scen] <- paste0(scenarios$lambda[i],"_",scenarios$phi[i]) # name!
  #}#lambda
  #}#phi
  #saveRDS(resu,here::here("outputs","temp-resu.rds"))
}# scenarios loop


saveRDS(resu,here::here("data","raw_simul_resu","resu2_caseEx_MarshDeer.rds"))


