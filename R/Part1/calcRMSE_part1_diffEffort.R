
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE and get optimal number of visits (J) per scenario
##*  6 scenarios with different total effort (budget) = 4k 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# load results
results4k <- readRDS(here::here("data","processed_simul_resu","resultsP1_effort4k.rds"))

# scenarios
phi.seq <- c(0.2,0.4,0.6) # availability
lambda.seq <- c(0.2,1)   # local abundance

# * Calculate RMSE --------------------------------------------------------

#* Root mean square error for each scenario and J
rmse4k <- lapply(results4k, function(x) tapply(x$error, list(x$J,x$obs2), function(y) sqrt(mean(y^2)) ))

#* calculate relative RMSE
rel.rmse4k <- list()
for(i in 1:length(rmse4k)){
  rel.rmse4k[[i]] <- rmse4k[[i]] / as.numeric(strsplit(names(rmse4k)[[i]],"_")[[1]][1])
}
names(rel.rmse4k) <- names(rmse4k)

#* data.frame type object
rel.rmse4k.2 <- rel.rmse4k
for(i in 1:length(rel.rmse4k)){
  rel.rmse4k.2[[i]] <- as.data.frame(as.table(rel.rmse4k[[i]]))
  names(rel.rmse4k.2[[i]]) <- c("J","obs2","RMSE")
}

# * Get optimal J based on the lowest RMSE --------------------------------
myOptJ3 <- getOptJ(rel.rmse4k, lambda.seq, phi.seq, range=T,equiLim=0.005)


