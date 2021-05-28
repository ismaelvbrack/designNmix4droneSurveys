
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE and get optimal number of visits (J) per scenario
##*  6 scenarios of local abundance and availability 
##*  and different proportions of double obs. protocol
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# scens 1-6 props
results <- readRDS(here::here("data","processed_simul_resu","resultsP2_propObs.rds"))

# scenarios
lambda.seq <- c(0.2,1) # local abundance
phi.seq <- c(0.2,0.4,0.6) # availability

# * Calculate RMSE --------------------------------------------------------

#* Root mean square error for each scenario and J
rmse <- lapply(results, function(x) tapply(x$error, list(x$J,x$obs2), function(y) sqrt(mean(y^2)) ))

# exclude differences in J's between 0%,100% (from part1) and the others (part2)
rmse[[1]] <- rmse[[1]][1:15,]
rmse[[2]] <- rmse[[2]][1:15,]


#* calculate relative RMSE
rel.rmse <- list()
for(i in 1:length(rmse)){
  rel.rmse[[i]] <- rmse[[i]] / as.numeric(strsplit(names(rmse)[[i]],"_")[[1]][1])
}
names(rel.rmse) <- names(rmse)

#* data.frame type object
rel.rmse2 <- rel.rmse
for(i in 1:length(rel.rmse)){
  rel.rmse2[[i]] <- as.data.frame(as.table(rel.rmse[[i]]))
  names(rel.rmse2[[i]]) <- c("J","obs2","RMSE")
}

# * Get optimal J based on the lowest RMSE --------------------------------
myOptJ <- getOptJ(rel.rmse, lambda.seq, phi.seq, range=T,equiLim=0.005)


