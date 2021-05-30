
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE and get optimal number of visits (J) per scenario
##*  65-73 scenarios of high local abundance 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# scens 65-73
results <- readRDS(here::here("data","processed_simul_resu","resultsP1_scen65-73.rds"))

# scenarios
phi.seq <- c(0.1,0.2,0.5) # availability
lambda.seq <- c(12,20,40) # local abundance

# * Calculate RMSE --------------------------------------------------------

#* Root mean square error for each scenario and J
rmse <- lapply(results, function(x) tapply(x$error, list(x$J,x$obs2), function(y) sqrt(mean(y^2)) ))

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

myOptJ2 <- getOptJ(rel.rmse, lambda.seq, phi.seq, range=T,equiLim=0.005)
