
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE and get optimal number of visits (J) per scenario
##*  1-73 scenarios of local abundance and availability
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# scens 1-64
results <- readRDS(here::here("data","processed_simul_resu","resultsP1_scen1-64.rds"))

# scens 65-73
#results2 <- readRDS(here::here("data","processed_simul_resu","resultsP1_scen65-73.rds"))

# scenarios
phi.seq <- seq(0.1,0.8,0.1) # availability
lambda.seq <- c(0.1,0.2,0.3,0.5,1,2,4,8) # local abundance

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

myOptJ <- getOptJ(rel.rmse, lambda.seq, phi.seq, range=T,equiLim=0.005)

table1obs <- matrix(paste0(myOptJ$optJ$'0'," (",myOptJ$range$'0',")"),ncol=length(lambda.seq),
                    dimnames=list(phi.seq,lambda.seq))

table2obs <- matrix(paste0(myOptJ$optJ$'1'," (",myOptJ$range$'1',")"),ncol=length(lambda.seq),
                    dimnames=list(phi.seq,lambda.seq))

library(gt)


tab1 <- gt(as.data.frame(table1obs),rownames_to_stub=T)
tab1 %>% tab_spanner(label="Local abundance",columns=1:8) %>% 
  data_color(columns=2:9,colors=scales::col_factor(palette=c("mistyrose","red3"),domain=NULL,
                                                   levels=levels(as.factor(table1obs))[order(as.numeric(substr(levels(as.factor(table1obs)),1,2)))]))
### como adicionar phi?
