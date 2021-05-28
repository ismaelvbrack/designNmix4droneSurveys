
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE per scenarios with reduced budget for 2obs model
##*  6 scenarios of local abundance and availability
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run script 'calcRMSE_part1.R' to get optimal Js


# * Defining scenarios ----------------------------------------------------

phi.seq <- c(0.2,0.4,0.6)  # seq(0.1,0.8,0.1)
lambda.seq <- c(0.2,1)
effort <- 2000 # sites x visits

scenarios <- data.frame(lambda=rep(lambda.seq,length(phi.seq)),phi=rep(phi.seq,each=length(lambda.seq)))

scenarios$optJ1 <- as.vector(t(myOptJ$optJ$`0`[row.names(myOptJ$optJ$`0`) %in% scenarios$phi,
                                               colnames(myOptJ$optJ$`0`) %in% scenarios$lambda]))
scenarios$optJ2 <- as.vector(t(myOptJ$optJ$`1`[row.names(myOptJ$optJ$`1`) %in% scenarios$phi,
                                               colnames(myOptJ$optJ$`1`) %in% scenarios$lambda]))
scenarios$RMSE1ref <- sapply(rel.rmse[paste0(scenarios$lambda,"_",scenarios$phi)],function(x) min(x[,"0"]))
scenarios$RMSE2ref <- sapply(rel.rmse[paste0(scenarios$lambda,"_",scenarios$phi)],function(x) min(x[,"1"]))
scenarios$Sref <- round(effort/scenarios$optJ2)

# clean workspace
rm(list=setdiff(ls(),"scenarios"))

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# scens 1-64
results <- readRDS(here::here("data","processed_simul_resu","resultsP3_budgets.rds"))

# * Calculate RMSE --------------------------------------------------------

#* Root mean square error for each scenario and S
rmse <- lapply(results, function(x) tapply(x$error, list(x$S), function(y) sqrt(mean(y^2)) ))

#* calculate relative RMSE
rel.rmse <- list()
for(i in 1:length(rmse)){
  rel.rmse[[i]] <- rmse[[i]] / as.numeric(strsplit(names(rmse)[[i]],"_")[[1]][1])
}
names(rel.rmse) <- names(rmse)

#* data.frame type object
rel.rmse2 <- rel.rmse
for(i in 1:length(rel.rmse)){
  rel.rmse2[[i]] <- data.frame(S=as.numeric(c(scenarios[i,"Sref"],names(rel.rmse[[i]]))),
                               RMSE2=as.numeric(c(scenarios[i,"RMSE2ref"],rel.rmse[[i]])),
                               RMSE1ref=scenarios[i,"RMSE1ref"],
                               budget=as.numeric(c(scenarios[i,"Sref"],names(rel.rmse[[i]])))*scenarios[i,"optJ2"]
                               #scenario=paste0(expression(lambda),"=",strsplit(names(rmse)[i],"_")[[1]][1],";",
                               #               expression(phi),"=",strsplit(names(rmse)[i],"_")[[1]][2])
  )
}

lapply(rel.rmse2, function(x) rbind(x[1,],
                                    x[which.min(abs(x$RMSE2 - x$RMSE1ref)),]))

lapply(rel.rmse2, function(x) x[which.min(abs(x$RMSE2 - x$RMSE1ref)),"budget"]/
         x[1,"budget"])
