
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Extract and clean simulation results part 1
##*  6 scenarios with a higher total effor (budget=4k)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* read raw results data
raw.resu <- readRDS(here::here("data","raw_simul_resu","resuPart1_effort4k.rds"))

# scenarios
phi.seq <- c(0.2,0.4,0.6) # availability
lambda.seq <- c(0.2,1)  # local abundance

# get only local abundance (lambda) estimates
results <- lapply(raw.resu, function(x) x[grep(rownames(x), pattern="lambda"),])

# create a list with the rownames of the estimates to be excluded
# - marginal estimates: p*, phi, p <0.001 or =1
# - SE(lambda) == NaN
exclude <- lapply(raw.resu, function(x)
  rownames(rbind(x[which(substr(rownames(x), 1,2)=="p*" & round(x$ucl,5)==1)-1,],
                 x[which(substr(rownames(x), 1,3)=="phi" & round(x$ucl,5)==1)-1,],
                 x[which(substr(rownames(x), 1,1)=="p" & substr(rownames(x), 1,2)!="p*" &
                           substr(rownames(x), 1,3)!="phi" & round(x$ucl,5)==1)-2,],
                 x[which(substr(rownames(x), 1,2)=="la" & is.na(x$se)),]
  )))

# exclude marginal and NaN estimates for each scenario and exclude huge estimates (lambda<40 and SE(lambda)<100)
for(i in 1:length(results)){
  if(length(exclude[[i]])>0){
    results[[i]] <- results[[i]][-which(rownames(results[[i]]) %in% exclude[[i]]),]
  }
  if(any(results[[i]]$estimate>40 | results[[i]]$ucl>200)){
    results[[i]] <- results[[i]][-which(results[[i]]$estimate>40 | results[[i]]$ucl>200),]
  }
  
  #for(i in 1:length(results)){
  # calculate raw error for each iteration
  results[[i]]$error <- results[[i]]$estimate - results[[i]]$lambda
}

saveRDS(results, here::here("data","processed_simul_resu","resultsP1_effort4k.rds"))
