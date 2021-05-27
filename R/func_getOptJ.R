#-----------------------------------------------------
####** function to extract optimal J and range for each scenario

getOptJ <- function(data, lambda.seq, phi.seq, range=TRUE){
  if(length(lambda.seq)*length(phi.seq) != length(data)){
    stop("Number of scenarios in data differs from lambda and phi values")
  }
  
  obs <- colnames(data[[1]])
  
  tabs <- list()
  for(ob in 1:length(obs)){
    tabs[[ob]] <- matrix(as.numeric(sapply(data, 
                                           function(x) names(which(x[,obs[ob]]==min(x[,obs[ob]])))
    )),
    ncol=length(lambda.seq), byrow=T,dimnames=list(phi.seq,lambda.seq))
  }
  
  names(tabs) <- obs
  
  if(range==TRUE){
    tabs2 <- list()
    for(ob in 1:length(obs)){
      tabs[[ob]] <-   matrix(sapply(data, function(x)
        paste(range(as.numeric(names(which(x[,obs[ob]]<(min(x[,obs[ob]])+equiLim))
        ))), collapse = "-")),
        ncol=length(lambda.seq),byrow=T,dimnames=list(phi.seq,lambda.seq))
    }
    
    names(tabs2) <- obs
    
    tabs <- list(optJ=tabs, range=tabs2)
  } #range=T
  
  return(tabs)
}




