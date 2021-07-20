
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##* Calculate RMSE and get optimal number of visits (J) per scenario
##*  1-64 scenarios of local abundance and availability
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("R","func_getOptJ.R"))

# * Importing simulations results -----------------------------------------

# scens 1-64
results <- readRDS(here::here("data","processed_simul_resu","resultsP1_scen1-64.rds"))

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

# join with "high abundances" analysis
  # rmseDFs <- rel.rmse2

  # rmseDFs <- c(rmseDFs,rel.rmse2)

  # save object to be used in the shiny app
  # saveRDS(rmseDFs, here::here("apps","simulNmix_results","rel.rmse.rds"))

# * Get optimal J based on the lowest RMSE --------------------------------

myOptJ <- getOptJ(rel.rmse, lambda.seq, phi.seq, range=T,equiLim=0.005)

myOptJ2 # from "highabund"

#table1obs <- matrix(paste0(myOptJ$optJ$'0'," (",myOptJ$range$'0',")"),ncol=length(lambda.seq),
#                   dimnames=list(phi.seq,lambda.seq))

#table2obs <- matrix(paste0(myOptJ$optJ$'1'," (",myOptJ$range$'1',")"),ncol=length(lambda.seq),
#                    dimnames=list(phi.seq,lambda.seq))

##* FANCY TABS:
library(flextable)
library(scales)
library(officer)
library(webshot)
library(ggplot2)
library(grid)
library(gridExtra)

#* Single observer
tab1 <- as.data.frame(cbind("Availability",merge(myOptJ$optJ$`0`,myOptJ2$optJ$`0`,by=0,all=T)))
rownames(tab1) <- NULL
colnames(tab1)[1:2] <- c(" ","`")
tab1[,3:13] <- sapply(tab1[,3:13],as.numeric)

colors1 <- col_numeric(palette=c("lightblue","royalblue4"),domain=range(myOptJ$optJ$`0`),na.color="white")

table1obs <- flextable(tab1) %>%  merge_v(part="body",j=1) %>%
  #set_formatter_type(na_str="-") %>% 
  bg(part="body",bg=colors1,j=3:13) %>%
  
  colformat_double(i=1:8,j=3,suffix=paste0(" (",myOptJ$range$`0`[,1],")"),digits=0) %>%
  colformat_double(i=1:8,j=4,suffix=paste0(" (",myOptJ$range$`0`[,2],")"),digits=0) %>% 
  colformat_double(i=1:8,j=5,suffix=paste0(" (",myOptJ$range$`0`[,3],")"),digits=0) %>% 
  colformat_double(i=1:8,j=6,suffix=paste0(" (",myOptJ$range$`0`[,4],")"),digits=0) %>% 
  colformat_double(i=1:8,j=7,suffix=paste0(" (",myOptJ$range$`0`[,5],")"),digits=0) %>% 
  colformat_double(i=1:8,j=8,suffix=paste0(" (",myOptJ$range$`0`[,6],")"),digits=0) %>% 
  colformat_double(i=1:8,j=9,suffix=paste0(" (",myOptJ$range$`0`[,7],")"),digits=0) %>%
  colformat_double(i=1:8,j=10,suffix=paste0(" (",myOptJ$range$`0`[,8],")"),digits=0) %>%
  colformat_double(i=1:8,j=8,suffix=paste0(" (",myOptJ$range$`0`[,6],")"),digits=0) %>% 
  
  colformat_double(i=c(1,2,5),j=11,suffix=paste0(" (",myOptJ2$range$`0`[,1],")"),digits=0) %>%
  colformat_double(i=c(1,2,5),j=12,suffix=paste0(" (",myOptJ2$range$`0`[,2],")"),digits=0) %>%
  colformat_double(i=c(1,2,5),j=13,suffix=paste0(" (",myOptJ2$range$`0`[,3],")"),digits=0) %>%
  
  fontsize(j=3:13,part="body",size=9) %>% 
  add_header_row(values=c("","Local abundance"),colwidths=c(2,11)) %>% 
  align(i=1:2,part="header",align="center") %>% border_remove() %>% 
  hline(i=1,j=3:13,part="header",border=fp_border(color="gray70",width=2)) %>% 
  hline(i=2,j=1:13,part="header",border=fp_border(color="gray70",width=2)) %>%
  hline(i=8,part="body",border=fp_border(color="gray70",width=2)) %>% 
  vline(j=1,part="body",border=fp_border(color="gray70",width=1.5)) %>%
  width(width=5,j=3:13) %>% 
  fix_border_issues() #%>% autofit()

 # saveRDS(table1obs,here::here("apps","simulNmix_results","table1obs.rds"))

#* Double observers
tab2 <- as.data.frame(cbind("Availability",merge(myOptJ$optJ$`1`,myOptJ2$optJ$`1`,by=0,all=T)))
rownames(tab2) <- NULL
colnames(tab2)[1:2] <- c(" ","`")
tab2[,3:13] <- sapply(tab2[,3:13],as.numeric)

colors2 <- col_numeric(palette=c("mistyrose","red3"),domain=range(myOptJ$optJ$`1`),na.color="white")

table2obs <- flextable(tab2) %>%  merge_v(part="body",j=1) %>% 
  bg(part="body",bg=colors2,j=3:13) %>% 
  
  colformat_double(i=1:8,j=3,suffix=paste0(" (",myOptJ$range$`1`[,1],")"),digits=0) %>%
  colformat_double(i=1:8,j=4,suffix=paste0(" (",myOptJ$range$`1`[,2],")"),digits=0) %>% 
  colformat_double(i=1:8,j=5,suffix=paste0(" (",myOptJ$range$`1`[,3],")"),digits=0) %>% 
  colformat_double(i=1:8,j=6,suffix=paste0(" (",myOptJ$range$`1`[,4],")"),digits=0) %>% 
  colformat_double(i=1:8,j=7,suffix=paste0(" (",myOptJ$range$`1`[,5],")"),digits=0) %>% 
  colformat_double(i=1:8,j=8,suffix=paste0(" (",myOptJ$range$`1`[,6],")"),digits=0) %>% 
  colformat_double(i=1:8,j=9,suffix=paste0(" (",myOptJ$range$`1`[,7],")"),digits=0) %>%
  colformat_double(i=1:8,j=10,suffix=paste0(" (",myOptJ$range$`1`[,8],")"),digits=0) %>% 
  
  colformat_double(i=c(1,2,5),j=11,suffix=paste0(" (",myOptJ2$range$`1`[,1],")"),digits=0) %>%
  colformat_double(i=c(1,2,5),j=12,suffix=paste0(" (",myOptJ2$range$`1`[,2],")"),digits=0) %>%
  colformat_double(i=c(1,2,5),j=13,suffix=paste0(" (",myOptJ2$range$`1`[,3],")"),digits=0) %>%
  
  fontsize(j=3:13,part="body",size=9) %>% 
  add_header_row(values=c("","Local abundance"),colwidths=c(2,11)) %>% 
  align(i=1:2,part="header",align="center") %>% border_remove() %>% 
  hline(i=1,j=3:13,part="header",border=fp_border(color="gray70",width=2)) %>% 
  hline(i=2,j=1:13,part="header",border=fp_border(color="gray70",width=2)) %>%
  hline(i=8,part="body",border=fp_border(color="gray70",width=2)) %>% 
  vline(j=1,part="body",border=fp_border(color="gray70",width=1.5)) %>%
  width(width=3.8,j=3:13) %>% 
  fix_border_issues() #%>% autofit()

  # saveRDS(table2obs,here::here("apps","simulNmix_results","table2obs.rds"))

#* Export as figure
ggsave(here::here("outputs","tables","optimal_Js.png"),width=22,height=12,units="cm",dpi=600,
       plot=grid.arrange(ggplot() + labs(title="  Single observer") + theme_void() + theme(title=element_text(size=12,color="gray40",face="italic")) +
                           annotation_custom(rasterGrob(as_raster(table1obs)),xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
                         ggplot() + labs(title="  Double observer") + theme_void() + theme(title=element_text(size=12,color="gray40",face="italic")) +
                           annotation_custom(rasterGrob(as_raster(table2obs)),xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
)

ggplot() + labs(title="  Single observer") + theme_void() + theme(title=element_text(size=12,color="gray40",face="italic")) +
  annotation_custom(rasterGrob(as_raster(table1obs)),xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
