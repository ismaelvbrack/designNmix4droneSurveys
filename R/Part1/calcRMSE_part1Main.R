
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


# * Get optimal J based on the lowest RMSE --------------------------------

myOptJ <- getOptJ(rel.rmse, lambda.seq, phi.seq, range=T,equiLim=0.005)

#table1obs <- matrix(paste0(myOptJ$optJ$'0'," (",myOptJ$range$'0',")"),ncol=length(lambda.seq),
#                   dimnames=list(phi.seq,lambda.seq))

#table2obs <- matrix(paste0(myOptJ$optJ$'1'," (",myOptJ$range$'1',")"),ncol=length(lambda.seq),
#                    dimnames=list(phi.seq,lambda.seq))

##* FANCY TABS:
library(flextable)
library(officer)
library(webshot)

#* Single observer
tab1 <- as.data.frame(cbind("Availability",rownames(myOptJ$optJ$`0`),myOptJ$optJ$`0`))
rownames(tab1) <- NULL
colnames(tab1)[1:2] <- c(" ","`")
tab1[,3:10] <- sapply(tab1[,3:10],as.numeric)

colors1 <- col_numeric(palette=c("mistyrose","red3"),domain=range(myOptJ$optJ$`0`))

table1obs <- flextable(tab1) %>%  merge_v(part="body",j=1) %>% 
  bg(part="body",bg=colors1,j=3:10) %>% 
  colformat_double(i=1:8,j=3,suffix=paste0(" (",myOptJ$range$`0`[,1],")"),digits=0) %>%
  colformat_double(i=1:8,j=4,suffix=paste0(" (",myOptJ$range$`0`[,2],")"),digits=0) %>% 
  colformat_double(i=1:8,j=5,suffix=paste0(" (",myOptJ$range$`0`[,3],")"),digits=0) %>% 
  colformat_double(i=1:8,j=6,suffix=paste0(" (",myOptJ$range$`0`[,4],")"),digits=0) %>% 
  colformat_double(i=1:8,j=7,suffix=paste0(" (",myOptJ$range$`0`[,5],")"),digits=0) %>% 
  colformat_double(i=1:8,j=8,suffix=paste0(" (",myOptJ$range$`0`[,6],")"),digits=0) %>% 
  colformat_double(i=1:8,j=9,suffix=paste0(" (",myOptJ$range$`0`[,7],")"),digits=0) %>%
  colformat_double(i=1:8,j=10,suffix=paste0(" (",myOptJ$range$`0`[,8],")"),digits=0) %>% 
  add_header_row(values=c("","Local abundance"),colwidths=c(2,8)) %>% 
  align(i=1:2,part="header",align="center") %>% border_remove() %>% 
  hline(i=1,j=3:10,part="header",border=fp_border(color="gray70",width=2)) %>% 
  hline(i=2,j=1:10,part="header",border=fp_border(color="gray70",width=2)) %>%
  hline(i=8,part="body",border=fp_border(color="gray70",width=2)) %>% 
  vline(j=1,part="body",border=fp_border(color="gray70",width=1.5)) %>%
  width(width=3.2,j=3:10) %>% 
  fix_border_issues() #%>% autofit()

#* Double observers
tab2 <- as.data.frame(cbind("Availability",rownames(myOptJ$optJ$`1`),myOptJ$optJ$`1`))
rownames(tab2) <- NULL
colnames(tab2)[1:2] <- c(" ","`")
tab2[,3:10] <- sapply(tab2[,3:10],as.numeric)

colors2 <- col_numeric(palette=c("lightblue","royalblue4"),domain=range(myOptJ$optJ$`1`))

table2obs <- flextable(tab2) %>%  merge_v(part="body",j=1) %>% 
  bg(part="body",bg=colors2,j=3:10) %>% 
  colformat_double(i=1:8,j=3,suffix=paste0(" (",myOptJ$range$`1`[,1],")"),digits=0) %>%
  colformat_double(i=1:8,j=4,suffix=paste0(" (",myOptJ$range$`1`[,2],")"),digits=0) %>% 
  colformat_double(i=1:8,j=5,suffix=paste0(" (",myOptJ$range$`1`[,3],")"),digits=0) %>% 
  colformat_double(i=1:8,j=6,suffix=paste0(" (",myOptJ$range$`1`[,4],")"),digits=0) %>% 
  colformat_double(i=1:8,j=7,suffix=paste0(" (",myOptJ$range$`1`[,5],")"),digits=0) %>% 
  colformat_double(i=1:8,j=8,suffix=paste0(" (",myOptJ$range$`1`[,6],")"),digits=0) %>% 
  colformat_double(i=1:8,j=9,suffix=paste0(" (",myOptJ$range$`1`[,7],")"),digits=0) %>%
  colformat_double(i=1:8,j=10,suffix=paste0(" (",myOptJ$range$`1`[,8],")"),digits=0) %>% 
  add_header_row(values=c("","Local abundance"),colwidths=c(2,8)) %>% 
  align(i=1:2,part="header",align="center") %>% border_remove() %>% 
  hline(i=1,j=3:10,part="header",border=fp_border(color="gray70",width=2)) %>% 
  hline(i=2,j=1:10,part="header",border=fp_border(color="gray70",width=2)) %>%
  hline(i=8,part="body",border=fp_border(color="gray70",width=2)) %>% 
  vline(j=1,part="body",border=fp_border(color="gray70",width=1.5)) %>%
  width(width=3.2,j=3:10) %>% 
  fix_border_issues() #%>% autofit()

#* Export as figure
ggsave(here::here("outputs","tables","optimal_Js.png"),width=20,height=12,units="cm",dpi=600,
       plot=grid.arrange(ggplot() + labs(title="  Single observer") + theme_void() + theme(title=element_text(size=12,color="gray40",face="italic")) +
                           annotation_custom(rasterGrob(as_raster(table1obs)),xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
                         ggplot() + labs(title="  Double observer") + theme_void() + theme(title=element_text(size=12,color="gray40",face="italic")) +
                           annotation_custom(rasterGrob(as_raster(table2obs)),xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
)

