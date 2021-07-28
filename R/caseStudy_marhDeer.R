


#  clean and extract simulations... ---------------------------------------

#* read raw results data
raw.resu <- readRDS(here::here("data","raw_simul_resu","resu2_caseEx_MarshDeer.rds"))[[1]]

# get only local abundance (lambda) estimates
results <- raw.resu[grep(rownames(raw.resu),pattern="lambda"),]

# exclude marginal and NaN estimates 
exclude <- rownames(raw.resu[c(which(substr(rownames(raw.resu), 1,2)=="p*" & round(raw.resu$ucl,5)==1)-1,
which(substr(rownames(raw.resu), 1,3)=="phi" & round(raw.resu$ucl,5)==1)-1,
which(substr(rownames(raw.resu), 1,1)=="p" & substr(rownames(raw.resu), 1,2)!="p*" &
                 substr(rownames(raw.resu), 1,3)!="phi" & round(raw.resu$ucl,5)==1)-2,
which(substr(rownames(raw.resu), 1,2)=="la" & is.na(raw.resu$se))),])

results <-results[-which(rownames(results) %in% exclude),]

results <- results[-which(results$estimate>40 | results$ucl>200),]

# calculate error
results$error <- results$estimate - results$lambda

# number of excluded iterations
table(results$obs2,results$J)


# Calculate relative RMSE ----------------------------------------

rel.rmse <- tapply(results$error, list(results$J,results$obs2), function(y) sqrt(mean(y^2))) / results$lambda[1]

rel.rmse2 <- as.data.frame(as.table(rel.rmse))
names(rel.rmse2) <- c("J","obs2","RMSE")


# RMSE curves and optimal J -----------------------------------------------
library(ggplot2)
library(wesanderson)
library(ggpubr)

figRMSE <- 
  ggplot(data=rel.rmse2,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
  geom_line(size=1.6) + scale_x_continuous(breaks=4:26) +
  #coord_cartesian(ylim=yax) +
  theme_classic() + scale_color_manual(values=wes_palette("Zissou1",length(unique(rel.rmse2$obs2)),type="continuous")) + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=16),legend.position="top") +
  labs(y="relative RMSE",x="Number of visits (J)",color=" Prop. of \nDouble Obs.")


tab <- data.frame(optJ=tapply(rel.rmse2$RMSE, rel.rmse2$obs2,
                              function(x) as.numeric(as.character(rel.rmse2[rel.rmse2$RMSE==min(x),"J"]))),
                  Jmin=tapply(rel.rmse2$RMSE, rel.rmse2$obs2,
                              function(x) range(as.numeric(as.character(rel.rmse2[which(x<min(x)+0.005),"J"])))[1]),
                  Jmax=tapply(rel.rmse2$RMSE, rel.rmse2$obs2,
                              function(x) range(as.numeric(as.character(rel.rmse2[which(x<min(x)+0.005),"J"])))[2]),
                  obs2=unique(rel.rmse2$obs2),
                  RMSE=tapply(rel.rmse2$RMSE, rel.rmse2$obs2,min))

figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax)) + 
  geom_point(size=5,shape=18) + geom_errorbarh(size=1.2,height=0.5) +
  theme_classic() + xlim(range(as.numeric(as.character(rel.rmse2$J)))) +
  labs(x="Optimal J") + scale_y_discrete(limits=rev) +
  theme(panel.background = element_rect(colour = "black"),legend.position="none",
        axis.text=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(face="bold",size=12)) + 
  scale_color_manual(values=wes_palette("Zissou1",length(unique(rel.rmse2$obs2)),type="continuous"))


figRMSE <- ggarrange(figRMSE,figJs,nrow=2,align="v",heights=c(0.8,0.2))


ggsave(here::here("outputs","figs","figRMSE_caseEx.png"),figRMSE,width=15,height=15,units="cm")

### exploring some results...
tab

t(apply(rel.rmse, 1, function(x) x[2:4] / x[1]))
  # reduction in performance for 20% of 2obs is generally very small! (p is high)

tab$RMSE[4] / tab$RMSE[1] #~11% reduction with full 2obs in optimal J (even with this high p)


resu <- matrix(NA, ncol=ncol(rel.rmse),nrow=nrow(rel.rmse))
for(i in 1:ncol(rel.rmse)){
  resu[,i] <-   rel.rmse[,i] / rel.rmse[1,i]
}

1- round(apply(resu, 2, min),dig=3) # 62-64% (one third) reduction for survey effort optimally allocated

