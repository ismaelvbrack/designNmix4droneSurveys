


# * Importing simulations results -----------------------------------------

# scens 1-64
results <- readRDS(here::here("data","processed_simul_resu","resultsP1_scen1-64.rds"))

# scenarios
phi.seq <- seq(0.1,0.8,0.1) # availability
lambda.seq <- c(0.1,0.2,0.3,0.5,1,2,4,8) # local abundance


# Figures ------------------------------------------------------------------
library(ggplot2)
library(wesanderson)
library(gridExtra)
library(ggpubr) #!
library(grid)


#* single scenario
scen <- 8

ggplot(data=results[[scen]], aes(x=(as.factor(J)),y=(error/real),fill=as.factor(obs2))) +
  geom_boxplot(color="black",position=position_dodge()) +
  coord_cartesian(ylim=c(-1,1)) +
  theme_classic() +
  theme(axis.text=element_text(size=13),axis.title=element_text(size=16),
       legend.position="top") +
  scale_fill_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
  labs(y="Error",x="Number of visits (J)",fill="Model")

#* One fig per scenario
for(scen in 1:length(results)){
  assign(paste0("figError",scen),
         {ggplot(data=results[[scen]], aes(x=(as.factor(J)),y=(error/real),fill=as.factor(obs2))) +
             geom_boxplot(color="black",position=position_dodge()) +
             coord_cartesian(ylim=c(-1,1)) +
             scale_fill_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
             theme_classic() + theme(legend.position="none",axis.text=element_text(size=14),
                                     axis.title=element_blank())}
  )
}

# 1st step for the big figure... arranging all together
figError <- 
  grid.arrange(figError1,figError2,figError3,figError4,figError5,figError6,figError7,figError8,
               figError9,figError10,figError11,figError12,figError13,figError14,figError15,figError16,
               figError17,figError18,figError19,figError20,figError21,figError22,figError23,figError24,
               figError25,figError26,figError27,figError28,figError29,figError30,figError31,figError32,
               figError33,figError34,figError35,figError36,figError37,figError38,figError39,figError40,
               figError41,figError42,figError43,figError44,figError45,figError46,figError47,figError48,
               figError49,figError50,figError51,figError52,figError53,figError54,figError55,figError56,
               figError57,figError58,figError59,figError60,figError61,figError62,figError63,figError64,
        nrow=8,ncol=8)

# add x- and y- axis labels
figError <- annotate_figure(figError,left=text_grob("relative error", size=28, rot=90),
                           bottom=text_grob("Number of visits (J)", size=28))

# legend
my.legend <- get_legend(ggplot(data=results[[1]], aes(x=(as.factor(J)),y=(error/real),fill=as.factor(obs2))) +
                          geom_boxplot(color="black",position=position_dodge()) +
                          coord_cartesian(ylim=c(-1,1)) +
                          theme_classic() +
                          theme(axis.text=element_text(size=13),axis.title=element_text(size=16),
                                legend.position="top") +
                          scale_fill_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
                          labs(y="Error",x="Number of visits (J)",fill="Model"))

#* arrange final figure with simulation scenarios labels
figRMSE <- grid.arrange(
  #lambda values
  text_grob("0.1",face="bold",size=32),text_grob("0.2",face="bold",size=32),text_grob("0.3",face="bold",size=32),
  text_grob("0.5",face="bold",size=32),text_grob("1",face="bold",size=32),text_grob("2",face="bold",size=32),
  text_grob("4",face="bold",size=32),text_grob("8",face="bold",size=32),
  
  #phi values
  text_grob("0.1",face="bold",size=32),text_grob("0.2",face="bold",size=32),text_grob("0.3",face="bold",size=32),
  text_grob("0.4",face="bold",size=32),text_grob("0.5",face="bold",size=32),text_grob("0.6",face="bold",size=32),
  text_grob("0.7",face="bold",size=32),text_grob("0.8",face="bold",size=32),
  
  #big 8x8 figure
  figError,
  #legend
  my.legend,
  #specs
  ncol=9,nrow=9,
  layout_matrix=cbind(c(18,9:16), rbind(1:8, matrix(17,ncol=8,nrow=8))),
  heights=c(.1,rep(1,8)),widths=c(.1,rep(1,8)),
  vp=viewport(width=.9,height=.9),
  
  # scenarios labels
  top=textGrob("Local abundance", vjust=0,gp = gpar(fontsize=40,fontface="bold")),
  left=textGrob("           Availability", rot=90,gp=gpar(fontsize=40,fontface="bold"), vjust=0)
)


# save figure!
ggsave(here::here("outputs","figs","figError_P1Main.pdf"),figRMSE,width=160,height=120,units="cm",limitsize=F)
