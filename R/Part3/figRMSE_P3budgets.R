
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##*   Figure
##*     RMSE (in optimal J) reducing budget for 2obs model
##*     and comparing with RMSE for 1obs model
##*     6 scenarios 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run scripts 'calcRMSE_part1Main.R' and 'calcRMSE_part3.R'

library(ggplot2)
library(gridExtra)
library(ggpubr) #!
library(grid)

#* Single scenario plot
scen <- 2

ggplot(data=rel.rmse2[[scen]], aes(x=budget,y=RMSE2,color="2obs")) + #aes(x=S,y=RMSE2)
  geom_point(size=2) + geom_line(size=1) +
  geom_hline(aes(yintercept=RMSE1ref[1],color="1obs"),size=1,linetype="dashed") +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) +
  #scale_x_continuous(breaks=seq(round(min(a$S)/100)*100,round(max(a$S)/100)*100,length.out=6)) +
  theme_classic() + 
  labs(x="Total effort (budget)",y="relative RMSE",color="Model")

#* One fig per scenario
for(scen in 1:length(rel.rmse2)){
  assign(paste0("figRMSE",scen),
         {ggplot(data=rel.rmse2[[scen]], aes(x=budget,y=RMSE2,color="2obs")) + #aes(x=S,y=RMSE2)
             geom_point(size=2) + geom_line(size=1) +
             geom_hline(aes(yintercept=RMSE1ref[1],color="1obs"),size=1,linetype="dashed") +
             scale_color_manual(values=c("#3B9AB2","#F21A00")) +
             #scale_x_continuous(breaks=seq(round(min(a$S)/100)*100,round(max(a$S)/100)*100,length.out=6)) +
             theme_classic() + theme(legend.position="none") +
             labs(x="Total effort (budget)",y="RMSE",color="Model")}
  )
}

#* Arranging all scenarios...
figRMSE <- grid.arrange(figRMSE1+theme(axis.title=element_blank()),figRMSE2+theme(axis.title=element_blank()),
                        figRMSE3+theme(axis.title=element_blank()),figRMSE4+theme(axis.title=element_blank()),
                        figRMSE5+theme(axis.title=element_blank()),figRMSE6+theme(axis.title=element_blank()),
                        nrow=3,ncol=2)
# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("relative RMSE", size=16, rot=90),
                                  bottom=text_grob("Total effort (budget)", size=16))

#* arrange final figure with simulation scenarios labels
figRMSE <- grid.arrange(
  #lambda values
  text_grob("0.2",face="bold",size=14),text_grob("1",face="bold",size=14),
  
  #phi values
  text_grob("0.2",face="bold",size=14),text_grob("0.4",face="bold",size=14),text_grob("0.6",face="bold",size=14),
  
  #big 2x3 figure
  figRMSE,
  #legend
  get_legend(ggplot(data=rel.rmse2[[1]], aes(x=budget,y=RMSE2,color="2obs")) + #aes(x=S,y=RMSE2)
               geom_point(size=2) + geom_line(size=1) +
               geom_hline(aes(yintercept=RMSE1ref[1],color="1obs"),size=1,linetype="dashed") +
               scale_color_manual(values=c("#3B9AB2","#F21A00")) +
               #scale_x_continuous(breaks=seq(round(min(a$S)/100)*100,round(max(a$S)/100)*100,length.out=6)) +
               theme_classic() + 
               labs(x="Total effort (budget)",y="RMSE",color="Model")),
  #specs
  ncol=3,nrow=4,
  layout_matrix=cbind(c(7,3:5), rbind(1:2, matrix(6,ncol=2,nrow=3))),
  heights=c(.1,rep(1,3)),widths=c(.1,rep(1,2)),
  vp=viewport(width=.9,height=.9),
  
  # scenarios labels
  top=textGrob(expression(lambda), vjust=0,gp = gpar(fontsize=22,fontface="bold")),
  left=textGrob(expression(phi), rot=90,gp=gpar(fontsize=22,fontface="bold"), vjust=0)
)

# save figure!
ggsave(here::here("outputs","figs","figRMSE_P3budgets.png"),figRMSE,width=18,height=24,units="cm")




