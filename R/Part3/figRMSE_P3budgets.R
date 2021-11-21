
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
             theme_classic() + theme(legend.position="none",axis.text=element_text(size=14)) +
             labs(x="Total effort (budget)",y="RMSE",color="Model")}
  )
}

#* Arranging all scenarios...
figRMSE <- grid.arrange(figRMSE1+theme(axis.title=element_blank()),figRMSE2+theme(axis.title=element_blank()),
                        figRMSE3+theme(axis.title=element_blank()),figRMSE4+theme(axis.title=element_blank()),
                        figRMSE5+theme(axis.title=element_blank()),figRMSE6+theme(axis.title=element_blank()),
                        nrow=3,ncol=2)
# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("relative RMSE", size=18, rot=90),
                                  bottom=text_grob("Total effort (budget)", size=18))

#* arrange final figure with simulation scenarios labels
figRMSE <- grid.arrange(
  #lambda values
  text_grob("0.2",face="bold",size=14),text_grob("1",face="bold",size=16),
  
  #phi values
  text_grob("0.2",face="bold",size=14),text_grob("0.4",face="bold",size=16),text_grob("0.6",face="bold",size=14),
  
  #big 2x3 figure
  figRMSE,
  #legend
  get_legend(ggplot(data=rel.rmse2[[1]], aes(x=budget,y=RMSE2,color="2obs")) + #aes(x=S,y=RMSE2)
               geom_point(size=2) + geom_line(size=1) +
               geom_hline(aes(yintercept=RMSE1ref[1],color="1obs"),size=1,linetype="dashed") +
               scale_color_manual(values=c("#3B9AB2","#F21A00")) +
               #scale_x_continuous(breaks=seq(round(min(a$S)/100)*100,round(max(a$S)/100)*100,length.out=6)) +
               theme_classic() + 
               labs(x="Total effort (budget)",y="RMSE",color="Model") +
               theme(legend.position="top",legend.key.size=unit(1.4,"cm"),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16,face="bold"))),
  #specs
  ncol=3,nrow=5,
  layout_matrix=rbind(cbind(c(NA,3:5), rbind(1:2, matrix(6,ncol=2,nrow=3))),c(NA,7,7)),
  heights=c(.1,rep(1,3),.2),widths=c(.1,rep(1,2)),
  vp=viewport(width=.9,height=.9),
  
  # scenarios labels
  top=grobTree(rectGrob(height=3.4,width=.8,vjust=.3,gp=gpar(fill="gray80")),
               textGrob("Local abundance", vjust=0,gp = gpar(fontsize=18,fontface="bold"))),
  left=grobTree(rectGrob(height=.75,width=3.4,hjust=1,vjust=0.45,gp=gpar(fill="gray80")),
                textGrob("Availability", rot=90,gp=gpar(fontsize=18,fontface="bold"), hjust=0,vjust=-.5))
)

# save figure!
ggsave(here::here("outputs","figs","figRMSE_P3budgets.png"),figRMSE,width=24,height=36,units="cm")




