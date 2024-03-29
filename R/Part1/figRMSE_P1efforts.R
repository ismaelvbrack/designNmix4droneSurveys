
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##*   Figure: RMSE curves
##*     6 scenarios with total effort (budget) = 4k & 2k
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run the scripts caclRMSE_part1Main & "calcRMSE_part1_diffEffort"

source(here::here("R","func_plotRMSE.R"))

# just testing function...
plotRMSE(data=rel.rmse2,data2=rel.rmse4k.2, scen=2,type="effortDiff",complete=T,yax=c(0,0.5))

#* Start figure arranging all scenarios...
figRMSE <- 
  grid.arrange(
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=1,type="effortDiff",yax=c(0,.5)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=2,type="effortDiff",yax=c(0,.5)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=3,type="effortDiff",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=4,type="effortDiff",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=5,type="effortDiff",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=6,type="effortDiff",yax=c(0,.2)),
    nrow=3,ncol=2
  )

# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("relative RMSE", size=20, rot=90),
                           bottom=text_grob("Number of visits (J)", size=20))

# legend
my.legend <- get_legend(ggplot(data.frame(optJ=1:6,
                               Jmin=1,
                               Jmax=6,
                               obs2=c(0,1),
                               effort=c("4k","2k")
                               ),
                  aes(x=optJ,y=obs2,color=as.factor(obs2),xmin=Jmin,xmax=Jmax,linetype=effort)) + 
                  geom_errorbarh(size=1.2,height=0.5) +
                  labs(color="Model",linetype="Effort") +
                  scale_linetype_manual(values=c("solid", "twodash")) +
                  scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
                          theme(legend.position="top",legend.key.size=unit(3,"cm"),
                                legend.text=element_text(size=18),
                                legend.title=element_text(size=20,face="bold")))

figRMSE <- grid.arrange(
  #lambda values
  text_grob("0.2",face="bold",size=22),text_grob("1",face="bold",size=22),
  
  #phi values
  text_grob("0.2",face="bold",size=22),text_grob("0.4",face="bold",size=22),text_grob("0.6",face="bold",size=22),
  
  #big 3x2 figure
  figRMSE,
  #legend
  my.legend,
  #specs
  ncol=3,nrow=5,
  layout_matrix=rbind(cbind(c(NA,3:5), rbind(1:2, matrix(6,ncol=2,nrow=3))),c(NA,7,7)),
  heights=c(.1,rep(1,3),.2),widths=c(.1,rep(1,2)),
  vp=viewport(width=.9,height=.9),

  # scenarios labels
  top=grobTree(rectGrob(height=5,width=.8,vjust=.3,gp=gpar(fill="gray80")),
               textGrob("Local abundance", vjust=0,gp = gpar(fontsize=22,fontface="bold"))),
  left=grobTree(rectGrob(height=.75,width=5,hjust=1,vjust=0.45,gp=gpar(fill="gray80")),
                textGrob("Availability", rot=90,gp=gpar(fontsize=22,fontface="bold"), hjust=0,vjust=-.5))
)

# save figure!
ggsave(here::here("outputs","figs","figRMSE_P1efforts.png"),figRMSE,width=45,height=64,units="cm")



