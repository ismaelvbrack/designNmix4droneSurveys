
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
                  scale_linetype_manual(values=c("solid", "twodash")) +
                  scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
                          theme(legend.position="right",legend.key.size=unit(1.6,"cm"),
                                legend.text=element_text(size=14),legend.title=element_blank()))

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
  ncol=3,nrow=4,
  layout_matrix=cbind(c(7,3:5), rbind(1:2, matrix(6,ncol=2,nrow=3))),
  heights=c(.1,rep(1,3)),widths=c(.1,rep(1,2)),
  vp=viewport(width=.9,height=.9),
  
  # scenarios labels
  top=textGrob("Local abundance", vjust=0,gp = gpar(fontsize=22,fontface="bold")),
  left=textGrob("           Availability", rot=90,gp=gpar(fontsize=22,fontface="bold"), vjust=0)
)

# save figure!
ggsave(here::here("outputs","figs","figRMSE_P1efforts.png"),figRMSE,width=45,height=60,units="cm")



