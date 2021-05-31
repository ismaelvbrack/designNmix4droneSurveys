
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##*   Figure: RMSE curves
##*     9 scenarios of high local abundance
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run the script "calcRMSE_part1_highabund"

source(here::here("R","func_plotRMSE.R"))

# just testing function...
plotRMSE(data=rel.rmse2, scen=2,type="simple",complete=T,yax=c(0,0.5))

#* Start figure arranging all scenarios...
figRMSE <- 
  grid.arrange(
    plotRMSE(rel.rmse2,scen=1,yax=c(0,1)),
    plotRMSE(rel.rmse2,scen=2,yax=c(0,1)),
    plotRMSE(rel.rmse2,scen=3,yax=c(0,1)),
    plotRMSE(rel.rmse2,scen=4,yax=c(0,.5)),
    plotRMSE(rel.rmse2,scen=5,yax=c(0,.5)),
    plotRMSE(rel.rmse2,scen=6,yax=c(0,.5)),
    plotRMSE(rel.rmse2,scen=7,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=8,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=9,yax=c(0,.2)),
    nrow=3,ncol=3
  )

# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("RMSE", size=20, rot=90),
                                 bottom=text_grob("Number of visits (J)", size=20))

# legend
my.legend <- get_legend(ggplot(rel.rmse2[[1]],aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
                          geom_line(size=1.6) +
                          scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
                          labs(color="Model") +
                          theme(legend.position="right",legend.key.size=unit(1.6,"cm"),
                                legend.text=element_text(size=14),legend.title=element_blank()))

#* arrange final figure with simulation scenarios labels
figRMSE <- grid.arrange(
  #lambda values
  text_grob("12",face="bold",size=22),text_grob("20",face="bold",size=22),text_grob("40",face="bold",size=22),
  
  #phi values
  text_grob("0.1",face="bold",size=22),text_grob("0.2",face="bold",size=22),text_grob("0.5",face="bold",size=22),
  
  #big 3x3 figure
  figRMSE,
  #legend
  my.legend,
  #specs
  ncol=4,nrow=4,
  layout_matrix=cbind(c(8,4:6), rbind(1:3, matrix(7,ncol=3,nrow=3))),
  heights=c(.1,rep(1,3)),widths=c(.1,rep(1,3)),
  vp=viewport(width=.9,height=.9),
  
  # scenarios labels
  top=textGrob("Local abundance", vjust=0,gp = gpar(fontsize=22,fontface="bold")),
  left=textGrob("           Availability", rot=90,gp=gpar(fontsize=22,fontface="bold"), vjust=0)
)

# save figure!
ggsave(here::here("outputs","figs","figRMSE_P1highAbund.png"),figRMSE,width=40,height=30,units="cm")

