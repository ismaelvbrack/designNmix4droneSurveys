
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##*   The BIG figure!!! RMSE curves
##*     64 scenarios of local abundance and availability
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run the script "calcRMSE_part1Main"

source(here::here("R","func_plotRMSE.R"))

# just testing function...
plotRMSE(data=rel.rmse2, scen=2,type="simple",complete=T,yax=c(0,1))

# 1st step for the big figure... arranging all together
figRMSE <- 
  grid.arrange(
    plotRMSE(rel.rmse2,scen=1,yax=c(0,1)),
    plotRMSE(rel.rmse2,scen=2,yax=c(0,1)),plotRMSE(rel.rmse2,scen=3,yax=c(0,1)),plotRMSE(rel.rmse2,scen=4,yax=c(0,1)),plotRMSE(rel.rmse2,scen=5,yax=c(0,1)),plotRMSE(rel.rmse2,scen=6,yax=c(0,1)),plotRMSE(rel.rmse2,scen=7,yax=c(0,1)),plotRMSE(rel.rmse2,scen=8,yax=c(0,1)),
    plotRMSE(rel.rmse2,scen=9,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=10,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=11,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=12,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=13,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=14,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=15,yax=c(0,.5)), plotRMSE(rel.rmse2,scen=16,yax=c(0,.5)),
    plotRMSE(rel.rmse2,scen=17,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=18,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=19,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=20,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=21,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=22,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=23,yax=c(0,.3)), plotRMSE(rel.rmse2,scen=24,yax=c(0,.3)),
    plotRMSE(rel.rmse2,scen=25,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=26,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=27,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=28,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=29,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=30,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=31,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=32,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=33,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=34,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=35,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=36,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=37,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=38,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=39,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=40,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=41,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=42,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=43,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=44,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=45,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=46,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=47,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=48,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=49,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=50,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=51,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=52,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=53,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=54,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=55,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=56,yax=c(0,.2)),
    plotRMSE(rel.rmse2,scen=57,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=58,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=59,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=60,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=61,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=62,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=63,yax=c(0,.2)), plotRMSE(rel.rmse2,scen=64,yax=c(0,.2)),
    nrow=8,ncol=8)

# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("relative RMSE", size=28, rot=90),
                           bottom=text_grob("Number of visits (J)", size=28))

# legend
my.legend <- get_legend(ggplot(rel.rmse2[[1]],aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
                geom_line(size=1.6) +
                scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
                labs(color="Model") +
                theme(legend.position="right",legend.key.size=unit(2,"cm"),
                      legend.text=element_text(size=20),legend.title=element_blank()))

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
  figRMSE,
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
ggsave(here::here("outputs","figs","figRMSE_P1Main.png"),figRMSE,width=160,height=120,units="cm",limitsize=F)

ggsave(here::here("outputs","figs","figRMSE_P1Main.pdf"),figRMSE,width=160,height=120,units="cm",limitsize=F)
