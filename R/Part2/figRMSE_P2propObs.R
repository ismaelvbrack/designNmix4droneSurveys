
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##*  3 Figures part2: 
##* 1. RMSE curves 6 scenarios with different prop. of double obs. protocol
##* 2. RMSE in optJ in relation to 2obs prop.
##* 3.  Difference (%) in RMSE with the increase of 2obs prop.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#* IMPORTANT: run the script "caclRMSE_part2"

source(here::here("R","func_plotRMSE.R"))


# just testing function...
plotRMSE(data=rel.rmse2, scen=2,type="propObs",complete=T,yax=c(0,0.5))


# 1. RMSE curves -------------------------------------------------------------

#* Start figure arranging all scenarios...
figRMSE <- 
  grid.arrange(
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=1,type="propObs",yax=c(0,.5)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=2,type="propObs",yax=c(0,.5)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=3,type="propObs",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=4,type="propObs",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=5,type="propObs",yax=c(0,.2)),
    plotRMSE(rel.rmse2,rel.rmse4k.2,scen=6,type="propObs",yax=c(0,.2)),
    nrow=3,ncol=2
  )

# add x- and y- axis labels
figRMSE <- annotate_figure(figRMSE,left=text_grob("RMSE", size=20, rot=90),
                           bottom=text_grob("Number of visits (J)", size=20))

# legend
my.legend <- get_legend(ggplot(data=rel.rmse2[[1]],aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
                geom_line(size=1.6) + 
              scale_color_manual(values=wes_palette("Zissou1",length(unique(rel.rmse2[[1]]$obs2)),type="continuous")) + 
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
ggsave(here::here("outputs","figs","figRMSE_P2propObs.png"),figRMSE,width=40,height=55,units="cm")


# 2. Figure optRMSE ~ obs2 ---------------------------------------------------

optRMSE <- as.data.frame(as.table(sapply(rel.rmse2, function(x) tapply(x$RMSE,x$obs2,min))))
optRMSE <- cbind(optRMSE,
                 do.call(c,tapply(optRMSE[,3], optRMSE[,2], function(x) 1-(x/x[1]))),
                 do.call(rbind,strsplit(as.character(optRMSE[,2]),"_")))
names(optRMSE) <- c("obs2","scen","RMSE","RMSEdiff","lambda","phi")

fig1RMSEobs <- ggplot(data=optRMSE, aes(x=obs2, y=RMSE, col=phi, linetype=lambda,group=interaction(phi,lambda))) +
  geom_line(size=1.2) + scale_color_manual(values=c("gray70","gray40","black")) +
  labs(x="Prop. of double observer protocol",col=expression(phi),linetype=expression(lambda)) +
  scale_x_discrete(labels=paste0(as.numeric(as.character(unique(optRMSE$obs2)))*100,"%")) +
  theme_classic() + theme(legend.position="right")

#ggsave(here::here("outputs","figs","figP2_OptRMSE~propObs.png"),fig1RMSEobs,width=12,height=10,units="cm")


# 3. Figure difference in RMSE ~ obs2 --------------------------------------------

figRMSEdiffObs <- ggplot(data=optRMSE, aes(x=obs2, y=RMSEdiff*-100, col=phi, linetype=lambda,group=interaction(phi,lambda))) +
  geom_line(size=1.2) + scale_color_manual(values=c("gray70","gray40","black")) +
  labs(y="Difference in RMSE (%)",x="Prop. of double observer protocol",col=expression(phi),linetype=expression(lambda)) +
  scale_x_discrete(labels=paste0(as.numeric(as.character(unique(optRMSE$obs2)))*100,"%")) +
  theme_classic() + theme(legend.position="right")

#ggsave(here::here("outputs","figs","figP2_RMSEdiff~propObs.png"),figRMSEdiffObs,width=12,height=10,units="cm")

ggsave(here::here("outputs","figs","figP2_RMSE~propObs.png"),
       grid.arrange(fig1RMSEobs+theme(legend.position="none",text=element_text(size=14)),
                    figRMSEdiffObs+theme(text=element_text(size=14)), ncol=2),
       width=24,height=10,units="cm")




