
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
figRMSE <- annotate_figure(figRMSE,left=text_grob("relative RMSE", size=22, rot=90),
                           bottom=text_grob("Number of visits (J)", size=22))

# legend
my.legend <- get_legend(ggplot(data=rel.rmse2[[1]],aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
                geom_line(size=1.6) + 
              scale_color_manual(values=wes_palette("Zissou1",length(unique(rel.rmse2[[1]]$obs2)),type="continuous")) + 
              labs(color="Prop. of Double Obs.") +
              theme(legend.position="bottom",legend.key.size=unit(1.6,"cm"),
                      legend.text=element_text(size=16),legend.title=element_text(size=20,face="bold")))
  

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
ggsave(here::here("outputs","figs","figRMSE_P2propObs.png"),figRMSE,width=40,height=64,units="cm")


# 2. Figure optRMSE ~ obs2 ---------------------------------------------------

optRMSE <- as.data.frame(as.table(sapply(rel.rmse2, function(x) tapply(x$RMSE,x$obs2,min))))
optRMSE <- cbind(optRMSE,
                 do.call(c,tapply(optRMSE[,3], optRMSE[,2], function(x) 1-(x/x[1]))),
                 do.call(rbind,strsplit(as.character(optRMSE[,2]),"_")))
names(optRMSE) <- c("obs2","scen","RMSE","RMSEdiff","lambda","phi")

lm1 <- lm(RMSE ~ interaction(phi,lambda)*as.numeric(as.character(obs2))-1,data=optRMSE)
lm2 <- lm(RMSEdiff ~ interaction(phi,lambda)*as.numeric(as.character(obs2))-1,data=optRMSE)

# based on tendency regression lines
optRMSE$RMSEdiff2 <- do.call(c,tapply(predict(lm1), optRMSE[,2], function(x) 1-(x/x[1])))


fig1RMSEobs <- ggplot(data=optRMSE,aes(x=as.numeric(as.character(obs2)),y=RMSE,
                  col=phi,linetype=lambda,shape=lambda,group=interaction(phi,lambda))) +
  geom_point(size=2) + stat_smooth(method="lm",se=F)+
  scale_color_manual(values=c("gold2","chocolate1","orangered4")) +
  scale_linetype_manual(values=c(4,1)) + 
  labs(y="relative RMSE",x="Prop. of double observer protocol",
       col=expression(phi),linetype=expression(lambda),shape=expression(lambda)) +
  scale_x_continuous(breaks=as.numeric(as.character(unique(optRMSE$obs2))),
                     labels=paste0(as.numeric(as.character(unique(optRMSE$obs2)))*100,"%")) +
  theme_classic() + theme(legend.position="right") +
  guides(linetype=guide_legend(override.aes=list(color=1)))


#ggsave(here::here("outputs","figs","figP2_OptRMSE~propObs.png"),fig1RMSEobs,width=12,height=10,units="cm")


# 3. Figure difference in RMSE ~ obs2 --------------------------------------------

# figRMSEdiffObs <- ggplot(data=optRMSE, aes(x=obs2, y=RMSEdiff*-100, col=phi, linetype=lambda,group=interaction(phi,lambda))) +
#   geom_line(size=1.2) + scale_color_manual(values=c("gold2","chocolate1","orangered4")) +
#   scale_linetype_manual(values=c(4,1)) + 
#   labs(y="Difference in rel.RMSE (%)",x="Prop. of double observer protocol",col=expression(phi),linetype=expression(lambda)) +
#   scale_x_discrete(labels=paste0(as.numeric(as.character(unique(optRMSE$obs2)))*100,"%")) +
#   theme_classic() + theme(legend.position="right")

figRMSEdiffObs <- ggplot(data=optRMSE,
       aes(x=as.numeric(as.character(obs2)), y=RMSEdiff2*-100, col=phi, linetype=lambda,group=interaction(phi,lambda))) +
  geom_line(size=1.2) +
  scale_color_manual(values=c("gold2","chocolate1","orangered4")) +
  scale_linetype_manual(values=c(4,1)) + 
  labs(y="Difference in rel.RMSE (%)",x="Prop. of double observer protocol",
       col=expression(phi),linetype=expression(lambda)) +
  scale_x_continuous(breaks=as.numeric(as.character(unique(optRMSE$obs2))),
              labels=paste0(as.numeric(as.character(unique(optRMSE$obs2)))*100,"%")) +
  theme_classic() + theme(legend.position="right") 


#ggsave(here::here("outputs","figs","figP2_RMSEdiff~propObs.png"),figRMSEdiffObs,width=12,height=10,units="cm")

ggsave(here::here("outputs","figs","figP2_RMSE~propObs.png"),
       annotate_figure(ggarrange(fig1RMSEobs+theme(axis.title.x=element_blank(),legend.position="none",text=element_text(size=14)),
                    figRMSEdiffObs+theme(axis.title.x=element_blank(),legend.position="none",text=element_text(size=14)),
                    get_legend(fig1RMSEobs+theme(legend.title=element_text(size=18),legend.text=element_text(size=14))),
                    ncol=3,widths=c(0.45,.45,0.1)),bottom=text_grob("Prop. of double-observer protocol",size=14)),
       width=26,height=10,units="cm",bg="white")



