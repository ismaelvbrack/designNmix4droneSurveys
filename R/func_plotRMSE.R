#---------------------------------------------------------------------------------------
####** function to plot RMSE curves for selected scenario
#####* using ggplot

####* This function uses a object like rel.rmse2 (data.frame with RMSE, J, obs...)
####* type= simple: for 1 and 2 obs models
####* type= propObs: for different proportions of 2obs protocol based on scenario
####* type = effortDiff: 

plotRMSE <- function(data,data2=NULL,scen,type="simple",complete=FALSE,yax=NULL){
  require(ggplot2)
  require(gridExtra)
  require(ggpubr)
  require(grid)
  require(wesanderson)
  

# type="simple", 1obs and 2obs --------------------------------------------

  if(type=="simple"){
    i=scen
    data <- data[[i]]
    # arranging needed information
    if(is.null(yax)){
      yax <- c(0, max(data$RMSE))
    }
    
    tab <- data.frame(optJ=tapply(data$RMSE, data$obs2,
                                  function(x) as.numeric(as.character(data[data$RMSE==min(x),"J"]))),
                      Jmin=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[1]),
                      Jmax=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[2]),
                      obs2=unique(data$obs2))

    
    if(complete==FALSE){
      # ggplot figure
      fig.out <- 
              ggplot(data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
              geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
              coord_cartesian(ylim=yax) +
              theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
              theme(axis.title=element_blank(),axis.text=element_text(size=13),legend.position="none")
      
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax)) + 
                  geom_point(size=5,shape=18) + geom_errorbarh(size=1.2,height=0.5) +
                  theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
                  labs(x="Optimal J") + scale_y_discrete(limits=rev) +
                  theme(panel.background = element_rect(colour = "black"),legend.position="none",
                        axis.text=element_blank(),axis.title.y=element_blank(),
                        axis.title.x=element_text(face="bold",size=12)) + 
                  scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
    }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16),
              legend.position="top") +
        scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        labs(y="relative RMSE",x="Number of visits (J)",color="Model")
        
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax)) + 
        geom_point(size=5,shape=18) + geom_errorbarh(size=1.2,height=0.5) +
        theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
        labs(x="Optimal J") + scale_y_discrete(limits=rev) +
        theme(panel.background = element_rect(colour = "black"),legend.position="none",
              axis.text=element_blank(),axis.title.y=element_blank(),
              axis.title.x=element_text(face="bold",size=12)) + 
        scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
      
    }
    
    return(fig.out)
  } # type= "simple"
  

# type="propObs", proportions of 2obs -------------------------------------

  if(type=="propObs"){
    i=scen
    data <- data[[i]]
    # arranging needed information
    if(is.null(yax)){
      yax <- c(0, max(data$RMSE))
    }
    
    tab <- data.frame(optJ=tapply(data$RMSE, data$obs2,
                                  function(x) as.numeric(as.character(data[data$RMSE==min(x),"J"]))),
                      Jmin=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[1]),
                      Jmax=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[2]),
                      obs2=unique(data$obs2))
    
    if(complete==FALSE){
      # ggplot figure
      fig.out <-
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous")) + 
        theme(axis.title=element_blank(),axis.text=element_text(size=18),legend.position="none")
      
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax)) + 
        geom_point(size=5,shape=18) + geom_errorbarh(size=1.2,height=0.5) +
        theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
        labs(x="Optimal J") + scale_y_discrete(limits=rev) +
        theme(panel.background = element_rect(colour = "black"),legend.position="none",
              axis.text=element_blank(),axis.title.y=element_blank(),
              axis.title.x=element_text(face="bold",size=16)) + 
        scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
    }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() +
        scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous")) + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16),legend.position="top") +
        labs(y="relative RMSE",x="Number of visits (J)",color=" Prop. of \nDouble Obs.")
    
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax)) + 
        geom_point(size=5,shape=18) + geom_errorbarh(size=1.2,height=0.5) +
        theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
        labs(x="Optimal J") + scale_y_discrete(limits=rev) +
        theme(panel.background = element_rect(colour = "black"),legend.position="none",
              axis.text=element_blank(),axis.title.y=element_blank(),
              axis.title.x=element_text(face="bold",size=16)) + 
        scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
    }
    
    return(fig.out)
   }
 #type= "propObs"


# type="effortDiff", scenarios with different total effort (budget) -------

  if(type=="effortDiff"){
    
    data <-  cbind(rbind(
               data2[[scen]],
               data[[which(names(data) %in% names(data2)[scen])]]
             ),
             effort=rep(c("4k","2k"),c(nrow(data2[[scen]]),
                              nrow(data[[which(names(data) %in% names(data2)[scen])]])))
             )
    
    # arranging needed information
    if(is.null(yax)){
      yax <- c(0, max(data$RMSE))
    }
    
    tab <- data.frame(optJ=tapply(data$RMSE, data$obs2,
                                  function(x) as.numeric(as.character(data[data$RMSE==min(x),"J"]))),
                      Jmin=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[1]),
                      Jmax=tapply(data$RMSE, data$obs2,
                                  function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[2]),
                      obs2=unique(data$obs2),
                      effort=rep(unique(data$effort),each=2))
    
    if(complete==FALSE){
      # ggplot figure
      fig.out <-
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2,linetype=effort,group=interaction(obs2,effort))) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        scale_linetype_manual(values=c("solid", "twodash")) + coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.title=element_blank(),axis.text=element_text(size=18),legend.position="none")
      
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax,linetype=effort)) + 
        geom_point(size=5,shape=18,position=position_dodge(0.9)) + 
        geom_errorbarh(size=1.2,height=0.5,position=position_dodge(0.9)) +
        theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
        labs(x="Optimal J") + scale_y_discrete(limits=rev) +
        theme(panel.background = element_rect(colour = "black"),legend.position="none",
              axis.text=element_blank(),axis.title.y=element_blank(),
              axis.title.x=element_text(face="bold",size=16)) + 
        scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
        scale_linetype_manual(values=c("solid", "twodash"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
      
     }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2,linetype=effort,group=interaction(obs2,effort))) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        scale_linetype_manual(values=c("solid", "twodash")) + coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16),legend.position="top") +
        labs(y="relative RMSE",x="Number of visits (J)",color=" Model",linetype="Effort")
      
      figJs <- ggplot(tab, aes(x=optJ,y=obs2,color=obs2,xmin=Jmin,xmax=Jmax,linetype=effort)) + 
        geom_point(size=5,shape=18,position=position_dodge(0.9)) + 
        geom_errorbarh(size=1.2,height=0.5,position=position_dodge(0.9)) +
        theme_classic() + xlim(range(as.numeric(as.character(data$J)))) +
        labs(x="Optimal J") + scale_y_discrete(limits=rev) +
        theme(panel.background = element_rect(colour = "black"),legend.position="none",
              axis.text=element_blank(),axis.title.y=element_blank(),
              axis.title.x=element_text(face="bold",size=16)) + 
        scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) +
        scale_linetype_manual(values=c("solid", "twodash"))
      
      fig.out <- ggarrange(fig.out,figJs,nrow=2,align="v",heights=c(0.8,0.2))
      
      }
    
    return(fig.out)
  }# type= "effortDiff"

  
}# function

 