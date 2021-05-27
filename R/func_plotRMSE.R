#---------------------------------------------------------------------------------------
####** function to create figure with ggplot ploting RMSE curves for selected scenario
#
####* type= simple: for 1 and 2 obs models
####* type= propObs: for different proportions of 2obs protocol based on scenario
####* type = effortDiff: 

plotRMSE <- function(data,data2=NULL,scen,type="simple",complete=FALSE){
  require(ggplot2)
  require(gridExtra)
  require(ggpubr)
  require(grid)
  require(wesanderson)
  
  if(type=="simple"){
    i=scen
    data <- data[[i]]
    # arranging needed information
    my.scale <- (max(data$RMSE) - min(data$RMSE))*.3
    yax <- c(min(data$RMSE)-my.scale, max(data$RMSE))
    
    opt1 <- as.numeric(as.character(data[which(data$RMSE == min(data[which(data$obs2==0),"RMSE"])),"J"]))
    opt2 <- as.numeric(as.character(data[which(data$RMSE == min(data[which(data$obs2==1),"RMSE"])),"J"]))
    
    opt1.range <- range(as.numeric(as.character(
      data[which(data$RMSE < (min(data[which(data$obs2==0),"RMSE"])+0.005) & data$obs2==0),"J"] )))
    opt2.range <- range(as.numeric(as.character(
      data[which(data$RMSE < (min(data[which(data$obs2==1),"RMSE"])+0.005) & data$obs2==1),"J"] )))
    
    Jmin=c(opt1.range[1],opt2.range[1])
    Jmax=c(opt1.range[2],opt2.range[2])
    
    if(complete==FALSE){
      # ggplot figure
      fig.out <- 
        ggplot(data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.title=element_blank(),axis.text=element_text(size=13),legend.position="none") +
        geom_errorbarh(aes(xmin=Jmin[1],xmax=Jmax[1],y=0.01+(my.scale/2)),size=1,height=my.scale/3,colour="#3B9AB2") +
        geom_errorbarh(aes(xmin=Jmin[2],xmax=Jmax[2],y=0.01),size=1,height=my.scale/3,colour="#F21A00") +
        annotation_custom(textGrob("*", gp = gpar(col="#F21A00",fontsize=36)), 
                          xmin=opt2, xmax=opt2,ymin=0.01, ymax=0.01) +
        annotation_custom(textGrob("*", gp = gpar(col="#3B9AB2",fontsize=36)), 
                          xmin=opt1, xmax=opt1,ymin=0.01+(my.scale/2), ymax=0.01+(my.scale/2))
    }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16)) + labs(x="Number of visits (J)",color="Model")+#,title=names(data[i])) +
        geom_errorbarh(aes(xmin=Jmin[1],xmax=Jmax[1],y=yax[1]+(my.scale/2)),size=1,height=my.scale/3,colour="#3B9AB2") +
        geom_errorbarh(aes(xmin=Jmin[2],xmax=Jmax[2],y=yax[1]),size=1,height=my.scale/3,colour="#F21A00") +
        annotation_custom(textGrob("*", gp = gpar(col="#F21A00",fontsize=36)), 
                          xmin=opt2, xmax=opt2,ymin=yax[1], ymax=yax[1]) +
        annotation_custom(textGrob("*", gp = gpar(col="#3B9AB2",fontsize=36)), 
                          xmin=opt1, xmax=opt1,ymin=yax[1]+(my.scale/2), ymax=yax[1]+(my.scale/2))
      
    }
    
    return(fig.out)
  } # type= "simple"
  
  if(type=="propObs"){
    i=scen
    data <- data[[i]]
    # arranging needed information
    my.scale <- (max(data$RMSE) - min(data$RMSE))*.3
    yax <- c(min(data$RMSE)-my.scale, max(data$RMSE))
    
    optJ <- data.frame(optJ=tapply(data$RMSE, data$obs2,
                                   function(x) as.numeric(as.character(data[data$RMSE==min(x),"J"]))),
                       Jmin=tapply(data$RMSE, data$obs2,
                                   function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[1]),
                       Jmax=tapply(data$RMSE, data$obs2,
                                   function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[2]),
                       obs2=unique(data$obs2),
                       y=seq(yax[1],min(data$RMSE)-(my.scale/6),length.out=length(unique(data$obs2))))
    #optJ <- optJ[nrow(optJ):1,]
    
    if(complete==FALSE){
      optJ$y <- seq(0,min(data$RMSE)-.025,length.out=length(unique(data$obs2)))
      # ggplot figure
      fig.out <-
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous")) + 
        theme(axis.title=element_blank(),axis.text=element_text(size=18),legend.position="none") +
        geom_errorbarh(data=optJ,mapping=aes(xmin=Jmin,xmax=Jmax,y=y,colour=obs2),inherit.aes=FALSE,size=1,height=my.scale/4)+
        geom_text(data=optJ,mapping=aes(x=optJ,y=y,label="*",colour=obs2),inherit.aes=FALSE,size=12,fontface="bold",show.legend=F)
    }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2)) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(values=wes_palette("Zissou1",length(unique(data$obs2)),type="continuous")) + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16)) + labs(x="Number of visits (J)",color=" Prop. of \nDouble Obs.")+#,title=names(data[i])) +
        geom_errorbarh(data=optJ,mapping=aes(xmin=Jmin,xmax=Jmax,y=y,colour=obs2),inherit.aes=FALSE,size=1,height=my.scale/6)+
        geom_text(data=optJ,mapping=aes(x=optJ,y=y,label="*",colour=obs2),inherit.aes=FALSE,size=12,fontface="bold",show.legend=F)
    }
    
    return(fig.out)
  }
  #rmseObs.fig(data=rel.rmse2,2)
  } #type= "propObs"

  if(type=="effortDiff"){
    
    data <-  cbind(rbind(
               data2[[scen]],
               data[[which(names(data) %in% names(data2)[scen])]]
             ),
             effort=rep(c("4k","2k"),c(nrow(rel.rmse.eff2[[scen]]),
                              nrow(rel.rmse2[[which(names(rel.rmse2) %in% names(rel.rmse.eff2)[scen])]])))
             )
    
    # arranging needed information
    my.scale <- (max(data$RMSE) - min(data$RMSE))*.3
    yax <- c(min(data$RMSE)-my.scale, max(data$RMSE))
    
    optJ <- data.frame(optJ=as.numeric(tapply(data$RMSE, list(data$obs2,data$effort),
                                              function(x) as.character(data[data$RMSE==min(x),"J"]))),
                       Jmin=as.numeric(tapply(data$RMSE, list(data$obs2,data$effort),
                                              function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[1])),
                       Jmax=as.numeric(tapply(data$RMSE, list(data$obs2,data$effort),
                                              function(x) range(as.numeric(as.character(data[which(x<min(x)+0.005),"J"])))[2])),
                       obs2=rep(unique(data$obs2),2),
                       effort=rep(unique(data$effort),each=2),
                       y=seq(yax[1],min(data$RMSE)-(my.scale/6),length.out=length(unique(data$obs2))*length(unique(data$effort)))
    )
    #optJ <- optJ[nrow(optJ):1,]
    
    if(complete==FALSE){
      optJ$y <- seq(0,min(data$RMSE)-.025,length.out=length(unique(data$obs2))*length(unique(data$effort)))
      # ggplot figure
      fig.out <-
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2,linetype=effort,group=interaction(obs2,effort))) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        scale_linetype_manual(values=c("solid", "twodash")) + coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.title=element_blank(),axis.text=element_text(size=18),legend.position="none") +
        geom_errorbarh(data=optJ,mapping=aes(xmin=Jmin,xmax=Jmax,y=y,colour=obs2,linetype=effort),
                       inherit.aes=FALSE,size=1,height=my.scale/4)+
        geom_text(data=optJ,mapping=aes(x=optJ,y=y,label="*",colour=obs2),inherit.aes=FALSE,size=12,fontface="bold",show.legend=F)
    }
    
    if(complete==TRUE){
      # ggplot figure
      fig.out <- 
        ggplot(data=data,aes(x=as.numeric(as.character(J)),y=RMSE,color=obs2,linetype=effort,group=interaction(obs2,effort))) +
        geom_line(size=1.6) + scale_x_continuous(breaks=2:30) +
        scale_linetype_manual(values=c("solid", "twodash")) + coord_cartesian(ylim=yax) +
        theme_classic() + scale_color_manual(labels=c("1obs","2obs"),values=c("#3B9AB2","#F21A00")) + 
        theme(axis.text=element_text(size=13),axis.title=element_text(size=16)) +
        labs(x="Number of visits (J)",color=" Model",linetype="Effort")+#,title=names(data[i])) +
        geom_errorbarh(data=optJ,mapping=aes(xmin=Jmin,xmax=Jmax,y=y,colour=obs2,linetype=effort),
                       inherit.aes=FALSE,size=1,height=my.scale/6)+
        geom_text(data=optJ,mapping=aes(x=optJ,y=y,label="*",colour=obs2),inherit.aes=FALSE,size=12,fontface="bold",show.legend=F)
    }
    
    return(fig.out)
  }
  #rmseObs.fig(data=rel.rmse2,2)
  }# type= "effortDiff"
  
}# function

 