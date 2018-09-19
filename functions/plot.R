plotfunc <- function(dattab,re,re0,max.id.fit,id.update){
  
  ytab3=NULL
  for (timeID in dattab$time[dattab$id==id.update]){
    #sumtab <- as.data.frame(summary(get(paste0("predict",max.id.fit,id.update,timeID),envir=predictenvir)[[2]])$statistics)
    
    if(timeID==0) {ytab <- NULL} else {ytab <- data.frame(Mean=dattab[dattab$id==id.update & dattab$time <timeID,re0],SD=0)%>% 
    
      mutate(upper.ci=Mean+1.96*SD,
             lower.ci=Mean-1.96*SD)%>%dplyr::select(-SD)}
    
    sumtab <- as.data.frame(summary(get(paste0("predict",max.id.fit,id.update,timeID),envir=predictenvir)[[2]])$quantiles)[,c(1,3,5)]
    
    names(sumtab) <- c("lower.ci","Mean","upper.ci")
    
    #ytab2 <- rbind(ytab,sumtab[grepl(paste0(re,"_new"),rownames(sumtab)),c("Mean","SD")] %>% 
                     #mutate(upper.ci=Mean+1.96*SD,
                            #lower.ci=Mean-1.96*SD))
    
    ytab2 <- rbind(ytab,sumtab[grepl(paste0(re,"_new"),rownames(sumtab)),]%>%dplyr::select(Mean,upper.ci,lower.ci))
    
    ytab2$Simulated = dattab[dattab$id==id.update,re0] 
    ytab2$time = dattab$time[dattab$id==id.update]  
    ytab2$model = timeID
    ytab3 <- rbind(ytab3,ytab2)
  }
  
  
  ggplot(aes(x=time),data=ytab3)+
    facet_wrap(~model)+
    geom_point(aes(y=Mean),col="grey")+
    geom_line(aes(y=Mean),col="grey")+
    geom_point(aes(y=Simulated),col="red")+
    geom_line(aes(y=Simulated),col="red")+
    geom_ribbon(aes(ymin=lower.ci,ymax=upper.ci),alpha=0.1,col="grey70",linetype=2)+
    ggpubr::theme_pubr()
}
