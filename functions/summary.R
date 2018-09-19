## Summarize the results
summaryfunc <- function(dattab,re="y",re0="Ytime",max.id.fit,id.update,time.plot){
  modelnames=paste0("predict",max.id.fit,id.update,dattab$time[dattab$id==id.update])
  
  out.names <- paste0("predict",max.id.fit,id.update,dattab$time[dattab$id==id.update & dattab$time<=time.plot])
  
  re_var_new = paste0(re,"_new[",dattab$timeID[dattab$id==id.update & dattab$time==time.plot],"]")
  
  out.table = NULL
  for (name in out.names){
    update <- summary(get(name,envir=predictenvir)[[2]])
    
    out.table <- rbind(out.table,data.frame(cbind(name,t(update$quantiles[re_var_new,][c(1,3,5)]),t(update$statistics[re_var_new,]))))
    }
  
  
  names(out.table)[1:4] <- c("dataset","ci.lower","median","ci.upper")
  out.table$dataset <- factor(out.table$dataset,levels=modelnames)
  
  for (col in 2:ncol(out.table)){out.table[,col] <- as.numeric(as.character(out.table[,col]))}
  
  ggplot(data=out.table,aes(y=dataset,x=ci.lower))+
    geom_point()+
    geom_segment(aes(yend=dataset,xend=ci.upper))+
    geom_point(aes(x=ci.upper))+
    geom_point(aes(x=Mean))+
    ggpubr::theme_pubr()+
    geom_vline(xintercept = dattab[,re0][dattab$id==id.update & dattab$time==time.plot],linetype=2)+
    labs(title=paste0("ID=",id.update),x=paste0("time",time.plot))
  
  #return(list(out.table,p))
}
