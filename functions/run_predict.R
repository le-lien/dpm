# Function to run prediction
run_predict <- function(dattab,max.id.fit,id.update,start.time.update,seed,nt,nc,nb,ni){
  
  bugsfitm <- ifelse(exists(paste0("fit.out",max.id.fit),envir=fitenvir),paste0("fitenvir$fit.out",max.id.fit),"NULL")
  
  predict.out <- openbugs_predict(dattab=dattab,bugsfitmodel = eval(parse(text=bugsfitm)),max.id.fit = max.id.fit,id.update = id.update,start.time.update=start.time.update,seed=seed,nt=nt,nc=nc,nb=nb,ni=ni)
  
  assign(paste0("predict",max.id.fit,id.update,start.time.update),predict.out,envir=predictenvir)
}
