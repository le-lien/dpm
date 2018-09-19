openbugs_predict <- function(dattab,bugsfitmodel=NULL,max.id.fit,id.update,start.time.update,seed,nt,nc,nb,ni){ 

  if (is.null(bugsfitmodel)){
    fitdata <- dattab %>% filter(id<=max.id.fit)
    
    ### Fit model
    
    # Bundle data
    bugsfitdata <- list(nid=max(as.numeric(fitdata$id)),n=nrow(fitdata),y=as.numeric(fitdata$Ytime), time=as.numeric(fitdata$time),id=as.numeric(fitdata$id))
    
    # Inits function
    fit.inits <- function() {
      list( beta0=0,beta1=0,tau.V0=1, tau.V1=1, rho=0, tau=1)
    }
    
    # Parameters to estimate
    fit.params <- c("beta0","beta1","sigma.V0","sigma.V1","rho","sigma","tau","tau.V0","tau.V1","V0","V1")
    
    # Start Gibbs sampler
    fit.out <- bugs(bugsfitdata, fit.inits, fit.params, "fit.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, codaPkg=TRUE,save.history=T,bugs.seed=seed,working.directory=paste0(getwd(),"/model/"))
    
    fit.out.coda <- read.bugs(fit.out) 
    assign(paste0("fit.out",max.id.fit),fit.out.coda,envir=fitenvir)
    #save(fit.out.coda,file=paste0("fit.out",max.id.fit))
  } else {
    fit.out.coda <- bugsfitmodel
  }
  
  fit.out.summary <- summary(fit.out.coda)
  
  ### Update model
  updatedata <- rbind(dattab %>% filter(id<id.update) ,dattab %>% filter(id==id.update) %>% filter(time <start.time.update),dattab %>% filter(id==id.update) %>% filter(time >=start.time.update) %>% mutate(Ytime=NA))
  
  # Bundle data
  bugsupdatedata <- list(nid_new=max(as.numeric(updatedata$id)),n_new=nrow(updatedata),y_new=as.numeric(updatedata$Ytime), time_new=as.numeric(updatedata$time),id_new=as.numeric(updatedata$id),beta0_new=fit.out.summary$statistics["beta0","Mean"],beta1_new=fit.out.summary$statistics["beta1","Mean"],tau_new=fit.out.summary$statistics["tau","Mean"],rho_new=fit.out.summary$statistics["rho","Mean"],tau.V0_new=fit.out.summary$statistics["tau.V0","Mean"],tau.V1_new=fit.out.summary$statistics["tau.V1","Mean"])
  
  
  
  # Inits function
  update.inits <- function() {
    list(tau.V0_new=1, tau.V1_new=1, rho_new=0)
  }
  
  # Parameters to estimate
  update.params <- c("V0_new","V1_new","y_new")
  
  # Start Gibbs sampler
  update.out <- bugs(bugsupdatedata, update.inits, update.params, "update.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, codaPkg=TRUE,save.history=T,bugs.seed=seed,working.directory=paste0(getwd(),"/model/"))
  
  update.out.coda <- read.bugs(update.out)
  
  return(list(fit.out.coda,update.out.coda))
}
