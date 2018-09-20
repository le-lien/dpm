simulate <- function(nid,beta0,beta1,sd.y,V0,sigma0,V1,sigma1,rho,ncount,min.interval,max.interval,seed){
  
  def <- defData(varname = "nCount", dist = "noZeroPoisson", formula = ncount) # number of time points
  
  def <- defData(def, varname = "mInterval", dist = "uniform", formula = paste0(min.interval,";",max.interval)) # interval between time points
  
  # specifying a specific correlation matrix C
  corma <- c(1,rho,rho,1)
  C <- matrix(corma, nrow = length(corma)/2)
  
  # generate 2 correlated random effects
  set.seed(seed)
  rantab <- genCorData(nid, mu = c(V0,V1), sigma = c(sigma0,sigma1), corMatrix = C)
  
  # add time points
  fixtab <- addPeriods(genData(nid,def))
  
  betatab <- data.frame(matrix(c(beta0,beta1),ncol=length(c(beta0,beta1))))
  
  tab <- cbind(join(fixtab,rantab),betatab)
  names(tab) <- c("id","period","time","timeID","V0","V1","beta0","beta1")
  set.seed(seed)
  def2 <- defDataAdd(varname = "Ytime", dist = "normal", formula = "beta0+V0+(beta1+V1)*time", variance = sd.y^2)
  
  tab00 <- addColumns(def2, tab)
  
  # Delete duplicated time points and create table for analysis
  tab.full <- join(tab00 %>% select(id,time) %>% distinct(),tab00)
  tab <- tab.full %>% select(id,time,Ytime)
  
  return(list(tab,tab.full))
  
}

