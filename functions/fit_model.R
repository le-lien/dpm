### Openbugs model estimating fixed effects
fit_model <- function() {
  # Priors
  beta0 ~ dnorm(0,0.00001)
  beta1 ~ dnorm(0,0.00001)
  
  mu.V0 <- 0 # mean for random intercepts
  mu.V1 <- 0 # mean for random slopes
  
  tau.V0 ~ dgamma(1.00000E-04, 1.00000E-04)
  tau.V1 ~ dgamma(1.00000E-04, 1.00000E-04)
  
  sigma.V0<-1/sqrt(tau.V0) # SD of random intercepts
  sigma.V1<-1/sqrt(tau.V1) # SD of random slopes
  
  rho ~ dunif(-1, 1) # correlation between intercepts and slopes
  
  Sigma.B[1, 1] <- pow(sigma.V0, 2) # var-covar matrix for the random effects
  Sigma.B[2, 2] <- pow(sigma.V1, 2)
  Sigma.B[1, 2] <- rho*sigma.V0*sigma.V1
  Sigma.B[2, 1] <- Sigma.B[1, 2]
  covariance <- Sigma.B[1, 2]
  Tau.B[1:2, 1:2] <- inverse(Sigma.B[,])
  
  tau ~ dgamma(1.00000E-04, 1.00000E-04)
  sigma <-1/sqrt(tau)  # Residual standard deviation
  
  for (i in 1:nid) {
    B.hat[i, 1] <- mu.V0
    B.hat[i, 2] <- mu.V1
    B[i, 1:2]~dmnorm(B.hat[i, ], Tau.B[,]) # the pairs of correlated random effects
    V0[i] <- B[i, 1] # random intercept
    V1[i] <- B[i, 2] # random slope
  }
  
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + V0[id[i]] + (beta1 + V1[id[i]]) * time[i]
  }
  
}
write.model(fit_model, "model/fit.txt")