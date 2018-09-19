## Function to update estimates for random effects and future outcome
update_model <- function() {
  
  # beta0, beta1 ~ estimated from "fit_model"
  
  # Updating priors
  
  mu.V0_new <- 0 # mean for random intercepts
  mu.V1_new <- 0 # mean for random slopes
  
  #tau.V0_new, tau.V1_new ~ estimated from "fit_model"
  
  sigma.V0_new<-1/sqrt(tau.V0_new) # SD of random intercepts
  sigma.V1_new<-1/sqrt(tau.V1_new) # SD of random slopes
  
  #rho_new ~ estimated from "fit_model"
  
  Sigma.B_new[1, 1] <- pow(sigma.V0_new, 2) # var-covar matrix for the random effects
  Sigma.B_new[2, 2] <- pow(sigma.V1_new, 2)
  Sigma.B_new[1, 2] <- rho_new*sigma.V0_new*sigma.V1_new
  Sigma.B_new[2, 1] <- Sigma.B_new[1, 2]
  covariance_new <- Sigma.B_new[1, 2]
  Tau.B_new[1:2, 1:2] <- inverse(Sigma.B_new[,])
  
  # tau_new ~ estimated from "fit_model"
  sigma_new <-1/sqrt(tau_new)  # Residual standard deviation
  
  for (i in 1:nid_new) {
    B.hat_new[i, 1] <- mu.V0_new
    B.hat_new[i, 2] <- mu.V1_new
    B_new[i, 1:2]~dmnorm(B.hat_new[i, ], Tau.B_new[,]) # the pairs of correlated random effects
    V0_new[i] <- B_new[i, 1] # random intercept
    V1_new[i] <- B_new[i, 2] # random slope
  } 
  
  
  for (i in 1:n_new) {
    y_new[i] ~ dnorm(mu_new[i], tau_new)
    mu_new[i] <- beta0_new + V0_new[id_new[i]] + (beta1_new + V1_new[id_new[i]]) * time_new[i]
  }
}

write.model(update_model, "model/update.txt")
