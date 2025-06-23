# =================
#  GARCH(1,1) std
# =================

params_ <- c("alpha", "gamma_1", "gamma_2", "gamma_3", "sigma")

GARCH_1_1_std <- function()
{  
  for (t in 1:N) { 
    y[t] ~ dnorm(alpha, pow(sigma[t],-2)) 
  }  
  sigma[1] ~ dunif(0,10) 
  for(t in 2:N) { 
    sigma[t] <- sqrt(gamma_1 + gamma_2 * pow(y[t-1] - alpha, 2) + gamma_3 * pow(sigma[t-1], 2)) 
  }  
  
  alpha ~ dnorm(0, pow(10,-2)) 
  gamma_1 ~ dunif(0, 10) 
  gamma_2 ~ dunif(0, 1) 
  gamma_3 ~ dunif(0, 1) 
}

