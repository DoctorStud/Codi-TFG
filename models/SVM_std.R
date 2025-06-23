# ===================
#    SVM std
# ===================

params_ <- c("alpha", "mu_", "phi", "sigma", "h")

SVM_std <- function()
{  
  for (t in 1:N) { 
    y[t] ~ dnorm(alpha, sigma_h[t]^-2) 
    sigma_h[t] <- sqrt(exp(h[t])) 
  }  
  h[1] <- mu_ 
  for(t in 2:N) { 
    h[t] ~ dnorm(mu_ + phi * h[t-1], sigma^-2) 
  }  
  alpha ~ dnorm(0, 100^-2) 
  mu_ ~ dnorm(0, 100^-2) 
  phi ~ dunif(-1, 1) 
  sigma ~ dunif(0,100)
}


