# ===================
#    SVM UR2
# ===================

params_ <- c("q_b0", "q_b1", "w_b0","alpha", "mu_", "phi", "sigma", "h", "component_chosen")

SVM_UR2 <- function()
{  
  for (t in 1:N) { 
    x[t] ~ dnorm(alpha, sigma_h[t]^-2) 
    sigma_h[t] <- sqrt(exp(h[t])) 
  }  
  h[1] <- mu_ 
  for(t in 2:N) { 
    h[t] ~ dnorm(mu_ + phi * h[t-1], sigma^-2) 
  }  
  for (t in 1:N)
  {
    logit(q[t]) <- q_b0+q_b1*At[t]
    logit(omega[t]) <- w_b0
    mu[1, t] <- alpha
    mu[2, t] <- q[t]*alpha
    tau.m[1, t] <- sigma_h[t]^-2
    tau.m[2, t] <- (1/(q[t]^2))*sigma_h[t]^-2
    y[t] ~ dnormmix(mu[, t], tau.m[, t], p.m[, t])
    p.m[1, t] <- 1-omega[t]
    p.m[2, t] <- omega[t]
    component_chosen[t] ~ dcat(p.m[, t])
  } 
  
  
  q_b0 ~ dnorm(0, 0.01)
  q_b1 ~ dnorm(0, 0.01)
  w_b0 ~ dnorm(0, 0.01)
  sigma ~ dunif(0,100)
  alpha ~ dnorm(0, 100^-2) 
  mu_ ~ dnorm(0, 100^-2) 
  phi ~ dunif(-1, 1) 
}
