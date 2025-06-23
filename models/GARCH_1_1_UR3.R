# ===================
#    GARCH(1,1) UR3
# ===================

params_ <- c("w_b0", "q_b0", "w_b1", "alpha", "gamma_1", "gamma_2", "gamma_3", "sigma", "component_chosen")

GARCH_1_1_UR3 <- function()
{  
  for (t in 1:N) { 
    x[t] ~ dnorm(alpha, pow(sigma[t],-2)) 
  }  
  sigma[1] ~ dunif(0,10) 
  for(t in 2:N) { 
    sigma[t] <- sqrt(gamma_1 + gamma_2 * pow(x[t-1] - alpha, 2) + gamma_3 * pow(sigma[t-1], 2)) 
  }  
  for (t in 1:N)
  {
    logit(q[t]) <- q_b0
    logit(omega[t]) <- w_b0 + w_b1 * At[t]
    mu[1, t] <- alpha
    mu[2, t] <- q[t]*alpha
    tau.m[1, t] <- pow(sigma[t],-2)
    tau.m[2, t] <- (1/(q[t]^2))*pow(sigma[t],-2)
    y[t] ~ dnormmix(mu[, t], tau.m[, t], p.m[, t])
    p.m[1, t] <- 1-omega[t]
    p.m[2, t] <- omega[t]
    component_chosen[t] ~ dcat(p.m[, t])
  }
  
  q_b0 ~ dnorm(0, 0.01)
  w_b0 ~ dnorm(0, 0.01)
  w_b1 ~ dnorm(0, 0.01)
  alpha ~ dnorm(0, pow(10,-2)) 
  gamma_1 ~ dunif(0, 10) 
  gamma_2 ~ dunif(0, 1) 
  gamma_3 ~ dunif(0, 1) 
}

