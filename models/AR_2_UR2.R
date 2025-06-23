# ===================
#    AR(2) UR2
# ===================

params_ <- c("q_b0", "q_b1", "w_b0", "delta", "phi1", "phi2", "sigma", "component_chosen")

AR_2_UR2 <- function()
{
  x[1] ~ dnorm((delta)/(1-phi1-phi2), (sigma/(1-phi1^2-phi2^2))^-1)
  x[2] ~ dnorm((delta)/(1-phi1-phi2), (sigma/(1-phi1^2-phi2^2))^-1)
  for (t in 3:N) {
    x[t] ~ dnorm((delta) + phi1*x[t-1]+ phi2*x[t-2], (sigma/(1-phi1^2-phi2^2))^-1)
  }
  for (t in 1:N)
  {
    logit(q[t]) <- q_b0 + q_b1 * At[t]
    logit(omega[t]) <- w_b0
    mu[1, t] <- (delta)/(1-phi1-phi2)
    mu[2, t] <- q[t]*(delta)/(1-phi1-phi2)
    tau.m[1, t] <- (sigma/(1-phi1^2-phi2^2))^-1
    tau.m[2, t] <- (1/(q[t]^2))*(sigma/(1-phi1^2-phi2^2))^-1
    y[t] ~ dnormmix(mu[, t], tau.m[, t], p.m[, t])
    p.m[1, t] <- 1-omega[t]
    p.m[2, t] <- omega[t]
    component_chosen[t] ~ dcat(p.m[, t])
  }
  
  q_b0 ~ dnorm(0, 0.01)
  q_b1 ~ dnorm(0, 0.01)
  w_b0 ~ dnorm(0, 0.01)
  delta ~ dnorm(0, 1/1000)
  phi1 ~ dunif(-1, 1)
  phi2 ~ dunif(-1, 1)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1/tau
}