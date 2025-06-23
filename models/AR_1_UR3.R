# ===================
#    AR(1) UR3
# ===================

params_ <- c("q_b0", "w_b0", "w_b1", "delta", "phi", "sigma", "component_chosen")

AR_1_UR3 <- function()
{
  x[1] ~ dnorm((delta)/(1-phi), (sigma/(1-phi^2))^-1)
  for (t in 2:N) {
    x[t] ~ dnorm((delta) + phi*x[t-1], (sigma/(1-phi^2))^-1)
  }
  for (t in 1:N)
  {
    logit(q[t]) <- q_b0
    logit(omega[t]) <- w_b0 + w_b1 * At[t]
    mu[1, t] <- (delta)/(1-phi) 
    mu[2, t] <- q[t]*(delta)/(1-phi)
    tau.m[1, t] <- (sigma/(1-phi^2))^-1
    tau.m[2, t] <- (1/(q[t]^2))*(sigma/(1-phi^2))^-1
    y[t] ~ dnormmix(mu[, t], tau.m[, t], p.m[, t])
    p.m[1, t] <- 1-omega[t]
    p.m[2, t] <- omega[t]
    component_chosen[t] ~ dcat(p.m[, t])
  }
  q_b0 ~ dnorm(0, 0.01)
  w_b0 ~ dnorm(0, 0.01)
  w_b1 ~ dnorm(0, 0.01)
  delta ~ dnorm(0, 1/1000)
  phi ~ dunif(-1, 1)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1/tau
}