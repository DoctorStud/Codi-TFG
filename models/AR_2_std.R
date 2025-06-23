# ===================
#    AR(2) std
# ===================

params_ <- c("delta", "phi1", "phi2", "sigma")

AR_2_std <- function()
{
  y[1] ~ dnorm((delta)/(1-phi1-phi2), (sigma/(1-phi1^2-phi2^2))^-1)
  y[2] ~ dnorm((delta)/(1-phi1-phi2), (sigma/(1-phi1^2-phi2^2))^-1)
  for (t in 3:N) {
    y[t] ~ dnorm((delta) + phi1*y[t-1]+ phi2*y[t-2], (sigma/(1-phi1^2-phi2^2))^-1)
  }
  
  delta ~ dnorm(0, 1/1000)
  phi1 ~ dunif(-1, 1)
  phi2 ~ dunif(-1, 1)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1/tau
}