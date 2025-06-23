# ===================
#    AR(1) std
# ===================

params_ <- c("delta", "phi", "sigma")

AR_1_std <- function()
{
  y[1] ~ dnorm((delta)/(1-phi), (sigma/(1-phi^2))^-1)
  for (t in 2:N) {
    y[t] ~ dnorm((delta) + phi*y[t-1], (sigma/(1-phi^2))^-1)
  }

  delta ~ dnorm(0, 1/1000)
  phi ~ dunif(-1, 1)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1/tau
}

