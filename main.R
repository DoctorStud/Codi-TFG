library(dplyr)
library(R2jags)
library(doParallel)

load("../dades/df_update_12_2024.Rdata")

df <- df[df$date_month >= "2016-01-01" & df$date_month <= "2023-11-01", ]

AR_1_models <- c("AR_1_std", "AR_1_UR1", "AR_1_UR2", "AR_1_UR3", "AR_1_UR4")
AR_2_models <- c("AR_2_std", "AR_2_UR1", "AR_2_UR2", "AR_2_UR3", "AR_2_UR4")
AR_3_models <- c("AR_3_std", "AR_3_UR1", "AR_3_UR2", "AR_3_UR3", "AR_3_UR4")
GARCH_1_1_models <- c("GARCH_1_1_std", "GARCH_1_1_UR1", "GARCH_1_1_UR2", "GARCH_1_1_UR3", "GARCH_1_1_UR4")
SVM_models <- c("SVM_std", "SVM_UR1", "SVM_UR2", "SVM_UR3", "SVM_UR4")
std_models <- c("AR_1_std", "AR_2_std", "AR_3_std", "GARCH_1_1_std")

# Models que s'executaran:
models <- c(SVM_models)

for (m in models){
    source(paste0("models/",m,".R"))

    ncores <- detectCores()
    registerDoParallel(cores = ncores)
    cl <- makeCluster(ncores)

    data_route <- list(
      At = df$hhi_deaths_month,
      y = df$mortality_rate*100,
      N = length(df$mortality_rate)
    )

    clusterExport(cl, list("data_route", "params_"))

    print(m)

    system.time(assign(paste0("CMR_",m),
                       jags.parallel(
                         data = data_route,
                         inits = NULL,
                         parameters.to.save = params_,
                         model.file = get(m),
                         n.chains = 5,
                         n.iter = 1500,
                         n.burnin = 100,
                         n.thin = 50,
                         DIC = T,
                         jags.module = "mix"
                       )
            )
    )

    stopCluster(cl)

    save(list = paste0("CMR_",m), file = paste0("resultats/", "CMR_", m, ".RData"))
    closeAllConnections()
  }

