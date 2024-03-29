rm(list=ls())
source("./LoadPackages/RDependPackages.R")
source("./3_Simulation/Simulation 2/Simu_stData.R")
load("./3_Simulation/Simulation 2/Simu_data.RData")
Simu_data0 <- Simu_data
assign("Simu.data", Simu_data, envir = .GlobalEnv)
# library(varycoef)
# ######################################################################
#                       save simulated data
# ######################################################################
Database <- TRUE
if(isTRUE(Database)){
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd", believeNRows = FALSE, case = "toupper")
}
# ######################################################################
#                       the setting of simulation parameters
# ######################################################################
sim.para <- list( n = 3e2 #
                  , Nt = 30
                  , beta = c(1.5e1, 1e0, 1e0)
                  , sill = c(1e0)
                  , scale_s = as.numeric(100)
                  , scale_t = as.numeric(2E-1) # the range in time dimension: (0, 1]
                  , delta = 2 #>= 2
                  , a = 1  #(0, 2]
                  , sep = 1.5 # space–time interaction: > 0
                  , nu = 3
                  , nugget = 1e-1)
# ######################################################################
#                       create table names
# ######################################################################
simu <- paste0("Sim_", sim.para$n, "_")
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_SVCT_MLE")
# Simulation_f <- paste0(simu, "_f")
start <- c(1, 50)
nx <- dim(Simu_data$X_ts)[[1]]

# ######################################################################
# ######################################################################
index <- which(colnames(Simu_data0$Site) %in% c("LON_X", "LAT_Y"))
Simu_data0$Site  <- spCoords.transform(Simu_data0$Site[, -index], method  = 1)
for(iter in start[1]:start[2]){
  set.seed(iter)
  SVC_lme_Data <- Simu_stData(para = sim.para, site = Simu_data0$Site,
                              W_ts = Simu_data0$W_ts)
  ######################################################################
  #                       Train_Test_data
  ######################################################################
  train.id <- SVC_lme_Data$Site[SVC_lme_Data$Site$Simu == "Train", ]
  test.id <- SVC_lme_Data$Site[SVC_lme_Data$Site$Simu == "Test", ]
  

  
  for(t in 1:sim.para$Nt){
    
    Train.Data <- cbind(as.vector(Simu_data$Y_ts[t, train.id$ID]),
                        as.vector(Simu_data$W_ts[t, train.id$ID]))
    for(l in 2:nx){
      Train.Data <- cbind(Train.Data, as.vector(Simu_data$X_ts[l, t, train.id$ID]))
    }
    # Train.Data <- as.data.frame(Train.Data)
    colnames(Train.Data) <- c("y_ts", "W_ts",dimnames(Simu_data$X_ts)[[1]][2:nx])
    Da.mod <- as.data.frame(Train.Data)
    #training data
    
    
    Test.Data <- cbind(as.vector(Simu_data$Y_ts[t, test.id$ID]),
                       as.vector(Simu_data$W_ts[t, test.id$ID]))
    for(l in 2:nx){
      Test.Data <- cbind(Test.Data, as.vector(Simu_data$X_ts[l, t, test.id$ID]))
    }
    # Train.Data <- as.data.frame(Train.Data)
    colnames(Test.Data) <- c("y_ts", "W_ts",dimnames(Simu_data$X_ts)[[1]][2:nx])
    Da.pre <- as.data.frame(Test.Data)
    
    
    # fitting model and prediction
    star.time <- proc.time()
    X <- model.matrix(~1 + X1  + X2, data = Da.mod)
    coords <- as.matrix(train.id[, c("LON_X", "LAT_Y")])
    range(rdist(coords))
    fit_svc <- SVC_mle(y = Da.mod$y_ts, X = X 
                       , W = matrix(X[, 1], ncol = 1),
                       , locs = coords
                       , control = SVC_mle_control(
                         cov.name = "exp"
                         #   # inital values for 3 SVC
                         #   # 7 = (3 * 2 covariance parameters + nugget)
                         #   init = c(rep(c(0.4, 0.2), 3), 0.2),
                         , profileLik = TRUE
                         
                         , tapering = .6*max(rdist(coords))
                       )
                       # , optim.control = list(reltol = 1e-5)
    )
    
    X <- model.matrix(~1 + X1  + X2, data = Da.pre)
    coords <- as.matrix(test.id[, c("LON_X", "LAT_Y")])
    pred_svc <- predict(fit_svc, newX = X 
                        , newW = matrix(X[, 1], ncol = 1)
                        , newlocs = coords)
    
    
    spT <- spT_validation(Da.pre$y_ts,  pred_svc$y.pred,
                          sigma = NA, zhat.Ens = NULL, 
                          names = F, CC = F)[c(1, 4)]
    
    
    
    end.time <- proc.time()
    run_time <- (end.time - star.time)[3] 
    
    if(t == 1){
      Fit_Para <- data.table(iter = iter,
                             TIME = t,
                             beta0 = fit_svc$coefficients[1],
                             beta1 = fit_svc$coefficients[2],
                             beta2 = fit_svc$coefficients[3],
                             tau.sq = fit_svc$cov.par[3],
                             RMSE = spT[1],
                             run_time = run_time
                            )
    }else{
      Fit_Para <- rbind(Fit_Para, data.table(iter = iter,
                             TIME = t,
                             beta0 = fit_svc$coefficients[1],
                             beta1 = fit_svc$coefficients[2],
                             beta2 = fit_svc$coefficients[3],
                             tau.sq = fit_svc$cov.par[3],
                             RMSE = spT[1],
                             run_time = run_time)
      )
    }
    cat(paste0("Iter =  ", iter, "; time = ", t,
               "; tau.sq =  ", round(fit_svc$cov.par[3], 3),
               "; n =  ", sim.para$n,
               "; run_time = ", round(run_time), " seconds.", "\n\n"))
    print(spT)
  }
    
    if(isTRUE(Database)){
      if((iter == 1)){
        sqlDrop(DSN_01, Simulation_Para, errors = F)
      }
      sqlSave(
        DSN_01,
        Fit_Para,
        Simulation_Para,
        append = TRUE,
        colnames = FALSE,
        rownames = FALSE,
        safer = TRUE,
        fast = TRUE
      )}else{
        if((iter == 1)){ Simulation_Para = NULL}
        Simulation_Para <- rbind(Simulation_Para, setDT(Fit_Para))
        
      }
  cat("\n\n\n\n*************************************************\n")
  cat(".................. iter = ", iter + 1, "..................\n")
  cat("*************************************************\n\n\n\n")
  if((isTRUE(Database))&(iter >= start[2])){
    save(Simulation_Para, file = paste0("./data/Simulation/Sim_SVC_Para_", sim.para$n, "_", start[1], "_", start[2], ".RData"))
  }
}