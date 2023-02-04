rm(list=ls())
source("./LoadPackages/RDependPackages.R")
# ######################################################################
#                       Oracle database
# ######################################################################
Database <- FALSE
if(isTRUE(Database)){
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd", believeNRows = FALSE, case = "toupper")
}
# ######################################################################
# ######################################################################
# ######################################################################
source("./3_Simulation/Simulation 1/Simu_stData.R")
load("./3_Simulation/Simulation 1/Simu_data.RData")
# ######################################################################
# ######################################################################
# ######################################################################
Simu_data0 <- Simu_data
para <- list( n = 1e2 #
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
if(!isTRUE(Database)){
simu <- paste0("Sim_MGCV_", para$n)
Simulation_Para <- paste0(simu, "_para")
}
start <- c(1, 200)

z.temp <- seq(0, 1,, para$n*para$Nt)
# ######################################################################
for(iter in start[1]:start[2]){
  run_time <- paste0(if_else(month(Sys.Date()) > 9
                             , as.character(month(Sys.Date()))
                             , paste0("0",  as.character(month(Sys.Date()))))
                     , "_", if_else(day(Sys.Date()) > 9
                                    , as.character(day(Sys.Date()))
                                    , paste0("0",  as.character(day(Sys.Date()))))
                     , "_", if_else(hour(Sys.time())> 9
                                    , as.character(hour(Sys.time()))
                                    , paste0("0",  as.character(hour(Sys.time()))))
                     
  )
  # load("./data/Simu_data.RData")
  set.seed(iter)
  Simu_data <- Simu_stData(para = para, site = Simu_data0$Site,
                           W_ts = Simu_data0$W_ts)

  nx <- dim(Simu_data$X_ts)[[1]]
  Train.site <- Simu_data$Site[Simu_data$Site$Simu == "Train", ]
  Train.Data <- cbind(as.vector(Simu_data$Y_ts[, Train.site$ID]),
                      as.vector(Simu_data$W_ts[, Train.site$ID]))
  for(l in 2:nx){
    Train.Data <- cbind(Train.Data, as.vector(Simu_data$X_ts[l, , Train.site$ID]))
  }
  # Train.Data <- as.data.frame(Train.Data)
  colnames(Train.Data) <- c("y_ts", "W_ts",dimnames(Simu_data$X_ts)[[1]][2:nx])
  Train.Data <- as.data.frame(Train.Data)
  
  Test.site <- Simu_data$Site[Simu_data$Site$Simu == "Test", ]
  Test.Data <- cbind(as.vector(Simu_data$Y_ts[, Test.site$ID]),
                     as.vector(Simu_data$W_ts[, Test.site$ID]))
  for(l in 2:nx){
    Test.Data <- cbind(Test.Data, as.vector(Simu_data$X_ts[l, , Test.site$ID]))
  }
  
  colnames(Test.Data) <- c("y_ts", "W_ts", dimnames(Simu_data$X_ts)[[1]][2:nx])
  Test.Data <- as.data.frame(Test.Data)
  
  
  bs <- " 'cc' "  
  Formula.express <- as.formula(paste0("y_ts ~ X1 + X2 + 
  s(time.index, k = ", 9, ", bs = ", bs, ", m = 2) +
  s(z, k = ", 50, ", bs = ", bs, ", m = 2) +
  s(LON, LAT, time.index, k = ", 200, ", bs = ", " 'tp' ", ", m = 2)
                                     "))
  
  Fit <- gam(Formula.express, data = Train.Data) 
  
  z.temp1 <- Train.Data$z
  Train.Data$z <- Train.Data$z_com <- z.temp
  # g.fun
  
  pred <- predict(Fit, newdata = Train.Data,
                  type = "terms", se.fit = F)
  ADCM::spT_validation()
  test.pred <- predict(Fit, newdata = Test.Data,
                  type = "response", se.fit = F)
  Fit_Para <- data.frame(iter = iter,
                         beta0 = as.vector(Fit$coefficients[1]),
                         beta1 = as.vector(Fit$coefficients[2]),
                         beta2 = as.vector(Fit$coefficients[3]),
                         G0 = as.vector(pred[, 3]),
                         G1 = pred[, 4],
                         W_ts_pre = pred[, 5],
                         W_ts = Train.Data$W_ts,
                         TIME = Train.Data$time.index,
                         Z = z.temp,
                         LON = Train.Data$LON_X,
                         LAT = Train.Data$LAT_Y,
                         Wtest_RMSE = RMSE(test.pred, Test.Data$y_ts)
                         )
  
  
  
  if(!isTRUE(Database)){
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

  cat("Fitted RMSE: ", RMSE(pred[, 5], Train.Data$W_ts))
  

  
  # RMSE(pred[, 4], g0(Train.Data$X5) - mean(g0(Train.Data$X5)))
  
  # smooth.num <- length(Fit$smooth)
  # if(smooth.num > 0){
  #   nr <- floor(sqrt(smooth.num + 1))
  #   for(i in 0:2){
  #     nc <- nr + i
  #     if(nr*nc >= smooth.num) break;
  #   }
  #   par(mfrow = c(nr, nc))
  #   op <- par(mar = c(4, 4, 2, 1) + 0.1)
  #   par(cex = 1)
  #   par(mgp = c(2, 1, 0))
  #   if(smooth.num >= 1){
  #     plot(Fit, residuals = TRUE, select = 1, 
  #          ylim = c(-5, 5), lwd = 4, se = F);
  #     for (sm in c(2:smooth.num)) {
  #       plot(Fit, residuals = TRUE, select = sm, 
  #            ylim = c(-5, 5), lwd = 4, se = F);#title("Fitting")
  #     }
  #   }
  # }
  
  
  cat("\n\n\n\n*************************************************\n")
  cat(".................. iter = ", iter + 1, "..................\n")
  cat("*************************************************\n\n\n\n")
  if(isTRUE(Database)&(iter >= start[2])){
    save(Simulation_Para, file = paste0("./data/Simulation/Sim_MGCV_", para$n, "_", start[1], "_", start[2], ".RData"))
  }
}


