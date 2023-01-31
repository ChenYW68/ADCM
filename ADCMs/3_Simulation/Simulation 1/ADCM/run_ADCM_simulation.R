# generate the process wts
# source("./code/4_Simulation/ADCM/generate_simuData.R")
# ITR = sample(1:1000, 1)
# set.seed(888)
# quantile(seq(0, 1,, para$Nt), prob = 0.2)
# para <- list( n = 2499 #
#               , Nt = 30
#               , beta = c(1.5e1, 1e0, 1e0)
#               , sill = c(1e0)
#               , scale_s = as.numeric(100)
#               , scale_t = as.numeric(0.2) # time is scaled to [0, 1]
#               , delta = 2 #>= 2
#               , a = 1  #(0, 2]
#               , sep = 1.5 # space–time interaction: > 0
#               , nu = 3
#               , nugget = 1e-1)
# 
# Simu_data <- Simu.spCali(para = para)


rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
source("./code/3_Simulation/ADCM/Simu_stData.R")
data("SiteData", package = "ADCM")
data("China_BTH_GeoMap", package = "ADCM")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/sim_MEnKS_EM.R")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/EM.R")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/sim_ADCM.R")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/util.R")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/MEnKS.R")
source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/Construct_TestTrain_Data.R")
save.Predict <- F

load("./data/Simu_data.RData")
Simu_data0 <- Simu_data
assign("Simu.data", Simu_data, envir = .GlobalEnv)
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
start <- c(151, 200)
z.temp <- seq(0, 1,, para$n*para$Nt)
# ######################################################################
#                       create table names
# ######################################################################
simu <- paste0("Sim_", para$n)
Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_para")
Simulation_f <- paste0(simu, "_f")

# ######################################################################
# ######################################################################
# Boundary <- data.frame(LON = c(113.4587, 119.8483),
#                        LAT = c(36.04610, 42.61761))
# Map_BTH <- fortify(China.province.map)
# # colnames(Map_BTH)[1:2] <- c("LON", "LAT")
# assign("Map_BTH", Map_BTH, envir = .GlobalEnv)
Boundary <- as.data.frame(t(bbox(China.province.map)))
names(Boundary) <- c("LON", "LAT")
# ######################################################################
# ######################################################################
R <- 2
model.Grid = ADCM::makeGrids(Boundary, R, 15, 3.5)
# ######################################################################
# ######################################################################
# ######################################################################
#                        mean function
# ######################################################################
bs <- " 'cc' "
formula.exp <- paste0("y_ts ~ X1 + X2 +
  s(time.index, k = ", 9, ", bs = ", bs, ", m = 2) + 
  s(z, k = ", 50, ", bs = ", bs, ", m = 2)")
# ######################################################################
# ######################################################################
for(iter in start[1]:start[2]){
  set.seed(iter)
  ADCM_Data <- Simu_stData(para = para, site = Simu_data0$Site,
                           W_ts = Simu_data0$W_ts)
  ######################################################################
  #                    basis matrix: H
  ######################################################################
  if(iter == start[1]){
    H.basic.data <- ADCM::CreateHmatrix(grid_coords = model.Grid,
                                        Geo_Map_Coord = China.province.map,
                              method = c("Wendland"), #Wendland, indicator
                              Site = Simu_data$Site, 
                              factor = 1, 
                              cs = 0.2, 
                              Knots.clip.ratio = .13)
  }else{
    H.basic.data <- ADCM::CreateHmatrix(grid_coords = model.Grid, 
                              method = c("Wendland"), #Wendland, indicator, INLA
                              Site = Simu_data$Site, 
                              factor = 1, 
                              cs = 0.2, 
                              Knots.clip.ratio = .13,
                              Re = H.basic.data)
  }
  
  # pch <- c(3, 4, 20, 18)
  # col <- c("red", "blue", "gray30")
  # cex.2 <- c(2, 1.5, 0.8)
  # plot(China.province.map, xlim = c(112, 122), 
  #      ylim = c(35, 44), lwd = 2)
  # if(length(model.Grid) > 0){
  #   for (g in 1:length(model.Grid)) {
  #     points(H.basic.data$Grid.infor$level[[g]][["latCoords"]][, 1], 
  #            H.basic.data$Grid.infor$level[[g]][["latCoords"]][, 2], 
  #            pch = pch[g], col = col[g], cex = cex.2[g])
  #     print(nrow(H.basic.data$Grid.infor$level[[g]][["latCoords"]]))
  #   }
  # }
  # which(H.basic.data$Grid.infor$level[[1]]$adjacent.matrix[2,]!=0)
  # print(H.basic.data$Grid.infor$summary$N.BAUs)
  # print(H.basic.data$Grid.infor$summary$Knots.count)
  # # View(H.basic.data$Grid.infor)
  # # plot(H.basic.data$Hs[1,])
  # sum(H.basic.data$Hs[2,])
  # max(range(H.basic.data$Grid.infor$summary$Hdist))*0.12
  # max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist)*0.21
  ######################################################################
  #                         Model set
  ######################################################################
  
  ######################################################################
  {
    p1 = dim(ADCM_Data$X_ts)[1]
    ######################################################################
    #                        initialize  parameters
    ######################################################################
    dis <- max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist)*0.1
    Para <- list(rho = list(E_rho = 1e-5)
                 , zeta = list(E_zeta = 1)
                 , zeta0 = list(E_zeta0 = 1)
                 , phi = list(E_phi = dis) 
                 , Obs.sigma.sq = list(E_sigma.sq = 1E0)
                 , Proc.tau.sq = list(E_tau.sq = 1e1)
                 , Proc0.tau.sq = list(E_tau.sq = 1e0)
    )
  }

  
    
  ######################################################################
  #                           fit model and prediction
  ######################################################################
  # Total <- FALS
  # ADCM_Data$siteid = "ID"
  start_time.1  <- proc.time()
  CV.spModel <- sim.ADCM(Mean.formula = formula.exp, 
                         Tab = paste0(simu),
                          Site = ADCM_Data$Site, 
                          ADCM.Data = ADCM_Data, 
                          H.basic.data = H.basic.data,
                          para = Para,  
                          Database = NULL,#list(DSN = odbcConnect("DSN_01", 
                                       #                     uid = "myname", 
                                       #                     pwd = "mypwd", 
                                       #                     believeNRows = FALSE, 
                                        #                    case = "toupper")), 
                          verbose.EM = TRUE,
                          verbose = TRUE, 
                          Object = "Simu",
                          response.transf = c("normal"),
                          Ensemble.size = 100,
                          ensemble.group = 1,
                          save.Predict = save.Predict,
                          cs = 0.2, 
                          ct = 1,
                          tol.real = 1e-2, 
                          itMin = 5 ,
                          itMax = 5E1)[[1]]
  
  end_time.1 <- proc.time()
  print(end_time.1 - start_time.1)
  # temp <- NULL
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
  # View(CV.spModel)
  # if(!Total){
  #   CV.spModel <- CV.spModel[[1]]
  # }
  
  ######################################################################
  #                           Random process Wts
  ######################################################################
  CV.spModel$w_ts_pre$ITER <- iter
  if(isTRUE(Database)){
    if((iter == 1)){
      sqlDrop(DSN_01, Simulation_wts, errors = F)
    }
    sqlSave(
      DSN_01,
      CV.spModel$w_ts_pre,
      Simulation_wts,
      append = TRUE,
      colnames = FALSE,
      rownames = FALSE,
      safer = TRUE,
      fast = TRUE
    )
  }else{
    if(iter == 1){
      Simulation_wts <- Simulation_Para <- Simulation_f <- NULL
    }
    Simulation_wts <- rbind(Simulation_wts, setDT(CV.spModel$w_ts_pre))
  }

  ######################################################################
  #                           Parameters
  ######################################################################
  beta.num  <- length(summary(CV.spModel$addtive.Fit$mean.ensemble.fit[[1]]$Fit)[[1]])
  beta.name <- names(CV.spModel$addtive.Fit$mean.ensemble.fit[[1]]$Fit$coefficients[1:beta.num])
  beta.estimate <- as.vector(colMeans(CV.spModel$data$ensemble$beta.ensemble))
  sigma.sq.estimate <- CV.spModel$Para.est$Obs.sigma.sq$E_sigma.sq
  

  ##   parameter estimation
  Para.est <- matrix(c(iter, beta.estimate,
                       sigma.sq.estimate,
                       CV.spModel$Para.est$rho$E_rho,
                       CV.spModel$Para.est$phi$E_phi,
                       CV.spModel$Para.est$zeta$E_zeta,
                       CV.spModel$Para.est$zeta0$E_zeta0,
                       CV.spModel$Para.est$Proc.tau.sq$E_tau.sq,
                       CV.spModel$Para.est$Proc0.tau.sq$E_tau.sq), nrow = 1)
  
  colnames(Para.est) <- c("Iter", paste0("beta", 1:beta.num), 
                          "E_sigma_sq",
                          paste0("rho_", 1:CV.spModel$data$Grid.infor$summary$res), 
                          paste0("Phi_", 1:CV.spModel$data$Grid.infor$summary$res),
                          paste0("zeta_", 1:CV.spModel$data$Grid.infor$summary$res), 
                          paste0("zeta0_", 1:CV.spModel$data$Grid.infor$summary$res),
                          paste0("E_tau_sq_", 1:CV.spModel$data$Grid.infor$summary$res), 
                          paste0("E_tau0_sq_", 1:CV.spModel$data$Grid.infor$summary$res))
  if(isTRUE(Database)){
    if((iter == 1)){
      sqlDrop(DSN_01, Simulation_Para, errors = F)
      # sqlDrop(DSN_01, Simulation_f, errors = F)
    }
    sqlSave(
      DSN_01,
      as.data.frame(Para.est),
      Simulation_Para,
      append = TRUE,
      colnames = FALSE,
      rownames = FALSE,
      safer = TRUE,
      fast = TRUE
    )
    
  }else{
    Simulation_Para <- rbind(Simulation_Para, setDT(as.data.frame(Para.est)))
  }
  
  ######################################################################
  #                           Smoothing functions
  ######################################################################
  z.temp1 <- CV.spModel$addtive.fit.Data$z
  CV.spModel$addtive.fit.Data$z <- CV.spModel$addtive.fit.Data$z_com <- z.temp
  # g.fun <- NULL
  g0.x <- g1.x <- se_g0.x <- se_g1.x <- matrix(0, nrow = nrow(CV.spModel$addtive.fit.Data), 
                                               ncol =  CV.spModel$data$ensemble$ensemble.group)
  
  g.fun <- list(g0 = g0.x, se_g0 = se_g0.x, g1 = g1.x, se_g1 = se_g1.x)
  for(g in 1:CV.spModel$data$ensemble$ensemble.group){
    pred <- predict(CV.spModel$addtive.Fit$mean.ensemble.fit[[g]]$Fit, 
                    newdata = CV.spModel$addtive.fit.Data,
                    type = "terms", se.fit = TRUE)
    g.fun$g0[, g] <- pred$fit[, 3]
    g.fun$se_g0[, g] <- pred$se.fit[, 3]
    g.fun$g1[, g] <- pred$fit[, 4]
    g.fun$se_g1[, g] <- pred$se.fit[, 4]
  }
  
  
  g0.50 <- g.fun$g0[, 1]
  se_g0.quan.estimate <- g.fun$se_g0[, 1]
  g0.25 <- g0.975 <- g0.sd.estimate <- NA 
  
  g1.50 <- g.fun$g1[, 1]
  se_g1.quan.estimate <- g.fun$se_g1[, 1]
  g1.25 <- g1.975 <- g1.sd.estimate <- NA  
  # 
  # plot(z.temp, g1.50, type = "l", col = "red")
  # points(z.temp, g1(z.temp) - mean(g1(z.temp)), type = "l", col = "black")
  
  
  pred <-  data.frame(Iter = iter, 
                      time = CV.spModel$addtive.fit.Data$time.index,
                      g0 = g0(CV.spModel$addtive.fit.Data$time.index) - 
                        mean(g0(CV.spModel$addtive.fit.Data$time.index)),
                      g0_25 = g0.25, g0_50 = g0.50, g0_975 = g0.975,
                      se_g0_ensemble = se_g0.quan.estimate,
                      se_g0_model = g0.sd.estimate,
                      z = z.temp,
                      g1 = g1(z.temp) - mean(g1(z.temp)),
                      g1_25 = g1.25, 
                      g1_50 = g1.50, 
                      g1_975 = g1.975,
                      se_g1_ensemble = se_g1.quan.estimate,
                      se_g1_model = g1.sd.estimate)
  setDT(pred)
  # dev.off()
  # if(!is.null(Database)){
  #   pdf(file = paste0("./figure/", Simulation_f ,".pdf"),width = 14, height = 4)
  # }else{
  #   pdf(file = paste0("./figure/estimated_func.pdf"),width = 14, height = 4) 
  # }
  # par(mfrow = c(1, 2))
  # op <- par(mar = c(4, 4, 2, 1) + 0.1) 
  # par(cex = 1)
  # par(mgp = c(2, 1, 0))
  # col <- c("red", "blue")
  # # head(pred)
  # time <- seq(0, 1, , para$Nt*para$n)
  # plot(time, g0(time) - mean(g0(time)), xlab = "t", 
  #      ylab =latex2exp::TeX(r"($g_0(t)$)"), 
  #      col = col[1], type = "l", lwd = 3, ylim = c(-4, 6))
  # points(pred$time, pred$g0_50, col = col[2], cex = 1, pch = 19)
  # legend(x = 0.0, y = 4, legend = c("True function", "Estimated value at sampling points"), 
  #        col = col, ncol = 1, 
  #        pch = as.numeric(c(".", 19)),
  #        lty = c("solid", "blank"),
  #        lwd = 3, bty = "n",
  #        cex = 1.0)
  
  
  # plot(z.temp, pred$g1_50, col = col[2], ylim = c(-4, 6),
  #      cex = 1, pch = 19, xlab = "z", ylab = latex2exp::TeX(r"($g_1(z)$)"))
  # points(z.temp, pred$g1,  col = col[1], type = "l", lwd = 3)
  # legend(x = 0.4, y = 4, legend = c("True function", "Estimated value at sampling points"), 
  #        col = col, ncol = 1, 
  #        pch = as.numeric(c(".", 19)),
  #        lty = c("solid", "blank"),
  #        lwd = 3, bty = "n",
  #        cex = 1.0)
  # dev.off()

  CV.spModel$addtive.fit.Data$z <- z.temp1
  pred.y <- predict(CV.spModel$addtive.Fit$mean.ensemble.fit[[1]]$Fit,
                    newdata = CV.spModel$addtive.fit.Data,
                    type = "response", se.fit = TRUE)
  pred$y_pre <- pred.y$fit
  pred$y_se_pre <- pred.y$se.fit
  pred$y_ts <- CV.spModel$addtive.fit.Data$y_ts
 
  pred <- data.frame(pred, run_time) 
  

  if(isTRUE(Database)){
    if((iter == 1)){
      sqlDrop(DSN_01, Simulation_f, errors = F)
    }
    sqlSave(
    DSN_01,
    pred,
    Simulation_f,
    append = TRUE,
    colnames = FALSE,
    rownames = FALSE,
    safer = TRUE,
    fast = TRUE)
    }else{
    Simulation_f <- rbind(Simulation_f, setDT(pred))
  }
  cat("\n\n\n\n*************************************************\n")
  cat(".................. iter = ", iter + 1, "..................\n")
  cat("*************************************************\n\n\n\n")
  if((isTRUE(Database))&(iter >= start[2])){
    save(Simulation_Para, file = paste0("./data/Simulation/Sim_ADCM_Para_", para$n, "_", start[1], "_", start[2], ".RData"))
    save(Simulation_wts, file = paste0("./data/Simulation/Sim_ADCM_wts_", para$n, "_", start[1], "_", start[2], ".RData"))
    save(Simulation_f, file = paste0("./data/Simulation/Sim_ADCM_f_", para$n, "_", start[1], "_", start[2], ".RData"))
  }
}




