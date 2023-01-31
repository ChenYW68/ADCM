  rm(list=ls())
  source("./code/LoadPackages/PSTVB_Packages.R")
  load("./code/3_Simulation/data/Simu_data.RData")
  source("./code/3_Simulation/Code/Simu_stData.R")
  data("SiteData", package = "ADCM")
  data("China_BTH_GeoMap", package = "ADCM")
  index <- which(colnames(Simu_data$Site) %in% c("LON_X", "LAT_Y"))
  Simu_data$Site  <- spCoords.transform(Simu_data$Site[, -index], method  = 1)
  
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
  para <- list( n = 1e3 #
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
  
  
  
  start <- c(1, 50)
  # z.temp <- seq(0, 1,, para$n*para$Nt)
  # ######################################################################
  #                       create table names
  # ######################################################################
  simu <- paste0("Sim_", para$n)
  # Simulation_wts <- paste0(simu, "_wts")
  Simulation_Para <- paste0(simu, "_ADCM_Linear")
  # Simulation_f <- paste0(simu, "_f")
  
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
  
  formula.exp <- paste0("y_ts ~ X1 + X2")
  # ######################################################################
  # ######################################################################
  
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/sim_MEnKS_EM.R")
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/EM.R")
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/sim_ADCM.R")
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/util.R")
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/MEnKS.R")
  source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/Construct_TestTrain_Data.R")
  
  Ch <- 0.2
  Cs <- 0.2
  save.Predict <- F
  for(iter in start[1]:start[2]){
    set.seed(iter)
    ADCM_Data <- Simu_stData(para = para, site = Simu_data0$Site,
                             W_ts = Simu_data0$W_ts)
    ######################################################################
    #                    basis matrix: H
    ######################################################################
    if(iter == start[1]){
      # ADCM_Data$siteid = "ID"
      # source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/CreateHmatrix.R")
      H.basic.data <- ADCM::CreateHmatrix(grid_coords = model.Grid,
                                          Geo_Map_Coord = China.province.map,
                                          method = c("Wendland"), #Wendland, indicator
                                          Site = Simu_data$Site, 
                                          factor = 1, 
                                          cs = Ch, 
                                          Knots.clip.ratio = .13)
      # range(H.basic.data$Hs)
    }else{
      H.basic.data <- ADCM::CreateHmatrix(grid_coords = model.Grid, 
                                          method = c("Wendland"), #Wendland, indicator, INLA
                                          Site = Simu_data$Site, 
                                          factor = 1, 
                                          cs = Ch, 
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
                           plot = F,
                           Object = "Simu",
                           response.transf = c("normal"),
                           Ensemble.size = 100,
                           ensemble.group = 1,
                           save.Predict = save.Predict,
                           cs = Cs, 
                           ct = 1,
                           tol.real = 1e-3, 
                           itMax = 2E1,
                           itMin = 5)[[1]]
    
    end_time.1 <- proc.time()
    run_time <- (end_time.1 - start_time.1)[3]
    # temp <- NULL
    
    
    Fit_Para <- data.table(iter = iter,
                           beta0 = CV.spModel$Para.est$beta.ensemble[1],
                           beta1 = CV.spModel$Para.est$beta.ensemble[2],
                           beta2 = CV.spModel$Para.est$beta.ensemble[3],
                           RMSE = tail(CV.spModel$run.Results$Testing_RMSE, 1),
                           Sigma.sq = CV.spModel$Para.est$Obs.sigma.sq$E_sigma.sq,
                           run_time = run_time)
    

    
    if(isTRUE(Database)){
      if((iter == 1)){
        sqlDrop(DSN_01, Simulation_Para, errors = F)
        # sqlDrop(DSN_01, Simulation_f, errors = F)
      }
      sqlSave(
        DSN_01,
        as.data.frame(Fit_Para),
        Simulation_Para,
        append = TRUE,
        colnames = FALSE,
        rownames = FALSE,
        safer = TRUE,
        fast = TRUE
      )
      
    }else{
      Simulation_Para <- rbind(Simulation_Para, setDT(as.data.frame(Fit_Para)))
    }
    
    
    cat("\n\n\n\n*************************************************\n")
    cat(".................. iter = ", iter + 1, "..................\n")
    cat("*************************************************\n\n\n\n")
    if((isTRUE(Database))&(iter >= start[2])){
      save(Simulation_Para, file = paste0("./data/Simulation/Sim_ADCM2_Para_", para$n, "_", start[1], "_", start[2], ".RData"))
    }
  }
  
  
  
  
