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
Simulation_Para <- paste0(simu, "_STAR_INLA")
# Simulation_f <- paste0(simu, "_f")



# ######################################################################
# ######################################################################
# ######################################################################
#                        mean function
# ######################################################################

formula.exp <- paste0("y_ts ~ X1 + X2")
# ######################################################################
load("./data/bth_map.RData")
Map_BTH <- fortify(bth_map) 
setnames(Map_BTH, c("long", "lat"), c("LON", "LAT"))
setDF(Map_BTH)
Map_BTH <- ADCM::spCoords.transform(Map_BTH, method = 1)
colnames(Map_BTH)
global.coords = as.matrix(Map_BTH[, c("LON", "LAT")])


mesh <- inla.mesh.2d(
  #boundary = boundary
  loc.domain = global.coords,
  loc = Site[, c("LON", "LAT")],
  # max.edge = c(.35, .65), #0.3,0.7
  # offset = c(1e-1, 0.5), #0.4, 0.6
  # cutoff = .05 # 0.5
  max.edge = c(.35, .65), #0.3,0.7
  offset = c(1e-1, 0.5), #0.4, 0.6
  cutoff = 0.04,
)
nx <- 3
# ######################################################################
for(iter in start[1]:start[2]){
  set.seed(iter)
  STAR_Data <- Simu_stData(para = para, site = Simu_data0$Site,
                           W_ts = Simu_data0$W_ts)
  ######################################################################
  #                       Train_Test_data
  ######################################################################
  train.id <- STAR_Data$Site[STAR_Data$Site$Simu == "Train", ]
  test.id <- STAR_Data$Site[STAR_Data$Site$Simu == "Test", ]
  
  Da.mod <- as.data.frame(Simu_data$Y_ts[, train.id$ID])
  Da.mod$time.index <- 1:para$Nt
  
  Da.mod <- gather(
                      data = Da.mod,
                      key = "ID",
                      value="Y_ts" ,
                      -time.index
                    )
  for(l in 2:nx){
    X <- as.data.frame(Simu_data$X_ts[l, , train.id$ID])
    X$time.index <- 1:para$Nt
    
    X <- gather(
                data = X,
                key = "ID",
                value= "X" ,
                -time.index
              )
    colnames(X)[3] <- paste0("X", l - 1)
    Da.mod <- Da.mod %>% left_join(X, by = c("ID", "time.index"))
  }
  
  
  # test data
  Da.pre <- as.data.frame(Simu_data$Y_ts[, test.id$ID])
  Da.pre$time.index <- 1:para$Nt
  
  Da.pre <- gather(
                      data = Da.pre,
                      key = "ID",
                      value = "Y_ts" ,
                      -time.index
                    )
  for(l in 2:nx){
    X <- as.data.frame(Simu_data$X_ts[l, , test.id$ID])
    X$time.index <- 1:para$Nt
    
    X <- gather(
                data = X,
                key = "ID",
                value = "X" ,
                -time.index
              )
    colnames(X)[3] <- paste0("X", l - 1)
    Da.pre <- Da.pre %>% left_join(X, by = c("ID", "time.index"))
  }
  Da.mod$ID <- as.numeric(Da.mod$ID)
  Da.pre$ID <- as.numeric(Da.pre$ID)
  
  Da.mod <- Da.mod %>% left_join(train.id, by = c("ID")) %>% setorderv(c("time.index", "ID"))
  Da.pre <- Da.pre %>% left_join(test.id, by = c("ID")) %>% setorderv(c("time.index", "ID"))
  
  
  colnames(Da.mod)
  setDF(Da.mod);setDF(Da.pre);
  #-------------------------------------------------------------------
  A.est = inla.spde.make.A(mesh,
                     loc = cbind(Da.mod$LON, Da.mod$LAT),
                     group = Da.mod$time.index,
                     n.group = para$Nt)
  
  #-------------------------------------------------------------------
  field.indices = inla.spde.make.index("field",
                                       n.spde = mesh$n,
                                       n.group = para$Nt) 
  
  stack.est =  inla.stack(data = list(Y_ts = Da.mod$Y_ts),
                          A = list(A.est, 1),
                          effects = list(c(field.indices,
                                           list(Intercept = 1)),
                                         list(Da.mod[, c("X1", "X2")])),
                          tag = "est")
  
  
  # test
  A.pred =
    inla.spde.make.A(mesh, loc = cbind(Da.pre$LON, Da.pre$LAT),
                     group = Da.pre$time.index, n.group = para$Nt)
  
  stack.pred =   inla.stack(data = list(Y_ts = NA),
                            A = list(A.pred, 1),
                            effects = list(c(field.indices,
                                             list(Intercept = 1)),
                                           list(Da.pre[, c("X1", "X2")])),
                            tag = "pred")
  
  #-- Create the "full" stack object (estimation + prediction)
  stack = inla.stack(stack.est, stack.pred)
  
  prior.range <- c(max(c(diff(range(Da.mod$LON)),
                         diff(range(Da.mod$LAT)))) * 1/20, 3)
  ## Medium sd, relative half-spread factor
  prior.sigma <- c(sd(Da.mod$REAL_PM25, na.rm = TRUE) / 2, 4)
  
  
  spde.alpha <- 1.5
  ## lognormal prior
  ## Centre the parameterisation at range=1, sigma=1
  # nu <- spde.alpha - 0.5
  # kappa.zero <- sqrt(8*nu) / 1
  # tau.zero <- (gamma(nu) / (gamma(spde.alpha) * 4*pi * kappa.zero^(2*nu)) )^0.5 / 1
  # ## sigma^2 = Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * kappa^(2*0.5) * tau^2 )
  # ## tau = [ Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * kappa^(2*0.5) ) ]^0.5 / sigma
  # ## kappa = sqrt(8*0.5) / range
  # ## tau = [ Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * (sqrt(8*0.5)/range)^(2*0.5) ) ]^0.5 / sigma
  # B.tau   <- cbind(log(tau.zero), nu, -1)
  # B.kappa <- cbind(log(kappa.zero), -1, 0)
  # 
  # theta.prior.mean <- log(c(prior.range[1], prior.sigma[2]))
  # ## Half-width of prior prediction interval, on log-scale: 2*sd = log(rel)
  # theta.prior.prec <- 4 / log(c(prior.range[2], prior.sigma[2]))^2
  # 
  
   spde <- inla.spde2.matern(mesh, alpha = spde.alpha,
                            # B.tau = B.tau, B.kappa = B.kappa,
                            # theta.prior.mean = theta.prior.mean,
                            # theta.prior.prec = theta.prior.prec,
                            constr = FALSE)
  
  # hyper.range.initial <- log(prior.range[1])
  # hyper.sigma.initial <- log(prior.sigma[1])
  
  # spde$f$hyper.default$theta1$initial <- hyper.range.initial
  # # spde$f$hyper.default$theta2$initial <- hyper.sigma.initial
  
  
  #-- Define the formula --#
  formula <- (Y_ts ~ -1 + Intercept + X1 +  X2  + 
                f(field, model = spde, group = field.group, 
                  control.group = list(model = "ar1")))
  
  t1 <- proc.time()
  mod <- inla(formula,
              data = inla.stack.data(stack, spde = spde),
              family = "gaussian",
              # control.family = list(hyper = list(theta = list(initial = 2))),
              control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
              control.compute = list(openmp.strategy = "large"),
              # control.mode=list(theta=mod.mode$mode$theta, restart=FALSE),
              keep = FALSE, 
              verbose = TRUE, 
              num.threads = 1:1,
              control.inla = list(reordering = "metis", 
                                  # tolerance = 1e-5, 
                                  # numint.maxfeval = 1E2,
                                  # numint.relerr = 1e-2,
                                  # numint.abserr = 1e-3,
                                  # h = 1e-5,
                                  strategy = "gaussian",
                                  int.strategy = "ccd"#,
                                  # force.diagonal = TRUE,
                                  # stupid.search = FALSE
              ))
  t2 <- proc.time()
  cat("\n\nEstimation of INLA takes time: \n")
  print(t2 - t1)
  run_time <- (t2 - t1)[3]
  print(summary(mod))
  
  # prec_post <- mod$marginals.hyperpar$"Precision for the Gaussian observations"

   prec_post <- mod$marginals.hyperpar[[1]]
   tau.sq <- inla.emarginal(fun = function(x) 1/x, marg = prec_post)
  ## ############################
  ## Mapping
  ## ############################
  ##--- Posterior mean of the linear predictor
  index.pred <- inla.stack.index(stack, "pred")$data
  # View(mod)

  Y_ts.Pred <- mod$summary.linear.predictor[index.pred, "0.5quant"] #+ 
  # mod$summary.random$field[index.pred, "0.5quant"]
 

  spT <- spT_validation(z = Da.pre$Y_ts, 
                        zhat = Y_ts.Pred, 
                        sigma = NA,
                        zhat.Ens = NULL, 
                        names = F, CC = F)#[c(1, 4)]
  print(spT)
  
  
  Fit_Para <- data.table(iter = iter,
                         beta0 = mod$summary.fixed$`0.5quant`[1],
                         beta1 = mod$summary.fixed$`0.5quant`[2],
                         beta2 = mod$summary.fixed$`0.5quant`[3],
                         tau.sq = tau.sq,
                         RMSE = spT[1],
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
    save(Simulation_Para, file = paste0("./data/Simulation/Sim_STAR_INLA_Para_", para$n, "_", start[1], "_", start[2], ".RData"))
  }
}




