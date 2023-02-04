# remove.packages("ADCM")
# install.packages("./code/LoadPackages/ADCM_1.0.zip", 
#                  repos = NULL, type = "win.binary")
rm(list = ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "ADCM")
data("China_BTH_GeoMap", package = "ADCM")

######################################################################
# set tuning parameters
######################################################################
Ch <- .3; Cs <- .1; Ct <- 1; Ne <- 100
######################################################################
######################################################################
# provide a name for all objects that will be saved
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]
# tab <- paste0(tab.1, "_", tab.2, "_", Ct)
tab <- paste0("_", tab.1, "_", tab.2)
hdcm.table <- paste0( "ADCM", tab)
######################################################################
# Combine other variables with time variable
######################################################################
DATE_TIME <- unique(obs_PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        DATE_TIME = DATE_TIME)
Model_Base_Table <- obs_PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))
######################################################################
######################################################################
# do transformation for some variables 
Model_Base_Table[, c("sim50_CMAQ_PM25")] <- sqrt(Model_Base_Table[, c("sim50_CMAQ_PM25")])
setDT(Model_Base_Table)
######################################################################
######################################################################
# Do grids and create a basis matrix H
######################################################################
colnames(Map_BTH)[1:2] <- c("LON", "LAT")
Boundary <- as.data.frame(t(bbox(China.province.map)))
names(Boundary) <- c("LON", "LAT")
model.Grid <- makeGrids(Boundary, nLayer = 2, NC = 15, nBuffer = 3.5) 
H.basic.data <- CreateHmatrix(grid_coords = model.Grid, 
                              Geo_Map_Coord = China.province.map,
                              method = c("Wendland"), 
                              Site = Site, 
                              factor = 1, 
                              Ch = Ch, 
                              distance.scale = 1e3,
                              Knots.clip.ratio = .10,
                              hs.normal = FALSE) 
######################################################################
#                Constructing Data forms used in the ADCM 
######################################################################
ADCM_Data <- Construct_ADCM_Data(data = Model_Base_Table,
                                 include = list(
                                   YEAR = c(2015, 2016),
                                   month_day = c("11-01", "1-31")
                                 ),
                                 Y = "REAL_PM25",
                                 X = c("sim50_CMAQ_PM25"
                                       , "time.scale"
                                       , "sim_TEMP"
                                       , "sim_SPRESS"
                                       , "sim_WIND_X"
                                       , "sim_WIND_Y"
                                 ), 
                                 standard = T, 
                                 center = T, 
                                 start.index = 2)
######################################################################
######################################################################
#                               Model setting
######################################################################
{
  ######################################################################
  #                        initialize  parameters
  ######################################################################
  nx <- dim(ADCM_Data$X_ts)[1]
  zeta <- sqrt(50/(2*sqrt(2)))
  zeta
  para <- list(beta = list(E_beta = c(3.5, 0.5, rep(0, nx - 2))), 
               rho = list(E_rho = 1e-1) 
               , zeta = list(E_zeta = zeta, lower = 1E-3, upper = 1e1)
               , zeta0 = list(E_zeta0 = zeta, lower = 1E-3, upper = 1e1)
               , phi = list(E_phi = 5e1, lower = 1e1, upper = 5e2) 
               , Obs.sigma.sq = list(E_sigma.sq = 1) 
               , Proc0.tau.sq = list(E_tau.sq = 1)
               , Proc.tau.sq = list(E_tau.sq = 1)
              )
}
######################################################################
######################################################################
#                 the formula for additive mdoels
######################################################################
# the list of parameters for the nonlinear part 
bs <- " 'cc' "; k <- 5
formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25  +
s(time.scale, k = ", k + 4, ", bs = ", bs, ", m = 2) +
s(sim_TEMP, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_SPRESS, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_X, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_Y, k = ", k + 4, ", bs = ", bs, ", m = 2)")
# formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25")
######################################################################
######################################################################
#                           fit model and prediction
######################################################################
star.time <- proc.time()
cv.ADCM <- ADCM(Mean.formula = formula.exp,
                Tab = hdcm.table,
                Site = Site, 
                ADCM.Data = ADCM_Data, 
                H.basic.data = H.basic.data,
                para = para, 
                Ensemble.size = Ne,
                CV = FALSE, 
                plot = TRUE,
                Database = NULL, #list(
                # DSN = odbcConnect(dsn = "DSN_01", 
                #                   uid = "myname",
                #                   pwd = "mypwd",
                #                   believeNRows = FALSE,
                #                   case = "toupper")),
                verbose.EM = TRUE,
                verbose = TRUE, 
                Object = "CITY",
                response.transf = c("sr"),
                itMin = 1e1,
                cs = Cs, 
                ct = Ct,
                tol.real = 1e-2, 
                itMax = 5e1, 
                Obj.Seq = 1)
end.time <- proc.time()
run_time <- (end.time - star.time)[3] 
# })
######################################################################
######################################################################
save(Fit, file = "./2_Calibration/all_Fit.RData")

