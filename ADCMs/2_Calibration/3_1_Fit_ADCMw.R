rm(list = ls())
source("./code/LoadPackages/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
data("China_BTH_GeoMap", package = "ADCM")

# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/MEnKS_EM.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/EM.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/ADCM.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/util.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/MEnKS.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/Construct_TestTrain_Data.R")
# source("I:/OneDrive - 中山大学/ThesisCode/NonDynamiCalib/ADCM/R/CreateHmatrix.R")
# # #############################################################################################
# ## the list of additiva moeld parameters
# #############################################################################################
hdcm.table <- "ADCM"
Ch <- .3
Cs <- .1
tau.sq <- c(1, 1)
# phi <- c(1e2, 1e1, 5e2)


{
  ######################################################################
  #                        initialize  parameters
  ######################################################################
  # dis <- max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist)*0.1
  nx <- 7#dim(ADCM_Data$X_ts)[1]
  zeta <- sqrt(50/(2*sqrt(2)))
  zeta
  para <- list(beta = list(E_beta = c(3.5, 0.5, rep(0, nx - 2))), 
               rho = list(E_rho = 1e-1) #1e-5
               ##  Q = tau.sq*(G + zeta*G)
               , zeta = list(E_zeta = zeta, lower = 1E-3, upper = 1e1)
               , zeta0 = list(E_zeta0 = zeta, lower = 1E-3, upper = 1e1)
               , phi = list(E_phi = 5e1, lower = 1e1, upper = 5e2) 
               , Obs.sigma.sq = list(E_sigma.sq = 1) 
               , Proc0.tau.sq = list(E_tau.sq = tau.sq[1])
               , Proc.tau.sq = list(E_tau.sq = tau.sq[2])
  )# 0.001,0.1; cs:0.15 ct = 5
}
##############################################################################################
Ct <- 1; Ne <- 100
##############################################################################################
# the list of parameters for the nonlinear part 
bs <- " 'cc' "; k <- 5
formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25  +
s(time.index, k = ", k + 4, ", bs = ", bs, ", m = 2) +
s(sim_TEMP, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_SPRESS, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_X, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_Y, k = ", k + 4, ", bs = ", bs, ", m = 2)")

# formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25")
##############################################################################################
##############################################################################################
Base_CAQRA_Table <- obs_PM25_2015w
setDF(Base_CAQRA_Table)

DATE_TIME <- unique(Base_CAQRA_Table$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        DATE_TIME = DATE_TIME)
Base_CAQRA_Table <- Base_CAQRA_Table  %>% left_join(date.time, 
                                                    by = c("DATE_TIME"))

# Map_BTH <- fortify(China.province.map)
colnames(Map_BTH)[1:2] <- c("LON", "LAT")
Boundary <- as.data.frame(t(bbox(China.province.map)))
names(Boundary) <- c("LON", "LAT")


R <- 2
pch <- c("+", "*", ".", "+")
col <- c("red", paste0("gray", round(seq(10, 80,, R))))


# par(mfrow = c(1, 1))
model.Grid <- makeGrids(Boundary, R, 15, 3.5) #12, 1.5) 15

# plot(China.province.map, xlim = c(112, 122), 
#      ylim = c(35, 44), lwd = 2)
# 
# if(length(model.Grid) > 0){
#   for (g in 1:length(model.Grid)) {
#     points(model.Grid[[g]][["latCoords"]][, 1], 
#            model.Grid[[g]][["latCoords"]][, 2], pch = pch[g], 
#            col = col[g], cex = 2/(1.1^(g - 1)))
#     print(nrow(model.Grid[[g]][["latCoords"]]))
#   }
# }

H.basic.data <- CreateHmatrix(grid_coords = model.Grid, 
                              Geo_Map_Coord = China.province.map,
                              method = c("Wendland"), #Wendland, indicator, INLA
                              Site = Site, 
                              factor = 1, 
                              Ch = Ch, 
                              distance.scale = 1e3,
                              Knots.clip.ratio = .10,
                              hs.normal = FALSE) 
print(H.basic.data$Grid.infor$summary$Knots.clip.distance)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# pdf(file = paste0("./figure/Fig5.pdf"), width = 10, height = 12)
# par(mfrow = c(1, 1))
# op <- par(mar = c(0, 0, 0, 0) + 0)
# par(cex = 1.15)
# cex <- 1.8
# par(mgp = c(0, 0, 0))
# pch <- c(3, 4, 20, 18)
# col <- c("red", "blue", "gray30")
# cex.2 <- c(2, 1.5, 0.8)
# # plot(CMAQ_Site$LON, CMAQ_Site$LAT, xlim = c(112, 122), 
# #      ylim = c(35, 44), cex = 0.5)
# library(raster)
# xlim = c(112, 122)
# ylim = c(34, 45)
# CP <- as(extent(c(xlim,ylim)), "SpatialPolygons")
# 
# out <- gIntersection(China.province.map, CP, byid=TRUE)
# plot(out, lwd = 2, axes = T, las=1)
# grid <- NULL
# min.Ei <- max.Ei <- vector()
# k0 <- 1
# if(length(model.Grid) > 0){
#   for (g in 1:length(model.Grid)) {
#     points(H.basic.data$Grid.infor$level[[g]][["latCoords"]][, 1], 
#            H.basic.data$Grid.infor$level[[g]][["latCoords"]][, 2], 
#            pch = pch[g], col = col[g], cex = cex.2[g])
#     print(nrow(H.basic.data$Grid.infor$level[[g]][["latCoords"]]))
#     
#     grid <- rbind(grid, data.frame(H.basic.data$Grid.infor$level[[g]][["latCoords"]],
#                                    level = g))
#     
#     min.Ei[k0] <- min(rARPACK::eigs_sym(as.matrix(H.basic.data$Grid.infor$level[[g]]$Adj.Mat), 
#                                         nrow(H.basic.data$Grid.infor$level[[g]]$Adj.Mat))$values)
#     max.Ei[k0] <- rARPACK::eigs_sym(as.matrix(H.basic.data$Grid.infor$level[[g]]$Adj.Mat), 1)$values
#     k0 <- k0 + 1
#   }
# }
# dev.off()

rm(China.province.map)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
which(H.basic.data$Grid.infor$level[[1]]$adjacent.matrix[2, ]!=0)
print(H.basic.data$Grid.infor$summary$N.BAUs)
print(H.basic.data$Grid.infor$summary$Knots.count)
sum(H.basic.data$Hs[1, ])
# max(H.basic.data$Grid.infor$summary$BAUs.Dist)*0.20
max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist)*0.11

da <- as.matrix(H.basic.data$Grid.infor$level[[1]]$Adj.Mat)
da[1, ]
# H.basic.data$Hs <- H.basic.data$Hs/rowSums(H.basic.data$Hs)
######################################################################
#                             Data truncation
######################################################################
colnames(Base_CAQRA_Table)

da.0 <- Validation.Group.Region(data = Base_CAQRA_Table , 
                                col = c("REAL_PM25", "sim50_CMAQ_PM25"),
                                by = "CITY")
da.0
# writexl::write_xlsx(da.0, path = "./Result/CMAQ.xlsx")

setDT(Base_CAQRA_Table)

# do transformation for some variables 
Base_CAQRA_Table[, c("sim50_CMAQ_PM25")] <- sqrt(Base_CAQRA_Table[, c("sim50_CMAQ_PM25")])


ADCM_Data <- Construct_ADCM_Data(data = Base_CAQRA_Table,
                                 include = list(
                                   YEAR = c(2015, 2016),
                                   month_day = c("11-01", "1-31")
                                 ),
                                 Y = "REAL_PM25",
                                 X = c("sim50_CMAQ_PM25"
                                       , "time.index"
                                       , "sim_TEMP"
                                       , "sim_SPRESS"
                                       , "sim_WIND_X"
                                       , "sim_WIND_Y"
                                 ), 
                                 standard = T, 
                                 center = T, 
                                 start.index = 2)




######################################################################
#                 the formula for additive mdoels
######################################################################

Vari <- var(sqrt(as.vector(ADCM_Data$Y_ts)))

# formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25")
# formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25 + 
# sim_TEMP + 
# # sim_SPRESS + 
# sim_WIND_X + 
# sim_WIND_Y + 
# time.index") 
######################################################################
#                         Model set
######################################################################

######################################################################

######################################################################
#                           fit model and prediction
######################################################################
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]
# tab <- paste0(tab.1, "_", tab.2, "_", Ct)
tab <- paste0("_", tab.1, "_", tab.2)

# library(profvis)
# profvis({
star.time <- proc.time()
Fit <- ADCM(Mean.formula = formula.exp,
                Tab = paste0(hdcm.table, tab),
                Site = Site, 
                ADCM.Data = ADCM_Data, 
                H.basic.data = H.basic.data,
                para = para, 
                Ensemble.size = Ne,
                ensemble.group = 1,
                CV = FALSE, 
                plot = TRUE,
                Database = list(
                  DSN = odbcConnect(dsn = "DSN_01", 
                                    uid = "myname",
                                    pwd = "mypwd",
                                    believeNRows = FALSE,
                                    case = "toupper")),
                verbose.EM = TRUE,
                verbose = TRUE, 
                Object = "CITY",
                response.transf = c("sr"),
                drop.intercept = F,
                Random.first = F,
                save.Predict = T,
                itMin = 1e1,
                cs = Cs, 
                ct = Ct,
                tol.real = 1e-2, 
                itMax = 5e1, 
                Obj.Seq = Obj.Seq)
end.time <- proc.time()
run_time <- (end.time - star.time)[3] 
save(Fit, file = "./code/2_Calibration/all_Fit.RData")