# rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
# source("./code/plot/Figure2_0.R")
# data("SiteData", package = "ADCM")
load("./Result/all_ADCM.RData")
# library(gstat)
maxtime <- 10
maxdist <- 3.2e5

Da.mod <- MODE_BASE_TABLE

Da.mod$Bias <- (Da.mod$Residuals) 

#####################################################################
#####################################################################
Da <- as.data.frame(Fit$addtive.Fit$data$Obs.minus.mean)
Da$DATE_TIME <- as.Date(rownames(Fit$addtive.Fit$data$Obs.minus.mean))

Da <- gather(
            data = Da,
            key = "ID",
            value="Residuals" ,
            - DATE_TIME
          )
Da$ID <- as.numeric(Da$ID)

PM25_CMAQ <- obs_PM25_2015w %>% setorder(ID) %>% 
  dplyr::select(ID, CITY 
                , DATE_TIME
                , YEAR, MONTH, DAY
                , YEAR_MONTH
                , REAL_PM25
                , sim_CMAQ_PM25
                , sim50_CMAQ_PM25
                , LON, LAT
                , LON_X
                , LAT_Y) %>% left_join(Da, by = c("ID", "DATE_TIME"))

Da.mod <- PM25_CMAQ

Da.mod$Bias <- (PM25_CMAQ$Residuals) 


#####################################################################
#####################################################################
Da.mod <- dplyr::select(Da.mod, DATE_TIME, ID, LON_X, LAT_Y, Bias, 
                        REAL_PM25, sim50_CMAQ_PM25) %>%  
  setorderv(c("DATE_TIME", "ID"))
temp_part <- unique(Da.mod$DATE_TIME)
loc <- unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
Da.long <- spacetime::STFDF(sp = mod_gridded
                            , time = temp_part
                            , data = Da.mod)

vv0 <- variogram(object = Bias ~ 1, # fixed effect component
                 data = Da.long, # July data
                 cressie = T,
                 covariogram = F,
                 # pseudo = -1,
                 width = 20000, # spatial bin (80 km)
                 cutoff = maxdist, # consider pts < 1000 km apart
                 tlags = seq(0.01, maxtime + 0.01, 2)) # 0 days to 6 days
vv0$avgDist <- vv0$avgDist/1e3



vv <- vv0[!is.na(vv0$gamma), c(5, 7, 3)]

vv$timelag <- as.numeric(vv$timelag)
vv$avgDist <- as.numeric(vv$avgDist)
vv$gamma <- as.numeric(vv$gamma)

Contour_da <- reshape2::dcast(vv, timelag ~ avgDist, value.var = "gamma")


surf <- MBA::mba.surf(vv, no.X = 200, no.Y = 200,
                      extend = TRUE)$xyz.est


surf$z <- ifelse(surf$z < 0, 0, surf$z)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
##########################################################################
#                      plot
##########################################################################
pdf(paste0(paste0("./figure/"), 'Fig22',".pdf"), width = 10, height =  8)
par(mfrow = c(1, 1))
# op <- par(mar = c(4, 4, 2, 5) + 0.5) 
op <- par(mar = c(5, 5, 1.5, 1) + 1)
par(mgp = c(4.0, 1.2, 0.3), cex.axis = 1.8, cex.lab = 2, cex.main = 2.0, cex = 1.25)
# par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.5, cex.lab = 1.5)
filled.contour(x = surf$x, y = surf$y, z = surf$z, 
               color.palette = myPalette, 
               plot.title = title(main = TeX("Residuals"), #: $PM_{2.5}$ ~1 + x + g(z)$
               ylab = "Spatial lags (km)", 
               xlab = "Temporal lags (days)"),
               xlim = c(0, maxtime),
               ylim = c(0, 300),
               zlim = c(0, 15),
               key.axes = axis(4, seq(0, 15, by = 3), asp = 1),
               plot.axes = {
                 # axis(1)
                 axis(1, at = seq(0, maxtime + 0.01, 2), labels = seq(0, maxtime + 0.01, 2))
                 axis(2, at = seq(0, 310, 50), labels = seq(0, 310, 50))
                 # contour(x = surf$x, y = surf$y, z = surf$z,
                 #         add = TRUE, lwd = 2, labcex = 1.5)
                 contour(x = Contour_da$timelag, 
                         y = as.numeric(colnames(Contour_da)[-1]),
                         z = as.matrix(Contour_da[, -1]),
                         add = TRUE, lwd = 2, labcex = 1.5)
               })
mtext("(b)", side = 3, line = 0.3, adj = -0.05, cex = 2.5, font = 1)
dev.off()
