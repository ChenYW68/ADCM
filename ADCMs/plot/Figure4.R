rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
######################################################################
#                             Data 
######################################################################
Base_CAQRA_Table <- obs_PM25_2015w
setDF(Base_CAQRA_Table)

######################################################################
######################################################################
y <- Base_CAQRA_Table$REAL_PM25 
Date <- unique(Base_CAQRA_Table$DATE_TIME)
pdf(file = paste0("./figure/Fig4.pdf"),
    width = 37, height = 6.5)
par(mfrow = c(1, 5))
op <- par(mar = c(3, 4, 0, 1) + 0.1)
par(cex = 1.8, cex.lab = 1.20)
cex <- 1.5
point.cex <- 0.4
ymx <- 720
# str <- r"(The CMAQ output bias ($\mu g/m^3$))"
str <- r"(The observed $PM_{2.5}$ ($\mu g/m^3$))"
par(mgp = c(2, 1, 0))
plot(Base_CAQRA_Table$DATE_TIME, y,
     cex = point.cex, pch = 20, xaxt = "n", xlab = "Date", 
     ylab = latex2exp::TeX(str))
# colnames(Base_CAQRA_Table)
text(x = unique(Base_CAQRA_Table$DATE_TIME[1]) + 3, 
     y = ymx, labels = "(a)", cex = cex)
axis(1, at = Date[c(1, 50, 92)], #seq(1, length(Date), 15)
     labels = c(paste0("        November 1, ", "\n", "   2015"),
                paste0("December 20, ", "\n", "2015"),
                paste0("January 31, ", "\n", "2016")))

plot(Base_CAQRA_Table$sim_TEMP, y,
     cex = point.cex, pch = 20, xlab = expression(paste("Surface temperature (", degree, "C)")),
     ylab = latex2exp::TeX(str))
text(x = -26.5, y = ymx, labels = "(b)", cex = cex)
# plot(Base_CAQRA_Table$sim_RHUID, Base_CAQRA_Table$REAL_PM25,
#      cex = 0.5, pch = 20, xlab = "Surface relative humidity (%)",
#      ylab = latex2exp::TeX(r"(Square root of observed $PM_{2.5}$ concentrations ($\mu g/m^3$))"))
# text(x = 24, y = 27, labels = "(c)", cex = cex)
plot(Base_CAQRA_Table$sim_SPRESS/1000, y,
     cex = point.cex, pch = 20, xlab = latex2exp::TeX(r"(Surface pressure ($\times 10^3 Pa $))"),
     ylab = latex2exp::TeX(str))
text(x = 91.2, y = ymx, labels = "(c)", cex = cex)
plot(Base_CAQRA_Table$sim_WIND_X, y,
     cex = point.cex, pch = 20, xlab = "Eastern cumulative wind power (m/s)", #Surface pressure
     ylab = latex2exp::TeX(str))
text(x = -9.5, y = ymx, labels = "(d)", cex = cex)
plot(Base_CAQRA_Table$sim_WIND_Y, y,
     cex = point.cex, pch = 20, xlab = "Northern cumulative wind power (m/s)",
     ylab = latex2exp::TeX(str))
text(x = -15, y = ymx, labels = "(e)", cex = cex)
# Fit <- CV_T_Dist_W[[1]]$addtive.Fit$mean.ensemble.fit[[1]]$Fit
# plot(Fit, residuals = TRUE,
#      select = 1, ylim = c(-5, 5), se = F,
#      xlab = "time", ylab = "g(t)", cex = 2, lwd = 3)
# legend(x = 1, y = 5,
#        legend = c("Residuals", "Estimated curve with the cyclic P-spline basis"),
#        col = c("black", "black"), ncol = 1,
#        # pch = as.numeric(c(".", 19)),
#        pch = as.numeric(c(19, ".")),
#        lty = c("blank", "solid"),
#        lwd = 3,
#        bty = "n",
#        cex = 0.8)
dev.off()



