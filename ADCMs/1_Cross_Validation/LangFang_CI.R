load("./1_Cross_Validation/CV_Langfang.RData")
data("SiteData", package = "ADCM")


Database <- FALSE
CV_Langfang <- Fit[[7]]
Pred.Data <- CV_Langfang$addtive.test.Data
Nt <- nrow(CV_Langfang$test.Data$Y_ts)
n <- ncol(CV_Langfang$test.Data$Y_ts)
SiteID <- colnames(CV_Langfang$test.Data$Y_ts)
M <- 100



DATE_TIME <- row.names(CV_Langfang$test.Data$Y_ts)

#  output prediction from fitting observed data
pred.y <- matrix(predict(CV_Langfang$addtive.Fit$mean.ensemble.fit[[1]]$Fit,
                         newdata = Pred.Data,
                         type = "response", se.fit = F),
                 nrow = Nt,
                 ncol = n)

#  output prediction by combining conditional simulation and results of fitted data 
Con.Simu.Pred <- array(0, dim = c(Nt, n, M), 
                       dimnames = list(DATE_TIME,
                                       SiteID,
                                       paste0("Cond.Sim.", 1:M)))
for(m in 1:M){
  load(paste0("./data/Con_Simu/CS_Fit_CV_LangFang_", m, ".RData"))
  Con.Simu.Pred[,, m] <- 2*pred.y - matrix(predict(Fit, newdata = Pred.Data,
                                             type = "response", se.fit = F),
                                         nrow = Nt, 
                                         ncol = n)
  
  cat(paste0("Con.Simu.Pred[,, ", m, "] = "), range(Con.Simu.Pred[,, m]), "\n")
}


# random effect 
# load(paste0(file, ADCM[1]))
#By ensemble
Vt <- CV_Langfang$Ks$EnXf[-1,, 1:M]

# or by MC
{
  Vt <- array(0, dim = c(Nt, CV_Langfang$data$N.BAUs, M),
              dimnames = list(base::rownames(CV_Langfang$data$Y_ts),
                              paste0("Grid.ID.", 1:CV_Langfang$data$N.BAUs),
                              paste0("En.", 1:M)))
  #
  
  {

    if(isTRUE(Database)){
      DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd"
                            , believeNRows = FALSE, case = "toupper")
      Vt.error <- sqlQuery(DSN_01, "select * from CS_VT_CV_LangFang")
    }else{
      Vt.error.temp <- NULL
      for(m in 1:M){
      load(paste0("./data/Con_Simu/CS_Vt_CV_LangFang_", m, ".RData"))
        Vt.error.temp <- rbind(Vt.error.temp, Vt.error) 
      }
      Vt.error <- Vt.error.temp
    }
    
    
    dim(Vt.error)
    colnames(Vt.error)[1:10]
    for(t in seq_len(CV_Langfang$data$Nt)){
      # Vt.MC.error.ensemble <- Vt.error[Vt.error$TIME == (t + 1), -c(1, 2)]
      Vt.MC.error.ensemble <- matrix(Vt.error[, c(t + 2)], 
                                     nrow = CV_Langfang$data$N.BAUs, ncol = M)
      Vt[t,, ] <- CV_Langfang$Ks$EnXf[t + 1,, 1:M] - (Vt.MC.error.ensemble)
    }
  }
}


Hs <- CV_Langfang$test.Data$Hs %>% t()
H.vt.ensemble.pred <- mcmapply(t = 1:Nt, 
                               SIMPLIFY = F, FUN = function(t)
                                 Matrix::crossprod(x = Hs, y = Vt[t, ,]),
                               mc.cores = 1)
Wts.forecast.ensemble <- noise <- array(0, dim = c(Nt, n, 
                                                   M), 
                                        dimnames = list(DATE_TIME, SiteID,
                                                        paste0("En.", 1:M)))
for(t in seq_len(Nt)){ 
  Wts.forecast.ensemble[t, , ] <- matrix(H.vt.ensemble.pred[[t]], 
                                         nrow = n, 
                                         ncol = M)
  # Wts.forecast.ensemble[t, , ] <- matrix(Hs%*%()
  #                                          , 
  #                                        nrow = Forecast.PM2.5$data$n, 
  #                                        ncol = Forecast.PM2.5$data$ensemble$Ensemble.size)
  
  noise[t, , ] <- rnorm(n*M, 0, 
                        sqrt(CV_Langfang$Para.est$Obs.sigma.sq$E_sigma.sq))
}
# forecast for response
# Yts.forecast.ensemble <- array(0, dim = c(Cali_Data_Inf$Nt, length(sample.id), 
#                                           Fit$data$ensemble$Ensemble.size), 
#                                dimnames = list(date.index$DATE_TIME,
#                                                as.character(sample.id),
#                                                paste0("En.", 1:Fit$data$ensemble$Ensemble.size)))
range(Con.Simu.Pred)
Yts.forecast.ensemble <- (Con.Simu.Pred + Wts.forecast.ensemble + noise)^2

# yHat <- apply(Yts.forecast.ensemble, c(1, 2), mean) %>% apply(c(1), mean)
# pred.sd <- apply(Yts.forecast.ensemble, c(1, 2), sd)%>% apply(c(1), mean)

# way 1
yHat <- apply(Yts.forecast.ensemble, c(1, 3), mean) %>% apply(c(1), mean)
pred.sd <- apply(Yts.forecast.ensemble, c(1, 3), mean)%>% apply(c(1), sd)


CI95Lower <- ifelse(yHat - 1.96*pred.sd < 0, 0, yHat - 1.96*pred.sd)
CI95Upper <- yHat + 1.96*pred.sd

da <- t(as.matrix(data.frame(CI95Lower, yHat, CI95Upper)))


# way 2
# da <- apply(Yts.forecast.ensemble, c(1, 3), mean) %>%
#   apply(c(1), quantile, prob = c(0.025, 0.5, 0.975))
# 
# 
# CI95Lower <- da[1, ]
# CI95Upper <- da[3, ]
# yHat <- da[2, ]



#
City_Name <- "Langfang"
# data("obs_PM25_2015w", package = "ADCM")
setDF(obs_PM25_2015w)
ADCM_fit <- data.frame(
  CITY = City_Name,
  Fit.L25 = as.numeric(da[1, ]),
  Fit.Median = as.numeric(da[2, ]),
  Fit.U95 = as.numeric(da[3, ]),
  DATE_TIME = as.Date(colnames(da))
)

ADCM_fit$Model <- "ADCM"
#CMAQ and Real data
PM25_2015w <- obs_PM25_2015w
# load("./5_1_Generate_Data/BaseTable/Model_2015_2017_Spline_Tab.Rda")
setDF(PM25_2015w)
PM25_2015w$True_REAL_PM25 <-
  PM25_2015w$REAL_PM25
# c(201506, 201507, 201508)
# c(201511, 201512, 201601)
MODE_BASE_TABLE <- PM25_2015w %>%
  filter(YEAR_MONTH %in% c(201511, 201512, 201601)
         , CITY %in% c(City_Name)) %>% setorder(DATE_TIME, ID) %>%
  ddply(
    .(CITY, DATE_TIME)
    , .fun = plyr::summarize
    # , LON = mean(LON, na.rm = TRUE)
    # , LAT = mean(LAT, na.rm = TRUE)
    # , FAC2 = mean(Pred.Median/REAL_PM25, na.rm = TRUE)
    , REAL_PM25 = mean(REAL_PM25, na.rm = TRUE)
    , sim_CMAQ_PM25 = mean(sim_CMAQ_PM25, na.rm = TRUE)
    , .progress = "text"
  )
######################################################################################
CMAQ_PM25 <- MODE_BASE_TABLE[, c(1, 2, 4)]
CMAQ_PM25$Fit.L25 <- CMAQ_PM25$Fit.U95 <- CMAQ_PM25$sim_CMAQ_PM25
setnames(CMAQ_PM25, "sim_CMAQ_PM25", "Fit.Median")
colnames(CMAQ_PM25)
CMAQ_PM25$Model <- "CMAQ"
#Real data
######################################################################################
Real_PM25 <- MODE_BASE_TABLE[, c(1, 2, 3)]
Real_PM25$Fit.L25 <- Real_PM25$Fit.U95 <- Real_PM25$REAL_PM25
setnames(Real_PM25, "REAL_PM25", "Fit.Median")
Real_PM25$Model = "Observation"
######################################################################################
num <- 53
Da <- rbind(ADCM_fit[1:num, ], Real_PM25[1:num, ], CMAQ_PM25[1:num, ]) %>%
  filter(month(DATE_TIME) %in% c(11, 12), day(DATE_TIME) %in% c(1:31))

##############
# Da <- Da[1:50, ]

######################################################################################
#             plot  credible  interval
######################################################################################
label <- c("Observation", "CMAQ output", "ADCM prediction with 95% CI")
Bcol <- c("black", "grey80", "grey50")
######################################################################################
Sys.setlocale("LC_TIME", "English")
######################################################################################

UP <- max(Da$Fit.U95, na.rm = T) + 20
scal <- 150
time <- unique(Da$DATE_TIME)

alpha <- c("1", "2", "3")
Da$alpha <- ifelse(Da$Model %in% "ADCM",
                   alpha[3],
                   ifelse(Da$Model %in% "CMAQ",
                          alpha[2], alpha[1]))
S <- c("1", "2", "3")
Da$size <- ifelse(Da$Model %in% "ADCM", S[3],
                  ifelse(Da$Model %in% "CMAQ",
                         S[2], S[1]))

Da$Model <- ifelse(Da$Model %in% "ADCM",
                   "ADCM",
                   ifelse(Da$Model %in% "CMAQ",
                          "CMAQ", "Observation"))


Da$Model <- ordered(Da$Model, levels = c("Observation", "CMAQ",
                                         "ADCM"))

