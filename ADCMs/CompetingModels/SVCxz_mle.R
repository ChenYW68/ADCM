rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
{
  
  MODE_BASE_TABLE <- obs_PM25_2015w %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(MODE_BASE_TABLE)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
  setDF(MODE_BASE_TABLE)
  DATE_TIME <- unique(MODE_BASE_TABLE$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  MODE_BASE_TABLE <- MODE_BASE_TABLE  %>% left_join(date.time,  by = c("DATE_TIME"))
  MODE_BASE_TABLE[, c("REAL_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("REAL_PM25")])
  MODE_BASE_TABLE[, c("sim50_CMAQ_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("sim50_CMAQ_PM25")])
  
  Covariate = c("sim50_CMAQ_PM25");
  
  Cova <- which(base::colnames(MODE_BASE_TABLE) %in% Covariate)
  if(length(Covariate) > 1){
    for(k in 1:(length(Covariate)))
    {
      MODE_BASE_TABLE[, Cova[k]] = scale(center = F, as.vector(
        MODE_BASE_TABLE[, Cova[k]]))[, 1]
    }}
}

# ###################################################################
# #                           2. Model
# ###################################################################
region <- unique(MODE_BASE_TABLE$CITY)
###################################################################
#                       SVC model
###################################################################
{
  region <- sort(as.character(unique(MODE_BASE_TABLE$CITY)))
  region_num <- 1:length(region)
  setDT(MODE_BASE_TABLE)
  
  # Ens <- ceil(0.2*n.samples)
  
  colNames <- c("CITY","LON", "LAT", "DATE_TIME",
                "YEAR_MONTH", "YEAR","MONTH","DAY",
                "REAL_PM25")
  for(r in region_num)
  {
    
    year_range <- unique(MODE_BASE_TABLE$YEAR)
    tem <- MODE_BASE_TABLE[CITY %in% region[r], "ID"]
    
    for(Year in year_range)
    {
      Base_Table <- MODE_BASE_TABLE[YEAR == Year,]
      month_range <- unique(Base_Table$MONTH)
      for(Month in month_range)
      {
        Base_Tab <- Base_Table[MONTH == Month,]
        day_range <- unique(Base_Tab$DAY)
        for(Day in day_range)
        {
          set.seed(1234)
          cat("\n\n   the ", r, "th region: ", region[r], "!!!\n\n")
          cat("   year: ", Year, "; month: ", Month, "; day: ", Day," \n\n")
          cat("...................SVC.................\n\n")
          # Database
          Da.mod <- Base_Tab[CITY %nin% region[r] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% region[r] & DAY == Day, ]
          
          setDF(Da.mod);setDF(Da.pre);
          
          # install.packages("varycoef")
          library(varycoef)
          
          X <- model.matrix(~1 + sim50_CMAQ_PM25
                            + sim_TEMP
                            + sim_SPRESS
                            + sim_WIND_X
                            + sim_WIND_Y
                            , data = Da.mod)
          
          
          coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])
          fit_svc <- SVC_mle(y = Da.mod$REAL_PM25, X = X 
                             , W = matrix(X[, 1], ncol = 1),
                             , locs = coords
                             , control = SVC_mle_control(
                               cov.name = "exp"
                               #   # inital values for 3 SVC
                               #   # 7 = (3 * 2 covariance parameters + nugget)
                               #   init = c(rep(c(0.4, 0.2), 3), 0.2),
                               #   profileLik = TRUE
                             )
          )
          
          ## summary and residual output
          # summary(fit_svc)
          # plot(fit_svc)
          # using method predict with whole data and corresponding locations
          coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
          X <- model.matrix(~1 + sim50_CMAQ_PM25
                            + sim_TEMP
                            + sim_SPRESS
                            + sim_WIND_X
                            + sim_WIND_Y
                            , data = Da.pre)
          
          svc_pred <- predict(fit_svc, newX = X 
                              , newW = matrix(X[, 1], ncol = 1)
                              , newlocs = coords)
          temp.Pred <- svc_pred$y.pred^2
          
          
          Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
          spT <- spT_validation(Da.pre$REAL_PM25, temp.Pred,
                                sigma = NA, zhat.Ens = NULL, 
                                names = F, CC = F)[c(1, 4)]
          print(spT)
          
          if(r == region_num[1] & Year == year_range[1]  &
             Month == month_range[1] & Day == day_range[1])
          {
            SVC <- data.frame(Da.pre[, colNames], 
                              # temp.L25 = temp.L25, 
                              PM25.Pred = temp.Pred
                              # temp.U95 = temp.U95
            )
            
          }else{
            SVC <- rbind(SVC, data.frame(Da.pre[, colNames]
                                         # , temp.L25 = temp.L25
                                         , PM25.Pred = temp.Pred
                                         # , temp.U95 = temp.U95
            ))
          }
          temp1 <- Validation.Group.Region(SVC,
                                           col = c("REAL_PM25", "PM25.Pred"),
                                           by = "CITY")
          cat("\n.............................\n")
          print(temp1)
        }
        cat("\n.............................\n")
      }
    }
    
    temp0 <- Validation.Group.Region(SVC, col = c("REAL_PM25", "PM25.Pred"),
                                     by = "CITY")
    cat("\n.............................\n")
    print(temp0)
  }
}
writexl::write_xlsx(temp0, path = "./data/SVCxz_mle_cv.xlsx")
writexl::write_xlsx(SVC, path = "./Result/pred_SVCxz_mle_cv.xlsx")