rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "ADCM")
{
  MODE_BASE_TABLE <- obs_PM25_2015w  %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(MODE_BASE_TABLE)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  setDF(MODE_BASE_TABLE)
  DATE_TIME <- unique(MODE_BASE_TABLE$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  MODE_BASE_TABLE <- MODE_BASE_TABLE  %>% left_join(date.time,  by = c("DATE_TIME"))
  MODE_BASE_TABLE[, c("REAL_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("REAL_PM25")])
  MODE_BASE_TABLE[, c("sim50_CMAQ_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("sim50_CMAQ_PM25")])
  
  Covariate = c("sim50_CMAQ_PM25" 
                # , "time.index"
                , "sim_TEMP"
                , "sim_SPRESS"
                , "sim_WIND_Y"
                , "sim_WIND_X"
                , "LON_X"
                , "LAT_Y"
  );
  
  
  setDF(MODE_BASE_TABLE)
  Cov.Index <- which(base::colnames(MODE_BASE_TABLE) %in% Covariate)
  if(length(Cov.Index) > 2){
    mean_covariates <- apply(MODE_BASE_TABLE[, Cov.Index], 2, mean)
    sd_covariates <- apply(MODE_BASE_TABLE[, Cov.Index], 2, sd)
    MODE_BASE_TABLE[, Cov.Index] <- scale(MODE_BASE_TABLE[, Cov.Index],
                                     center = mean_covariates,
                                     scale = sd_covariates)
  }
}
region <- sort(as.character(unique(MODE_BASE_TABLE$CITY)))
region_num <- 1:length(region)

rmse <- matrix(0, nrow = 13, ncol = 2)
bs <- "cc"
t0 <- Sys.time()
k <- 5

ADM <- NULL
for(r  in region_num)
{
  cat(paste0("\n\n   The ", r, "th region: ", region[r], "!!!\n\n"))
  
  DataValPred <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %in% region[r],]
  DataFit <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %nin% region[r],]
  DataValPred$REAL_PM25 <- DataValPred$REAL_PM25^2
  
  
  fit_ADM <- mgcv::gam(REAL_PM25 ~ sim50_CMAQ_PM25 + 
                        s(LON_X, LAT_Y, time.index, k = 180, bs = "tp") +
                        s(time.index, k = k, bs = bs, m = 2) +
                        s(sim_TEMP, k = k, bs = bs, m = 2) +
                        s(sim_SPRESS, k = k, bs = bs, m = 2) +
                        s(sim_WIND_X, k = k, bs = bs, m = 2) +
                        s(sim_WIND_Y, k = k, bs = bs, m = 2),
                      drop.intercept = F,
                      data = DataFit)
  
  summary(fit_ADM)
  
  pred_value <- predict(fit_ADM, newdata = DataValPred)
  DataValPred$PM25.Pred <- pred_value^2
  
  ADM  <- rbind(ADM, DataValPred %>% left_join(DataValPred %>%
                      ddply(.(ID)
                            , .fun = plyr::summarize
                            , PM25.Pred.sd = sd(PM25.Pred, na.rm = TRUE)
                            , .progress = "text"), by = c("ID")))
  temp <- Validation.Group.Region(ADM, #Sigma = ADM$PM25.Pred.sd, 
                                   col = c("REAL_PM25", "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp)
  
}
writexl::write_xlsx(temp, path = "./Result/ADMxz_cv.xlsx")
writexl::write_xlsx(ADM, path = "./Result/pred_ADMxz_cv.xlsx")
