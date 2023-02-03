rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
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

# setDF(MODE_BASE_TABLE)
# DATE_TIME <- unique(MODE_BASE_TABLE$DATE_TIME) %>% sort()
# Nt <- length(DATE_TIME)
# date.time <- data.frame(time.index = 1:Nt,
#                         time.scale = seq(0, 1, , Nt),
#                         DATE_TIME = DATE_TIME)
# MODE_BASE_TABLE <- MODE_BASE_TABLE  %>% left_join(date.time, 
#                                                   by = c("DATE_TIME"))
# MODE_BASE_TABLE$REAL_PM25 <- sqrt(MODE_BASE_TABLE$REAL_PM25)
# 
# MODE_BASE_TABLE[, c("sim50_CMAQ_PM25")]=
#   sqrt(MODE_BASE_TABLE[,  c("sim50_CMAQ_PM25")])
# 
# Covariate <- c("sim50_CMAQ_PM25" 
#               , "time.index"
#               , "time.scale"
#               , "sim_TEMP"
#               , "sim_SPRESS"
#               , "sim_WIND_Y"
#               , "sim_WIND_X"
#               , "LON_X"
#               , "LAT_Y"
# );
# # fmla <- as.formula(paste0("sqrt(PM25)~", paste(Covariate, collapse = "+")))
# Cova <- which(base::colnames(MODE_BASE_TABLE) %in% Covariate)
# if(length(Covariate) > 1){
#   for(k in 1:length(Covariate))
#   {
#     MODE_BASE_TABLE[, Cova[k]] = scale(as.vector(
#       MODE_BASE_TABLE[, Cova[k]]), center = F)[, 1]
#   }}


rmse <- matrix(0, nrow = 13, ncol = 2)
bs = "cc"
t0 <- Sys.time()
k <- 5

ADM <- NULL
for(i  in region_num)
{
  print(i)
  
  DataValPred <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %in% region[i],]
  DataFit <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %nin% region[i],]
  DataValPred$REAL_PM25 <- DataValPred$REAL_PM25^2
  
  
  fitres <- mgcv::gam(REAL_PM25 ~ sim50_CMAQ_PM25 + 
                        s(LON_X, LAT_Y, time.index, k = 180, bs = "tp"),# +
                        # s(time.index, k = k, bs = bs, m = 2) +
                        # s(sim_TEMP, k = k, bs = bs, m = 2) +
                        # s(sim_SPRESS, k = k, bs = bs, m = 2) +
                        # s(sim_WIND_X, k = k, bs = bs, m = 2) +
                        # s(sim_WIND_Y, k = k, bs = bs, m = 2),
                      drop.intercept = F,
                      data = DataFit)
  
  summary(fitres)
  
  pred_value <- predict(fitres, newdata = DataValPred)
  DataValPred$PM25.Pred <- pred_value^2
  
  ADM  <- rbind(ADM, DataValPred %>% left_join(DataValPred %>%
    ddply(.(ID)
          , .fun = plyr::summarize
          , PM25.Pred.sd = sd(PM25.Pred, na.rm = TRUE)
          , .progress = "text"), by = c("ID")))
  
  # Yts <- (ADM %>% dcast(DATE_TIME~ID, value.var = "REAL_PM25"))[, -1] %>% as.matrix()
  # Yts.pred <- (ADM %>% dcast(DATE_TIME~ID, value.var = "PM25.Pred"))[, -1]%>% as.matrix()
  # Yts.pred.sd <- (ADM %>% dcast(DATE_TIME~ID, value.var = "PM25.Pred.sd"))[, -1]%>% as.matrix()
  # 
  # #print(pred.gp)
  # rmse[i, ] <- ADCM::spT_validation(Yts, Yts.pred, 
  #                                   sigma = Yts.pred.sd, 
  #                                   names = F, CC = F)[c(1, 4)]
  # 
  # # sqrt(sum((pred_value^2-DataValPred$REAL_PM25^2)^2)/length(DataValPred$REAL_PM25))
  # 
  # print(rmse[i, ])
  # print(Sys.time() - t0)
  # t0 <- Sys.time()
  temp0 <- Validation.Group.Region(ADM, #Sigma = ADM$PM25.Pred.sd, 
                                   col = c("REAL_PM25", "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
  
}
# rmse <- as.data.frame(rmse)
# names(rmse)<-c("RMSE", "CRPS")
# rmse<-rbind(rmse, apply(rmse, 2, mean))
# rownames(rmse)[1:13] <- region
# rownames(rmse)[14] <-  "Average"
# (rmse)
# writexl::write_xlsx(temp0, path = "./Result/ADMxz_cv.xlsx")
# writexl::write_xlsx(ADM, path = "./Result/pred_ADMxz_cv.xlsx")
