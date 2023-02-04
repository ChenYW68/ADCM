rm(list=ls())
source("./LoadPackages/RDependPackages.R")
Database <- TRUE
if(isTRUE(Database)){
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd", believeNRows = FALSE, case = "toupper")
}
# ######################################################################
#                         Generating table 3
# ######################################################################
# ######################################################################
# ######################################################################
# ######################################################################
n <- 300; # Need to modify: n = 300 or 1000
# ######################################################################
# ######################################################################
# ######################################################################
num <- 20;
start <- c(1, 50)
Nt <- 30; iter <- 50
simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_ADCM_Linear")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_ADCM2_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

beta.true <- c(15, 1, 1); nugget.true <- 1e-1
beta <- as.data.frame(unique(Result[, c(1:7)]))
setnames(beta, "SIGMASQ", "TAU_SQ")

data_summery <- function(beta, n, model = "ADCM", num = 5){
  tab.summary <- cbind(variable = c("beta_0", "beta_1", "beta_2", "TAU_SQ"), 
                       rbind(data.frame(Bias = Round(mean(beta$BETA0 - beta.true[1]), num),
                                        Sd = Round(sd(beta$BETA0), num),
                                        MSE = Round((mean(beta$BETA0 - beta.true[1]))^2 +
                                                      var(beta$BETA0), num)),
                             data.frame(  
                               Bias = Round(mean(beta$BETA1 - beta.true[2]), num),
                               Sd = Round(sd(beta$BETA1), num),
                               MSE = Round((mean(beta$BETA1 - beta.true[2]))^2 +
                                             var(beta$BETA1), num)),
                             data.frame(
                               Bias = Round(mean(beta$BETA2 - beta.true[3]), num),
                               Sd = Round(sd(beta$BETA2), num),
                               MSE = Round((mean(beta$BETA2 - beta.true[3]))^2 +
                                             var(beta$BETA2), num)),
                             data.frame(
                               Bias = Round(mean(beta$TAU_SQ - nugget.true), num),
                               Sd = Round(sd(beta$TAU_SQ), num),
                               MSE = Round((mean(beta$TAU_SQ - nugget.true))^2 +
                                             var(beta$TAU_SQ), num))
  ))
  da.re <- as.data.frame(t(tab.summary))#[-1,]
  colnames(da.re) <- da.re[1, ]
  da.re <- data.frame(n = c(n, "", ""),
                      Model = c(model, "", ""),
                      RUN_TIME = c(round(sum(beta$RUN_TIME)), "", ""), 
                      Criteria = rownames(da.re[-1, ]),
                      da.re[-1, ],  
                      Prediction = c("", "", Round((mean(beta$RMSE))^2, num)))
  rownames(da.re) <- NULL
  da.re
}
# ######################################################################
ADCM <- data_summery(beta, n, model = "ADCM", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "__SVC_MLE")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

Result <- Result %>% plyr::ddply(
                                .(ITER)
                                , plyr::summarize
                                , BETA0 = mean(BETA0, na.rm = T)
                                , BETA1 = mean(BETA1, na.rm = T)
                                , BETA2 = mean(BETA2, na.rm = T)
                                , TAU_SQ = mean(TAUSQ, na.rm = T)
                                , RMSE = mean(RMSE, na.rm = T)
                                , RUN_TIME = sum(RUN_TIME)
                              )
beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))

# setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
SVC <- data_summery(beta, n, model = "SVC", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "__SVCT_MLE")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

Result <- Result %>% plyr::ddply(
                              .(ITER)
                              , plyr::summarize
                              , BETA0 = mean(BETA0, na.rm = T)
                              , BETA1 = mean(BETA1, na.rm = T)
                              , BETA2 = mean(BETA2, na.rm = T)
                              , TAU_SQ = mean(TAUSQ, na.rm = T)
                              , RMSE = mean(RMSE, na.rm = T)
                              , RUN_TIME = sum(RUN_TIME)
                            )
beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))
# setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
SVCT <- data_summery(beta, n, model = "SVC", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
# simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_STAR_INLA")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))

setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
STAR <- data_summery(beta, n, model = "STAR", num = num)

Sum_Da_1 <- rbind(SVC, SVCT, STAR, ADCM)
# ######################################################################
n <- 1000;
# ######################################################################

simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_ADCM_Linear")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_ADCM2_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result[, c(1:7)]))
setnames(beta, "SIGMASQ", "TAU_SQ")
# data_summery <- function(beta, n, model = "ADCM", num = num){
#   tab.summary <- cbind(variable = c("beta_0", "beta_1", "beta_2"), 
#                        rbind(data.frame(Bias = Round(mean(beta$BETA0 - beta.true[1]), num),
#                                         Sd = Round(sd(beta$BETA0), num),
#                                         MSE = Round((mean(beta$BETA0 - beta.true[1]))^2 +
#                                                       var(beta$BETA0), num)),
#                              data.frame(  
#                                Bias = Round(mean(beta$BETA1 - beta.true[2]), num),
#                                Sd = Round(sd(beta$BETA1), num),
#                                MSE = Round((mean(beta$BETA1 - beta.true[2]))^2 +
#                                              var(beta$BETA1), num)),
#                              data.frame(
#                                Bias = Round(mean(beta$BETA2 - beta.true[3]), num),
#                                Sd = Round(sd(beta$BETA2), num),
#                                MSE = Round((mean(beta$BETA2 - beta.true[3]))^2 +
#                                              var(beta$BETA2), num))
#                        ))
#   da.re <- as.data.frame(t(tab.summary))#[-1,]
#   colnames(da.re) <- da.re[1, ]
#   da.re <- data.frame(n = c(n, "", ""),
#                       Model = c(model, "", ""),
#                       RUN_TIME = c(round(sum(beta$RUN_TIME)), "", ""), 
#                       Criteria = rownames(da.re[-1, ]),
#                       da.re[-1, ],  
#                       Prediction = c("", "", Round((mean(beta$RMSE))^2, num)))
#   rownames(da.re) <- NULL
#   da.re
# }
# ######################################################################
ADCM <- data_summery(beta, n, model = "ADCM", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
# simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "__SVC_MLE")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

Result <- Result %>% plyr::ddply(
                              .(ITER)
                              , plyr::summarize
                              , BETA0 = mean(BETA0, na.rm = T)
                              , BETA1 = mean(BETA1, na.rm = T)
                              , BETA2 = mean(BETA2, na.rm = T)
                              , TAU_SQ = mean(TAUSQ, na.rm = T)
                              , RMSE = mean(RMSE, na.rm = T)
                              , RUN_TIME = sum(RUN_TIME)
                            )
beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))
# setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
SVC <- data_summery(beta, n, model = "SVC", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "__SVCT_MLE")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

Result <- Result %>% plyr::ddply(
                            .(ITER)
                            , plyr::summarize
                            , BETA0 = mean(BETA0, na.rm = T)
                            , BETA1 = mean(BETA1, na.rm = T)
                            , BETA2 = mean(BETA2, na.rm = T)
                            , TAU_SQ = mean(TAUSQ, na.rm = T)
                            , RMSE = mean(RMSE, na.rm = T)
                            , RUN_TIME = sum(RUN_TIME)
                          )
beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))
# setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
SVCT <- data_summery(beta, n, model = "SVC", num = num)
# ######################################################################
# ######################################################################
# n <- 300; 
# simu <- paste0("Sim_", n)
# Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_STAR_INLA")
if(isTRUE(Database)){
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                    Simulation_Para,
                                    " where ITER <= ", iter), 
                     errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_SVC_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result))
setnames(beta, "TAUSQ", "TAU_SQ")
# ######################################################################
STAR <- data_summery(beta, n, model = "STAR", num = num)

Sum_Da_2 <- rbind(SVC, SVCT, STAR, ADCM)

Sum_Da <- rbind(Sum_Da_1, Sum_Da_2)

Sum_Da

writexl::write_xlsx(Sum_Da, path = paste0("./data/Table_S2(", n,").xlsx"))

