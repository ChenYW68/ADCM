source("./code/LoadPackages/PSTVB_Packages.R")
Database <- NULL
if(!is.null(Database)){
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd", believeNRows = FALSE, case = "toupper")
}
# ######################################################################
#                       the list of tables
# ######################################################################
# ######################################################################
n <- 100; # Need to modify: n = 100 or 2000
# ######################################################################
# ######################################################################
# ######################################################################
Nt <- 30; iter <- 200; num <- 5
start <- c(1, iter)
Database <- NULL
if(!is.null(Database)){
  simu <- paste0("Sim_MGCV_", n)
  # Simulation_wts <- paste0(simu, "_wts")
  Simulation_Para <- paste0(simu, "_para")
  Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                  Simulation_Para), errors = F)%>% setDT()
}else{
  load(paste0("./data/Sim_MGCV_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para 
}
colnames(Result) <- toupper(colnames(Result))


beta.true <- c(15, 1, 1)
beta <- distinct(as.data.frame(unique(Result[, c(1:4)])))
# setnames(beta, 1:4, c("ITER", "BETA0", "BETA2", "BETA3"))
# TIME <- sort(unique(da$TIME))

G0.PRE <- distinct(Result[, c(1, 9, 5)]) %>%  dcast( TIME ~ ITER, value.var = "G0") %>% setDF()
TIME <- G0.PRE$TIME
G0.TRUE = g0(TIME) - mean(g0(TIME))

Da <- distinct(Result[, c(1, 10, 6)])
G1.PRE <- Da %>%  dcast( Z ~ ITER, value.var = "G1") %>% setDF()
Z <- G1.PRE[, 1]
G1.TRUE = g1(Z) - mean(g1(Z))


{
  tab.summary <- cbind(variable = c("$\beta_0$", "$\beta_1$", "$\beta_2$", "$f_1$", "$f_2$"), 
                       rbind(data.frame(Bias = Round(mean(beta$BETA0 - beta.true[1]), 
                                                     num),
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
                             data.frame(   Bias = Round(sfsmisc::integrate.xy(x = as.data.frame(G0.PRE)[, 1], 
                                              fx = (rowMeans(G0.PRE[, 2:(iter + 1)]) - G0.TRUE)^2), num),
                                           
                                           # Bias.3.2 =r2[1]*mean((rowMeans(alpha.est1[1:Nt, 1:iter])- theta.1)^2),
                                           Sd = Round(sfsmisc::integrate.xy(x = as.data.frame(G0.PRE)[, 1],
                                                                            fx = apply(G0.PRE[, 2:(iter + 1)], 1, sd)
                                           ), num), 
                                           # Sd.3.2 = sqrt(mean(rowVar(alpha.est1[1:Nt, 1:iter]))),
                                           MSE= Round(sfsmisc::integrate.xy(x = as.data.frame(G0.PRE)[, 1], 
                                                                            fx = (rowMeans(G0.PRE[, 2:(iter + 1)])- G0.TRUE)^2) +
                                                        sfsmisc::integrate.xy(x = as.data.frame(G0.PRE)[, 1],
                                                                              fx = apply(G0.PRE[, 2:(iter + 1)], 1, var)), num) 
                             ),
                             data.frame(    Bias = Round(sfsmisc::integrate.xy(x = as.data.frame(G1.PRE)[, 1], 
                                                   fx = (rowMeans(G1.PRE[, 2:(iter + 1)]) - G1.TRUE)^2), num),
                                            
                                            # Bias.3.2 =r2[1]*mean((rowMeans(alpha.est1[1:Nt, 1:iter])- theta.1)^2),
                                            Sd = Round(sfsmisc::integrate.xy(x = as.data.frame(G1.PRE)[, 1],
                                                                             fx = apply(G1.PRE[, 2:(iter + 1)], 1, sd)
                                            ), num), 
                                            # Sd.3.2 = sqrt(mean(rowVar(alpha.est1[1:Nt, 1:iter]))),
                                            MSE = Round(sfsmisc::integrate.xy(x = as.data.frame(G1.PRE)[, 1], 
                                                                             fx = (rowMeans(G1.PRE[, 2:(iter + 1)])- G1.TRUE)^2) +
                                                         sfsmisc::integrate.xy(x = as.data.frame(G1.PRE)[, 1],
                                                                               fx = apply(G1.PRE[, 2:(iter + 1)], 1, var)), num) 
                             )
                       )
  )
}

test.rmse <- distinct(Result[, c(1, 13)])


da.re <- as.data.frame(t(tab.summary))#[-1,]
colnames(da.re) <- da.re[1, ]
da.re <- da.re[-1, ]

(da.re <- data.frame( n = paste0("multirow{3}{*}{", n, "}"),
                      Method = paste0("multirow{3}{*}{ADM}"),
                      Criterion = rownames(da.re), da.re,
                      wts = mean(test.rmse$WTEST_RMSE)^2))

writexl::write_xlsx(da.re, path = paste0("./data/ADM_Table_1(", n,").xlsx"))
