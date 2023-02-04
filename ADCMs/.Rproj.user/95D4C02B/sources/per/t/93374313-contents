source("./LoadPackages/RDependPackages.R")
Database <- FALSE
if(isTRUE(Database)){
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd", believeNRows = FALSE, case = "toupper")
}
# ######################################################################
#                         Generating table 1
# ######################################################################
n <- 100; # Need to modify: n = 100 or 2000
# ######################################################################
# ######################################################################
# ######################################################################
Nt <- 30; iter <- 200
simu <- paste0("Sim_", n)
Simulation_wts <- paste0(simu, "_wts")
Simulation_Para <- paste0(simu, "_para")
Simulation_f <- paste0(simu, "_f")
start <- c(1, 3)
# ######################################################################
#                          beta
# ######################################################################
if(isTRUE(Database)){
 Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                  Simulation_Para,
                                  " where ITER <= ", iter), 
                   errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_ADCM_Para_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_Para
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))
colnames(Result)

beta.true <- c(15, 1, 1)
beta <- as.data.frame(unique(Result[, c(1:4)]))
# colnames(beta) <- c("ITER", paste0("BETA", 0:2))
# ######################################################################
#                          Smoothing functions
# ######################################################################
if(isTRUE(Database)){
Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                  Simulation_f,
                                  " where ITER <= ", iter), 
                   errors = F) %>% setorder("ITER")
}else{
  load(paste0("./data/Simulation/Sim_ADCM_f_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_f
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))

# setnames(Result, "X4", "Z")
# ######################################################################
#                          Summary for smoothing functions
# ######################################################################
num <- 5; r3 <- 1
f <- setDF(Result[, c("ITER", "G0", "G1", 
                      "G0_50", "G1_50", "TIME", "Z")])
for (i in 1:iter) {
  temp <- f[f$ITER == i, ] %>% setorder(TIME) %>% plyr::ddply(.(TIME), 
                   .fun = plyr::summarize, G0_50 = mean(G0_50, na.rm = TRUE), 
                   G0 = mean(G0, na.rm = TRUE), .progress = "text")
  if(i == 1){
    G0.PRE <- cbind(temp[, "TIME"], temp[, "G0"] ) 
  }
  G0.PRE <- cbind(G0.PRE, temp[, "G0_50"])
  # F1 <- temp[, "F1"]
  par(mfrow = c(1, 2))
  plot(G0.PRE[, 1], G0.PRE[, i + 1], col = "red")
  points(G0.PRE[, 1], G0.PRE[, i + 2], col = "blue")

  temp <- f[f$ITER == i, ]  %>% plyr::ddply(.(Z), 
           .fun = plyr::summarize, G1_50 = mean(G1_50, na.rm = TRUE), 
           G1 = mean(G1, na.rm = TRUE), .progress = "text")
  # temp <- temp[id, ]
  if(i == 1){
    G1.PRE <- cbind(temp[, "Z"], temp[, "G1"]) 
  }
  G1.PRE <- cbind(G1.PRE, temp[, "G1_50"])
  # F2 <- temp[, "F2"] 
  # X4 <- temp[, "X4_COM"] 
  plot(G1.PRE[, 1], G1.PRE[, i + 1], col = "red")
  points(G1.PRE[, 1], G1.PRE[, i + 2], col = "blue")
}
time <- G0.PRE[, 1]
z <- G1.PRE[, 1]

colnames(G0.PRE) <- c("t", "func", paste0("est", 1:iter))
colnames(G1.PRE) <- c("z", "func", paste0("est", 1:iter))
G0.PRE <- as.data.frame(G0.PRE) %>% setorder("t")
G1.PRE <- as.data.frame(G1.PRE) %>% setorder("z")
save(G0.PRE, G1.PRE, file = paste0("./data/G0_G1_PRE_", n, ".RData"))

# plot(G1.PRE$z, rowMeans(G1.PRE[, -c(1, 2)]), col = "blue",
#      lwd = 3, xlab = "z", ylab = "g_1(z)")
# points(G1.PRE$z, G1.PRE$func, col = "red", pch = 19, cex = 1, type = "l")

# ######################################################################
#                             Obtain table 1
# ######################################################################
{
  tab.summary <- cbind(variable = c("$\beta_0$", "$\beta_1$", "$\beta_2$", "$f_1$", "$f_2$"), 
                       rbind(data.frame(Bias = Round(mean(beta$BETA1 - beta.true[1]), num),
                                        Sd = Round(sd(beta$BETA1), num),
                                        MSE = Round((mean(beta$BETA1 - beta.true[1]))^2 +
                                                      var(beta$BETA1), num)),
                             data.frame(  
                               Bias = Round(mean(beta$BETA2 - beta.true[2]), num),
                               Sd = Round(sd(beta$BETA2), num),
                               MSE = Round((mean(beta$BETA2 - beta.true[2]))^2 +
                                             var(beta$BETA2), num)),
                             data.frame(
                               Bias = Round(mean(beta$BETA3 - beta.true[3]), num),
                               Sd = Round(sd(beta$BETA3), num),
                               MSE = Round((mean(beta$BETA3 - beta.true[3]))^2 +
                                             var(beta$BETA3), num)),
                             data.frame(   Bias = Round(sfsmisc::integrate.xy(x = G0.PRE[, 1], 
                                            fx = (rowMeans(G0.PRE[1:Nt, 3:(iter + 2)]) - G0.PRE[, 2])^2), num),
                                           
                                           # Bias.3.2 =r2[1]*mean((rowMeans(alpha.est1[1:Nt, 1:iter])- theta.1)^2),
                                           Sd = Round(sfsmisc::integrate.xy(x = G0.PRE[, 1],
                                                                            fx = apply(G0.PRE[, 3:(iter + 2)], 1, sd)
                                           ), num), 
                                           # Sd.3.2 = sqrt(mean(rowVar(alpha.est1[1:Nt, 1:iter]))),
                                           MSE= Round(sfsmisc::integrate.xy(x = G0.PRE[, 1], 
                                                                            fx = (rowMeans(G0.PRE[1:Nt, 3:(iter + 2)])- G0.PRE[, 2])^2) +
                                                        sfsmisc::integrate.xy(x = G0.PRE[, 1],
                                                                              fx = apply(G0.PRE[, 3:(iter + 2)], 1, var)), num) 
                             ),
                             data.frame(   Bias = Round(sfsmisc::integrate.xy(x = G1.PRE[, 1], 
                                                                              fx = (rowMeans(G1.PRE[, 3:(iter + 2)]) - G1.PRE[, 2])^2), num),
                                           
                                           # Bias.3.2 =r2[1]*mean((rowMeans(alpha.est1[1:Nt, 1:iter])- theta.1)^2),
                                           Sd = Round(sfsmisc::integrate.xy(x = G1.PRE[, 1],
                                                 fx = apply(G1.PRE[, 3:(iter + 2)], 1, sd)
                                           ), num), 
                                           # Sd.3.2 = sqrt(mean(rowVar(alpha.est1[1:Nt, 1:iter]))),
                                           MSE= Round(sfsmisc::integrate.xy(x = G1.PRE[, 1], 
                                                                            fx = (rowMeans(G1.PRE[, 3:(iter + 2)])- G1.PRE[, 2])^2) +
                                           sfsmisc::integrate.xy(x = G1.PRE[, 1],
                                                fx = apply(G1.PRE[, 3:(iter + 2)], 1, var)), num) 
                             )
                       )
  )
}

da.re <- as.data.frame(t(tab.summary))#[-1,]
colnames(da.re) <- da.re[1, ]
da.re <- da.re[-1, ]

################################################################################
#                         Prediction in the test sets
################################################################################
if(isTRUE(Database)){
Simulation_wts <- paste0(simu, "_WTS")
Result <- sqlQuery(DSN_01, paste0("select * from ", 
                                  Simulation_wts#,
                                  # " where ITER <= ", iter
                                  ), 
                   errors = F)
}else{
  load(paste0("./data/Simulation/Sim_ADCM_wts_", n, "_", start[1], "_", start[2], ".RData"))
  Result <- Simulation_wts
}
setDT(Result)
colnames(Result) <- toupper(colnames(Result))


x <- distinct(Result[Result$METHOD == 1, c("RMSE_YTEST", "ITER")])
(da.re <- data.frame( n = paste0("multirow{3}{*}{", n, "}"),
                      Method = paste0("multirow{3}{*}{ADCM}"),
                      Criterion = rownames(da.re), da.re,
                      Yts.test.MSE = mean(x$RMSE_YTEST)^2))

writexl::write_xlsx(da.re, 
                    path = paste0("./data/ADCM_Table_1(", n,").xlsx"))
################################################################################
#           Prediction for the process Wt(s) in the test sets
################################################################################
da <- (Result[Result$SIMU == "Test", ])
x0 <- da[METHOD == 1, c(2:4, 6, 7, 9)] %>% left_join(
        da[METHOD == 2, c(2:4, 6,  7, 9)],            #  1: true 2: prediction
        by = c("TIME", "LON", "LAT", "ITER"))
u <-  x0$W_TS.x - x0$W_TS.y
round(sqrt(sum(u^2)/length(u)), 5)
