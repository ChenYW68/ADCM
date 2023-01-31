rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
# First, need to run Call_ADCM_cv.R with Obj.Seq = 7 and save results to CV_Langfang.Rdata
load("./code/1_Cross_Validation/CV_Langfang.RData")
bs <- " 'cc' "; k <- 5
formula.exp <- paste0("REAL_PM25 ~ sim50_CMAQ_PM25  +
s(time.index, k = ", k + 4, ", bs = ", bs, ", m = 2) +
s(sim_TEMP, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_SPRESS, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_X, k = ", k, ", bs = ", bs, ", m = 2) +
s(sim_WIND_Y, k = ", k + 4, ", bs = ", bs, ", m = 2)")

Database = list(DSN = NULL,#"DSN_01", 
                # uid = "myname",
                # pwd = "mypwd",
                # believeNRows = FALSE,
                # case = "toupper",
                Table = "CV_LangFang"
)

star.time <- proc.time()
Cond.Simu(Obj =  Fit[[7]], 
               formula.exp, 
               Database = Database, 
               path = "./data/Con_Simu",
               # sim_Count = 100,
               tol.real = 1E-2,
               itMin = 10,
               itMax = 50,
               nThreads = 8)
end.time <- proc.time()
end.time - star.time 
