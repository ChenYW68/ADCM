# # 1 loading packages ---------------------------------------
packages <- c("RandomFields", "data.table", "ggplot2"
              ,"plyr", "parallel","sqldf", "ADCM"
              , "randomForest", "mgcv", "spTimer", "rARPACK"
              , "spBayes", "latex2exp", "gstat", "ggtext"
              , "lubridate", "dplyr", "INLA", "spTDyn"
              , "Hmisc", "MASS", "inlabru", "tidyr", "reshape2"
              , "progress", "RODBC", "fields", "rgdal"
              , "cowplot", "invgamma", "geoR", "RColorBrewer"
              , "MBA", "scoringRules", "Rcpp", "writexl"
              , "SpecsVerification", "ggmap", "rgeos", "Matrix"
              , "verification", "mapproj", "sp", "mvnfast"
              , "rgdal", "rgeos", "raster", "grid", "jcolors") 
# ,'MASS'
# 2  library
for(i in 1:length(packages))
{
  if(!lapply(packages[i], require,
             character.only = TRUE)[[1]])
  {
    install.packages(packages[i])
    # library(packages[i])
    lapply(packages[i], require,
           character.only = TRUE)
  }else{lapply(packages[i], require,
               character.only = TRUE)}
}
# x=lapply(packages, require, character.only = TRUE)
# rm(list=ls())
rm(i, packages)


