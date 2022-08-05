# ADCM
Data and Codes for the paper: “Additive Dynamic Models for Calibrating Numerical Model Outputs” by Y. Chen, X. Chang, F. Luo, and H. Huang. 

## Data
Daily PM2.5 concentrations of China's Beijing-Tianjin-Hebei (BTH) region from the Community Multiscale Air Quality (CMAQ) system and national monitoring stations. The datasets contain data from November 1, 2015 to January 31, 2016. Besides PM2.5 concentrations, these datasets contain four gridded meteorological variables from the NAQPMS. All data are described in Section 2 of the manuscript.

There are 3 .RData files. 
-	Cali_ADCM_W.RData contains all covariables used to calibrate the CMAQ outputs in the BTH region, which is from 124,950 spatial locations;
-	SiteData.RData is for 68 monitoring stations;
-	CMAQ_NAQPMS_Grid_Cell contains grid cells of two numerical model outputs; 
-	GeoMap.RData is the map data from other data sources. 

We have developed an R package - [ADCM] for this work. Using our ADCM package, these data files can be loaded by using the ``data'' function. 

## Codes
There are two parts to our codes: 
(1) Our developed MEnKS-EM algorithm were written in the ADCM package in the R environment;
(2) A project entitled ``[ADCM_Code.Rproj] in the RStudio (https://www.rstudio.com/products/rstudio/download/) environment was built to reproduce all the results (e.g., figures and tables) in this work. 

```
## Required Core Packages
1. R >= 4.1.1
2. Rcpp >= 1.0.7
3. spam >= 2.9-0
```


To open the project file, ``[ADCM_Code.Rproj]'' based on the RStudio tool, in which the package ADCM can be installed using the following command: 
```
install.packages("./code/LoadPackages/ADCM_1.0.zip", repos = NULL, type = "win.binary")
```

Moreover, most of the dependent packages can be loaded (automatically installed if necessary), use the following command:
```
source("./code/LoadPackages/PSTVB_Packages.R")
```

