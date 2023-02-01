
# ADCM
Data and Codes for the paper: “Additive Dynamic Models for Correcting Numerical Model Outputs” by Y. Chen, X. Chang, F. Luo, and H. Huang. 

## Data

Daily PM2.5 concentrations of China's Beijing-Tianjin-Hebei (BTH) region from the Community Multiscale Air Quality (CMAQ) system and national monitoring stations. The datasets contain Winter of 2015 described in Section 2 of the manuscript. Besides PM2.5 concentrations, these datasets contain many necessary covariates, such as longitude, latitude, air pressure, temperature, dew point, cumulative wind power, and other variables.

There are 3 .RData files. 
-	CMAQ_PM25.RData contains numerical model outputs;
-	SiteData.RData is for 68 monitoring stations;
-	GeoMap.RData from other data sources. 

We have developed an R package - [ADCM](https://github.com/ChenYW68/ADCM/tree/main/ADCM/package) for this work. Using our $\texttt{ADCM}$ package, these data files can be loaded by using the ``data'' function. 

## Codes
There are two parts to our codes: 
1. The MEnKS-EM algorithm was written into the [ADCM](https://github.com/ChenYW68/HDCM/tree/main/ADCM/package) package in the R statistical environment;
2. A project entitled ``[ADCMs.Rproj](https://github.com/ChenYW68/ADCM/tree/main/ADCMs)'' in the [RStudio](https://www.rstudio.com/products/rstudio/download/) environment was built to reproduce all the results (e.g., figures and tables) in this work. 

```
# Require core package
1. R >= 4.1.1
2. Rcpp >= 1.0.7
```
## Installing and loading dependent packages
-	Open the project file, ``[ADCMs.Rproj](https://github.com/ChenYW68/ADCM/tree/main/ADCM)'', based on the [RStudio](https://www.rstudio.com/products/rstudio/download/) tool.

-	Install all the dependent packages via the following command:
```
source("./R/PSTVB_Packages.R")
```
Moreover, the [ADCM](https://github.com/ChenYW68/ADCM/tree/main/ADCM/package) package can be installed by running:
```
 install.packages("./package/ADCM_1.0.zip", repos = NULL, type = "win.binary")
```

## Data
Maps of the BTH region under different gridding systems with the locations of $68$ monitoring stations marked by the red dots. (a) Map with the centroids of $5{, }587$ $9$-km CMAQ grids (gray dots). (b) Map with the centroids of $2{, }141$ $15$-km NAQPMS grids (gray dots).


## An example for the proposed addictive dynamic correction model

################################ An example for fitting ADCM ############################

### Estimated nonlinear functions
Estimated nonlinear functions $\hat{g}(\cdot)$ with conditionally simulated 95\% confidence intervals (CI). Figures (a)-(e) represent the estimates of functions for time, surface temperature, surface pressure, and eastern and northern cumulative wind powers, respectively.
![ADCM](./ADCMs/figure/Fig10.png)

### Space-time calibration

Based on the ADCM, we perform a space-time calibration of the CMAQ system outputs for the entire BTH region using the proposed addictive dynamic correction model (ADCM). The following figure displays CMAQ numerical model outputs before and after the ADCM correction from November 26 to December 1, 2015. The
solid squares represent the average PM2.5 levels at the monitoring stations, i.e.,
![ADCM](./ADCMs/figure/Fig11.png)

In each of the 13 cities, the average PM2.5 concentration of all the stations in the city is marked using a solid square. The smoother the transition from the cities to the rural areas, the better the overall calibration results. It is evident that the before-calibration CMAQ outputs do not match well with most of the pollution data. After calibration, the transition from the cities to their surrounding areas becomes much smoother.
