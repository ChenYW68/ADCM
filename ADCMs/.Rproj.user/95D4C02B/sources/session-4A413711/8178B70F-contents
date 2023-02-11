#########################################################################
rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015w);
season <- "Winter"
Width <- c(25, 1, 0.3)
library(latex2exp)
file <- paste0("./figure/")
#########################################################################
#########################################################################
{
  PM25_CMAQ <- obs_PM25_2015w %>% setorder(ID) %>%
    dplyr::select(ID, CITY
                  , DATE_TIME
                  , YEAR, MONTH, DAY
                  , YEAR_MONTH
                  , REAL_PM25
                  , sim_CMAQ_PM25
                  , sim50_CMAQ_PM25
                  , LON, LAT
                  , LON_X, LAT_Y)
  setDF(PM25_CMAQ)
  # PM25_CMAQ$SQRT_BIAS25 = sqrt(PM25_CMAQ$REAL_PM25) - sqrt(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$LOG_BIAS25 = log10(PM25_CMAQ$REAL_PM25) - log10(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$BIAS25 = PM25_CMAQ$REAL_PM25 - PM25_CMAQ$sim_CMAQ_PM25
  setDT(PM25_CMAQ)
  #########################################################################
  # pm25_cmaq_data<- plyr::ddply(PM25_CMAQ
  #                        , .(CITY, SITEID, MONTH, DATE_TIME)
  #                        , plyr::summarize
  #                        , REAL_PM25_Avg = mean(REAL_PM25 , na.rm = TRUE)
  #                        , SQRT_REAL_PM25_Avg = mean(sqrt(REAL_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_REAL_PM25_Avg = mean(log(REAL_PM25)
  #                                                   , na.rm = TRUE)
  #                        , CMAQ_PM25_Avg = mean(CMAQ_PM25 , na.rm = TRUE)
  #                        , SQRT_CMAQ_PM25_Avg = mean(sqrt(CMAQ_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_CMAQ_PM25_Avg = mean(log(CMAQ_PM25)
  #                                                   , na.rm = TRUE))
  pm25_cmaq_data <- PM25_CMAQ
  #####################################################################
  #  correlation map
  #####################################################################
  
  x0 = pm25_cmaq_data$REAL_PM25
  y0 = pm25_cmaq_data$sim_CMAQ_PM25
  
  x1 = sqrt(pm25_cmaq_data$REAL_PM25)
  y1 = sqrt(pm25_cmaq_data$sim_CMAQ_PM25)
  
  x2 = log(pm25_cmaq_data$REAL_PM25)
  y2 = log(pm25_cmaq_data$sim_CMAQ_PM25)
  # par(mfrow = c(1, 3))
  # plot(x0, y0)
  # plot(x1, y1)
  # plot(x2, y2)
  
  r0 = round(cor(x0, y0), 3)
  r1 = round(cor(x1, y1), 3)
  r2 = round(cor(x2, y2), 3)
  
  da <- rbind(data.frame(CMAQ = x0, PM25 = y0, z = abs(x0 - y0),
                         r = r0, FLAG = "Original scale", GROUP = 1),
              data.frame(CMAQ = x1, PM25 = y1, z = abs(x1 - y1),
                         r = r1, FLAG = "Square root scale", GROUP = 2))
  setDT(da)
}
################################################################
library(latex2exp)
size = c(25, 23, 25)

#####################################################################
#####################################################################
size = c(23, 20, 20)
g = 1
y.h = 0.19
Da <- da[GROUP ==g]
{
  P1 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1,
                   position="identity"
                   ,binwidth = Width[1], colour = "black") +
    geom_label(x= 1, y = 0.19, label = paste0("(a)"),
               size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() +
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = " ", y = "Density") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
  
  g = 2
  Da <- da[GROUP ==g]
  P2 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1,
                   position="identity"
                   ,binwidth = Width[2], colour = "black") +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    geom_label(x= 1, y = 0.19,
               label = paste0("(b)"),
               size = 12, label.size = 0) +
    # geom_label(x= 12, y = 0.16,
    #            label = paste0("11/01/15~01/31/16"),
    #            size = 10, label.size = 0) +
    theme_bw() +
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = TeX("Observed PM$_{2.5}$ at different scales"), y = "") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title = element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text = element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
}

#####################################################################
#####################################################################
library(cowplot)
p <- cowplot::plot_grid(P1, P2, nrow = 1)
#####################################################################
#####################################################################
ggsave(plot = p, paste0(file, "FigS1.pdf"),
       width = 14, height = 6.5)
#####################################################################
#####################################################################
