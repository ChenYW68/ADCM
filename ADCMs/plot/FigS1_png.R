#########################################################################
# rm(list=ls())
source("./code/LoadPackages/PSTVB_Packages.R")
load("./Result/all_ADCM.RData")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015w);
season = "Winter"
Width = c(20, 1.0, 1) 
y.h = c(0.19, 0.1, 0.2)
t <- 1:92#seq(1, 92, 2)
library(latex2exp)
file <- paste0("./figure/")
#########################################################################
#########################################################################

Da <- as.data.frame(Fit$addtive.Fit$data$Obs.minus.mean)
Da$DATE_TIME <- as.Date(rownames(Fit$addtive.Fit$data$Obs.minus.mean))

Da <- gather(
              data = Da,
              key = "ID",
              value="Residuals" ,
              - DATE_TIME
            )
Da$ID <- as.numeric(Da$ID)


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
                  , LON_X
                  , LAT_Y) %>% left_join(Da, by = c("ID", "DATE_TIME"))
  setDF(PM25_CMAQ)
  # PM25_CMAQ$SQRT_BIAS25 = sqrt(PM25_CMAQ$REAL_PM25) - sqrt(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$LOG_BIAS25 = log10(PM25_CMAQ$REAL_PM25) - log10(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$BIAS25 = PM25_CMAQ$REAL_PM25 - PM25_CMAQ$sim_CMAQ_PM25
  setDT(PM25_CMAQ)
  #########################################################################
  # PM25_CMAQ<- plyr::ddply(PM25_CMAQ
  #                        , .(CITY, MONTH, DATE_TIME)
  #                        , plyr::summarize
  #                        , REAL_PM25 = mean(REAL_PM25 , na.rm = TRUE)
  #                        , sim_CMAQ_PM25 = mean(sqrt(sim_CMAQ_PM25)
  #                                                 , na.rm = TRUE)
  #                        , sim50_CMAQ_PM25 = mean(sqrt(sim50_CMAQ_PM25)
  #                                                    , na.rm = TRUE)
  #                        , Residuals = mean(Residuals
  #                                                   , na.rm = TRUE))
 
  DATE_TIME <- unique(PM25_CMAQ$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  pm25_cmaq_data <- PM25_CMAQ  %>% left_join(date.time, 
                                                      by = c("DATE_TIME"))
  
  #####################################################################
  #  correlation map
  #####################################################################
  
  x0 = pm25_cmaq_data$REAL_PM25
  y0 = pm25_cmaq_data$sim50_CMAQ_PM25
  
  x1 = sqrt(pm25_cmaq_data$REAL_PM25)
  y1 = sqrt(pm25_cmaq_data$sim_CMAQ_PM25)
  
  x2 = pm25_cmaq_data$REAL_PM25
  y2 = pm25_cmaq_data$Residuals
  # par(mfrow = c(1, 3))
  # plot(x0, y0)
  # plot(x1, y1)
  # plot(x2, y2)

  
  da <- rbind(data.frame(time.index = pm25_cmaq_data$time.index, 
                         CMAQ = x0, PM25 = y0, z = abs(x0 - y0),
                         FLAG = "Original scale", GROUP = 1),
              data.frame(time.index = pm25_cmaq_data$time.index, 
                         CMAQ = x1, PM25 = y1, z = abs(x1 - y1),
                         FLAG = "Square root scale", GROUP = 2),
              data.frame(time.index = pm25_cmaq_data$time.index,
                         CMAQ = x2, PM25 = y2, z = abs(x2 - y2),
                         FLAG = "Residuals from the model: y ~ 1 + x + g(z)",
                         GROUP = 3))
  setDT(da)
}
################################################################
library(latex2exp)
size = c(60, 50, 60)
# op <- par(mar = c(5, 5, 1, 1) + 1)
# par(mgp = c(14.0, 1.2, 110.3), cex.axis = 1.8, cex.lab = 2, cex.main = 2.0)


g = 1

Da <- da[GROUP ==g]
{
  P1 <- ggplot(Da, aes(PM25))  + 
    geom_histogram(aes(y = after_stat(density)), #/sum(after_stat(density)
                   alpha  = 0.1, 
                   position = "identity", binwidth = Width[1], 
                   colour = "black") +
    # geom_label(x= -.5, y = 0.19, label = paste0("(a)"),
    #            size = 16, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() + 
    scale_y_continuous(limits = c(0, y.h[1])) +
    # labs(x = " ", y = "Density") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          # , axis.title.y = element_text()
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
    geom_histogram(aes(y=after_stat(density)), #/sum(after_stat(density))
                   alpha=0.1, 
                   position="identity", binwidth = Width[2], colour = "black") +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    # geom_label(x= -0.5, y = 0.19, 
    #            label = paste0("(b)"),
               # size = 16, label.size = 0) +
    # geom_label(x= 12, y = 0.16, 
    #            label = paste0("11/01/15~01/31/16"),
    #            size = 10, label.size = 0) +
    theme_bw() + 
    scale_y_continuous(limits = c(0, y.h[2])) +
    # labs(x = TeX("Observed PM$_{2.5}$ at different scales"), y = "") +
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
}
# 
# #####################################################################
g = 3
Da <- da[GROUP ==g & time.index %in% c(t)]
# Da <- data.frame(FLAG = "Original scale",PM25= as.vector(Simu_data$W_ts))
{
  P3 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y = after_stat(density)), #/sum(after_stat(density))
                   alpha = 0.1,
                   position = "identity", binwidth = Width[3], 
                   colour = "gray50")  +
    stat_function(fun = dnorm,
                  args = list(mean = mean(Da$PM25),
                              sd = sd(Da$PM25)),
                  col = "black",##1b98e0
                  size = 1) +
    # geom_label(x= 0.5, y = 0.19, label = paste0("(c)"),
    #            size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    scale_x_continuous(limits = c(floor(min(Da$PM25)), 
                                  ceiling(max(Da$PM25)))
                       , breaks = seq(floor(min(Da$PM25)), 
                                      ceiling(max(Da$PM25)), 2)
                       , labels = seq(floor(min(Da$PM25)), 
                                      ceiling(max(Da$PM25)), 2)
                       , expand = c(0, 0)
    ) +
    theme_bw() +
    # labs(x = "", y = "") +
    labs(x = TeX("Residuals"), y = "Density") +
    scale_y_continuous(limits = c(0, y.h[3])) +
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
}
#####################################################################
#####################################################################
library(cowplot)
# p <- cowplot::plot_grid(P1, P2, nrow = 1)
#####################################################################
#####################################################################
ggsave(plot = P1, paste0(file, 'FigS1_', season,".png"),  
       dpi = 300, width = 8, height = 7)

ggsave(plot = P2, paste0(file, 'FigS2_', season,".png"),  
       dpi = 300, width = 8, height = 7)

ggsave(plot = P3, paste0(file, 'FigS3_', season,".png"),  
       dpi = 300, width = 8, height = 7)
#####################################################################
#####################################################################