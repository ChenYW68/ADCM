source("./LoadPackages/RDependPackages.R")
# source("./code/2_Calibration/3_3_Cali_CMAQ_Prediction(mean).R")
load("./2_Calibration/Calibrated_Data.RData")
y.max <- ceiling(max(Calibrated_Data$CMAQ_PM25, PM2.5$REAL_PM25, na.rm = T))
y.max

Label <- as_labeller(c(`1` = "After correction", 
                       `2` = "Before correction"))
# y.max <- 610
int <- 1e2
######################################################################
######################################################################
{
  p1 <- ggplot() +
    geom_point(data = Calibrated_Data
               , aes(x = LON, y = LAT
                     , color = CMAQ_PM25
                     , fill =  CMAQ_PM25
               )
               , size = 0.8, pch = 20) +
    geom_point(data = PM2.5#[day(DATE_TIME)==18]
               , aes(x = LON, y = LAT
                     , color = REAL_PM25
                     , fill =  REAL_PM25
               )
               , size = 1.2, pch = 18) +
    facet_grid(Method ~ Date
               , labeller = labeller(Method = Label), switch = "both") +
    scale_x_continuous(limits = c(113.5, 120),
                     breaks = seq(113.5, 120, 2), 
                     labels = paste0(seq(113.5, 120, 2), "° E")) +
    scale_y_continuous(limits = c(36, 42.6),
                       breaks = seq(36, 42.6, 2),
                       labels = paste0(seq(36, 42.6, 2), "° N"))+
    theme_bw() +
    theme(
      axis.text = element_blank()
      , axis.title = element_blank()
      , axis.ticks = element_blank()
      # rect = element_blank()
      # axis.text = element_text(size = 12, colour = "black")
      #     , axis.text.x = element_text(angle=90, hjust=1)
      #     , axis.title = element_text(size = 14, colour = "black")
      , legend.title = element_text(size = 10, colour = "black")
      , legend.text = element_text(size = 10, colour = "black")
      , legend.key.width = unit(7.8,"line")
      , legend.key.height = unit(1,"line")
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , legend.position = "bottom"
      , legend.background = element_rect("transparent")
      , legend.margin= ggplot2::margin(t = 10, 0, 0, 0)
      # , legend.box.margin= margin(t=10,0,0,0)
      , legend.box.spacing = ggplot2::margin(t = -5,0,0,0)
      , strip.text = element_text(size = 10, colour = "black")
    ) + guides(fill = "none")#+ guides(col = "none")
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
  sc <- scale_colour_gradientn(colours = myPalette(nrow(unique(Calibrated_Data[Calibrated_Data$Method == 1, c("LON", "LAT")]))), #
                               , limits = c(0, y.max), 
                               name = expression(atop(atop(textstyle("PM"[2.5]),
                                                           textstyle(paste("(", mu, g, "/", m^{3},")"))), NA)),
                               breaks = c(0, round(c(seq(0, y.max - int, int)), 0)[-1], y.max),
                               labels = c(0, round(c(seq(0, y.max - int, int)), 0)[-1], y.max))
  
  p1 <- p1 + sc
}
ggsave(plot = p1, file = "./figure/Fig11.pdf", 
       width = 9, height = 4, dpi = 300)

# save(ADCM, file = "./ADCM_Process.RData")
