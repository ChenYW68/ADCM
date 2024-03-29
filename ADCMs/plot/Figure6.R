rm(list=ls())
source("./LoadPackages/RDependPackages.R")
source("./3_Simulation/Simulation 1/Simu_stData.R")
load("./3_Simulation/Simulation 1/Simu_data.RData")
data("China_BTH_GeoMap", package = "ADCM")

Simu_data0 <- Simu_data
size = c(18, 16, 10)
p1 <- ggplot() + coord_equal()+ 
  geom_polygon(data = Map_BTH,
               aes(x = long,y = lat, group = group),
               colour = 'gray25', size = 0.5,
               fill = NA)+
  scale_x_continuous(limits=c(113.0, 120),
                     breaks = seq(113, 120, 2), 
                     labels = paste0(seq(113, 120, 2), "° E")) +
  scale_y_continuous(limits = c(36, 42.7),
                     breaks = seq(36, 42.6, 2),
                     labels = paste0(seq(36, 42.6, 2), "° N"))+
  geom_point(data = Simu_data$Site, aes(x = LON, y = LAT)
             , shape = 20, col = "black", size = 1) +
  geom_text(aes(x = 113, y = 42.7
                , label =  "(a)"),
            # family = c("sans"),
            # fontface = 'bold',
            color = "black", size = 8) +
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude") +
  theme( axis.ticks = element_line(size = 1, colour = "black")
         , axis.text = element_text(size = size[2], colour = "black")#, face = "bold"
         , axis.title= element_text(size = size[1], colour = "black")
         , legend.title = element_blank()
         , legend.position = "none"
         , panel.grid.major = element_blank()
         , panel.grid.minor = element_blank()
  )
###############################################################################
# ggsave(plot = p1, height = 6, file = './Simulation/R/figure/Simu_Loc.pdf')
###############################################################################
para <- list( n = 100  #
              , Nt = 30
              , beta = c(5e0, 1e0, 1e0)
              , sill = c(1e0)
              , scale_s = as.numeric(100)
              , scale_t = as.numeric(5) # the smoothness in time: (0, 1]
              , sep = 8e-1 # space–time interaction: [0, 1]
              , power_s = 8e-1 #(0, 2]
              , power_t = 5e-1         # >= 0
              , nugget = 1e-1
)
# library(semiBase)
Simu_data <- Simu_stData(para = para, site = Simu_data0$Site, 
                         W_ts = Simu_data0$W_ts)
Site.train <- Simu_data$Site[Simu_data$Site$Simu == "Train",]

p2 <- ggplot() + coord_equal()+ 
  geom_polygon(data = Map_BTH,
               aes(x = long,y = lat, group = group),
               colour = 'gray25', size = 0.5,
               fill = NA)+
  scale_x_continuous(limits=c(113.0, 120),
                     breaks = seq(113, 120, 2), 
                     labels = paste0(seq(113, 120, 2), "° E")) +
  scale_y_continuous(limits = c(36, 42.7),
                     breaks = seq(36, 42.6, 2),
                     labels = paste0(seq(36, 42.6, 2), "° N"))+
  geom_point(data = Site.train, aes(x = LON, y = LAT)
             , shape = 3
             , col = "black", size = 2) +
  geom_text(aes(x = 117.8, y = 36.0
                , label =  paste0("+: Randomly sampled locations")),
            color = "black", size = 5.5) +
  geom_text(aes(x = 113, y = 42.70
                , label =  "(b)"),
            # family = c("sans"),
            # fontface = 'bold',
            color = "black", size = 8) +
  theme_bw() + 
  labs(x =  "Longitude", y = "Latitude") +
  theme(  axis.ticks.y=element_blank()
          , axis.ticks = element_line(size = 1, colour = "black")
         , axis.text = element_text(size = size[2], colour = "black")#, face = "bold"
         , axis.title= element_text(size = size[1], colour = "black")
         , legend.title = element_blank()
         , legend.position = "none"
         , panel.grid.major = element_blank()
         , panel.grid.minor = element_blank()
  )
p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
###############################################################################
ggsave(plot = p, height = 6, width = 12, file = './figure/Fig6.pdf')
###############################################################################