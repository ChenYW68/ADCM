###################################################################
###################################################################
rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("CMAQ_NAQPMS_Grid_Cell", package = "ADCM")
data("SiteData", package = "ADCM")
data("China_BTH_GeoMap", package = "ADCM")
# CMAQ_PM25_COM$Flag <- ifelse(CMAQ_PM25_COM$CMAQ_ID %in% Choose_CMAQ_Index[, 1], 1, 0)
# save(CMAQ_PM25_COM, file = "./data/CMAQ_PM25_COM.RData")
###################################################################
###################################################################
Site <- setDF(Site)
Site$CITY <- as.character(Site$CITY)
Site_label <- plyr::ddply(Site[, c("LON", "LAT", "CITY")],
                          .(CITY),
                          plyr::summarize,
                          LON = mean(LON),
                          LAT  =  mean(LAT),
                          .progress = "text")
Site_label$LAT[5] <- Site_label$LAT[5] - 0.3
Site_label$LAT[12] <- Site_label$LAT[12] + 0.1
Site_label$LAT[3] <- Site_label$LAT[3] - 0.3
Site_label$LAT[6] <- Site_label$LAT[6] - 0.3
Site_label$LAT[7] <- Site_label$LAT[7] - 0.4
Site_label$LON[7] <- Site_label$LON[7] - 0.3
Site_label$LAT[11] <- Site_label$LAT[11] - 0.4


size <- c(12, 14, 5, 8)
CMAQ_PM25_COM.site <- CMAQ_Grid_Cell[distToNearestProvince <= 100,]
###################################################################
#                       Figure 1 (a)
###################################################################
{
  p1 <- ggmap::ggmap(GoogleMap_BTH) +
    geom_polygon(data = Map_BTH,
                 aes(x = long,y = lat, group = group),
                 colour = '#808080',
                 fill = NA) +
    geom_point(data = CMAQ_PM25_COM.site , aes(x = LON, y = LAT)
               , shape = 20,#ifelse(CMAQ_PM25_COM$Flag == 1, 20, 3), 
               col = ifelse(CMAQ_PM25_COM.site$Flag == 1,
                            "gray50", "gray50")##A9A9A9
               , size = 0.5) + #"#A9A9A9"
    geom_point(data = Site, aes(x = LON, y = LAT)
               , shape = 20
               , col = "red", size = 1) +
    # geom_text(aes(x = 112.2, y = 43
    #               , label =  "(a)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = size[3] + 2,
    # ) +
    geom_text(data = Site_label
              , aes(x = LON, y = LAT + 0.2
                    , label = CITY), 
              color = "black", size = size[3]) +
    # geom_label(y = 590,
    #            aes(x = 120, label = paste0("Corr = ", Corr)),
    #            size = 7, label.size = NA
    #            # ,family=c("serif")
    # ) +
    geom_text(aes(x = 112.5, y = 43.57
                   , label =  "(a)"),
               vjust = -0.1, 
               # fill = "transparent",
               # family = c("sans"),
               # fontface = 'bold',
               color = "black", size = size[4]) +
    # geom_label(aes(x = 112.8, y = 43.450
    #               , label =  "(a)"),
    #           vjust = -0.1, 
    #           label.size = NA,
    #           fill = "transparent",
    #           # family = c("sans"),
    #           # fontface = 'bold',
    #           color = "black", size = size[4]) +
    scale_x_continuous(limits=c(112.5, 121),
                       breaks = seq(113, 120.5, 2.5), 
                       labels = paste0(seq(113, 120.5, 2.5), "° E")) +
    scale_y_continuous(limits=c(35, 43.58),
                       breaks = seq(35, 43.5, 2),
                       labels = paste0(seq(35, 43.2, 2), "° N")
                       )+
    xlab("Longitude") + ylab("Latitude") +
    theme_bw() +
    theme(
      # axis.ticks = element_blank(),
      # axis.title = element_blank(),
      # axis.text = element_blank(),
      # axis.ticks = element_line(size = 0.5, colour = "black")
      axis.text = element_text(size = size[1])
      , axis.title= element_text(size = size[2], colour = "black")
      , legend.title = element_blank()
      , legend.position = "none"
      , panel.grid.major = element_blank() 
      , panel.grid.minor = element_blank()
    )
}
# p1
# ggsave(plot = p1, height = 6, width = 5, #dpi = 300, 
#        file = paste0(file, '/Fig1_Site_CMAQ.pdf'))

###################################################################
#                       Figure 1 (B)
###################################################################
CAQRA_Site <- NAQPMS_Grid_Cell[distToNearestProvince <= 100,]
{
  p2 <- ggmap::ggmap(GoogleMap_BTH) +
    geom_polygon(data = Map_BTH,
                 aes(x = long,y = lat, group = group),
                 colour = '#808080',
                 fill = NA) +
    geom_point(data = CAQRA_Site , aes(x = LON, y = LAT)
               , shape = 20,#ifelse(CMAQ_PM25_COM$Flag == 1, 20, 3), 
               col = "gray50"#ifelse(CMAQ_PM25_COM$Flag == 1, "#A9A9A9", "#A9A9A9")##A9A9A9
               , size = 0.5) + #"#A9A9A9"
    geom_point(data = Site, aes(x = LON, y = LAT)
               , shape = 20
               , col = "red", size = 1) +
    # geom_text(aes(x = 112.2, y = 43
    #               , label =  "(a)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = size[3] + 2,
    # ) +
    geom_text(data = Site_label
              , aes(x = LON, y = LAT + 0.2
                    , label = CITY), 
              color = "black", size = size[3]) +
    geom_text(aes(x = 112.5, y = 43.57
                  , label =  "(b)"),
              vjust = -0.1,
              # family = c("sans"),
              # fontface = 'bold',
              color = "black", size = size[4]) +
    scale_x_continuous(#expand=c(-0.1, 0),
                       limits=c(112.5, 121),
                       breaks = seq(113, 120.5, 2.5), 
                       labels = paste0(seq(113, 120.5, 2.5), "° E")) +
    scale_y_continuous(limits=c(35, 43.58),
                       breaks = seq(35, 43.5, 2),
                       labels = paste0(seq(35, 44, 2), "° N")  #paste0(seq(35, 43.2, 2), "° N")
                      )+
    xlab("Longitude") + ylab("") +
    theme_bw() +
    theme(
      # axis.ticks = element_blank(),
      # axis.title = element_blank(),
      # axis.text = element_blank(),
      # axis.ticks = element_line(size = 0.5, colour = "black")
       #   axis.text.y=element_blank()
       axis.ticks.y=element_blank()
      # , axis.title.y=element_blank()
      , axis.text = element_text(size = size[1])
      , axis.title = element_text(size = size[2], colour = "black")
      , legend.title = element_blank()
      , legend.position = "none"
      , panel.grid.major = element_blank() 
      , panel.grid.minor = element_blank()
    )
}


p <-cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
ggsave(plot = p, height = 8, width = 13,# dpi = 500,
       file = './figure/Fig1.pdf')

# ggsave(plot = p1, width = 12, height = 8,
#         file = './figure/Fig1_a.pdf')
# ggsave(plot = p2, width = 15, height = 6,
#         file = './figure/Fig1_b.pdf')
