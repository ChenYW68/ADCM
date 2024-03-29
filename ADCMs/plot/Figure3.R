rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "ADCM")
# setDT(Base_CAQRA_Table)
Sys.setlocale("LC_TIME", "English")
PM25_CMAQ <- obs_PM25_2015w
setDT(PM25_CMAQ)
da <- PM25_CMAQ %>% 
  plyr::ddply(.(CITY)
              , dplyr::summarize
              , Corr = round(mean(cor(sim_CMAQ_PM25, REAL_PM25)), 3)
              , LAT_label = round(mean(LAT), 2)
  )
setorder(da, LAT_label)
# da$City_lat <- paste0(da$CITY, " (lat: ", da$LAT, ")")
da$City_lat <- unique(paste0(da$CITY))
da$Latt <-  paste0("latitude: ", da$LAT_label, "° N")
da$LAT_label = rep(1:13, each = 1)

PM25_CMAQ <- PM25_CMAQ %>% left_join(da, by = c("CITY"))
setDT(PM25_CMAQ)
city <- unique(as.character(da$CITY))
Label <- as_labeller(c(`1` = da$City_lat[1], `2` =  da$City_lat[2],
                       `3` = da$City_lat[3], `4` =  da$City_lat[4],
                       `5` = da$City_lat[5], `6` =  da$City_lat[6],
                       `7` = da$City_lat[7], `8` =  da$City_lat[8],
                       `9` = da$City_lat[9], `10` =  da$City_lat[10],
                       `11` = da$City_lat[11], `12` =  da$City_lat[12],
                       `13` = da$City_lat[13]))
data_base0 <- PM25_CMAQ[CITY %in% c("Beijing", "Zhangjiakou")]

Up <- max(data_base0$REAL_PM25, data_base0$sim_CMAQ_PM25, na.rm = T) + 50
Low <- 0
library(latex2exp)
library(ggtext)
size <- c(18, 16)
p1 <- ggplot(data = data_base0) +
  geom_label(y = 590,
             aes(x = 120, label = paste0("Corr = ", Corr)),
             size = 7, label.size = NA
             # ,family=c("serif")
  ) +
  geom_richtext(x = (Up - Low)*1.25/2.52, label.size = NA,
                # fill  ="transparent",
                y = Up*0.90,
                angle = 61,
                label = "k = 2",
                color = "gray",
                size = 7) +
  geom_richtext(x = (Up)*0.92, label.size = NA,
                y = Up*0.86,
                angle = 43,
                label = "k = 1",
                size = 7) +
  geom_richtext(x = (Up)*0.88, label.size = NA,
                y = Up*0.48,
                angle = 25,
                label = "k = 0.5",
                color = "gray",
                size = 7) +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  scale_x_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
  # geom_abline(slope = 1, colour = "gray") +
  geom_point(aes(x = REAL_PM25, y = sim_CMAQ_PM25), 
             colour = "black", size = 1.5)  +
  labs(x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)"),
       y = TeX("CMAQ outputs ($μg/m^3$)")) +
  facet_wrap(~ LAT_label, ncol = 2
             , labeller = labeller(LAT_label = Label)) + theme_bw() +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title = element_text(size = size[1], colour = "black")
        , legend.title = element_text(size = size[1], colour = "black")
        , legend.text = element_text(size = size[2], colour = "black")
        , strip.text =  element_text(size = size[1], colour = "black"))

ggsave(plot = p1, paste0("./figure/", 'Fig3',".pdf"),
       width = 12, height = 6)

