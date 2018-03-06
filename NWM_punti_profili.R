library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

mappa1 <- get_map(location = c(4,41.5, 8.5,44),maptype =c('toner'))
mappa1b <- get_map(location = c(10.5,41.3, 12.5,43),maptype =c('toner'))

Nwm_1<-POM_med[(POM_med$Latitude>39.5 & POM_med$Longitude<=9),]
Nwm_2<-POM_med[(POM_med$Latitude>41 & POM_med$Latitude<44 & POM_med$Longitude<=13.5),]
Nwm<-rbind(Nwm_1,Nwm_2)

summary(Nwm$POC_ugL)

years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")

nw1<-ggmap(mappa1) + geom_point(data = Nwm, alpha=.8,size=5,
                           aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Nwm, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "North Western Med Sea") +
    scale_colour_manual(values=years_col[1:8])+
scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()+
  theme(legend.position="none")
  
nw21<-ggmap(mappa1b) + geom_point(data = Nwm, alpha=.8,size=5,
                                 aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Nwm, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "North Western Med Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()
#  theme(legend.position="none")


#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

library(ggpubr)


png(file = "NWM_punti_anni2.png",width = 24, height = 18, units = "cm", res = 300)
ggarrange(nw1, nw21, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()


getwd()
  
