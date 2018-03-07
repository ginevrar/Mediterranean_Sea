library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

#Tir<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 
 #             & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]
Tir<-filter(POM_med, Zone ==  'Tir')

mappat <- get_map(location = c(10,37, 16,41.5),maptype =c('toner'))
years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")

tir1<-ggmap(mappat) + geom_point(data = Tir, alpha=.8,size=5,
                                aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Tir, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "Tirrenian Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()#+
 # theme(legend.position="none")

png(file = "Tir_punti_anni.png",width = 24, height = 18, units = "cm", res = 300)
plot(tir1)
dev.off()
