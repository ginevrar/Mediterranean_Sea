setwd('L:/Il mio Drive/MedSea')
library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

POM_med<-read.csv('POM_Med.csv')
#Jon<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 
#             & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]
Swe<-filter(POM_med, Zone ==  'Swe')

mappat <- get_map(location = c(5,36, 9,40),maptype =c('toner'))
years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")

Swe1<-ggmap(mappat) + geom_point(data = Swe, alpha=.8,size=5,
                                 aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Swe, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "Swe Med Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()#+
# theme(legend.position="none")
plot(Swe1)

png('POC_Swe.png',width = 28, height = 19, units = "cm", res = 300)
par(mfrow=c(1,1))
plot(Swe$POC_ugL[1:10], Swe$Depth[1:10], xlim=c(0,50), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'16 Sep 38.0 | 3.83', cex=1.3)
dev.off()
