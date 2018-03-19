setwd('L:/Il mio Drive/MedSea')
library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

POM_med<-read.csv('POM_Med.csv')
#Jon<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 
#             & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]
Alb<-filter(POM_med, Zone ==  'Alb')


Alb<-POM_med[(POM_med$Longitude<=-0.5),]; #write.csv(Alb, file='POC_alboran.csv')

  mappat <- get_map(location = c(-7,35, 0,38),maptype =c('toner'))
years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")

alb1<-ggmap(mappat) + geom_point(data = Alb, alpha=.8,size=5,
                                 aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Alb, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "Alboran Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()#+
# theme(legend.position="none")
plot(alb1)

ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
col_months<-ggplotColours(n = 11)

ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
col_months<-ggplotColours(n = 11)


png('POC_Alb.png',width = 28, height = 19, units = "cm", res = 300)
par(mfrow=c(1,2))
plot(Alb$POC_ugL[1:9], Alb$Depth[1:9], xlim=c(0,120), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'14 Sep 36.10 | -5.20', cex=1.3)

plot(Alb$POC_ugL[10:19], Alb$Depth[10:19], xlim=c(0,120), ylim=c(200,0), 
     bg='#B385FF44',col=col_months[9], type='b', main='PROSOPE (1999)',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2)
text(60,130,'15 Sep 36.41 | -0.85', cex=1.3)
dev.off()
Alb[1:9]
