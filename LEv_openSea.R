setwd('L:/Il mio Drive/MedSea')
library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

POM_med<-read.csv('POM_Med.csv')
#Jon<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 
#             & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]
Lev<-filter(POM_med, Zone ==  'Lev')

mappat <- get_map(location = c(20,32, 28,37),maptype =c('toner'))
years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")

Lev1<-ggmap(mappat) + geom_point(data = Lev, alpha=.8,size=5,
                                 aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Lev, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "Levantine Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()#+
# theme(legend.position="none")
plot(Lev1)

png('POC_Lev.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,6),mar=c(2,1.1,2,0), 
    oma=c(0,4.2,3,2.2))
plot(Lev$POC_ugL[1:10], Lev$Depth[1:10], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'20 Sep \n 33.90 | 22.02', cex=1.3)

plot(Lev$POC_ugL[11:20], Lev$Depth[11:20], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'20 Sep \n 33.97 | 22.05', cex=1.3)

plot(Lev$POC_ugL[21:30], Lev$Depth[21:30], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'21 Sep  \n 33.99 | 22.01', cex=1.3)

plot(Lev$POC_ugL[31:40], Lev$Depth[31:40], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'24 Sep \n 33.96 | 22.00', cex=1.3)

plot(Lev$POC_ugL[41:50], Lev$Depth[41:50], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'23 Sep  \n 33.93 | 22.01', cex=1.3)

plot(Lev$POC_ugL[51:58], Lev$Depth[51:58], xlim=c(0,105), ylim=c(200,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='PROSOPE (1999)')
text(60,130,'24 Sep \n 33.88 | 21.95', cex=1.3)
dev.off()
