setwd('L:/Il mio Drive/MedSea')
library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

POM_med<-read.csv('POM_Med.csv')
#Jon<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 
#             & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]
Aeg<-filter(POM_med, Zone ==  'Aeg')

mappat <- get_map(location = c(23,38, 28,41),maptype =c('toner'))
years_col<-c("#ff6995","#dc0041","#a15d5c","#ffaf95","#c16f00","#a0c30b","#00a56a","#4544b0")
Aeg1<-ggmap(mappat) + geom_point(data = Aeg, alpha=.8,size=5,
                                 aes(x = Longitude, y = Latitude, color=factor(Year), shape=Dataset))+
  geom_point(data = Aeg, alpha=.8,size=5,
             aes(x = Longitude, y = Latitude,  color=factor(Year), shape=Dataset))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "Aeg Med Sea") +
  scale_colour_manual(values=years_col[1:8])+
  scale_size_continuous(range = c(.2,20),limits=c(1.9,1406.16))+
  theme_minimal()#+
# theme(legend.position="none")
plot(Aeg1)

Aeg_1999<-filter(Aeg, Year ==  1999)
Aeg_2000<-filter(Aeg, Year ==  2000)

Aeg_1999[1:9,]
Aeg_2000[20:27,1:10]


png('POC_Aeg_2000.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,3),mar=c(2,1.1,2,0), 
    oma=c(0,4.2,3,2.2))
plot(Aeg_2000$POC_ugL[1:7], Aeg_2000$Depth[1:7], xlim=c(0,105), ylim=c(300,0), col=col_months[3], 
     bg='#AEA20044',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')
text(60,130,'31 Mar 39.43 | 25.73', cex=1.4)

plot(Aeg_2000$POC_ugL[8:11], Aeg_2000$Depth[8:11], xlim=c(0,200), ylim=c(300,0), 
     col=col_months[4], yaxt='n',
     bg='#64B20044',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')
par(new=T)
plot(Aeg_2000$POC_ugL[12:15], Aeg_2000$Depth[12:15], xlim=c(0,200), ylim=c(300,0), 
     col=col_months[4], yaxt='n',
     bg='#64B20044',type='b', 
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')
par(new=T)
plot(Aeg_2000$POC_ugL[16:19], Aeg_2000$Depth[16:19], xlim=c(0,200), ylim=c(300,0), 
     col=col_months[4], yaxt='n',
     bg='#64B20044',type='b', 
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')
legend(60,130,legend=c('2 Apr 39.67 | 25.75', '3 Apr 39.78 | 25.75','4 Apr 39.85 | 25.75'),
       cex=1.6, col=col_months[4], pch=c(21,8,22), bty='n')

plot(Aeg_2000$POC_ugL[20:23], Aeg_2000$Depth[20:23], xlim=c(0,200), ylim=c(300,0), 
     col=col_months[4], yaxt='n',
     bg='#64B20044',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')
par(new=T)
plot(Aeg_2000$POC_ugL[24:27], Aeg_2000$Depth[24:27], xlim=c(0,200), ylim=c(300,0), 
     col=col_months[4], yaxt='n',
     bg='#64B20044',type='b', 
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (2000)')

legend(60,130,legend=c('4 Apr 39.92 | 25.75', '6 Apr 39.98 | 25.75'),
       cex=1.6, col=col_months[4], pch=c(21,22), bty='n')
dev.off()


png('POC_Aeg_1999.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,8),mar=c(2,1.1,2,0), 
    oma=c(0,4.2,3,2.2))
plot(Aeg_1999$POC_ugL[1:7], Aeg_1999$Depth[1:7], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'22 Sep \n 39.43  | 25.73', cex=1.1)

plot(Aeg_1999$POC_ugL[8:11], Aeg_1999$Depth[8:11], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'24 Sep \n 39.67 | 25.75', cex=1.1)

plot(Aeg_1999$POC_ugL[12:15], Aeg_1999$Depth[12:15], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'22 Sep \n  39.78 | 25.75', cex=1.1)


plot(Aeg_1999$POC_ugL[16:18], Aeg_1999$Depth[16:18], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=24, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'24 Sep \n 39.85 | 25.75', cex=1.1)

plot(Aeg_1999$POC_ugL[19:22], Aeg_1999$Depth[19:22], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=25, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'25 Sep  \n 39.92 | 25.75', cex=1.1)

plot(Aeg_1999$POC_ugL[23:26], Aeg_1999$Depth[23:26], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch='*', cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'26 Sep \n 39.98 | 25.75', cex=1.1)

plot(Aeg_1999$POC_ugL[27:30], Aeg_1999$Depth[27:30], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'29 Sep \n 40.05 | 25.57', cex=1.1)

plot(Aeg_1999$POC_ugL[31:36], Aeg_1999$Depth[31:36], xlim=c(0,105), ylim=c(220,0), col=col_months[9], 
     bg='#B385FF44',type='b', yaxt='n',
     pch=3, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)')
text(60,130,'29 Sep \n 40.13 | 25.38', cex=1.1)
dev.off()

png('POC_Aeg_deep.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,1),mar=c(2,1.1,2,0), 
    oma=c(0,4.2,3,2.2))

plot(Aeg_1999$POC_ugL[37:44], Aeg_1999$Depth[37:44], xlim=c(0,105), ylim=c(1300,0), col=col_months[9], 
     bg='#B385FF44',type='b',fg="chartreuse3",
     pch=4, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Keycop (1999)') 
text(60,130,'30 Sep \n 40.23 | 25.18', cex=1.1)

dev.off()
