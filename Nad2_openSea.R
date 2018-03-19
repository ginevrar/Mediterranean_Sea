setwd('L:/Il mio Drive/MedSea')
library(ggplot2)
library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

POM_med<-read.csv('POM_Med.csv')

summary(POM_med$Zone)
Nad<-filter(POM_med, Zone ==  'Nad')

Nad_97<-filter(Nad, Year ==  1996)
Nad_97<-filter(Nad, Year ==  1997)

Nad_97[1:12,]
Nad_97[13:24,]
Nad_97[25:36,]
Nad_97[37:48,]
Nad_97[49:59,]

summary(factor(Nad_97$Station))

Nad_21<-filter(Nad_97, Station ==  2223)
Nad_22<-filter(Nad_97, Station ==  2224)
Nad_23<-filter(Nad_97, Station ==  2225)
Nad_24<-filter(Nad_97, Station ==  2226)
Nad_25<-filter(Nad_97, Station ==  2227)
Nad_26<-filter(Nad_97, Station ==  2228)
Nad_27<-filter(Nad_97, Station ==  2229)
Nad_28<-filter(Nad_97, Station ==  2230)
Nad_29<-filter(Nad_97, Station ==  2231)
Nad_30<-filter(Nad_97, Station ==  2232)
Nad_31<-filter(Nad_97, Station ==  2233)
Nad_32<-filter(Nad_97, Station ==  2234)
Nad_33<-filter(Nad_97, Station ==  2235)
Nad_34<-filter(Nad_97, Station ==  2236)
Nad_35<-filter(Nad_97, Station ==  2237)
Nad_36<-filter(Nad_97, Station ==  2240)
Nad_37<-filter(Nad_97, Station ==  2241)
Nad_38<-filter(Nad_97, Station ==  2242)
Nad_39<-filter(Nad_97, Station ==  2243)
Nad_40<-filter(Nad_97, Station ==  2244)




Nad_21_1<-filter(Nad_21, Longitude == 13.30)
Nad_21_2<-filter(Nad_21, Longitude == 13.34)
Nad_21_3<-filter(Nad_21, Longitude == 13.44)


Nad_26_1<-filter(Nad_26, Longitude == 12.59)
Nad_26_2<-filter(Nad_26, Longitude == 12.80)
Nad_31_1<-filter(Nad_31, Longitude==13.04)
Nad_31_2<-filter(Nad_31, Longitude==13.13)
Nad_32_1 <-filter(Nad_32,Longitude==13.18)
Nad_32_2 <-filter(Nad_32,Longitude==13.28)

Nad_33_1 <-filter(Nad_33,Longitude==13.22)
Nad_33_2 <-filter(Nad_33,Longitude==13.27)

Nad_34_1 <-filter(Nad_34,Longitude==13.35)
Nad_34_2 <-filter(Nad_34,Longitude==13.31)

Nad_35_1 <-filter(Nad_35,Longitude==13.44)
Nad_35_2 <-filter(Nad_35,Longitude==13.53)

Nad_36_1 <-filter(Nad_36,Longitude==13.04)
Nad_36_2 <-filter(Nad_36,Longitude==13.30)
Nad_36_3 <-filter(Nad_36,Longitude==13.39)

png('POC_NAD_1997.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,4),mar=c(4.5,1.1,2,0), 
    oma=c(2,4.2,3,2.2))
par(mfrow=c(1,4))
plot(Nad_21_1$POC_ugL, Nad_21_1$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_21_2$POC_ugL, Nad_21_2$Depth, xlim=c(0,400), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_21_3$POC_ugL, Nad_21_3$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,pch=c(21,22),legend=c('16 Jun 44.64 | 12.59','16 Jun 44.64 | 12.80'),
       col=col_months[6], bty='n',cex=1.4)

plot(Nad_22$POC_ugL, Nad_22$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('17 Jun 44.64 | 12.87','17 Jun 44.64 | 12.59','18 Jun 44.47 | 12.87'),
       cex=1.4, col=col_months[6], bty='n',pch=c(22,8,21))
par(new=T)
plot(Nad_23$POC_ugL, Nad_23$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_24$POC_ugL, Nad_24$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')


plot(Nad_25$POC_ugL, Nad_25$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_26_1$POC_ugL, Nad_26_1$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_26_2$POC_ugL, Nad_26_2$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')

legend(20,50,legend=c('18 Jun 44.47 | 13.08','19 Jun 44.47 | 12.59','19 Jun 44.47 | 12.80'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8,22))


plot(Nad_27$POC_ugL, Nad_27$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
text(90,130,'20 Jun ', cex=1.4)
par(new=T)
plot(Nad_28$POC_ugL, Nad_28$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('20 Jun 44.55 | 12.87','20 Jun 44.55 | 13.08'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8,22))
dev.off()

png('POC_NAD_1997b.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,4),mar=c(4.5,1.1,2,0), 
    oma=c(2,4.2,3,2.2))
plot(Nad_29$POC_ugL, Nad_29$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
text(90,130,'21 Jun 44.55 | 12.66', cex=1.4)
par(new=T)
plot(Nad_30$POC_ugL, Nad_30$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
text(90,130,'21 Jun 44.55 | 13.37', cex=1.4)

legend(20,50,legend=c('21 Jun 44.55 | 12.66','21 Jun 44.55 | 13.37'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,22))

plot(Nad_31_1$POC_ugL, Nad_31_1$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_31_2$POC_ugL, Nad_31_2$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('27 Jun 43.90 | 13.04','27 Jun 43.98 | 13.13'),
       cex=1.4, col=col_months[6],bty='n', pch=c(8,21))


plot(Nad_32_1$POC_ugL, Nad_32_1$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_32_2$POC_ugL, Nad_32_2$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('28 Jun 44.02 | 13.18','28 Jun 44.09 | 13.28'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8))

plot(Nad_33_1$POC_ugL, Nad_33_1$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_33_2$POC_ugL, Nad_33_2$Depth, xlim=c(0,250), ylim=c(60,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('29 Jun 43.93 | 13.22','28 Jun 43.96 | 13.27'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8))
dev.off()


png('POC_NAD_1997c.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,3),mar=c(4.5,1.1,2,0), 
    oma=c(2,4.2,3,2.2))

plot(Nad_34_1$POC_ugL, Nad_34_1$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_34_2$POC_ugL, Nad_34_2$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('30 Jun 43.91 | 13.35','30 Jun 43.87 | 13.31'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8))

plot(Nad_35_1$POC_ugL, Nad_35_1$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[6], 
     bg='#00BADE44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_35_2$POC_ugL, Nad_35_2$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[7], 
     bg='#00BADE44',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('1 Jul 43.85 | 13.44','1 Jul 43.93 | 13.53'),
       cex=1.4, col=col_months[6],bty='n', pch=c(21,8))

plot(Nad_36_1$POC_ugL, Nad_36_1$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[7], 
     bg='#00BADE44',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_36_2$POC_ugL, Nad_36_2$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[7], 
     bg='#00BADE44',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
par(new=T)
plot(Nad_36_3$POC_ugL, Nad_36_3$Depth, xlim=c(0,200), ylim=c(65,0), col=col_months[7], 
     bg='#00BADE44',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1996)')
legend(20,50,legend=c('2 Jul 43.90 | 13.04','2 Jul 43.74 | 13.30','2 Jul 43.81 | 13.39'),
       cex=1.4, col=col_months[7],bty='n', pch=c(21,8))
dev.off()

