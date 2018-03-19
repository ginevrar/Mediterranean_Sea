setwd('L:/Il mio Drive/MedSea')
library(ggplot2);library(rworldmap);library(ggmap);
library(mapproj);library(gridExtra);library(dplyr)
setwd('C:/Users/gi/Dropbox')

POM_med<-read.csv('POM_Med.csv')
summary(POM_med$Zone)
str(POM_med)
ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
col_months<-ggplotColours(n = 11)

Nad<-filter(POM_med, Zone==  'Nad')
Nad_97<-filter(Nad, Year ==  1997)

Nad_97_feb<-filter(Nad_97, Month ==  2)
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

Nad_41<-filter(Nad_97, Station ==  2245)
Nad_42<-filter(Nad_97, Station ==  2248)
Nad_43<-filter(Nad_97, Station ==  2249)
Nad_44<-filter(Nad_97, Station ==  2250)
Nad_45<-filter(Nad_97, Station ==  2251)
Nad_46<-filter(Nad_97, Station ==  2252)
Nad_47<-filter(Nad_97, Station ==  2254)
Nad_48<-filter(Nad_97, Station ==  2255)

Nad_26_1<-filter(Nad_26, Longitude == 13.09)
Nad_26_2<-filter(Nad_26, Longitude == 13.16)
Nad_31_1<-filter(Nad_31, Longitude==13.09)
Nad_31_2<-filter(Nad_31, Longitude==13.30)
Nad_31_3<-filter(Nad_31, Longitude==13.16)

Nad_33_1 <-filter(Nad_33,Longitude==13.22)
Nad_33_2 <-filter(Nad_33,Longitude==13.27)

Nad_34_1 <-filter(Nad_34,Longitude==13.06)
Nad_34_2 <-filter(Nad_34,Longitude==13.31)

Nad_36_1 <-filter(Nad_36,Longitude==12.58)
Nad_36_2 <-filter(Nad_36,Longitude==12.65)

Nad_21_1<-filter(Nad_21, Longitude == 13.30)
Nad_21_2<-filter(Nad_21, Longitude == 13.34)
Nad_21_3<-filter(Nad_21, Longitude == 13.44)

Nad_22_1<-filter(Nad_22, Latitude == 43.78)
Nad_22_2<-filter(Nad_22, Latitude ==  43.84)
Nad_22_3<-filter(Nad_22, Latitude ==  43.89)

Nad_23_1<-filter(Nad_23, Latitude ==  43.75)
Nad_23_2<-filter(Nad_23, Latitude ==  43.80)
Nad_23_3<-filter(Nad_23, Latitude ==  43.86)

Nad_24_1<-filter(Nad_24, Latitude ==  43.78)
Nad_24_2<-filter(Nad_24, Latitude ==  43.80)
Nad_24_3<-filter(Nad_24, Latitude ==  43.89)

Nad_28_1<-filter(Nad_28, Latitude ==  44.47)
Nad_28_2<-filter(Nad_28, Latitude ==  44.49)
Nad_28_3<-filter(Nad_28, Latitude ==  44.52)

Nad_30_1<-filter(Nad_30, Latitude ==  44.57)
Nad_30_2<-filter(Nad_30, Latitude ==  44.60)

Nad_42_1<-filter(Nad_42, Longitude==13.08)
Nad_42_2<-filter(Nad_42, Longitude==13.13)
Nad_42_3<-filter(Nad_42, Longitude==13.22)

Nad_45_1<-filter(Nad_45, Longitude==13.18)
Nad_45_2<-filter(Nad_45, Longitude==13.23)
Nad_45_3<-filter(Nad_45, Longitude==13.32)


max(Nad_21$POC_ugL)
png('POC_NADfeb_1997.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,4),mar=c(4.5,1.1,2,0), 
    oma=c(2,4.2,3,2.2))
par(mfrow=c(1,4))
plot(Nad_21_1$POC_ugL, Nad_21_1$Depth, xlim=c(0,1250), ylim=c(40,0),
     col=col_months[2], 
     bg='#DB8E0044',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,
     ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_21_2$POC_ugL, Nad_21_2$Depth, xlim=c(0,1250),
     ylim=c(40,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
legend(150,20,pch=c(21,23),
       legend=c('11 Feb 43.75 | 13.30',
                '11 Feb 43.77 | 13.34'),
       col=col_months[2], bty='n',cex=1.4)

plot(Nad_22_1$POC_ugL, Nad_22_1$Depth, 
     xlim=c(0,1100),
     ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,
     ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_22_2$POC_ugL, Nad_22_2$Depth, 
     xlim=c(0,1100), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='')
par(new=T)
plot(Nad_23_1$POC_ugL, Nad_23_1$Depth, xlim=c(0,1100),
     ylim=c(50,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
legend(20,30,legend=c('11 Feb 43.86 | 13.44',
                      '12 Feb 43.78 | 13.24',
                      '13 Feb 43.75 | 13.30'),
       cex=1.4, col=col_months[2], bty='n',
       pch=c(23,8,21))

plot(Nad_21_3$POC_ugL, Nad_21_3$Depth, xlim=c(0,200),
     ylim=c(50,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_23_2$POC_ugL, Nad_23_2$Depth, xlim=c(0,200),
     ylim=c(50,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,
     ylab='Depth (m)',xlab='POC ug/l',lty=4,
     main='Medar (1997)')
par(new=T)
plot(Nad_23_3$POC_ugL, Nad_23_3$Depth, xlim=c(0,200),
     ylim=c(50,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
legend(10,30,legend=c('11 Feb 43.86 | 13.44',
                      '13 Feb 43.80 | 13.37',
                      '13 Feb 43.86 | 13.44'),
       cex=1.4, col=col_months[2], 
       bty='n',pch=c(21,23,8))

plot(Nad_22_3$POC_ugL, Nad_22_3$Depth, 
     xlim=c(0,350), ylim=c(50,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,
     ylab='Depth (m)',xlab='POC ug/l',lty=5,
     main='Medar (1997)')
par(new=T)
plot(Nad_24_3$POC_ugL, Nad_24_3$Depth, 
     xlim=c(0,350), ylim=c(50,0), 
     col=col_months[2], yaxt='n',
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,
     ylab='Depth (m)',xlab='POC ug/l',lty=3,
     main='Medar (1997)')
par(new=T)
plot(Nad_24_2$POC_ugL, Nad_24_2$Depth, 
     xlim=c(0,350), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')

legend(90,20,legend=c('12 Feb 43.89 | 13.39',
    '14 Feb 43.89 | 13.39','14 Feb 43.80 | 13.29'),
       cex=1.4, col=col_months[2], 
    bty='n',pch=c(22,8,21))

dev.off()


png('POC_NADfeb2_1997.png',width = 29, height = 19, units = "cm", res = 300)
par(mfrow=c(1,4),mar=c(4.5,1.1,2,0), 
    oma=c(2,4.2,3,2.2))
par(mfrow=c(1,4))
plot(Nad_25$POC_ugL, Nad_25$Depth, 
     xlim=c(0,600), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_27$POC_ugL, Nad_27$Depth, xlim=c(0,600), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='')
par(new=T)
plot(Nad_30_2$POC_ugL, Nad_30_2$Depth, xlim=c(0,600), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
legend(90,20,legend=c(
                      '15 Feb 43.75 | 13.30',
                      '24 Feb 44.65 | 12.97',
                      '26 Feb 44.60 | 12.85'),
       cex=1.4, col=col_months[2], 
       bty='n',pch=c(8,21,23))
# 2II

plot(Nad_24_1$POC_ugL, Nad_24_1$Depth, 
     xlim=c(0,520), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T) 
plot(Nad_29$POC_ugL, Nad_29$Depth, xlim=c(0,520), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_31_1$POC_ugL, Nad_31_1$Depth, xlim=c(0,520), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_31_2$POC_ugL, Nad_31_2$Depth, xlim=c(0,520), ylim=c(60,0),
     col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=24, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='')
legend(90,20,legend=c('14 Feb 43.78 | 13.24 ',
                      '26 Feb 44.51 | 13.05 ','27 Feb 44.59 | 13.19','27 Feb 44.48 | 13.30'), 
       cex=1.4, col=col_months[2],bty='n',pch=c(21, 22,8,24))   


plot(Nad_26_1$POC_ugL, Nad_26_1$Depth, xlim=c(0,300), ylim=c(60,0), col=col_months[2], 
     bg='#00C1A744',type='b',  yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main=' ')
par(new=T)
plot(Nad_26_2$POC_ugL, Nad_26_2$Depth, xlim=c(0,300), ylim=c(60,0), 
     col=col_months[2],bg='#DB8E0044',type='b',  yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T) #350
plot(Nad_28_1$POC_ugL, Nad_28_1$Depth, xlim=c(0,300), ylim=c(60,0),
     col=col_months[2], bg='#DB8E0044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='')
par(new=T)
plot(Nad_30_1$POC_ugL, Nad_30_1$Depth, xlim=c(0,300), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
legend(40,40,legend=c('24 Feb 44.59 | 13.09 ',
                      '24 Feb 44.55 | 13.16','25 Feb 44.47 | 13.12',
                      '26 Feb 44.57 | 12.92'), 
       cex=1.4, col=col_months[2], 
       bty='n',pch=c(8,22,21))   #ok


plot(Nad_32$POC_ugL, Nad_32$Depth, xlim=c(0,300), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_33$POC_ugL, Nad_33$Depth, xlim=c(0,300), ylim=c(60,0), col=col_months[2], 
     bg='#DB8E0044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main=' ')
legend(20,40,legend=c('28 Feb 44.57 | 12.92 ','28 Feb 44.62 | 13.03'),
                      cex=1.4, col=col_months[2],bty='n',pch=c(21, 8))   
dev.off()

Nad_36_1<-filter(Nad_36, Longitude==12.58)
Nad_36_2<-filter(Nad_36, Longitude==12.65)

Nad_37_1<-filter(Nad_37, Longitude==12.73)
Nad_37_2<-filter(Nad_37, Longitude==12.80)

Nad_41_1<-filter(Nad_41,Longitude==12.58)
Nad_41_2<-filter(Nad_41,Longitude==12.65)

dev.new()
par(mfrow=c(1,4),mar=c(4.5,1.1,2,0), oma=c(2,4.2,3,2.2))
plot(Nad_34_1$POC_ugL, Nad_34_1$Depth, xlim=c(0,200), ylim=c(60,0), 
     col=col_months[3], 
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Medar (1997)')
par(new=T)
plot(Nad_34_2$POC_ugL, Nad_34_2$Depth, xlim=c(0,200), ylim=c(60,0), col=col_months[3], 
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main=' ')
par(new=T)
plot(Nad_35$POC_ugL, Nad_35$Depth, xlim=c(0,200), ylim=c(60,0), col=col_months[3], 
     type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)

legend(20,40,legend=c('01 Mar 44.50 | 13.06 ','01 Mar 44.44 | 13.18',
                      '01 Mar 44.54 | 12.99'),
       cex=1.4, col=col_months[2],bty='n',pch=c(21,23, 8))   
#2
plot(Nad_36_1$POC_ugL, Nad_36_1$Depth, xlim=c(0,200), ylim=c(60,0),
     col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_37_1$POC_ugL, Nad_37_1$Depth, xlim=c(0,200), ylim=c(60,0), 
     col=col_months[6], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2,main='Medar (1997)')
legend(20,40,legend=c('13 Jun 44.73 | 12.58 ',
                      '14 Jun 44.63 | 12.73'),
       cex=1.4, col=col_months[2],bty='n',pch=c(21, 8))   

plot(Nad_36_2$POC_ugL, Nad_36_2$Depth, xlim=c(0,200), ylim=c(60,0),
     col=col_months[3], 
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_37_2$POC_ugL, Nad_37_2$Depth, xlim=c(0,200), ylim=c(60,0), col=col_months[3], 
     bg='#00C1A744',type='b', yaxt='n',
     pch=24, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_38$POC_ugL, Nad_38$Depth, xlim=c(0,200), ylim=c(60,0),
     col=col_months[3], 
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
legend(20,40,legend=c('13 Jun 44.73 | 12.65', '14 Jun 44.73 | 12.80 ','15 Jun 44.63 | 12.65'),
       cex=1.4, col=col_months[2],bty='n',pch=c(21,24, 8))   
dev.new()
plot(Nad_39$POC_ugL, Nad_39$Depth, xlim=c(0,1000), ylim=c(60,0),
     col=col_months[3], 
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_41_1$POC_ugL, Nad_41_1$Depth, xlim=c(0,1000), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_41_2$POC_ugL, Nad_41_2$Depth, xlim=c(0,1000), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)

plot(Nad_40$POC_ugL, Nad_40$Depth, xlim=c(0,400), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_42_1$POC_ugL, Nad_42_1$Depth, xlim=c(0,400), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_42_2$POC_ugL, Nad_42_2$Depth, xlim=c(0,400), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=1.8,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_42_3$POC_ugL, Nad_42_3$Depth, xlim=c(0,400), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)


plot(Nad_43$POC_ugL, Nad_43$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_44$POC_ugL, Nad_44$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)


plot(Nad_45_1$POC_ugL, Nad_45_1$Depth, xlim=c(0,250), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_45_2$POC_ugL, Nad_45_2$Depth, xlim=c(0,250), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_45_3$POC_ugL, Nad_45_3$Depth, xlim=c(0,250), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)

Nad_46_1<-filter(Nad_46, Longitude==13.08)
Nad_46_2<-filter(Nad_46, Longitude==13.12)
Nad_46_3<-filter(Nad_46, Longitude==13.22)

Nad_48_1<-filter(Nad_48, Longitude==13.18)
Nad_48_2<-filter(Nad_48, Longitude==13.22)
Nad_48_3<-filter(Nad_48, Longitude==13.27)


dev.new()
plot(Nad_46_1$POC_ugL, Nad_46_1$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_46_3$POC_ugL, Nad_46_3$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)
plot(Nad_46_3$POC_ugL, Nad_46_3$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=8, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)

plot(Nad_47$POC_ugL, Nad_47$Depth, xlim=c(0,100), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
par(new=T)

dev.new()

plot(Nad_48_1$POC_ugL, Nad_48_1$Depth, xlim=c(0,200), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)

plot(Nad_48_2$POC_ugL, Nad_48_2$Depth, xlim=c(0,250), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)

plot(Nad_48_3$POC_ugL, Nad_48_3$Depth, xlim=c(0,250), ylim=c(60,0),
     col=col_months[3],  #270
     bg='#AEA20044',type='b', yaxt='n',
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.2,ylab='Depth (m)',xlab='POC ug/l',lty=2)
