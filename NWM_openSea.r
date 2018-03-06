setwd('L:\\il mio Drive\\MedSea')
#setwd('C:/Users/gi/Documents/GitHub/Mediterranean_Sea')
POM<-read.csv('POM_data_DRYAD_.csv', sep=';');str(POM)
POM_med<-POM[(POM$Latitude>29 & POM$Latitude<45 & POM$Longitude>-6 & 
                POM$Longitude<36),]

POM_med$POC_ugL = (POM_med$POC*12.0107)    #12.0107 g mol-1 --> ug/L = mg m-3

#col_months<-c("#c15063","#5fb14d","#7362cd","#b6b342","#b966bf",
 #             "#d38c30","#688ccd","#cd4e34","#4bb092","#ce5b95","#737e38","#c07f56")
ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
col_months<-ggplotColours(n = 11)

POM_med$Zone[POM_med$Longitude<=-0.5] <- "Alb"
POM_med$Zone[POM_med$Longitude>-0.5 & POM_med$Longitude<=5 & POM_med$Latitude<=39.5]<-'Sww'
POM_med$Zone[POM_med$Longitude>5 & POM_med$Longitude<=9 & POM_med$Latitude<=39.5]<-'Swe'
POM_med$Zone[POM_med$Longitude>9 & POM_med$Longitude<=16 & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5]<-'Tir'
POM_med$Zone[POM_med$Latitude>39.5 & POM_med$Longitude<=9]<-'Nw'
POM_med$Zone[POM_med$Latitude>41.5 & POM_med$Longitude<=13]<-'Nw'
POM_med$Zone[POM_med$Longitude>12.5 & POM_med$Longitude<=16 & POM_med$Latitude<=38]<-'Ion'
POM_med$Zone[POM_med$Longitude>16 & POM_med$Longitude<=21.5 & POM_med$Latitude<=40]<-'Ion'
POM_med$Zone[POM_med$Latitude>42.3 & POM_med$Longitude>12.5]<-'Nad'
POM_med$Zone[POM_med$Longitude>21.5 & POM_med$Longitude<=23]<-'Lev'
POM_med$Zone[POM_med$Longitude>23 & POM_med$Longitude<=28 & POM_med$Latitude<=35.2]<-'Lev'
POM_med$Zone[POM_med$Longitude>28 & POM_med$Latitude<=37.5]<-'Lev'
POM_med$Zone[POM_med$Longitude>23 & POM_med$Longitude<=28 & POM_med$Latitude>35.2]<-'Aeg'


Nwm_1<-POM_med[(POM_med$Latitude>39.5 & POM_med$Longitude<=9),]
Nwm_2<-POM_med[(POM_med$Latitude>41.5 & POM_med$Longitude<=13),]
Nwm<-rbind(Nwm_1,Nwm_2); write.csv(Nwm, file='POC_nwm.csv')

#value=c('Copin-Montegut','Medar','MOOGLI','PROSOPE')
split(Nwm, Nwm$Dataset, drop = T) 
summary(Nwm$Dataset)

#NORTH WEST MED SEA
Nwm_copin<-Nwm[(Nwm$Dataset=='Copin-Montegut'),]
Nwm_copin<-Nwm_copin[1:6,]
Nwm_pros<-Nwm[(Nwm$Dataset=='PROSOPE'),]
Nwm_meda<-Nwm[(Nwm$Dataset=='Medar'),]
Nwm_moog<-Nwm[(Nwm$Dataset=='MOOGLI'),] #stessi dati di MEDAR (?) moo4 = m10; moo1 = m7

summary(factor(Nwm_moog$Station))

library(dplyr)
#moo1<-filter(Nwm_moog, Station ==  1328) #   SLOPE ----
#moo2<-filter(Nwm_moog, Station ==  1329) #   SLOPE ----
#moo3<-filter(Nwm_moog, Station ==  1330) #   SLOPE ----
#moo4<-filter(Nwm_moog, Station ==  1331) #   COSTA ----
#moo5<-filter(Nwm_moog, Station ==  1332) #   osta ----
#moo6<-filter(Nwm_moog, Station ==  1333) #   osta ----
#moo7<-filter(Nwm_moog, Station ==  1334) #   osta ----
#moo8<-filter(Nwm_moog, Station ==  1335) #   osta ----
#moo9<-filter(Nwm_moog, Station ==  1336) #   osta ----
#moo10<-filter(Nwm_moog, Station ==  1337) #   osta ----
#moo11<-filter(Nwm_moog, Station ==  1338) #   osta ----

m26<-filter(Nwm_meda, Station ==  2202) #   osta ----
m27<-filter(Nwm_meda, Station ==  2203) #    km dalla costa ----
m28<-filter(Nwm_meda, Station ==  2204) #    km dalla costa ----
m29<-filter(Nwm_meda, Station ==  2205) #    km dalla costa ----
m30<-filter(Nwm_meda, Station ==  2206) #    km dalla costa ----
m31<-filter(Nwm_meda, Station ==  2207) #    km dalla costa ----
m32<-filter(Nwm_meda, Station ==  2208) #    km dalla costa ----
m33<-filter(Nwm_meda, Station ==  2210) #    km dalla costa ----
m34<-filter(Nwm_meda, Station ==  2212) #    km dalla costa ----
m35<-filter(Nwm_meda, Station ==  2213) #    km dalla costa ----
m36<-filter(Nwm_meda, Station ==  2215) #    km dalla costa ----
m37<-filter(Nwm_meda, Station ==  2229) #    km dalla costa ----
m38<-filter(Nwm_meda, Station ==  2232) #    km dalla costa ----
m40<-filter(Nwm_meda, Station ==  2234) #    km dalla costa ----
m41<-filter(Nwm_meda, Station ==  2237) #    km dalla costa ----
m42<-filter(Nwm_meda, Station ==  2238) #    km dalla costa ----
m43<-filter(Nwm_meda, Station ==  2239) #    km dalla costa ----
m44<-filter(Nwm_meda, Station ==  2240) #    km dalla costa ----
m45<-filter(Nwm_meda, Station ==  2241) #    km dalla costa ----
m46<-filter(Nwm_meda, Station ==  2242) #    km dalla costa ----
m47<-filter(Nwm_meda, Station ==  2243) #    km dalla costa ----
m48<-filter(Nwm_meda, Station ==  2244) #    km dalla costa ----
m49<-filter(Nwm_meda, Station ==  2245) #    km dalla costa ----
m50<-filter(Nwm_meda, Station ==  2263) #    km dalla costa ----
m51<-filter(Nwm_meda, Station ==  2264) #    km dalla costa ----
m52<-filter(Nwm_meda, Station ==  2265) #    km dalla costa ----
m53<-filter(Nwm_meda, Station ==  2266) #    km dalla costa ----
m54<-filter(Nwm_meda, Station ==  2267) #    km dalla costa ----
m56<-filter(Nwm_meda, Station ==  2269) #    km dalla costa ----

m67<-filter(Nwm_meda, Station ==  2280) #    km dalla costa ----
m68<-filter(Nwm_meda, Station ==  2281) #    km dalla costa ----
m69<-filter(Nwm_meda, Station ==  2282) #    km dalla costa ----

m14<-filter(Nwm_meda, Station ==  1333) #  23 gen 98  43.43      7.25 
m15<-filter(Nwm_meda, Station ==  1334) #  23 gen 98  43.43      7.25 
m16<-filter(Nwm_meda, Station ==  1335) #  23 gen 98  43.43      7.25 
m19<-filter(Nwm_meda, Station ==  1336) #  23 gen 98  43.43      7.25 

m17<-filter(Nwm_meda, Station ==  2259) #  23 gen 98  43.43      7.25 
m18<-filter(Nwm_meda, Station ==  2260) #  

m20<-filter(Nwm_meda, Station ==  2261) #  23 gen 98  43.43      7.25 
m21<-filter(Nwm_meda, Station ==  2262) #  23 gen 98  43.43      7.25 
m24<-filter(Nwm_meda, Station ==  2200) #  8 ago 91    41.52   12.11   !! SLOPE 28 km dalla costa ----


dati_1999<-filter(Nwm_meda, Year==1999)
dati_1999_open<-dati_1999[(dati_1999$Latitude>42 & dati_1999$Longitude>7),]

dati_2000<-filter(Nwm_meda, Year==2000)
dati_2000_b<-dati_2000[(dati_2000$Longitude>7),]

summary(factor(dati_2000_b$Station))
m55<-filter(Nwm_meda, Station ==  2268) #    km dalla costa ----
m57<-filter(Nwm_meda, Station ==  2270) #    km dalla costa ----
m58<-filter(Nwm_meda, Station ==  2271) #    km dalla costa ----
m59<-filter(Nwm_meda, Station ==  2272) #    km dalla costa ----
m60<-filter(Nwm_meda, Station ==  2273) #    km dalla costa ----
m61<-filter(Nwm_meda, Station ==  2274) #    km dalla costa ----
m62<-filter(Nwm_meda, Station ==  2275) #    km dalla costa ----
m63<-filter(Nwm_meda, Station ==  2276) #    km dalla costa ----
m64<-filter(Nwm_meda, Station ==  2277) #    km dalla costa ----
m65<-filter(Nwm_meda, Station ==  2278) #    km dalla costa ----
m66<-filter(Nwm_meda, Station ==  2279) #    km dalla costa ----


dati_2001<-filter(Nwm_meda, Year==2001)

dati_1999_open<-dati_1999[(dati_1999$Latitude>42 & dati_1999$Longitude>7),]


#par(new=T)
#feb     [77:87] --- uguale al secondo plot
#feb  [88:99] # uguale al III
str(m1$Depth)

p1<-filter(Nwm_pros, Station == 933); p1<-p1[1:10,]
p2<-filter(Nwm_pros, Station == 934);  p2<-p2[1:10,]
p3<-filter(Nwm_pros, Station == 935);  p3<-p3[1:10,]
p4<-filter(Nwm_pros, Station == 936);  p4<-p4[1:10,]
p5<-filter(Nwm_pros, Station == 937);  p5<-p5[1:10,]


png('POC_nwms2.png',width = 29.7, height = 19, units = "cm", res = 300)
par(mfrow=c(1,4))
######### plot POM 1975 --- maggio
plot(Nwm_copin$POC_ugL, Nwm_copin$Depth, xlim=c(0,180), ylim=c(100,0), col=col_months[5], type='b', 
     pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
     main='Copin-Montegut (1975)')
text(55,10,'May \n 42.00 | 4.75', cex=1.2)
############ --------------------1991
plot(m24$POC_ugL[1:11], m24$Depth[1:11], ylim=c(500,0),xlim=c(0,80), col=col_months[8],
     type='b', pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,xlab='POC ug/l',ylab='Depth (m)',lty=2,
     main='Cruise Medar (1991)') #
text(18,10,'Ago \n 41.60 | 12.11 ', cex=1.2)
####################### ------------------ feb-apr 1999
plot(m18$POC_ugL[13:24], m18$Depth[13:24], ylim=c(200,0),xlim=c(0,300), col=col_months[2],lty=2,
     type='o', pch=1, cex.axis=1.7,cex=2,cex.lab=1.4,xlab='POC ug/l',ylab='Depth (m)') #  24 feb '99   43.42 |   7.85
par(new=T)
plot(m20$POC_ugL[12:22], m20$Depth[12:22], ylim=c(200,0),xlim=c(0,300), col=col_months[3],lty=2,
     xaxt='n',yaxt='n',type='o', pch=2,cex.axis=1.7,cex=2, xlab='',ylab='',
     main='Cruise Medar (1999)') #  19 mar '99   43.42 |  7.86
par(new=T)
plot(m21$POC_ugL[1:12], m21$Depth[1:12], ylim=c(200,0),xlim=c(0,300), col=col_months[4],lty=2,
     xaxt='n',yaxt='n',type='o', pch=3, cex.axis=1.7,cex=2,xlab='',ylab='') #  11 apr '99   43.42 |  7.86
par(new=T)
plot(m51$POC_ugL[1:10], m51$Depth[1:10], ylim=c(200,0),xlim=c(0,300), col=col_months[6],lty=2,
     xaxt='n',yaxt='n',type='o', pch=3, cex.axis=1.7,cex=2,xlab='',ylab='') #  11 apr '99   43.42 |  7.86

legend(60,160,bty='n',cex=1.2,
       legend=c('24 feb 43.42 | 7.85', '19 mar 43.42 | 7.86',
                       '11 apr 43.42 | 7.86', '12 giu 42.42 | 7.85'),
       col=c(col_months[2],col_months[3],col_months[4],col_months[6]), 
       pch=c(1,2,3))


### plot PROSOPE NWM -- POM Set-Oct 1999 ######### 
plot(p1$POC_ugL, p1$Depth, cex.axis=1.7,cex=2,cex.lab=1.4, ylim=c(160,0),lty=2,
     xlim=c(0,70), col=col_months[9], type='o', pch=1, 
     main='Cruise PROSOPE (1999)', xlab='POC ug/l',ylab='Depth (m)') #set
par(new=T)
plot(p2$POC_ugL, p2$Depth, ylim=c(160,0),xlim=c(0,70),cex.axis=1.7,cex=2,xaxt='n',yaxt='n',lty=2,
     cex.lab=1.4, col=col_months[9], type='o', pch=2,xlab='',ylab='')  #set
par(new=T)
plot(p3$POC_ugL, p3$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[10],xaxt='n',yaxt='n',lty=2,
     cex.axis=1.7,cex=2,cex.lab=1.4,
     type='o', pch=3,xlab='',ylab='')  #ott
par(new=T)
plot(p4$POC_ugL, p4$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[10], xaxt='n',yaxt='n',lty=2,
     type='o', pch=4,xlab='',ylab='',cex.axis=1.7,cex=2,cex.lab=1.4)  #ott
par(new=T)
plot(p5$POC_ugL, p5$Depth, ylim=c(160,0),xlim=c(0,70),cex.axis=1.7,cex=2,cex.lab=1.4, xaxt='n',yaxt='n',lty=2,
     col=col_months[10], type='o', pch=5,xlab='',ylab='')  #ott

legend(11,100,bty='n',legend=c('29 Sep 43.41 | 7.86','30 Sep 43.40 | 7.82',
                       '02 Oct 43.35 | 7.8','01 Oct 43.37 | 7.86',
                       '03 Oct 43.43 | 7.72'), cex=1.2,
       col=c(col_months[9],col_months[9],col_months[10],col_months[10],col_months[10]),
       pch=c(1,2,3,4,5))

###-------------------- Medar 2000
png(file = "NWM_medar2000_profiles4.png",width = 28, height = 28, units = "cm", res = 200)
par(mfrow=c(2,6),mar=c(2,0.5,2,0), 
    oma=c(0,4.2,3,2.2))
plot(m55$POC_ugL[1:12], m55$Depth[1:12], ylim=c(200,0),xlim=c(0,100),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2,
    col=col_months[1], type='o', pch=1,xlab='',ylab='')  #ott
text(20, 100,cex=1.5,'30 Jan \n 43.42 | 7.85')
mtext('Depth',side=2,outer=F, at=100, line=3.4)

plot(m57$POC_ugL[1:10], m57$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[2], type='o', pch=1,xlab='',ylab='')  #ott
text(140, 150,cex=1.5,'22 Feb  \n 43.41 | 7.85')

plot(m58$POC_ugL[1:12], m58$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[3], type='o', pch=3,xlab='',ylab='')  #ott
text(45,80,cex=1.5,'28 Mar \n 43.42 | 7.85')

plot(m59$POC_ugL[1:12], m59$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, lty=2, yaxt='n',
     col=col_months[4], type='o', pch=4,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'14 Apr \n 43.43 | 7.85')

plot(m60$POC_ugL[1:9], m60$Depth[1:9], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, lty=2, yaxt='n',
     col=col_months[5], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'25 May \n 43.42 | 7.85')

plot(m61$POC_ugL[1:12], m61$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[6], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'17 Jun \n 43.4 | 7.83')

plot(m62$POC_ugL[1:12], m62$Depth[1:12], ylim=c(200,0),xlim=c(0,230),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2,  
     col=col_months[7], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'21 Jul \n 43.41 | 7.82')
mtext('Depth',side=2,outer=F, at=100, line=3.4)

plot(m63$POC_ugL[1:10], m63$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[9], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'1 Sep \n 43.42 | 7.85')

plot(m64$POC_ugL[1:12], m64$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[9], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'23 Sep \n 43.42 | 7.85')

plot(m65$POC_ugL[1:10], m65$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[10], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'17 Oct \n 43.42 | 7.85')

plot(m66$POC_ugL[1:12], m66$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,yaxt='n',lty=2,
     col=col_months[9], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.5,'6 Dec \n 43.43 | 7.85')
title(main='Monthly POC (ug /l) in the NWM \n Medar cruise (2000)', outer=T, cex=2.8)
dev.off()

#text(15,.100,'stations 933 -937')
###-------------------- Medar 2000
png(file = "NWM_medar2000_profiles544.png",width = 33, height = 15, units = "cm", res = 200)
par(mfrow=c(1,11),mar=c(2,0.5,2,0), 
    oma=c(0,4.2,3,2.2))
plot(m55$POC_ugL[1:12], m55$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2,
     col=col_months[1], type='o', pch=1,xlab='',ylab='')  #ott
text(130, 150,cex=1.2,'30 Jan \n 43.42 | 7.85')
mtext('Depth',side=2,outer=F, at=100, line=3.4)

plot(m57$POC_ugL[1:10], m57$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[2], type='o', pch=1,xlab='',ylab='')  #ott
text(140, 150,cex=1.2,'22 Feb  \n 43.41 | 7.85')

plot(m58$POC_ugL[1:12], m58$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[3], type='o', pch=3,xlab='',ylab='')  #ott
text(45,80,cex=1.2,'28 Mar \n 43.42 | 7.85')

plot(m59$POC_ugL[1:12], m59$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, lty=2, yaxt='n',
     col=col_months[4], type='o', pch=4,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'14 Apr \n 43.43 | 7.85')

plot(m60$POC_ugL[1:9], m60$Depth[1:9], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, lty=2, yaxt='n',
     col=col_months[5], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'25 May \n 43.42 | 7.85')

plot(m61$POC_ugL[1:12], m61$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2, yaxt='n',
     col=col_months[6], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'17 Jun \n 43.4 | 7.83')

plot(m62$POC_ugL[1:12], m62$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,lty=2,   yaxt='n',
     col=col_months[7], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'21 Jul \n 43.41 | 7.82')
#mtext('Depth',side=2,outer=F, at=100, line=3.4)

plot(m63$POC_ugL[1:10], m63$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[9], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'1 Sep \n 43.42 | 7.85')

plot(m64$POC_ugL[1:12], m64$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[9], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'23 Sep \n 43.42 | 7.85')

plot(m65$POC_ugL[1:10], m65$Depth[1:10], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4, yaxt='n',lty=2,
     col=col_months[10], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'17 Oct \n 43.42 | 7.85')

plot(m66$POC_ugL[1:12], m66$Depth[1:12], ylim=c(200,0),xlim=c(0,200),
     cex.axis=1.7,cex=2,cex.lab=1.4,yaxt='n',lty=2,
     col=col_months[11], type='o', pch=5,xlab='',ylab='')  #ott
text(140,100,cex=1.2,'6 Dec \n 43.43 | 7.85')
title(main='Monthly POC (ug /l) in the NWM \n Medar cruise (2000)', outer=T, cex=2.8)
dev.off()


m61<-filter(Nwm_meda, Station ==  2274) #    km dalla costa ----
m62<-filter(Nwm_meda, Station ==  2275) #    km dalla costa ----
m63<-filter(Nwm_meda, Station ==  2276) #    km dalla costa ----
m64<-filter(Nwm_meda, Station ==  2277) #    km dalla costa ----
m65<-filter(Nwm_meda, Station ==  2278) #    km dalla costa ----
m66<-filter(Nwm_meda, Station ==  2279) #    km dalla costa ----


#text(15,10,'min dist coast = 41 km')



dati_2000_surf<-dati_2000_b[(dati_2000_b$Depth<=80),]
dati_2000_prof<-dati_2000_b[(dati_2000_b$Depth>80),]

gg1<-ggplot(dati_2000_surf, colour=col_months,aes(y=POC_ugL, x=factor(Month), group=Month )) +
  geom_boxplot(aes(fill=factor(Month), alpha=.8)
               )+
  theme_minimal()+
  ggtitle('Monthly POC, NWM Medar 2000 (0 - 80 m depth)')+ 
  theme(
    plot.title = element_text(size =12))+ 
  guides(fill=FALSE)+
  guides(alpha=FALSE)

gg2<- ggplot(dati_2000_prof, colour=col_months,aes(y=POC_ugL, x=factor(Month), group=Month )) +
  geom_boxplot(aes(fill=factor(Month), alpha=.8)
  )+
  theme_minimal()+
  ggtitle('Monthly POC, NWM Medar 2000 (80 - 100 m depth)')+
  theme(
    plot.title = element_text(size =12))+ 
  guides(fill=FALSE)+
  guides(alpha=FALSE)


png(file = "NWM_boxplot_surf_deep.png",width = 24, height = 18, units = "cm", res = 300)
ggarrange(gg1, gg2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()




