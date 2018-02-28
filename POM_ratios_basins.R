setwd('L:\\il mio Drive\\MedSea')
setwd('C:/Users/gi/Documents/GitHub/Mediterranean_Sea')

POM<-read.csv('POM_data_DRYAD_.csv', sep=';');str(POM)
str(POM$Latitude)
POM_med<-POM[(POM$Latitude>29 & POM$Latitude<45 & POM$Longitude>-6 & 
                POM$Longitude<36),]

POM_med$POC_ugL = (POM_med$POC*12.0107)    #12.0107 g mol-1 --> ug/L = mg m-3

col_months<-c("#c15063","#5fb14d","#7362cd","#b6b342","#b966bf",
              "#d38c30","#688ccd","#cd4e34","#4bb092","#ce5b95","#737e38","#c07f56")

POM_med$C_N_ratio = POM_med$POC/POM_med$PON
POM_med$C_P_ratio = POM_med$POC/POM_med$POP
POM_med$N_P_ratio = POM_med$PON/POM_med$POP

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

a<-POM_med[(POM_med$Zone=='Nad'),]
Alb<-POM_med[(POM_med$Longitude<=-0.5),]; write.csv(Alb, file='POC_alboran.csv')
Sww<-POM_med[(POM_med$Longitude>-0.5 & POM_med$Longitude<=5 & POM_med$Latitude<=39.5),];# write.csv(Sww, file='POC_sww.csv')
Swe<-POM_med[(POM_med$Longitude>5 & POM_med$Longitude<=9 & POM_med$Latitude<=39.5),];# write.csv(Swe, file='POC_swe.csv')
Tir<-POM_med[(POM_med$Longitude>9 & POM_med$Longitude<=16 & POM_med$Latitude<=41.5 & POM_med$Latitude>36.5),]; #write.csv(Tir, file='POC_tir.csv')
Nwm_1<-POM_med[(POM_med$Latitude>39.5 & POM_med$Longitude<=9),]
Nwm_2<-POM_med[(POM_med$Latitude>41.5 & POM_med$Longitude<=13),]
Nwm<-rbind(Nwm_1,Nwm_2); #write.csv(Nwm, file='POC_nwm.csv')
Io1<-POM_med[(POM_med$Longitude>12.5 & POM_med$Longitude<=16 & POM_med$Latitude<=38),]
Io2<-POM_med[(POM_med$Longitude>16 & POM_med$Longitude<=21.5 & POM_med$Latitude<=40),]
Ion<-rbind(Io1,Io2)  #write.csv(Ion, file='POC_jon.csv')
Nad<-POM_med[(POM_med$Latitude>42.3 & POM_med$Longitude>12.5),] #write.csv(Nad, file='POC_nad.csv')
Sad1<-POM_med[(POM_med$Latitude<=42.3 & POM_med$Latitude>42 & POM_med$Longitude>14.8),]
Sad2<-POM_med[(POM_med$Latitude<=42 & POM_med$Latitude>40 & POM_med$Longitude>16 & POM_med$Longitude<=20),]
Sad<-rbind(Sad1,Sad2)  #write.csv(Sad, file='POC_sad.csv')
Lev1<-POM_med[(POM_med$Longitude>21.5 & POM_med$Longitude<=23),]
Lev2<-POM_med[(POM_med$Longitude>23 & POM_med$Longitude<=28 & POM_med$Latitude<=35.2),]
Lev3<-POM_med[(POM_med$Longitude>28 & POM_med$Latitude<=37.5),]
Lev<-rbind(Lev1,Lev2,Lev3)#write.csv(Lev, file='POC_lev.csv')
Aeg<-POM_med[(POM_med$Longitude>23 & POM_med$Longitude<=28 & POM_med$Latitude>35.2),] #write.csv(Aeg, file='POC_aeg.csv')

boxplot(Alb$POC_ugL, Sww$POC_ugL, Swe$POC_ugL, Tir$POC_ugL, Nwm$POC_ugL, 
Ion$POC_ugL, Nad$POC_ugL, Sad$POC_ugL, Lev$POC_ugL, Aeg$POC_ugL)

dev.new()
par(mfrow=c(1,2))
boxplot(Alb$C_N_ratio, Sww$C_N_ratio, 
        Swe$C_N_ratio, Tir$C_N_ratio, Nwm$C_N_ratio, 
        Ion$C_N_ratio, Nad$C_N_ratio, Sad$C_N_ratio, 
        Lev$C_N_ratio, Aeg$C_N_ratio, 
        names=c('Alb','Sww','Swe','Tir','Nwm',
             'Jon','Nad','Sad','Lev','Aeg') , main='CN ratio',  varwidth = T)
		
boxplot(Alb$C_N_ratio, Sww$C_N_ratio, 
        Swe$C_N_ratio, Tir$C_N_ratio, Nwm$C_N_ratio, 
        Ion$C_N_ratio, Nad$C_N_ratio, Sad$C_N_ratio, 
        Lev$C_N_ratio, Aeg$C_N_ratio, varwidth = T,ylim=c(0,20),
        names=c('Alb','Sww','Swe','Tir','Nwm',
                'Jon','Nad','Sad','Lev','Aeg') , main='C:N ratio')
par(mfrow=c(1,2))
boxplot(Alb$C_P_ratio, Sww$C_P_ratio, 
        Swe$C_P_ratio, Tir$C_P_ratio, Nwm$C_P_ratio, 
        Ion$C_P_ratio, Nad$C_P_ratio, Sad$C_P_ratio, 
        Lev$C_P_ratio, Aeg$C_P_ratio, varwidth = T,
        names=c('Alb','Sww','Swe','Tir','Nwm',
                'Jon','Nad','Sad','Lev','Aeg') , main='C:P ratio')
boxplot(Alb$C_P_ratio, Sww$C_P_ratio, 
        Swe$C_P_ratio, Tir$C_P_ratio, Nwm$C_P_ratio, 
        Ion$C_P_ratio, Nad$C_P_ratio, Sad$C_P_ratio, 
        Lev$C_P_ratio, Aeg$C_P_ratio, varwidth = T,ylim=c(0,400),
        names=c('Alb','Sww','Swe','Tir','Nwm',
                'Jon','Nad','Sad','Lev','Aeg') , main='C:P ratio')
dev.new()
par(mfrow=c(1,2))
boxplot(Alb$N_P_ratio, Sww$N_P_ratio, 
        Swe$N_P_ratio, Tir$N_P_ratio, Nwm$N_P_ratio, 
        Ion$N_P_ratio, Nad$N_P_ratio, Sad$N_P_ratio, 
        Lev$N_P_ratio, Aeg$N_P_ratio, varwidth = T,
        names=c('Alb','Sww','Swe','Tir','Nwm',
                'Jon','Nad','Sad','Lev','Aeg') , main='N:P ratio')				
boxplot(Alb$N_P_ratio, Sww$N_P_ratio, 
        Swe$N_P_ratio, Tir$N_P_ratio, Nwm$N_P_ratio, 
        Ion$N_P_ratio, Nad$N_P_ratio, Sad$N_P_ratio, 
        Lev$N_P_ratio, Aeg$N_P_ratio, varwidth = T,ylim=c(0,40),
        names=c('Alb','Sww','Swe','Tir','Nwm',
                'Jon','Nad','Sad','Lev','Aeg') , main='N:P ratio')				

library(lattice)
xyplot(POM_med$Depth~ POM_med$POC_ugL | POM_med$Zone,type='o',
       groups=POM_med$Month, ylim = c(1300,-30))



deep<-POM_med[(POM_med$Depth>1000),]

boxplot(POM_med$C_N_ratio)
str(Alb)

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

moo1<-filter(Nwm_moog, Station ==  1328) #   SLOPE ----
moo2<-filter(Nwm_moog, Station ==  1329) #   SLOPE ----
moo3<-filter(Nwm_moog, Station ==  1330) #   SLOPE ----
moo4<-filter(Nwm_moog, Station ==  1331) #   COSTA ----
moo5<-filter(Nwm_moog, Station ==  1332) #   osta ----
moo6<-filter(Nwm_moog, Station ==  1333) #   osta ----
moo7<-filter(Nwm_moog, Station ==  1334) #   osta ----
moo8<-filter(Nwm_moog, Station ==  1335) #   osta ----
moo9<-filter(Nwm_moog, Station ==  1336) #   osta ----
moo10<-filter(Nwm_moog, Station ==  1337) #   osta ----
moo11<-filter(Nwm_moog, Station ==  1338) #   osta ----


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
m55<-filter(Nwm_meda, Station ==  2268) #    km dalla costa ----
m56<-filter(Nwm_meda, Station ==  2269) #    km dalla costa ----
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
m67<-filter(Nwm_meda, Station ==  2280) #    km dalla costa ----
m68<-filter(Nwm_meda, Station ==  2281) #    km dalla costa ----
m69<-filter(Nwm_meda, Station ==  2282) #    km dalla costa ----

m14<-filter(Nwm_meda, Station ==  1333) #  23 gen 98  43.43      7.25 
m15<-filter(Nwm_meda, Station ==  1334) #  23 gen 98  43.43      7.25 
m16<-filter(Nwm_meda, Station ==  1335) #  23 gen 98  43.43      7.25 
m19<-filter(Nwm_meda, Station ==  1336) #  23 gen 98  43.43      7.25 

m17<-filter(Nwm_meda, Station ==  2259) #  23 gen 98  43.43      7.25 
m18<-filter(Nwm_meda, Station ==  2260) #  23 gen 98  43.43      7.25 
m20<-filter(Nwm_meda, Station ==  2261) #  23 gen 98  43.43      7.25 
m21<-filter(Nwm_meda, Station ==  2262) #  23 gen 98  43.43      7.25 
m24<-filter(Nwm_meda, Station ==  2200) #  8 ago 91    41.52   12.11   !! SLOPE 28 km dalla costa ----

#par(new=T)
#feb     [77:87] --- uguale al secondo plot
#feb  [88:99] # uguale al III

str(m1$Depth)

p1<-filter(Nwm_pros, Station == 933); p1<-p1[1:10,]
p2<-filter(Nwm_pros, Station == 934);  p2<-p2[1:10,]
p3<-filter(Nwm_pros, Station == 935);  p3<-p3[1:10,]
p4<-filter(Nwm_pros, Station == 936);  p4<-p4[1:10,]
p5<-filter(Nwm_pros, Station == 937);  p5<-p5[1:10,]


dev.new()
par(mfrow=c(1,4))
######### plot POM 1975 --- maggio
plot(Nwm_copin$POC_ugL, Nwm_copin$Depth, xlim=c(0,180), ylim=c(100,0), col=col_months[5], type='b', 
     pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',
     main='Copin-Montegut (1975)')
text(55,10,'May \n 42.00 | 4.75', cex=1.6)
############ --------------------1991
plot(m24$POC_ugL[1:11], m24$Depth[1:11], ylim=c(500,0),xlim=c(0,80), col=col_months[8],
     type='b', pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,xlab='POC ug/l',ylab='Depth (m)',
     main='Cruise Medar (1991)') #
text(5,10,'Ago \n 41.60 | 12.11 ', cex=1.6)
####################### ------------------ feb-apr 1999
plot(m18$POC_ugL[13:24], m18$Depth[13:24], ylim=c(200,0),xlim=c(0,300), col=col_months[2],
     type='o', pch=1, cex.axis=1.7,cex=2,cex.lab=1.4,xlab='POC ug/l',ylab='Depth (m)',
     ) #  24 feb '99   43.42 |   7.85
par(new=T)
plot(m20$POC_ugL[12:22], m20$Depth[12:22], ylim=c(200,0),xlim=c(0,300), col=col_months[3],
     xaxt='n',yaxt='n',type='o', pch=2,cex.axis=1.7,cex=2, xlab='',ylab='',
     main='Cruise Medar (1999)') #  19 mar '99   43.42 |  7.86
par(new=T)
plot(m21$POC_ugL[1:12], m21$Depth[1:12], ylim=c(200,0),xlim=c(0,300), col=col_months[4],
     xaxt='n',yaxt='n',type='o', pch=3, cex.axis=1.7,cex=2,xlab='',ylab='',
     ) #  11 apr '99   43.42 |  7.86
legend(60,160,bty='n',cex=1.3,
       legend=c('24 feb 99 43.42 | 7.85', '19 mar 99 43.42 | 7.86',
                       '11 apr 99 43.42 | 7.86'),
       col=c(col_months[2],col_months[3],col_months[4]), 
       pch=c(1,2,3))

### plot PROSOPE NWM -- POM Set-Oct 1999 ######### 
plot(p1$POC_ugL, p1$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[9], type='o', pch=1, 
     main='Cruise PROSOPE (1999)', xlab='ug/l',ylab='Depth (m)') #set
par(new=T)
plot(p2$POC_ugL, p2$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[9], type='o', pch=2,xlab='',ylab='')  #set
par(new=T)
plot(p3$POC_ugL, p3$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[10], type='o', pch=3,xlab='',ylab='')  #ott
par(new=T)
plot(p4$POC_ugL, p4$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[10], type='o', pch=4,xlab='',ylab='')  #ott
par(new=T)
plot(p5$POC_ugL, p5$Depth, ylim=c(160,0),xlim=c(0,70), col=col_months[10], type='o', pch=5,xlab='',ylab='')  #ott

legend(10,100,bty='n',legend=c('29 Sep 43.41 | 7.86','30 Sep 43.40 | 7.82',
                       '02 Oct 43.35 | 7.8','01 Oct 43.37 | 7.86',
                       '03 Oct 43.43 | 7.72'), 
       col=c(col_months[9],col_months[9],col_months[10],col_months[10],col_months[10]),
       pch=c(1,2,3,4,5))
text(15,.100,'stations 933 -937')
text(15,10,'min dist coast = 41 km')


Nwm_copin[1:6,]

split(Nwm, Nwm$Dataset, drop = T) <-value
names(Nwm$Dataset)

 #### plot lat e lon campioni totali e x sottobacino - controllo
plot(POM_med$Longitude, POM_med$Latitude, pch=19, col='grey', xlab='',ylab='',
     cex=1.8, xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Alb$Longitude, Alb$Latitude,pch=19, col='#9e0142', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Sww$Longitude, Sww$Latitude,pch=19, col='#d53e4f', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Swe$Longitude, Swe$Latitude,pch=19, col='#f46d43', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Nwm$Longitude, Nwm$Latitude,pch=19, col='#fdae61', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Tir$Longitude, Tir$Latitude,pch=19, col='#fee08b',xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Ion$Longitude, Ion$Latitude,pch=19, col='#e6f598', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Sad$Longitude, Sad$Latitude,pch=19, col='#abdda4', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Nad$Longitude, Nad$Latitude,pch=19, col='#66c2a5', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Lev$Longitude, Lev$Latitude,pch=19, col='#3288bd', xlab='',ylab='',
     xlim=c(-5,26), ylim=c(30,45))
par(new=T)
plot(Aeg$Longitude, Aeg$Latitude,pch=19, col='#5e4fa2', xlab='Longitude',ylab='Latitude',
     xlim=c(-5,26), ylim=c(30,45))
#abline(v=5, col='grey', lty=2)
#abline(h=39.5, col='grey', lty=2)
legend(-5,45,col=c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b',
      '#e6f598','#66c2a5','#3288bd','#5e4fa2'),  pch=19,legend=c('Alb','Sww','Swe','Nwm','Tir',
                                                                  'Ion','Nad','Lev', 'Aeg'))





C_N_ratio<-POM_med$C_N_ratio
  
C_N<-C_N[complete.cases(C_N_ratio)]
d<-density(C_N)
plot(d)
