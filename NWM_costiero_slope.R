
m23<-filter(Nwm_meda, Station ==  2199) #  8 ago 91    41.92     12.00 !! 6 km dalla costa, no plot 
m25<-filter(Nwm_meda, Station ==  2201) #  9 ago 91    42.13   11.74  !!COSTIERO, no plot


m22<-filter(Nwm_meda, Station ==  1337) #  23 gen 98  43.05 5.13  !!!! SLOPE circa 25 km da costa ()

m1<-filter(Nwm_meda, Station ==  2196)  #  5 feb 2000  43.15   5.12 !!!! COSTIEROcirca 25 km da costa,
m2<-filter(Nwm_meda, Station ==  2238) #  SLOPE 14maggio 97 SLOPE!!     43.43     7.252239 [13:24] == a 1:12
m3<-filter(Nwm_meda, Station ==  2239) # SLOPE 1:12 15 maggio 97        43.43     7.25 
m4<-filter(Nwm_meda, Station ==  2246) # SLOPE 18 giungo 97             43.43     7.25 
m5<-filter(Nwm_meda, Station ==  2247) # SLOPE 18 giungo 97             43.43     7.25 
m6<-filter(Nwm_meda, Station ==  2256) # SLOPE 12 lug 97                43.43     7.25 
m7<-filter(Nwm_meda, Station  ==  1328) #SLOPE  18 set 97               43.07     5.13
m8<-filter(Nwm_meda, Station  ==  1329) # COSTIERO 20 sett 97  43.43      7.25 
m9<-filter(Nwm_meda, Station  ==  1330) # SLOPE 22 set 97               43.07     5.13
m10<-filter(Nwm_meda, Station ==  1331) #  23 gen 98  43.43      7.25 
m11<-filter(Nwm_meda, Station ==  1332) #  23 gen 98  43.43      7.25 
m12<-filter(Nwm_meda, Station ==  2257) #  23 gen 98  43.43      7.25 
m13<-filter(Nwm_meda, Station ==  2258) #  23 gen 98  43.43      7.25 


##***************PLOT slope
plot(m2$POC_ugL[1:12], m2$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[5],
     type='o', pch=1, 
     main='POC NWM - Cruise Medar; 1997') #  14maggio 97  43.43      7.25
# [13:24] == a 1:12 14maggio 97  43.43      7.252239
par(new=T)
plot(m3$POC_ugL[1:11], m3$Depth[1:11], ylim=c(200,0),xlim=c(0,450), col=col_months[5],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  15 maggio 97  43.43      7.25
# [13:24] == a 1:12 14maggio 97  43.43      7.252239
par(new=T)
plot(m4$POC_ugL[1:12], m4$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[6],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  18 giungo 97  43.43      7.25 
par(new=T)
plot(m5$POC_ugL[1:12], m5$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[6],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  19 giungo 97  43.43      7.25 # [13:24] == a 1:12 
par(new=T)
plot(m6$POC_ugL[1:12], m6$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[7],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  12 luglio 97  43.43      7.25 # [13:24] == a 1:12 
par(new=T)
plot(m7$POC_ugL[1:9], m7$Depth[1:9], ylim=c(200,0),xlim=c(0,450), col=col_months[7],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  12 luglio 97  43.43    
par(new=T)
plot(moo1$POC_ugL[1:9], moo1$Depth[1:9], ylim=c(200,0),xlim=c(0,450), col=col_months[2],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1999') #  24 feb '99   43.42 |   7.85

par(new=T)
plot(m8$POC_ugL[1:11], m8$Depth[1:11], ylim=c(200,0),xlim=c(0,450), col=col_months[9],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  20 sett 97   43.07   |   5.12    
par(new=T)
plot(m9$POC_ugL[1:11], m9$Depth[1:11], ylim=c(200,0),xlim=c(0,450), col=col_months[9],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1997') #  22 sett 97   43.07   |   5.13  
par(new=T)
plot(m12$POC_ugL[1:12], m12$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[1],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  23 grn 98   43.43  |   7.25
par(new=T)
plot(m13$POC_ugL[1:12], m13$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[1],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  23 grn 98   43.43  |   7.25
par(new=T)
plot(m14$POC_ugL[1:12], m14$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[5],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  19 may 98   43.07  |  5.13
par(new=T)
plot(m16$POC_ugL[13:24], m16$Depth[13:24], ylim=c(200,0),xlim=c(0,450), col=col_months[9],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  9 sett '98    43.07 | 5.12 
par(new=T)
plot(m17$POC_ugL[13:24], m17$Depth[13:24], ylim=c(200,0),xlim=c(0,450), col=col_months[12],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  9 dic '98     43.43  |  7.25
par(new=T)
plot(m19$POC_ugL[13:24], m19$Depth[13:24], ylim=c(200,0),xlim=c(0,450), col=col_months[3],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1999') #  14 mar '99   43.07 |  5.12
par(new=T)
plot(m22$POC_ugL[13:24], m22$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[5],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1999') #   

##***************PLOT COAST
plot(m1$POC_ugL[1:11], m1$Depth[1:11], ylim=c(150,0),xlim=c(0,150), col=col_months[2],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m1$POC_ugL[12:22], m1$Depth[12:22], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=2, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #fe   = a 77:88
par(new=T)
plot(m1$POC_ugL[23:34], m1$Depth[23:34], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=3, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m1$POC_ugL[35:45], m1$Depth[35:45], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=4, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m1$POC_ugL[46:55], m1$Depth[46:55], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=5, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m1$POC_ugL[56:65], m1$Depth[56:65], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=6, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m1$POC_ugL[66:76], m1$Depth[66:76], ylim=c(150,0),xlim=c(0,150),
     col=col_months[2], type='o', pch=6, xlab='',ylab='',
     main='POC NWM - Cruise Medar; 2000') #feb
par(new=T)
plot(m10$POC_ugL[1:12], m10$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[9],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  23 grn 98  43.1    |  5.08
par(new=T)
plot(m11$POC_ugL[1:11], m11$Depth[1:11], ylim=c(200,0),xlim=c(0,450), col=col_months[1],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  23 grn 98   43.24  |    4.88
par(new=T)
plot(m15$POC_ugL[1:12], m15$Depth[1:12], ylim=c(200,0),xlim=c(0,450), col=col_months[5],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1998') #  19 may '98   43.24  |   4.88
par(new=T)
plot(m23$POC_ugL[1:4], m23$Depth[1:4], ylim=c(200,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   
par(new=T)
plot(m23$POC_ugL[5:10], m23$Depth[5:10], ylim=c(200,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   
par(new=T)
plot(m23$POC_ugL[11:17], m23$Depth[11:17], ylim=c(200,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   
par(new=T)
plot(m23$POC_ugL[18:27], m23$Depth[18:27], ylim=c(400,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   
par(new=T)
plot(m25$POC_ugL[20:25], m25$Depth[20:25], ylim=c(500,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') # 9 ago 91   42.13  |   11.74 

par(new=T)
plot(m24$POC_ugL[20:25], m24$Depth[20:25], ylim=c(500,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   
par(new=T)
plot(m24$POC_ugL[26:28], m24$Depth[20:25], ylim=c(500,0),xlim=c(0,450), col=col_months[8],
     type='o', pch=1, xlab='ug/l',ylab='Depth (m)',
     main='POC NWM - Cruise Medar; 1991') #   