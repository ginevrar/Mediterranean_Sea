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

Tir<-filter(POM_med, Zone ==  'Tir')

POM_med$Zone[POM_med$Longitude>9 & POM_med$Longitude<=16 & 
               POM_med$Latitude<=41.5 & POM_med$Latitude>36.5]<-'Tir'
#write.csv(Tir, file='POC_tir.csv')
summary(factor(Tir$Year))

library(dplyr); t_1991<-filter(Tir, Year ==  1991) #   osta ----
summary(factor(t_1991$Station))
t1<-filter(t_1991, Station ==  2197)
t2<-filter(t_1991, Station ==  2198)
t3<-filter(t_1991, Station ==  2199)
t4<-filter(t_1991, Station ==  2205)
t5<-filter(t_1991, Station ==  2206)

  png(file = "Tir_medar1991_profiles.png",width = 29, height = 19, units = "cm", res = 200)
  par(mfrow=c(1,4),mar=c(2,3.1,2,0), 
      oma=c(0,4.2,3,2.2))
  plot(t1$POC_ugL[1:12],t1$Depth[1:12] ,fg="chartreuse3", ylim=c(600,0), xlim=c(0,200),col=col_months[8],type='b', 
       pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=3)
  mtext('Depth',side=2,outer=F, at=300, line=3.4)
  par(new=T)
  plot(t1$POC_ugL[50:58],t1$Depth[50:58], fg="chartreuse3",ylim=c(600,0), xlim=c(0,200),col='#0378b7',type='b', 
       pch=2, cex=1.5,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=2, 
       main='Tir, Medar cruise (7 Ago 1991) ')
  legend(30,540,bty='n',cex=1.5,
         legend=c('2197: 41.02 | 12.78','2197: 41.26 | 12.31'),
         col=c(col_months[8],'#0378b7'), 
         pch=c(1,2))
  
  plot(t1$POC_ugL[13:19],t1$Depth[13:19] , ylim=c(150,0), xlim=c(0,200),col='#0378b7',type='b', 
       pch=2, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=2, 
       main='Tir, Medar cruise (7 Ago 1991)')
  par(new=T)
  plot(t1$POC_ugL[25:31],t1$Depth[25:31] , fg="royalblue",ylim=c(150,0), xlim=c(0,200),col=col_months[8],type='b', 
       pch=4, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2)
  legend(10,138,bty='n',cex=1.5,
         legend=c('2197: 41.15 | 12.95','2197: 41.41 | 12.54'),
         col=c('#0378b7',col_months[8]), 
         pch=c(2,4))
  
  plot(t1$POC_ugL[21:24],t1$Depth[21:24] , ylim=c(150,0), xlim=c(0,200),col='#0378b7',type='b', 
       pch=3, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2)
  par(new=T)
  plot(t1$POC_ugL[32:39],t1$Depth[32:39] , ylim=c(150,0), fg="royalblue",xlim=c(0,200),col=col_months[8],type='b', 
       pch=5, cex=2,cex.axis=1.7,cex.lab=1.4,ylab='Depth (m)',xlab='POC ug/l',lty=2,
       main='Tir, Medar cruise (7 Ago 1991)')
  legend(20,138,bty='n',cex=1.5,
         legend=c('2197: 41.43 | 12.57', '2197: 41.36 | 12.46'),
         col=c('#0378b7',col_months[8]), 
         pch=c(3,5))
  
  plot(t3$POC_ugL[1:11],t3$Depth[1:11] ,fg="chartreuse3", ylim=c(600,0), xlim=c(0,150),col=col_months[8],type='b', 
       pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=3, main='Tir, Medar cruise (8 Ago 1991) ')
  mtext('Depth',side=2,outer=F, at=300, line=3.4)
  text(90,460,'2199: 41.40 | 11.99', cex=1.5)
  dev.off()

plot(t5$POC_ugL[4:7],t5$Depth[4:7],fg="chartreuse3", ylim=c(600,0), xlim=c(0,200),col=col_months[8],type='b', 
     pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=3, main='Tir, Medar cruise (11 Ago 1991) ')
mtext('Depth',side=2,outer=F, at=300, line=3.4)
text(60,400,'41.49 | 12.1', cex=2)

t_1991

boxplot(t_1991$POC_ugL,t_1991$Depth)
summary(t_1991$Month)

t3_t1<-rbind(t1,t3)

t3_t1$dfac[t3_t1$Depth<=10] <- "0-10"
t3_t1$dfac[t3_t1$Depth>10 & t3_t1$Depth<=20] <- "10-20"
t3_t1$dfac[t3_t1$Depth>20 & t3_t1$Depth<=30] <- "20-30"
t3_t1$dfac[t3_t1$Depth>30 & t3_t1$Depth<=40] <- "30-40"
t3_t1$dfac[t3_t1$Depth>40 & t3_t1$Depth<=50] <- "40-50"
t3_t1$dfac[t3_t1$Depth>50 & t3_t1$Depth<=60] <- "50-60"
t3_t1$dfac[t3_t1$Depth>60 & t3_t1$Depth<=70] <- "60-70"
t3_t1$dfac[t3_t1$Depth>70 & t3_t1$Depth<=80] <- "70-80"
t3_t1$dfac[t3_t1$Depth>80 & t3_t1$Depth<=90] <- "80-90"
t3_t1$dfac[t3_t1$Depth>90 & t3_t1$Depth<=100] <- "90-100"
t3_t1$dfac[t3_t1$Depth>100 & t3_t1$Depth<=200] <- "100-200"
t3_t1$dfac[t3_t1$Depth>200 & t3_t1$Depth<=300] <- "200-300"
t3_t1$dfac[t3_t1$Depth>300 & t3_t1$Depth<=400] <- "300-400"
t3_t1$dfac[t3_t1$Depth>400 & t3_t1$Depth<=500] <- "400-500"
t3_t1$dfac[t3_t1$Depth>500 & t3_t1$Depth<=600] <- "500-600"

str(t3_t1$dfac)
t3_t1$dfac<-factor(t3_t1$dfac, levels = c("500-600","400-500","300-400","200-300","100-200",
                                          "90-100",'80-90','70-80','60-70','50-60',"40-50",
                                          "30-40","20-30","10-20","0-10"))
max(t_1991$Depth)


gg2<- ggplot(t3_t1, aes(y=POC_ugL, x=factor(dfac), group=dfac )) +
  geom_boxplot(fill= '#00A6FF',aes(alpha=.8))+
  coord_flip()+
  theme_minimal()+
  labs(title ='POC August, Tirrenian Sea', subtitle ='Slope (st. 2197, 2199) - Medar 1991')+
  theme(
    plot.title = element_text(size =12))+ 
  xlab('Depth layer')+
  ylab('POC (ug l)')+
  guides(fill=FALSE)+
  guides(alpha=FALSE)

png(file ="Tir_Boxplot_POC.png",width = 10.5, height = 14.5, units = "cm", res = 200)
plot(gg2)
dev.off()


gg3<- ggplot(t3_t1, aes(y=C_N_ratio, x=factor(dfac), group=dfac )) +
  geom_boxplot(fill= '#00A6FF',aes(alpha=.8))+
  coord_flip()+
  theme_minimal()+
  labs(title ='CN ratio August, Tirrenian Sea', subtitle ='Slope (st. 2197, 2199) - Medar 1991')+
  theme(
    plot.title = element_text(size =12))+ 
  xlab('Depth layer')+
  ylab('C/N')+
  guides(fill=FALSE)+
  guides(alpha=FALSE)

#N/P e C/P ratio not available
png(file ="Tir_Boxplot_CN_ratio.png",width = 10.5, height = 14.5, units = "cm", res = 200)
plot(gg3)
dev.off()

png(file ="Tir_Boxplot_POC_andCN_ratio.png",width = 21, height = 14.5, units = "cm", res = 200)
ggarrange(gg2,gg3,ncol = 2, nrow = 1)
dev.off()
t_1999<-filter(Tir, Year ==  1999) #   osta ----

png(file ="Tir_profile_1999.png",width = 10,height = 19, units = "cm", res = 200)
plot(t_1999$POC_ugL,t_1999$Depth, ylim=c(200,0), xlim=c(0,100),col=col_months[9],type='b', 
     pch=1, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=3,
     main='Tir, PROSOPE cruise (27 Set 1999)')
text(50,150,'931: 39.11 | 14.71', cex=1.2)
mtext('Depth',side=2,outer=F, at=300, line=3.4)
dev.off()

main='Tir, Medar cruise (11 Ago 1991) '




## COSTIERO
plot(t2$POC_ugL[1:7],t2$Depth[1:7], ylim=c(150,0), xlim=c(0,200),
     col=col_months[8],bg= "#00A6FF44",type='b', 
     pch=22, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=3)
mtext('Depth',side=2,outer=F, at=300, line=3.4)
par(new=T)
plot(t2$POC_ugL[8:13],t2$Depth[8:13],ylim=c(150,0), xlim=c(0,200),col='#0378b7',type='b', 
     pch=23, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=2, 
     main='Tir, Medar cruise (7-10 Ago 1991) ',bg='#0378b744')
par(new=T)
plot(t4$POC_ugL,t4$Depth ,ylim=c(150,0), xlim=c(0,200),col='#01486d',bg='#01486d44',type='b', 
     pch=21, cex=2,cex.axis=1.7,cex.lab=1.4,ylab=' ',xlab='POC ug/l',lty=1, 
     main=' ')
mtext('Depth',side=2,outer=F, at=300, line=3.4)
legend(100,110,bty='n',cex=1.5,
       legend=c('41.20 | 13.01', '41.22 | 13.04', '41.49 | 12.10'),
       col=c(col_months[8],'#0378b7','#01486d'),
       pt.bg = c( "#00A6FF44",'#0378b744','#01486d44'),
       pch=c(22,23,21))

