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
