library(rworldmap);library(ggmap);library(mapproj)
library(grid);library(gridExtra);library(dplyr)

setwd('L:\\il mio Drive\\MedSea')
POM<-read.csv('POM_data_DRYAD_.csv', sep=';');str(POM)
str(POM$Latitude)

POM_med<-POM[(POM$Latitude>29 & POM$Latitude<45 & POM$Longitude>-6 & POM$Longitude<36 & POM$POC != -9999.000),]
str(POM_med)
summary(POM_med$POC)

subset1<-select(POM_med, Dataset, Station, Year, Month, Day, Depth, Latitude, Longitude, POC)
# jan <- dplyr::filter(POM_med, grepl("1", Month)) questo prende sia il mese 1 che il mese 10, 11,12
# filter(bison, ITIScommonName == "Carolina Chickadee", 
# ITISscientificName != "Poecile carolinensis")
jan<-filter(POM_med, Month==1);feb<-filter(POM_med, Month==2);mar<-filter(POM_med, Month==3)
apr<-filter(POM_med, Month==4);may<-filter(POM_med, Month==5);jun<-filter(POM_med, Month==6)
jul<-filter(POM_med, Month==7);aug<-filter(POM_med, Month==8);sep<-filter(POM_med, Month==9)
oct<-filter(POM_med, Month==10);nov<-filter(POM_med, Month==11);dec<-filter(POM_med, Month==12)

mappa <- get_map(location = c(-7,30, 37,46),maptype =c('watercolor'))
mappa__ <- get_map(location = c(-6,33,28,45), maptype =c('toner'))

mappa2 <- get_map(location = c(-7,30, 37,46),maptype =c('terrain-background'))
mappa2 <- get_stamenmap(bbox = c(-7,37,30,46),maptype =c('terrain-background'))

ggmap(mappa2)
#ggmap(mappa)+ 
 # geom_point(aes(x = Longitude, y = Latitude, 
 #                fill=Month,size=Depth), 
             
 #            data = f,color="black",shape=21)
summary(POM_med$Year)
ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

col_months<-ggplotColours(n = 11)

png(file = "POC_med_sea_byMonth_oneplot_.png",width = 21, height = 18, units = "cm", res = 800)

ggmap(mappa__) + geom_point(data = POM_med, alpha=.1,aes(x = Longitude, y = Latitude,
                                              size = POC, color=factor(Month)))+
  scale_size_continuous(range = c(1,15))+
  theme_minimal()+
  scale_colour_manual(values = col_months)



dev.off()
png(file = "leg.png",width = 21, height = 18, units = "cm", res = 800)
ggmap(mappa__) + geom_point(data = POM_med, alpha=1,aes(x = Longitude, y = Latitude,
                                                         size = POC, color=factor(Month)))+
  scale_size_continuous(range = c(3,10))+
  theme_minimal()
dev.off()



myMap <-  get_map(location=c(-5,34,30,46), source="osm",color='bw')

mappa1 <- get_map(location = c(0,42, 10,46),maptype =c('watercolor'))
mappa2 <- get_map(location = c(4,42, 16,46),maptype =c('watercolor'))
mappa3 <- get_map(location = c(7,42.8, 9,45),maptype =c('watercolor'))
mappa4 <- get_map(location = c(3,41, 9,46),maptype =c('watercolor'))
mappa5 <- get_map(location = c(6,42, 15,45),maptype =c('watercolor'))
mappa6 <- get_map(location = c(7,40, 14,45),maptype =c('watercolor'))
mappa7 <- get_map(location = c(-5,29, 31,48),maptype =c('watercolor'))
mappa8 <- get_map(location = c(5,42.8, 9,45),maptype =c('watercolor'))
mappa5b <- get_map(location = c(6,41, 15,46),maptype =c('watercolor'))

dev.new()
ggmap(mappa3) + 
  geom_point(data = POM_med, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = POM_med, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "POC in the Mediterranean Sea - December", 
       subtitle = "Cruise: Medar \n source:https://www.nature.com/articles/sdata201448") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC"))+
  scale_size_continuous(range = c(.6,18))+         #1997 - 2001
  theme_minimal()


##  ,'#66CC00
  
  
f.map <- function (f){
p.map <- ggmap(mappa)+ 
geom_point(aes(size=POC,x = Longitude, y = Latitude), 
               data = f, color="#7a017744", shape=1)+#
  geom_point(aes(size=POC,x = Longitude, y = Latitude), 
             data = f, color="#7a017744", alpha=.1)+#
    #scale_shape_manual(values=c(3, 16, 17))+#,#
    facet_wrap( ~ Month)+
  theme_minimal()
  return(p.map)
} # funzione per plottare i punti sulla mappa
f.map(POM_med)

geom_point(data = df.asia_cities, aes(x = lon, y = lat, size = population), color = "red", alpha = .1) +

#sq_map3 <- get_map(location = ll_means,  maptype = "terrain", source = "google", zoom = 15)
ggmap(mappa) + 
  geom_point(data = POM_med, color = "red", size = 4) +
  geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")

geom_point(data = POM_med$POC, mapping = aes(x = Longitude, y = Latitude, color = Month))



summary(POM_med)
summary(POM_med$POC)

tiff(file = "map_1.tiff",width = 9200, height = 6200, units = "px", res = 800)
grid.arrange(prim,sec+theme(legend.position="none"))
dev.off()

