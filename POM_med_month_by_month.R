
#mappa1 <- get_map(location = c(0,42, 10,46),maptype =c('watercolor'))
#mappa2 <- get_map(location = c(4.5,42.8, 14,45),maptype =c('watercolor'))
#mappa3 <- get_map(location = c(7,42.8, 9,45),maptype =c('watercolor'))
tiff(file = "jan.tiff",width = 21, height = 18, units = "cm", res = 800)
ggmap(mappa1) + 
  geom_point(data = jan, alpha=.08,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = jan, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "January", 
       subtitle = "Cruise: MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c("#CC6666",  "#9999CC"))+
  scale_size_continuous(range = c(2,10))+
  theme_minimal()
dev.off()

tiff(file = "feb.tiff",width = 21, height = 18, units = "cm", res = 800)
ggmap(mappa2) + 
  geom_point(data = feb, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = feb, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = " February", 
       subtitle = "Cruise: Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()
dev.off()

tiff(file = "mar.tiff",width = 21, height = 18, units = "cm", res = 800)
g3<-ggmap(mappa2) + 
  geom_point(data = mar, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = mar, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = " March", 
       subtitle = "Cruise: MOOGLI, Medar, Keycop") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()
dev.off()

tiff(file = "apr.tiff",width = 21, height = 18, units = "cm", res = 800)
ggmap(mappa3) + 
  geom_point(data = apr, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = apr, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "April", 
       subtitle = "Cruise: Medar, Keycop") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c("#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()

tiff(file = "jan.tiff",width = 21, height = 18, units = "cm", res = 800)

g5<-ggmap(mappa4) + 
  geom_point(data = may, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = may, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "May", 
       subtitle = "Cruise: Copin-Montegut, MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('yellow','#660000',"#CC6666","#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")


g6<-ggmap(mappa5b) + 
  geom_point(data = jun, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = jun, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "June", 
       subtitle = "Cruise: Copin-Montegut, MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('orange','#660000',"#CC0099","#9999CC"))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")

g7<-ggmap(mappa5) + 
  geom_point(data = jul, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = jul, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "July", 
       subtitle = "Cruise: Copin-Montegut, MOOGLI, Medar ") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('orange','#660000',"#CC0099","#9999CC"))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")
#labels = scales::comma_format(), breaks = c(1500000, 10000000, 20000000)) +

g8<-ggmap(mappa6) + 
  geom_point(data = aug, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = aug, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "August", 
       subtitle = "Cruise: Copin-Montegut, MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('darkolivegreen1',"#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(2,10))+       #1991  #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")

g9<-ggmap(mappa7) + 
  geom_point(data = sep, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = sep, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "September", 
       subtitle = "Cruise: PROSOPE, Keycop,  Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC"))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")

g10<-ggmap(mappa8) + 
  geom_point(data = oct, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = oct, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "October", 
       subtitle = "Cruise: Copin-Montegut, MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c("#CC0099","#9999CC"))+
  scale_size_continuous(range = c(2,10))+        
  theme_minimal()+
  theme(legend.position="none")

g12<-ggmap(mappa8) + 
  geom_point(data = dec, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = dec, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "December", 
       subtitle = "Cruise: Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC"))+
  scale_size_continuous(range = c(2,10))+         #1997 - 2001
  theme_minimal()+
  theme(legend.position="none")
#'#660000',"#CC6666","#CC0099","#9999CC",'#5050a0' #1997 - 2001
##  ,'#66CC00

#tiff(file = "map_2.tiff",width = 9200, height = 6200, units = "px", res = 800)
#grid.arrange(g1,g2,g3,g4,g5,g6,g7, g8,g9, g10,g12)
#dev.off()

#tiff(file = "map_rev.tiff",width = 9200, height = 6200, units = "px", res = 800)
#grid.arrange(g12,g10, g9, g8,g7,g6,g5,g4,g3,g2,g1)
#dev.off()

tiff(file = "map_new.tiff",width = 21, height = 18, units = "cm", res = 800)
grid.arrange(g1,g5,g9, g2,g6,g10,g3,g7,g12,g4,g8)
dev.off()



