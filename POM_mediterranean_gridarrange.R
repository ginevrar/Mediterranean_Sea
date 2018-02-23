

g1<-ggmap(mappa1) + 
  geom_point(data = jan, alpha=.08,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = jan, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(x = NULL, y = NULL) + 
  labs(title = "POC in the Mediterranean Sea - January", 
       subtitle = "source:https://www.nature.com/articles/sdata201448 \n cruises MOOGLI, Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c("#CC6666",  "#9999CC"))+
  scale_size_continuous(range = c(.6,18)) 

g2<-ggmap(mappa2) + 
  geom_point(data = feb, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = feb, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "POC in the Mediterranean Sea - February", 
       subtitle = "source:https://www.nature.com/articles/sdata201448 \n cruises Medar") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(.6,18))+         #1997 - 2001
  theme_minimal()


g3<-ggmap(mappa2) + 
  geom_point(data = mar, alpha=.06,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  geom_point(data = mar, shape=1,alpha=1,
             aes(x = Longitude, y = Latitude, size = POC, color=factor(Year)))+
  labs(size = 'POC (umol)') +
  labs(color = "Sampling year")+
  labs(x = NULL, y = NULL) + 
  labs(title = "POC in the Mediterranean Sea - March", 
       subtitle = "Cruise: MOOGLI, Medar, Keycop  \n source:https://www.nature.com/articles/sdata201448") +
  #  scale_colour_brewer(palette="Spectral")+
  scale_colour_manual(values=c('#660000',"#CC6666","#CC0099","#9999CC",'#5050a0'))+
  scale_size_continuous(range = c(.6,18))+         #1997 - 2001
  theme_minimal()



#labels = scales::comma_format(), breaks = c(1500000, 10000000, 20000000)) +



tiff(file = "map_1.tiff",width = 9200, height = 6200, units = "px", res = 800)
grid.arrange(g1,g2,g3)
dev.off()
