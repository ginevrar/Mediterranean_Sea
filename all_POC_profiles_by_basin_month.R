library(ggplot2)

ggplotColours <- function(n = 12, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

col_months<-ggplotColours(n = 11)

png('all_profiles.png',width = 24, height = 18, units = "cm", res = 300)
ggplot(data=POM_med, 
       aes(x=POC_ugL, y=Depth, 
           group=factor(Month),
           #  shape=factor(Month),
           color=factor(Month))) + 
  geom_line() + 
  geom_point() +
  # scale_x_discrete("Year") +
  #  scale_y_continuous("Proportion Tasty") + 
  facet_grid(.~Zone)+
  scale_y_reverse( lim=c(2000,-5))+
  #  theme_minimal()+ 
  scale_colour_manual(values = col_months)
dev.off()

png('all_profiles_600m.png',width = 24, height = 18, units = "cm", res = 300)
ggplot(data=POM_med, 
       aes(x=POC_ugL, y=Depth, 
           group=factor(Month),
           #  shape=factor(Month),
           color=factor(Month))) + 
  geom_line() + 
  geom_point() +
  # scale_x_discrete("Year") +
  #  scale_y_continuous("Proportion Tasty") + 
  facet_grid(.~Zone)+
  scale_y_reverse( lim=c(600,-5))+
  #  theme_minimal()+ 
  scale_colour_manual(values = col_months)
dev.off()

getwd()