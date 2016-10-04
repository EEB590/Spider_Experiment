######## Spider Prey Cap Figures ##########
## 
## ##### Make final graph #######
ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_boxplot(fill="#999999")+theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) 

ggplot(preycapL, aes(x=island, y=preynum))+
  geom_boxplot(fill="#999999")+theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) 
#shows average number of prey captured per site
#need to put space between boxplots, change color to black and white (or dark/light grey), Make labels better, remove microlines on grid in background. 
