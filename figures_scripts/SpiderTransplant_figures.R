### Spider Transplant Experiment Graphics #####
#Raw data
ggplot(transplant, aes(Netting, Total.Days))+
  geom_boxplot()+
  theme_bw()+
  facet_grid(Island~.)+
  coord_flip()

#Model Results
preddata <- with(truetrans, expand.grid(Island = levels(Island), Netting=levels(Netting)))
X <- model.matrix(~ Island*Netting, data=preddata)
preddata$Pred <- X %*% coef(tmod) 
preddata$SE <- sqrt(  diag(X %*%vcov(tmod) %*% t(X))  )
preddata$mean<-exp(preddata$Pred)
preddata$uci<- exp(preddata$Pred + (1.96*preddata$SE))
preddata$lci<-exp(preddata$Pred - (1.96*preddata$SE))

limits<- aes(ymin=lci, ymax=uci)
ggplot(preddata, aes(Netting, mean))+
  geom_point()+
  theme_bw()+
  facet_grid(Island~.)+
  coord_flip()+
  geom_errorbar(limits, width=0.35)+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_rect(colour= "black"), 
        strip.background = element_blank(),
        text = element_text(size=20))+
  ylab("Number of days spider remained on web")+
  xlab("Spider protected by bird exclosure?")

###########################################
########  Q2)   ######################
############################################
#raw data
ggplot(truetrans, aes(Netting, WebPresBin))+
  geom_violin()+
  theme_bw()+
  facet_grid(.~Island)

#Model Results
with(truetrans, ftable(Island, Netting, WebPresBin))
webprop<-ddply(truetrans, .(Island, Netting), summarize, prop=sum(WebPresBin, na.rm=T)/length(Island))

ggplot(webprop, aes(Netting, prop))+
  geom_bar(stat="identity")+
  facet_grid(Island~.)+
  theme_bw()+
  xlab("Spider protected by bird exclosure?")+
  ylab("Proportion of events attributable to predation")+
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_rect(colour= "black"), 
        strip.background = element_blank(),
        text = element_text(size=20), 
        plot.background = element_rect(fill=NULL))
