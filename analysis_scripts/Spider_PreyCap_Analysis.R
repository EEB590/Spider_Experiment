###########################################
####### Prey Capture Analysis #############
###########################################

#Research Question:

#Do webs on Guam capture more prey than webs on Saipan? We want to know whether the number of prey captured varies by island (and thus, by bird presence/absence). 
#Model: preynum~island, family=poisson

setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/working")
preycapL<-read.csv("preycapturelong.csv")

###### Data Exploration ############

#A. Outliers in Y, Outliers in X


#B. Zero Inflation Y 
#

#C. 
table(preycapL$preynum,preycapL$island) #shows preynum by island-
with(preycapL, table(obs, site)) #concerns me that no obs2 through obs8 data on Guam- i.e. no data from transplanted spiders.
#see how preynum varies by site
with(preycapL, table(preynum, site))

#some exploratory graphs
plot(preycapL$preynum~preycapL$obs)

boxplot(preycapL$preynum~ preycapL$island, varwidth=T)

ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_bar(stat="identity") #shows sum of all counts per site. Not the best graph, but may be useful for identifying any errors. 

ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_boxplot() #shows boxplot of prey captured per site

####### Analysis ########
mod1<-glmer(preynum~island+(1|web)+(1|site),family=poisson,data=preycapL) 
mod2<-glmer(preynum~1+(1|web)+(1|site), family=poisson,data=preycapL)
anova(mod1,mod2) #mod1 sig better than mod2; island is important
confint(mod1)
summary(mod1)
