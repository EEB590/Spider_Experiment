#############################################
####### spider transplant analysis ##########
#############################################

#load data
transplant<-read.csv("transplant.csv")

###########################################################
#load libraries
library(lsmeans)
library(ggplot2)

###########################################################
#Research question 1) Does the duration of time a spider stays on its web differ between island or treatment? 
#using all data from transplanted spiders
#duration~island*netting, family=poisson (could do this as a hazard rate, but not for this )

###########################################################
#Notes about the study
#Transplanting was done in two sites on Guam and two sites on Saipan
#Use transplanted spiders only, because very few "native" spiders on Saipan.
#but can compare back to spiders found in wild, though to see if a similar duration. 
truetrans<-transplant[transplant$native=="no",]
native<-transplant[transplant$native=="yes",]
############ Data exploration #####################
##a.  Outliers in Y / Outliers in X 
#plot response and predictors to check for outliers  (only with continuous data)
hist(truetrans$duration)
hist(native$duration) #one possible outlier 

boxplot(truetrans$duration~ truetrans$island*truetrans$netting , xlab="island + treatment",  ylab="Total Days") #only those transplanted, no obvious outliers

boxplot(native$duration~native$island, ylab="Total Days") #only  spiders found in forest, not transplanted, so can't use "netting"
#outlier is on Saipan, without netting. 

max(truetrans$duration) #8 days
max(native$duration) #9 days

#another way of visualizing same thing - violin plots are like boxplots, but provide more information about the density of points than a boxplot. 
ggplot(truetrans, aes(island, duration, fill=netting))+
  geom_violin()+
  #geom_boxplot(width=0.2)+ #just to see what a boxplot would look like
  theme_bw() #a lot of zeros in Saipan without netting

ggplot(native, aes(island, duration))+
  geom_violin()+
  theme_bw() #can see the outlier here. 

#b.	Examine Zero inflation Y
# What proportion of response values = zero? 
with(truetrans, table(duration)) # no zeros. 
with(native, table(duration)) #no zeros

#c.	Collinearity X: correlation between covariates
#Plot each predictor against each other 
#our predictors are both factors, so use a table. 
with(truetrans, table(island, netting)) #we have all combinations represented. 
with(native, table(island, netting)) #no netting data for "native" spiders

#d.	Linearity and homogeneity - Look at relationships of Y vs X’s. Is the relationship linear (for continuous-continuous relationships)? Is the variance homogeneous?
# Plot response against each predictor and random effect. 
ggplot(truetrans, aes(netting, duration, color=island))+
  geom_boxplot()
#yes-  mostly homogeneous - the Saipan-NoNetting combo looks like it might have less variance- will check residuals for a more rigorous test. 

#e.	Independence Y
#Is there a pattern across time or space that is not incorporated in the model? 
#we didn't sample across time, but we did sample at two sites/island.
ggplot(truetrans, aes(netting, duration, color=island))+
  geom_boxplot()+
  facet_grid(.~site)
#could incorporate site into main effects- but prob not part of an interaction bc small sample size, and effect of netting is consistent across sites. I think it's okay to ignore site. 

#Are there other potential factors that reduce independence of Y’s? #not that I can think of... 

#f.	Sufficient and balanced data?  
# Are categorical covariates balanced (island, netting)? - Yes
# Adequate sample size- yes
with(truetrans, ftable(island, netting))

#Examine interactions - Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) - Yes
with(truetrans, table(island, netting))

####### summary #########
# in general, seems good. The Saipan-NoNetting combo for truetrans may not have homogeneous variance - will need to check residuals. 

# if we use native data, may have an issue with the outlier for Saipan-NoNetting. 

###### any decisions made that lead to changes in data frame?###
#nothing at this point. 

#######################################
####Analyze data#######################
#######################################

#### Question 1 ####################
#1) Does the duration of time a spider stays on its web differ between island or treatment? Does the effect of netting very depending on island (Guam = no birds, saipan = birds)? 
#using all data from transplanted spiders

tmod<-glm(duration~island*netting, family= poisson, data=truetrans)

# check model fit
plot(tmod)

#extract residuals
E1 <- resid(tmod, type = "pearson")

#check for overdispersion
sum(E1^2) / (nrow(truetrans) - length(coef(tmod))) #0.7 - not overdispersed

#plot fitted vs residuals
F1 <- fitted(tmod, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=truetrans$island, y=F1) #heterogeneity in residuals bt islands
plot(x=truetrans$netting, y=F1) #heterogeneity in residuals wrt netting
plot(x=truetrans$site, y=F1) #residual variance larger at Saipan sites than Guam sites, but homogeneity bt sites within an island

#hypothesis testing - do 

#
#
#######################################################
#Research Question 2) If a spider is missing, is the web more likely to be present without a spider inhabiting it (indicative of predation) on Saipan than on Guam? 
##webpresbin~island*netting, family=binomial
#using subset of data with spiders missing (omit ones where spiders remained entire time)
truetransnosp<-truetrans[!is.na(truetrans$webpresbin),]

#########################################################
############ Data exploration #####################
#########################################################
##a.  Outliers in Y / Outliers in X 
#plot response and predictors to check for outliers  (only with continuous data)
hist(truetrans$duration)

boxplot(truetrans$duration~ truetrans$island*truetrans$netting , xlab="island + treatment",  ylab="Total Days") #only those transplanted

boxplot(transplant$duration~transplant$island*transplant$netting, ylab="Total Days") #all spiders (already present + transplants)

#b.	Examine Zero inflation Y
# What proportion of response values = zero? 
with(truetrans, table(duration)) # no zeros. 
with(truetrans, table(webpresbin)) #binary- can't be zero-inflated

#c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other
with(truetrans, table(island, netting))

#d.	Linearity and homogeneity - Look at relationships of Y vs X’s:
# i.	Plot response against each predictor and random effect. 
ggplot(truetrans, aes(netting, duration, color=island))+
  geom_boxplot()
#yes- linear and mostly homogeneous

#e.	Independence Y
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(truetrans, aes(netting, duration, color=island))+
  geom_boxplot()+
  facet_grid(.~site)
#could incorporate transect into main effects- but prob not part of an interaction bc small sample size, and effect of netting is consistent across transects. But I think it's okay to ignore transect). 

with(truetrans, ftable(site, netting, webpresbin)) 

#ii. Are there other potential factors that reduce independence of Y’s? 
with(truetrans, ftable(island, netting))
with(truetrans, ftable(island, netting, webpresbin)) #only 3 situtaions where web still present but spider gone on Guam, two inside netting one without. 

#f.	Sufficient and balanced data?  - Yes 
#i.	Are categorical covariates balanced? - Yes
# Adequate sample size- yes

#ii.	Examine interactions
#1.	Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) - Yes
with(truetrans, table(island, netting))

# 3)	Fix up dataframe
# a.	Remove missing values (NA’s)

####### summary #########


#explore data visually

with(truetrans, table(island, webpresbin, netting))

ggplot(transplant, aes(x=island, y=WebSize.cm., fill=Native))+
  geom_boxplot()

ggplot(truetrans, aes(island, duration, fill=netting))+
  geom_violin()+
  theme_bw()

ggplot(transplant, aes(island, duration, fill=netting))+
  geom_violin()+
  theme_bw()+
  facet_grid(.~Native)

ggplot(truetrans, aes(island, webpresbin, fill=netting))+
  geom_violin()+
  theme_bw()

#### Analyze data ##################
todweb<-glm(webpresbin~island*netting, family=binomial, data=truetransnosp)

summary(todweb) #not overdispersed
plot(todweb)

#extract residuals
E1 <- resid(todweb, type = "pearson")

#check for overdispersion
sum(E1^2) / (nrow(truetransnosp) - length(coef(todweb))) #0.91 - not overdispersed

#plot fitted vs residuals
F1 <- fitted(todweb, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=truetransnosp$island, y=F1) #heterogeneity in residuals bt islands
plot(x=truetransnosp$netting, y=F1) #some heterogeneity in residuals wrt netting
plot(x=truetransnosp$site, y=F1) #residual variance larger at Saipan sites than Guam sites, but homogeneity bt sites within an island

#### Hypothesis testing #############
