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
truetrans$site<-factor(truetrans$site) #gets rid of ghost levels
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

###### any decisions made that lead to changes in data frame? perhaps remove NA's, standardize continuous predictors?###
#nothing at this point. 

#######################################
####Analyze data#######################
#######################################

#### Question 1 ####################
#1) Does the duration of time a spider stays on its web differ between island or treatment? Does the effect of netting very depending on island (Guam = no birds, saipan = birds)? 
#using all data from transplanted spiders

#We are going to employ the "we set up an experiment, so we will fit and interpret results from the full model" philosophy. 

tmod<-glm(duration~island*netting, family= poisson, data=truetrans)
summary(tmod)

# check model fit using plot function
plot(tmod) #gives fitted vs residuals, Normal Quantile-Quantile plot, leverage
#useful site for interpreting QQ plots: http://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot

#check model fit by hand. Check for overdispersion, plot fitted vs resduals, check for sources of heterogeneity in residuals. 

#extract residuals
E1 <- resid(tmod, type = "pearson") 

####### Overdispersion ############
#check for overdispersion: residual deviance/degrees of freedom
##can get this from the summary command above. Or use this below for a quick and dirty approach that will work even for glmer's. 
sum(E1^2) / (nrow(truetrans) - length(coef(tmod))) #0.65 - not overdispersed (which would be something >1); somewhat underdispersed, actually. 

#if model is over-dispersed, have some options: 
#  #A. Outliers? ==> Look at Cook’s Distances >1. If only one, remove it. But if a lot, maybe not the reason for all of the overdispersion.
cd<-cooks.distance(tmod)
cd[cd>1]
#B. Missing covariates or interactions?  ==> Go back or add a latent variable 
#C. Zero inflation?            ==> Check proportion of data that are zeros. Be concerned >25%. ZIP. 
#D. Large variance?            ==> Negative binomial
#F. Non-linear patterns        ==> Look at xyplot to see if nonlinear patterns (especially over time). Need a lot of observations to do GAM. Plot residuals against each linear covariate, and look for nonlinear patterns, if have them, may need GAM. 
#** if don’t have enough observations to do GAM, can do variable + variable^2 as predictors. 
#*** or can make time a factor
#G. Wrong link function        ==> Change it

#Other solutions: 
#***quasi-Poisson (blow up standard errors) – but not option in glmm. 
#***Observation-level random intercept (Pandora’s box) – latent covariate and it will take care of it., any info that can’t be captured in fixed or random component will be taken up by this latent variable. Doesn’t have full flexibility because we are imposing a distribution on him (normal with mean 0 and variance sigma-squared). Gives larger standard errors. If use this, add in full model, then do model selection. 
#Can use observation-level random effect for poisson, nb, binomial. However, if don’t have very many levels of a random effect, the observation-level random effect will steal all of the variance from the other random effect. 

## Back to model validation ######
#look for homogeneity of variances - plot fitted vs residuals
F1 <- fitted(tmod, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

#Look at independence by plotting residuals against covariates in model, and those not in model. 
plot(x=truetrans$island, y=E1) #heterogeneity in residuals bt islands
plot(x=truetrans$netting, y=E1) #heterogeneity in residuals wrt netting
plot(x=truetrans$site, y=E1) #residual variance slightly larger at Saipan sites than Guam sites, but homogeneity bt sites within an island

#Also consider: Plotting residuals versus time (if relevant)
#Plotting residuals versus spatial coordinates (if relevant)

#Check for normality of residuals
hist(E1) #looks pretty good. 

#look for influential values

######Results model validation ######
#Looks ok...no structure in residuals...go to next step

#############################
# hypothesis testing  #######
summary(tmod) #really, we care most about the interaction the effect of netting is different on Guam than on Saipan. Basically, being on saipan without netting is bad. Could stop here. 

#Or use lsmeans to test interaction too. 

tmod.grid1 <- ref.grid(tmod)
# start by creating a reference grid: essentially the cell structure
# the grid object contains the model and the data

summary(tmod.grid1)
#  gives cell means for each combination of factor levels

--------------

# tests of differences:

# can calculate both the lsmeans and contrasts at once
lsmeans(tmod.grid1, "island", contr="pairwise")
lsmeans(tmod.grid1, "netting", contr="pairwise")
# pairwise differences are in the $contrasts part of the result

# test the interaction
# compute pairwise differences within each level of one factor
int.isl <- pairs(tmod.grid1, by='island')
int.isl
#  No-Yes results for each Island

int.isl2 <- update(int.isl, by=NULL)
int.isl2
#  convert to a table with 2 rows (from a list of two contrasts)

test(pairs(int.isl2), joint=T)
# which we can then compare using a joint test

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
