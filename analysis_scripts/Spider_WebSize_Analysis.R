######Spider Web Size Analysis ######

#Research Question: 
#Do spiders build smaller webs when birds are present? If so, then web size should be smaller on Saipan than on Guam. (note the N=1 problem here).
#Does this vary depending on whether spider was transplanted or found in the area? 

setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/working")
transplant<-read.csv("transplant.csv")

#################
#get subset that was actually transplanted rather than ones that were observed in place        
truetrans<-transplant[transplant$Native=="no",]

################

###### Data Exploration ############

##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(transplant$websize)
boxplot(transplant$websize~ transplant$Island, xlab="Island",  ylab="websize")
#no major outliers in Y, X is categorical

#b.	Examine Zero inflation Y
#Not applicable for web size question, because response is continuous

#c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(transplant, table(Island, Native)) #have all combinations here. 

#d.	Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(transplant, aes(Native, websize, color=Island))+
  geom_boxplot()
#maybe less variance on Saipan than on Guam, but nothing stands out as terrible. 

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(transplant, aes(Native, websize, color=Island))+
  geom_boxplot()+
  facet_grid(.~Site)
#well, we only sampled at 2 sites on Guam, and three sites on Saipan, and we don't have all levels of Native for all sites. But nothing really stands out in terms of site-level effects. 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(transplant, table(Island, Native))

#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

##################################################
###########  Analysis   ###############

#1) Does web size differ between islands, and does transplanting affect web size? 
#using all data
#websize~Island*Native, family=gaussian  #by default, an identity link

webmod1<-lm(websize~Island*Native, data=transplant)
summary(webmod1)

# Opinions on model selection- much disagreement about which is best. 
# 1. Classical Hypothesis testing: drop 1
# 2. Classic model selection: Use LRT to come up with best model
# 3. Information theoretic approach- compare models using AIC- get competing models
# 4. Don’t do any model selection at all- fit model that you think makes sense, and keep everything as it is, even non-significant parameters. Only ask about interactions using model selection, but once fit model with main effect terms, then stick with it. 

##
drop1(webmod1)

##
webmod2<-lm(websize~Island+Native, data=transplant)
webmod3<-lm(websize~Island, data=transplant)
webmod4<-lm(websize~Native, data=transplant)
webmod_null<-lm(websize~1, data=transplant)
AIC(webmod1, webmod2, webmod3, webmod4, webmod_null) #webmod3 has lowest AIC, by almost 2 points

##
anova(webmod1)  #matches AIC comparison results
##
confint(webmod1) 

#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#B. Look at influential values: Cook
#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
#D. Look at normality: histogram

#extract residuals
E1 <- resid(webmod1, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(webmod1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=transplant$Island, y=F1) #heterogeneity in residuals bt islands
plot(x=transplant$Native, y=F1) #heterogeneity in residuals wrt Native
plot(x=transplant$Site, y=F1) #residual variance larger at Guam sites than Saipan sites, but homogeneity bt sites within an island


