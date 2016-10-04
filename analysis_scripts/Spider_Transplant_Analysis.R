#########################################
####### spider transplant analysis ##########
#########################################

#Research questions: 
#1) Does the duration of time a spider stays on its web differ between island or treatment? 
#using all data
#Total.Days~island*netting, family=poisson (could do this as a hazard rate, but I don't see a reason to)

#2) If a spider is missing, is the web more likely to be present without a spider inhabiting it (indicative of predation) on Saipan than on Guam? 
#using subset of data with spiders missing (omit ones where spiders remained entire time)
#WebPresBin~island*Netting, family=binomial

#Notes about the study
#Transplanting was done in two sites on Guam and Two Sites on Saipan 
#
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(truetrans$Total.Days)
boxplot(truetrans$Total.Days~ truetrans$Island*truetrans$Netting , xlab="Island + Treatment",  ylab="Total Days") #only those transplanted
boxplot(transplant$Total.Days~transplant$Island*transplant$Netting, ylab="Total Days") #all spiders (already present + transplants)

#b.	Examine Zero inflation Y
#i.	What proportion of response values = zero? 
with(truetrans, table(Total.Days)) # no zeros. 
with(truetrans, table(WebPresBin)) #binary- can't be zero-inflated

#c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other
with(truetrans, table(Island, Netting))

#d.	Linearity and homogeneity - Look at relationships of Y vs X’s:
# i.	Plot response against each predictor and random effect. 
ggplot(truetrans, aes(Netting, Total.Days, color=Island))+
  geom_boxplot()
#yes- linear and mostly homogeneous

#e.	Independence Y
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(truetrans, aes(Netting, Total.Days, color=Island))+
  geom_boxplot()+
  facet_grid(.~Site)
#could incorporate transect into main effects- but prob not part of an interaction bc small sample size, and effect of netting is consistent across transects. But I think it's okay to ignore transect). 
with(truetrans, ftable(Site, Netting, WebPresBin)) 

#ii. Are there other potential factors that reduce independence of Y’s? 
with(truetrans, ftable(Island, Netting))
with(truetrans, ftable(Island, Netting, WebPresBin)) #only 3 situtaions where web still present but spider gone on Guam, two inside netting one without. 

#f.	Sufficient and balanced data?  - Yes 
#i.	Are categorical covariates balanced? - Yes
# Adequate sample size- yes

#ii.	Examine interactions
#1.	Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) - Yes
with(truetrans, table(Island, Netting))

# 3)	Fix up dataframe
# a.	Remove missing values (NA’s)

#explore data visually

bargraph.CI(truetrans$Island, truetrans$Total.Days, group=truetrans$Netting, legend=F, ylab="Number of days", color=c("grey", "black")) 
legend(3, 5.3, bty="n", c( "With netting", "Without netting"), fill=c("grey", "black"))

bargraph.CI(truetrans$Island, truetrans$WebPresBin, group=truetrans$Netting, legend=T, ylab="Fraction of Webs Present", main="Web Presents or Absent")
with(truetrans, table(Island, WebPresBin, Netting))

ggplot(transplant, aes(x=Island, y=WebSize.cm., fill=Native))+
  geom_boxplot()

ggplot(truetrans, aes(Island, Total.Days, fill=Netting))+
  geom_violin()+
  theme_bw()

ggplot(transplant, aes(Island, Total.Days, fill=Netting))+
  geom_violin()+
  theme_bw()+
  facet_grid(.~Native)

ggplot(truetrans, aes(Island, WebPresBin, fill=Netting))+
  geom_violin()+
  theme_bw()

####Analyze data##################

#### Question 1 ####################
#1) Does the duration of time a spider stays on its web differ between island or treatment? 
#using all data from transplanted spiders
#Total.Days~island*netting, family=poisson 

tmod<-glm(Total.Days~Island*Netting, family= poisson, data=truetrans)
tmod1<-glm(Total.Days~Island+Netting, family= poisson, data=truetrans)
tmod2<-glm(Total.Days~Island, family= poisson, data=truetrans)
tmod3<-glm(Total.Days~Netting, family= poisson, data=truetrans)
tmod4<-glm(Total.Days~1, family= poisson, data=truetrans)
AIC(tmod, tmod1, tmod2, tmod3, tmod4) #tmod (full model) is definitely best

summary(tmod)# a little underdispersed
confint(tmod) 
anova(tmod, test="Chisq") 
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

plot(x=truetrans$Island, y=F1) #heterogeneity in residuals bt islands
plot(x=truetrans$Netting, y=F1) #heterogeneity in residuals wrt Netting
plot(x=truetrans$Site, y=F1) #residual variance larger at Saipan sites than Guam sites, but homogeneity bt sites within an island

###### Question 2 #############
##2) If a spider is missing, is the web more likely to be present without a spider inhabiting it (indicative of predation) on Saipan than on Guam? 
#using subset of data with spiders missing (omit ones where spiders remained entire time)
#WebPresBin~island*Netting, family=binomial
truetransnosp<-truetrans[!is.na(truetrans$WebPresBin),]

todweb<-glm(WebPresBin~Island*Netting, family=binomial, data=truetransnosp)
todweb1<-glm(WebPresBin~Island+Netting, family= binomial, data=truetransnosp)
todweb2<-glm(WebPresBin~Island, family= binomial, data=truetransnosp)
todweb3<-glm(WebPresBin~Netting, family= binomial, data=truetransnosp)
todweb4<-glm(WebPresBin~1, family= binomial, data=truetransnosp)
AIC(todweb, todweb1, todweb2, todweb3, todweb4) #tmod (full model) is definitely best

summary(todweb) #not overdispersed
confint(todweb) 
anova(todweb, test="Chisq") 
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

plot(x=truetransnosp$Island, y=F1) #heterogeneity in residuals bt islands
plot(x=truetransnosp$Netting, y=F1) #some heterogeneity in residuals wrt Netting
plot(x=truetransnosp$Site, y=F1) #residual variance larger at Saipan sites than Guam sites, but homogeneity bt sites within an island
