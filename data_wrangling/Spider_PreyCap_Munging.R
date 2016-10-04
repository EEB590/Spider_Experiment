#Spider prey capture data munging
#
##data notes
# obs1, obs2, obs3 etc is when the same web was revisted on different days.
# Jeff took samples at 3 sites per island. I think the only data for preynum comes from the spiders that were transplanted- need to check with him.  
# 
### load libraries
library(tidyr)
library(dplyr)

######################
#load data
setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/raw")
preycap<-read.csv("preycapture_asentered.csv", header=T)

#totalprey is not raw data - shouldn't be in dataset
preycap<-preycap[,-4] #remove column 4

#Change data from wide to long format
#use tidyr (new way- much more efficient code than reshape!)
preycapL<-gather(preycap, "obs", "preynum", 4:11)

#####Clean up data ############
summary(preycapL)# Look at data
str(preycapL) #check to make sure factors are factors and numbers are numeric or integer
preycapL$preynum<-as.numeric(preycapL$preynum)
#what other components should be in a different class? 

#clean data entry errors
levels(preycapL$site) <- gsub("forbid", "forbi", levels(preycapL$site))
preycapL$site<-tolower(preycap$site)

#Look at response (preynum)
summary(preycapL$preynum) #where are all these NA's coming from? webs that were not observed multiple times - not useful data. 
preycapL<-preycapL[!is.na(preycapL$preynum),]

##########
#delete "obs" from start of variable describing observation number.
#using substr- this is saying "keep the 4th element (start at 4, stop at 4)". 
preycapL$obs<-substr(preycapL$obs, 4, 4)
preycapL$obs<-as.numeric(preycapL$obs) #change to numeric from character

##########

setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/working")
write.csv(preycapL,"preycapturelong.csv")
