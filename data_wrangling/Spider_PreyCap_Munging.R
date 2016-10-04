#Spider prey capture data munging
#
##data notes
# obs1, obs2, obs3 etc is when the same web was revisted on different days.
# Jeff took samples at 3 sites per island. I think the only data for preynum comes from the spiders that were transplanted- need to check with him.  

######################
#load data
preycap<-read.csv("preycapture_asentered.csv", header=T)

#totalprey is not raw data - shouldn't be in dataset
preycap<-preycap[,-4] #remove column 4

#Change data from wide to long format
#use reshape package (old way)
preycapL <- melt(preycap, id.vars=c("island","site","web"),                                 measure.vars=c("obs1", "obs2", "obs3","obs4","obs5", "obs6", "obs7", "obs8"),   variable.name="obs", value.name="preynum")

#Alternative method - use tidyr (new way- much more efficient code!)
preycapL<-gather(preycap, "obs", "preynum", 4:11)

#write.csv(preycapL,"preycapturelong.csv")

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
