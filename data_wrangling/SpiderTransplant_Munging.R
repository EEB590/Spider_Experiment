#Spider transplant experiment data munging

#load libraries
library(reshape2) #for melt
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

#read in data
setwd("data/raw")
# setwd("..") #may need to use this and one below if changed to working directory
# setwd("raw")
transplant<-read.csv("Saipan_Guam_Transplant_asentered.csv")

#look at data
str(transplant)

######## cleaning case for factors ############
#rename columns so they are lower case. There are a bunch of ways to do this. Here is one using the rename function in the dplyr package (note that this function has slightly different syntax than the one in the ddply package)
transplant<-rename(transplant, island=Island, site=Site, web=Web.., native=Native, netting=Netting, startdate=Start.Date, enddate=End.Date, totaldays=Total.Days, spidpres=SpidPres, webpres=WebPres, websize=WebSize.cm.)

#some data cleaning needed
levels(transplant$island) <- gsub("Gaum", "Guam", levels(transplant$island))
levels(transplant$island) <- gsub("Siapan", "Saipan", levels(transplant$island))
transplant$site<-as.factor(tolower(transplant$site))
transplant$island<-as.factor(tolower(transplant$island))

# remove trailing whitespace
levels(transplant$site)
transplant$site<-as.factor(trimws(transplant$site))

summary(transplant)

######### Dates & duration ###################
#change date format to standard yyyymmdd format
#helpful site: https://www.r-bloggers.com/date-formats-in-r/

levels(as.factor(transplant$startdate))
class(transplant$startdate)

#problems- missing year. Date format is okay, but want to change to a more standard yyyymmdd format

transplant$year<-2013 #this experiment happened in 2013. Added this.
#change both startdate & year to character
transplant$year<-as.character(transplant$year)
transplant$startdate<-as.character(transplant$startdate)
#combine using unite in tidyr package
transplant<-unite(transplant, startdate, c(startdate, year), sep="-")
#now, tell R startdate is a real date in a specific format
transplant$startdate<-as.Date(as.character(transplant$startdate), "%d-%b-%Y")

#repeat for end date, using lubridate instead of as.Date
transplant$year<-2013 #this experiment happened in 2013. Added this.
#change both startdate & year to character
transplant$year<-as.character(transplant$year)
transplant$enddate<-as.character(transplant$enddate)
transplant<-unite(transplant, enddate, c(enddate, year), sep="-")
transplant$enddate<-dmy(transplant$enddate)

#Now, create a new column called duration
transplant$duration<-as.numeric(transplant$enddate-transplant$startdate)

# Remove column total days because it is not raw data, but calculated data, so shouldn't be in final dataset
#use "Select" - to select columns from a dataframe to include or drop
transplant<-select(transplant, -totaldays)

###########
#create column of 1/0 for web present/absent and spider pres/absent
transplant$spidpresbin[transplant$spidpres=="no"] <-0 #if spider is absent at end, put 0
transplant$spidpresbin[transplant$spidpres=="yes"] <-1 #if spider is present at end, put 1
transplant$webpresbin[transplant$webpres=="no"] <-0 #if web is absent, put 0
transplant$webpresbin[transplant$webpres=="yes"] <-1 #if web is present, put 1; below we will change all 1's to NA's if the spider is also present so we can use this to evaluate whether spider was predated or moved. 
transplant$webpresbin[transplant$spidpres=="yes"] <-NA #adds a NA to webpresbin column for all webs where spider was present at the end (bc not an instance of predation)

########################
#clean up "web"column to remove the ' between letters. 
#separating one column into two columns
transplant<-separate(transplant, col=web, into=c("web_a", "web_b"), sep="'", remove=F)

#combining two columns into one column (note the underscore after unite), rename same as original name to overwrite it. 
transplant<-unite_(transplant, "web", c("web_a", "web_b"), sep="", remove=T)
#######################
summary(transplant)

#write csv file with final, cleaned dataset that we want to work with in the future. In this case, we want the full transplant dataset that has been merged with island. 

#create working database for analysis
setwd("..")
setwd("working")
write.csv(transplant, "transplant.csv", row.names=F)
