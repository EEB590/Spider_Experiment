#Spider transplant data munging

#load libraries
library(reshape2) #for melt
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#may need to install tibble first, and may need to restart R session to clear conflicting packages

#read in data
setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/raw")
transplant<-read.csv("Saipan_Guam_Transplant_asentered.csv")

#look at data
str(transplant)
summary(transplant)

#some data cleaning needed
levels(transplant$Island) <- gsub("Gaum", "Guam", levels(transplant$Island))
levels(transplant$Island) <- gsub("Siapan", "Saipan", levels(transplant$Island))
transplant$Site<-tolower(transplant$Site)

#Rename Web Size variable
transplant<-dplyr::rename(transplant, websize = WebSize.cm.)

#create column of 1/0 for web present/absent and spider pres/absent
transplant$SpidPresBin[transplant$SpidPres=="no"] <-0 #if spider is absent at end, put 0
transplant$SpidPresBin[transplant$SpidPres=="yes"] <-1 #if spider is present at end, put 1
transplant$WebPresBin[transplant$WebPres=="no"] <-0 #if web is absent, put 0
transplant$WebPresBin[transplant$WebPres=="yes"] <-1 #if web is present, put 1
transplant$WebPresBin[transplant$SpidPres=="yes"] <-NA #adds a NA for all webs where spider was present at the end. 

#check out data again
with(transplant, table(Site, Native))
with(transplant, table(Island, Native))
#what is sample size for the transplanted spiders? 
with(transplant[transplant$Native=="no",], table(Site, Netting))
with(transplant[transplant$Native=="no" & transplant$SpidPres=="no",], table(Netting, Island, WebPres))
#compare ftable to table
with(transplant[transplant$Native=="no" & transplant$SpidPres=="no",], ftable(Netting, Island, WebPres))

#create working database for analysis
setwd("~/Box Sync/Teaching/Rstats/Spider_Experiment/data/working")
write.csv(transplant, "transplant.csv")
