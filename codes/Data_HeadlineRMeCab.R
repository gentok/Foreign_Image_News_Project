#################################################################################
## File Name: Data_HeadlineRMecab.R                                            ##
## Creation Date: 27 Apr 2016                                                  ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Conduct Isomorphic Analysis and Create RMeCab Dataset              ##
#################################################################################

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
# ** NEED Current MeCab Installation prior to installing RMeCab **
#install.packages ("RMeCab", repos = "http://rmecab.jp/R")
library(RMeCab); library(rprojroot)

## Set Working Directory (Automatically or Manually) ##
#projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")

#######################################
## Data Import & Isomorphic Analysis ##
#######################################

# Read Text Data
datedata<-read.csv("../date_data/alldate_160228.csv")
# Conduct Isomorphic Analysis
MecabRes<-RMeCabDF(dataf=datedata,coln="Headline")

## Date Data Add Variables ##
datedata$Asahi[is.na(datedata$Asahi)]<-0
datedata$Yomiuri[is.na(datedata$Yomiuri)]<-0
datedata$Both <- datedata$Asahi*(4/9)+datedata$Yomiuri*(5/9) ## Headline Count
datedata$Asahi_w <- datedata$Asahi*datedata$wcount # Word count for Asahi
datedata$Yomiuri_w <- datedata$Yomiuri*datedata$wcount # Word count Yomiuri
datedata$Both_w <- datedata$Asahi_w*(4/9)+datedata$Yomiuri_w*(5/9)
## Note that Weighting in "Both" variable is made on the fact that 
## Actual circulation of two newspapers are roughly 4:5

# Create Subset Data
usdata<-subset(datedata, us==1)
chndata<-subset(datedata, chn==1)
skordata<-subset(datedata, kor==1)
nkordata<-subset(datedata, nkor==1)

# Isomorphic Analysis by Country (Use User Dictionary for user-defined words)
usMecabRes<-RMeCabDF(dataf=usdata,coln="Headline", mypref=1,dic="./codes/userdictionary/user.dic")
chnMecabRes<-RMeCabDF(dataf=chndata,coln="Headline", mypref=1,dic="./codes/userdictionary/user.dic")
skorMecabRes<-RMeCabDF(dataf=skordata,coln="Headline", mypref=1,dic="./codes/userdictionary/user.dic")
nkorMecabRes<-RMeCabDF(dataf=nkordata,coln="Headline", mypref=1,dic="./codes/userdictionary/user.dic")

################
## Save Image ##
################
save.image("../new_data/Data_HeadlineRMeCab.Rdata")
#load("../new_data/Data_HeadlineRMeCab.Rdata")