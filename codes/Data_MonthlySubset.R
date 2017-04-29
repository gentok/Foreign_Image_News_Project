################################################################################# 
## File Name: Data_MonthlySubset.R                                             ##
## Creation Date: 27 Apr 2016                                                  ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Aggregate Country Data Subsets & Add Monthly Variables / Data      ##
################################################################################# 

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
# ** NEED Current MeCab Installation prior to installing RMeCab **
#install.packages ("RMeCab", repos = "http://rmecab.jp/R")
library(rprojroot); library(doBy); library(descr)

## Set Working Directory (Automatically or Manually) ##
#projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")

## Load The Headline Subset Data ##
load("../new_data/Data_HeadlineSubset_170427.Rdata")

#############################
## Combine & Collapse Data ##
#############################

## Merge Variable ##
matchvar<-c("id_all","id","id_original","year","month","date",
            "ymonth","Headline","paper","wcount","us","chn",
            "kor","nkor","code_us","code_chn","code_sko",
            "code_nko","Asahi","Yomiuri",
            "Both","Asahi_w","Yomiuri_w","Both_w",
            "jijistartdate","jijiymonth")

## Merge Each Subset with Whole Data (Again, Temporarily)
usdatax <- merge(datedata, usdata, by=matchvar,all.x=TRUE) 
chndatax <- merge(datedata, chndata, by=matchvar,all.x=TRUE) 
skordatax <- merge(datedata, skordata, by=matchvar,all.x=TRUE) 
nkordatax <- merge(datedata, nkordata, by=matchvar,all.x=TRUE) 
rusdatax <- merge(datedata, rusdata, by=matchvar,all.x=TRUE) 
eurodatax <- merge(datedata, eurodata, by=matchvar,all.x=TRUE) 
mneastdatax <- merge(datedata, mneastdata, by=matchvar,all.x=TRUE) 
#indiadatax <- merge(datedata, indiadata, by=matchvar,all.x=TRUE) 
taiwandatax <- merge(datedata, taiwandata, by=matchvar,all.x=TRUE) 
seasiadatax <- merge(datedata, seasiadata, by=matchvar,all.x=TRUE) 
msamericadatax <- merge(datedata, msamericadata, by=matchvar,all.x=TRUE) 
oceaniadatax <- merge(datedata, oceaniadata, by=matchvar,all.x=TRUE) 
africadatax <- merge(datedata, africadata, by=matchvar,all.x=TRUE) 

## Collapse the Daily Data into Monthly Data ##
usmonth<-summaryBy(.~jijiymonth, data=usdatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
chnmonth<-summaryBy(.~jijiymonth, data=chndatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
skormonth<-summaryBy(.~jijiymonth, data=skordatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
nkormonth<-summaryBy(.~jijiymonth, data=nkordatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
rusmonth<-summaryBy(.~jijiymonth, data=rusdatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
euromonth<-summaryBy(.~jijiymonth, data=eurodatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
mneastmonth<-summaryBy(.~jijiymonth, data=mneastdatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
#indiamonth<-summaryBy(.~jijiymonth, data=indiadatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
taiwanmonth<-summaryBy(.~jijiymonth, data=taiwandatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
seasiamonth<-summaryBy(.~jijiymonth, data=seasiadatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
msamericamonth<-summaryBy(.~jijiymonth, data=msamericadatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
oceaniamonth<-summaryBy(.~jijiymonth, data=oceaniadatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)
africamonth<-summaryBy(.~jijiymonth, data=africadatax, FUN=c(sum),keep.names=TRUE,na.rm=TRUE)

## Remove temporal (and headline) datasets ##
rm(usdata, chndata, skordata, nkordata, rusdata, eurodata, mneastdata, #indiadata,
   taiwandata, seasiadata, msamericadata, oceaniadata, africadata, 
   usdatax, chndatax, skordatax, nkordatax, rusdatax, eurodatax, mneastdatax, #indiadatax,
   taiwandatax, seasiadatax, msamericadatax, oceaniadatax, africadatax,
   matchvar)
   
##################################
## Additional Monthly Variables ##
##################################

usmonth$statecount_w_both_per<-usmonth$statecount_w_both/usmonth$wcount*100
chnmonth$statecount_w_both_per<-chnmonth$statecount_w_both/chnmonth$wcount*100
skormonth$statecount_w_both_per<-skormonth$statecount_w_both/skormonth$wcount*100
nkormonth$statecount_w_both_per<-nkormonth$statecount_w_both/nkormonth$wcount*100
rusmonth$statecount_w_both_per<-rusmonth$statecount_w_both/rusmonth$wcount*100
euromonth$statecount_w_both_per<-euromonth$statecount_w_both/euromonth$wcount*100
mneastmonth$statecount_w_both_per<-mneastmonth$statecount_w_both/mneastmonth$wcount*100
#indiamonth$statecount_w_both_per<-indiamonth$statecount_w_both/indiamonth$wcount*100
taiwanmonth$statecount_w_both_per<-taiwanmonth$statecount_w_both/taiwanmonth$wcount*100
seasiamonth$statecount_w_both_per<-seasiamonth$statecount_w_both/seasiamonth$wcount*100
msamericamonth$statecount_w_both_per<-msamericamonth$statecount_w_both/msamericamonth$wcount*100
oceaniamonth$statecount_w_both_per<-oceaniamonth$statecount_w_both/oceaniamonth$wcount*100
africamonth$statecount_w_both_per<-africamonth$statecount_w_both/africamonth$wcount*100

usmonth$stateneg_w_both_per<-usmonth$stateneg_w_both/usmonth$statecount_w_both*100
chnmonth$stateneg_w_both_per<-chnmonth$stateneg_w_both/chnmonth$statecount_w_both*100
skormonth$stateneg_w_both_per<-skormonth$stateneg_w_both/skormonth$statecount_w_both*100
nkormonth$stateneg_w_both_per<-nkormonth$stateneg_w_both/nkormonth$statecount_w_both*100
chnmonth$stateneg_w_both_per[is.na(chnmonth$stateneg_w_both_per)]<-0
skormonth$stateneg_w_both_per[is.na(skormonth$stateneg_w_both_per)]<-0
nkormonth$stateneg_w_both_per[is.na(nkormonth$stateneg_w_both_per)]<-0

usmonth$history_w_both_per<-usmonth$history_w_both/usmonth$statecount_w_both*100
chnmonth$history_w_both_per<-chnmonth$history_w_both/chnmonth$statecount_w_both*100
skormonth$history_w_both_per<-skormonth$history_w_both/skormonth$statecount_w_both*100
nkormonth$history_w_both_per<-nkormonth$history_w_both/nkormonth$statecount_w_both*100
chnmonth$history_w_both_per[is.na(chnmonth$history_w_both_per)]<-0
skormonth$history_w_both_per[is.na(skormonth$history_w_both_per)]<-0
nkormonth$history_w_both_per[is.na(nkormonth$history_w_both_per)]<-0

usmonth$econ_w_both_per<-usmonth$econ_w_both/usmonth$statecount_w_both*100
chnmonth$econ_w_both_per<-chnmonth$econ_w_both/chnmonth$statecount_w_both*100
skormonth$econ_w_both_per<-skormonth$econ_w_both/skormonth$statecount_w_both*100
nkormonth$econ_w_both_per<-nkormonth$econ_w_both/nkormonth$statecount_w_both*100
chnmonth$econ_w_both_per[is.na(chnmonth$econ_w_both_per)]<-0
skormonth$econ_w_both_per[is.na(skormonth$econ_w_both_per)]<-0
nkormonth$econ_w_both_per[is.na(nkormonth$econ_w_both_per)]<-0

usmonth$defense_w_both_per<-usmonth$defense_w_both/usmonth$statecount_w_both*100
chnmonth$defense_w_both_per<-chnmonth$defense_w_both/chnmonth$statecount_w_both*100
skormonth$defense_w_both_per<-skormonth$defense_w_both/skormonth$statecount_w_both*100
nkormonth$defense_w_both_per<-nkormonth$defense_w_both/nkormonth$statecount_w_both*100
chnmonth$defense_w_both_per[is.na(chnmonth$defense_w_both_per)]<-0
skormonth$defense_w_both_per[is.na(skormonth$defense_w_both_per)]<-0
nkormonth$defense_w_both_per[is.na(nkormonth$defense_w_both_per)]<-0

usmonth$polcom_w_both_per<-usmonth$polcom_w_both/usmonth$statecount_w_both*100
chnmonth$polcom_w_both_per<-chnmonth$polcom_w_both/chnmonth$statecount_w_both*100
skormonth$polcom_w_both_per<-skormonth$polcom_w_both/skormonth$statecount_w_both*100
nkormonth$polcom_w_both_per<-nkormonth$polcom_w_both/nkormonth$statecount_w_both*100
chnmonth$polcom_w_both_per[is.na(chnmonth$polcom_w_both_per)]<-0
skormonth$polcom_w_both_per[is.na(skormonth$polcom_w_both_per)]<-0
nkormonth$polcom_w_both_per[is.na(nkormonth$polcom_w_both_per)]<-0

usmonth$stateneg_w_both_per2<-usmonth$stateneg_w_both/usmonth$wcount*100
chnmonth$stateneg_w_both_per2<-chnmonth$stateneg_w_both/chnmonth$wcount*100
skormonth$stateneg_w_both_per2<-skormonth$stateneg_w_both/skormonth$wcount*100
nkormonth$stateneg_w_both_per2<-nkormonth$stateneg_w_both/nkormonth$wcount*100

usmonth$history_w_both_per2<-usmonth$history_w_both/usmonth$wcount*100
chnmonth$history_w_both_per2<-chnmonth$history_w_both/chnmonth$wcount*100
skormonth$history_w_both_per2<-skormonth$history_w_both/skormonth$wcount*100
nkormonth$history_w_both_per2<-nkormonth$history_w_both/nkormonth$wcount*100

usmonth$econ_w_both_per2<-usmonth$econ_w_both/usmonth$wcount*100
chnmonth$econ_w_both_per2<-chnmonth$econ_w_both/chnmonth$wcount*100
skormonth$econ_w_both_per2<-skormonth$econ_w_both/skormonth$wcount*100
nkormonth$econ_w_both_per2<-nkormonth$econ_w_both/nkormonth$wcount*100

usmonth$defense_w_both_per2<-usmonth$defense_w_both/usmonth$wcount*100
chnmonth$defense_w_both_per2<-chnmonth$defense_w_both/chnmonth$wcount*100
skormonth$defense_w_both_per2<-skormonth$defense_w_both/skormonth$wcount*100
nkormonth$defense_w_both_per2<-nkormonth$defense_w_both/nkormonth$wcount*100

usmonth$polcom_w_both_per2<-usmonth$polcom_w_both/usmonth$wcount*100
chnmonth$polcom_w_both_per2<-chnmonth$polcom_w_both/chnmonth$wcount*100
skormonth$polcom_w_both_per2<-skormonth$polcom_w_both/skormonth$wcount*100
nkormonth$polcom_w_both_per2<-nkormonth$polcom_w_both/nkormonth$wcount*100

usmonth$negecon_w_both_per2<-usmonth$negecon_w_both/usmonth$wcount*100
chnmonth$negecon_w_both_per2<-chnmonth$negecon_w_both/chnmonth$wcount*100
skormonth$negecon_w_both_per2<-skormonth$negecon_w_both/skormonth$wcount*100
nkormonth$negecon_w_both_per2<-nkormonth$negecon_w_both/nkormonth$wcount*100

usmonth$negdefense_w_both_per2<-usmonth$negdefense_w_both/usmonth$wcount*100
chnmonth$negdefense_w_both_per2<-chnmonth$negdefense_w_both/chnmonth$wcount*100
skormonth$negdefense_w_both_per2<-skormonth$negdefense_w_both/skormonth$wcount*100
nkormonth$negdefense_w_both_per2<-nkormonth$negdefense_w_both/nkormonth$wcount*100

usmonth$statepos_w_both_per2<-usmonth$statecount_w_both_per-usmonth$stateneg_w_both_per2
chnmonth$statepos_w_both_per2<-chnmonth$statecount_w_both_per-chnmonth$stateneg_w_both_per2
skormonth$statepos_w_both_per2<-skormonth$statecount_w_both_per-skormonth$stateneg_w_both_per2
nkormonth$statepos_w_both_per2<-nkormonth$statecount_w_both_per-nkormonth$stateneg_w_both_per2

usmonth$posecon_w_both_per2<-usmonth$econ_w_both_per2-usmonth$negecon_w_both_per2
chnmonth$posecon_w_both_per2<-chnmonth$econ_w_both_per2-chnmonth$negecon_w_both_per2
skormonth$posecon_w_both_per2<-skormonth$econ_w_both_per2-skormonth$negecon_w_both_per2
nkormonth$posecon_w_both_per2<-nkormonth$econ_w_both_per2-nkormonth$negecon_w_both_per2

usmonth$posdefense_w_both_per2<-usmonth$defense_w_both_per2-usmonth$negdefense_w_both_per2
chnmonth$posdefense_w_both_per2<-chnmonth$defense_w_both_per2-chnmonth$negdefense_w_both_per2
skormonth$posdefense_w_both_per2<-skormonth$defense_w_both_per2-skormonth$negdefense_w_both_per2
nkormonth$posdefense_w_both_per2<-nkormonth$defense_w_both_per2-nkormonth$negdefense_w_both_per2

usmonth$statnegbal_w_both_per2<-usmonth$stateneg_w_both_per2-usmonth$statepos_w_both_per2
chnmonth$statnegbal_w_both_per2<-chnmonth$stateneg_w_both_per2-chnmonth$statepos_w_both_per2
skormonth$statnegbal_w_both_per2<-skormonth$stateneg_w_both_per2-skormonth$statepos_w_both_per2
nkormonth$statnegbal_w_both_per2<-nkormonth$stateneg_w_both_per2-nkormonth$statepos_w_both_per2

usmonth$time<-seq(1:329)
chnmonth$time<-seq(1:329)
skormonth$time<-seq(1:329)
nkormonth$time<-seq(1:329)
rusmonth$time<-seq(1:329)
euromonth$time<-seq(1:329)
mneastmonth$time<-seq(1:329)
#indiamonth$time<-seq(1:329)
taiwanmonth$time<-seq(1:329)
seasiamonth$time<-seq(1:329)
msamericamonth$time<-seq(1:329)
oceaniamonth$time<-seq(1:329)
africamonth$time<-seq(1:329)

########################
## Other Monthly Data ##
########################

## Jiji Monthly Poll ##
jijidata<-read.csv("C:/GoogleDrive/Data/Jiji Poll/Jiji_simplified_160213_8711to1503.csv")
jijidata$econbetter<-jijidata$econpast_better+jijidata$econpast_swbetter
jijidata$econworse<-jijidata$econpast_worse+jijidata$econpast_swworse
jijidata$lifeeasy<-jijidata$lifepast_easy+jijidata$lifepast_sweasy
jijidata$lifedifficult<-jijidata$lifepast_difficult+jijidata$lifepast_swdifficult
jijidata$time<-seq(1:329)
write.csv(jijidata,"../new_data/jijidata_170427.csv",row.names=FALSE)

## Trade Data ##
tradedata<-read.csv("../month_otherdata/trade_gdp.csv")
tradedata$us_trade<-tradedata$us_export+tradedata$us_import
tradedata$chn_trade<-tradedata$chn_export+tradedata$chn_import
tradedata$skor_trade<-tradedata$skor_export+tradedata$skor_import
tradedata$nkor_trade<-tradedata$nkor_export+tradedata$nkor_import
tradedata$rus_trade<-tradedata$rus_export+tradedata$rus_import
tradedata$eu_trade<-tradedata$eu_export+tradedata$eu_import
tradedata$asean_trade<-tradedata$asean_export+tradedata$asean_import
tradedata$msame_trade<-tradedata$msame_export+tradedata$msame_import
tradedata$meast_trade<-tradedata$meast_export+tradedata$meast_import+
                       tradedata$turkey_export+tradedata$turkey_import
tradedata$africa_trade<-tradedata$africa_export+tradedata$africa_import
tradedata$oceania_trade<-tradedata$oceania_export+tradedata$oceania_import
tradedata$india_trade<-tradedata$india_export+tradedata$india_import
tradedata$taiwan_trade<-tradedata$taiwan_export+tradedata$taiwan_import

tradedata$us_surplus<-tradedata$us_export-tradedata$us_import
tradedata$chn_surplus<-tradedata$chn_export-tradedata$chn_import
tradedata$skor_surplus<-tradedata$skor_export-tradedata$skor_import
tradedata$nkor_surplus<-tradedata$nkor_export-tradedata$nkor_import
tradedata$rus_surplus<-tradedata$rus_export-tradedata$rus_import
tradedata$eu_surplus<-tradedata$eu_export-tradedata$eu_import
tradedata$asean_surplus<-tradedata$asean_export-tradedata$asean_import
tradedata$msame_surplus<-tradedata$msame_export-tradedata$msame_import
tradedata$meast_surplus<-tradedata$meast_export-tradedata$meast_import+
                         tradedata$turkey_export-tradedata$turkey_import
tradedata$africa_surplus<-tradedata$africa_export-tradedata$africa_import
tradedata$oceania_surplus<-tradedata$oceania_export-tradedata$oceania_import
tradedata$india_surplus<-tradedata$india_export-tradedata$india_import
tradedata$taiwan_surplus<-tradedata$taiwan_export-tradedata$taiwan_import

tradedata$us_trade_gdp<-tradedata$us_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$chn_trade_gdp<-tradedata$chn_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$skor_trade_gdp<-tradedata$skor_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$nkor_trade_gdp<-tradedata$nkor_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$rus_trade_gdp<-tradedata$rus_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$eu_trade_gdp<-tradedata$eu_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$asean_trade_gdp<-tradedata$asean_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$msame_trade_gdp<-tradedata$msame_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$meast_trade_gdp<-tradedata$meast_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$africa_trade_gdp<-tradedata$africa_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$oceania_trade_gdp<-tradedata$oceania_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$india_trade_gdp<-tradedata$india_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$taiwan_trade_gdp<-tradedata$taiwan_trade/(tradedata$gdp_original*tradedata$gdp_tradefit)*100

tradedata$us_surplus_gdp<-tradedata$us_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$chn_surplus_gdp<-tradedata$chn_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$skor_surplus_gdp<-tradedata$skor_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$nkor_surplus_gdp<-tradedata$nkor_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$rus_surplus_gdp<-tradedata$rus_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$eu_surplus_gdp<-tradedata$eu_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$asean_surplus_gdp<-tradedata$asean_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$msame_surplus_gdp<-tradedata$msame_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$meast_surplus_gdp<-tradedata$meast_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$africa_surplus_gdp<-tradedata$africa_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$oceania_surplus_gdp<-tradedata$oceania_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$india_surplus_gdp<-tradedata$india_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100
tradedata$taiwan_surplus_gdp<-tradedata$taiwan_surplus/(tradedata$gdp_original*tradedata$gdp_tradefit)*100

tradedata$time<-seq(1:329)
write.csv(tradedata,"../new_data/tradedata_170427.csv",row.names=FALSE)

###################################
## Add Variables to monthly Data ##
###################################

## Importance of the Country ##
usmonth$imprel<-jijidata$imprel_us
chnmonth$imprel<-jijidata$imprel_china
skormonth$imprel<-jijidata$imprel_skorea
nkormonth$imprel<-jijidata$imprel_nkorea
rusmonth$imprel<-jijidata$imprel_rus
euromonth$imprel<-jijidata$imprel_europe
mneastmonth$imprel<-jijidata$imprel_mneast
#indiamonth$imprel<-jijidata$imprel_india
taiwanmonth$imprel<-jijidata$imprel_taiwan
seasiamonth$imprel<-jijidata$imprel_seasia
msamericamonth$imprel<-jijidata$imprel_csamerica
oceaniamonth$imprel<-jijidata$imprel_oceania
africamonth$imprel<-jijidata$imprel_africa

## Dummy Variable for having India option or not
usmonth$imprel_opt_india<-jijidata$imprel_opt_india
chnmonth$imprel_opt_india<-jijidata$imprel_opt_india
skormonth$imprel_opt_india<-jijidata$imprel_opt_india
nkormonth$imprel_opt_india<-jijidata$imprel_opt_india
rusmonth$imprel_opt_india<-jijidata$imprel_opt_india
euromonth$imprel_opt_india<-jijidata$imprel_opt_india
mneastmonth$imprel_opt_india<-jijidata$imprel_opt_india
#indiamonth$imprel_opt_india<-jijidata$imprel_opt_india
taiwanmonth$imprel_opt_india<-jijidata$imprel_opt_india
seasiamonth$imprel_opt_india<-jijidata$imprel_opt_india
msamericamonth$imprel_opt_india<-jijidata$imprel_opt_india
oceaniamonth$imprel_opt_india<-jijidata$imprel_opt_india
africamonth$imprel_opt_india<-jijidata$imprel_opt_india

# Best 3 States You Like
usmonth$likestate<-jijidata$likestate_us
chnmonth$likestate<-jijidata$likestate_china
skormonth$likestate<-jijidata$likestate_skorea
nkormonth$likestate<-jijidata$likestate_nkorea

# Worst 3 States you dislike
usmonth$dislikestate<-jijidata$dislikestate_us
chnmonth$dislikestate<-jijidata$dislikestate_china
skormonth$dislikestate<-jijidata$dislikestate_skorea
nkormonth$dislikestate<-jijidata$dislikestate_nkorea

## Like - Dislike Percent
usmonth$ldlstate<-usmonth$likestate-usmonth$dislikestate
chnmonth$ldlstate<-chnmonth$likestate-chnmonth$dislikestate
skormonth$ldlstate<-skormonth$likestate-skormonth$dislikestate
nkormonth$ldlstate<-nkormonth$likestate-nkormonth$dislikestate

## Trade per gdp
usmonth$trade_gdp<-tradedata$us_trade_gdp
chnmonth$trade_gdp<-tradedata$chn_trade_gdp
skormonth$trade_gdp<-tradedata$skor_trade_gdp
nkormonth$trade_gdp<-tradedata$nkor_trade_gdp
rusmonth$trade_gdp<-tradedata$rus_trade_gdp
euromonth$trade_gdp<-tradedata$eu_trade_gdp
mneastmonth$trade_gdp<-tradedata$meast_trade_gdp
#indiamonth$trade_gdp<-tradedata$india_trade_gdp
taiwanmonth$trade_gdp<-tradedata$taiwan_trade_gdp
seasiamonth$trade_gdp<-tradedata$asean_trade_gdp
msamericamonth$trade_gdp<-tradedata$msame_trade_gdp
oceaniamonth$trade_gdp<-tradedata$oceania_trade_gdp
africamonth$trade_gdp<-tradedata$africa_trade_gdp

## Surplus per gdp
usmonth$surplus_gdp<-tradedata$us_surplus_gdp
chnmonth$surplus_gdp<-tradedata$chn_surplus_gdp
skormonth$surplus_gdp<-tradedata$skor_surplus_gdp
nkormonth$surplus_gdp<-tradedata$nkor_surplus_gdp
rusmonth$surplus_gdp<-tradedata$rus_surplus_gdp
euromonth$surplus_gdp<-tradedata$eu_surplus_gdp
mneastmonth$surplus_gdp<-tradedata$meast_surplus_gdp
#indiamonth$surplus_gdp<-tradedata$india_surplus_gdp
taiwanmonth$surplus_gdp<-tradedata$taiwan_surplus_gdp
seasiamonth$surplus_gdp<-tradedata$asean_surplus_gdp
msamericamonth$surplus_gdp<-tradedata$msame_surplus_gdp
oceaniamonth$surplus_gdp<-tradedata$oceania_surplus_gdp
africamonth$surplus_gdp<-tradedata$africa_surplus_gdp

## Economic Perception Better (dummy)
usmonth$econbetter<-jijidata$econbetter
chnmonth$econbetter<-jijidata$econbetter
skormonth$econbetter<-jijidata$econbetter
nkormonth$econbetter<-jijidata$econbetter

## Economic Perception worse (dummy)
usmonth$econworse<-jijidata$econworse
chnmonth$econworse<-jijidata$econworse
skormonth$econworse<-jijidata$econworse
nkormonth$econworse<-jijidata$econworse


################
## Save Data  ##
################

## Save All ##
save.image("../new_data/Data_MonthlySubset_170427.RData")

## Save Simplified Data ##
rm(datedata,jijidata,tradedata)
save.image("./data/Data_MonthlySubset_Simple_170427.RData")