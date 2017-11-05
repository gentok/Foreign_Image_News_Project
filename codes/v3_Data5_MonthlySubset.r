
################################################################################# 
## File Name: v3_Data5_MonthlySubset.R                                         ##
## Creation Date: 4 Nov 2017                                                   ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Aggregate Country Data Subsets & Add Monthly Variables / Data      ##
################################################################################# 

## For Jupyter Notebook (Ignore if Using Other Software) ##
library(IRdisplay)

display_html(
'<script>  
code_show=true; 
function code_toggle() {
  if (code_show){
    $(\'div.input\').hide();
  } else {
    $(\'div.input\').show();
  }
  code_show = !code_show
}  
$( document ).ready(code_toggle);
</script>
  <form action="javascript:code_toggle()">
    <input type="submit" value="Click here to toggle on/off the raw code.">
 </form>'
)


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
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")


## Load The Headline Subset Data ##
load("data/v3_Data4_HeadlineSubset.Rdata")

ls()


#############################
## Combine & Collapse Data ##
#############################

## Merge Variable (Remove Following Names) ##
usdatax <- merge(hldata, usdata, by="id_all",all.x=TRUE)
rusdatax <- merge(hldata, rusdata, by="id_all",all.x=TRUE)
rmnames1 <- gsub(".y","",names(usdatax),fixed=TRUE)[grep(".y",names(usdatax),fixed=TRUE)]
rmnames2 <- gsub(".y","",names(rusdatax),fixed=TRUE)[grep(".y",names(rusdatax),fixed=TRUE)]
rmnames <- c(rmnames1,rmnames2)

## Merge Each Subset with Whole Data (Again, Temporarily)
usdatax <- merge(hldata, usdata[,!(names(usdata) %in% rmnames)], by="id_all",all.x=TRUE) 
chndatax <- merge(hldata, chndata[,!(names(chndata) %in% rmnames)], by="id_all",all.x=TRUE) 
skordatax <- merge(hldata, skordata[,!(names(skordata) %in% rmnames)], by="id_all",all.x=TRUE) 
nkordatax <- merge(hldata, nkordata[,!(names(nkordata) %in% rmnames)], by="id_all",all.x=TRUE) 
rusdatax <- merge(hldata, rusdata[,!(names(rusdata) %in% rmnames)], by="id_all",all.x=TRUE) 
eurodatax <- merge(hldata, eurodata[,!(names(eurodata) %in% rmnames)], by="id_all",all.x=TRUE) 
mneastdatax <- merge(hldata, mneastdata[,!(names(mneastdata) %in% rmnames)], by="id_all",all.x=TRUE) 
#indiadatax <- merge(hldata, indiadata[,!(names(indiadata) %in% rmnames)], by="id_all",all.x=TRUE) 
taiwandatax <- merge(hldata, taiwandata[,!(names(taiwandata) %in% rmnames)], by="id_all",all.x=TRUE) 
seasiadatax <- merge(hldata, seasiadata[,!(names(seasiadata) %in% rmnames)], by="id_all",all.x=TRUE) 
msamericadatax <- merge(hldata, msamericadata[,!(names(msamericadata) %in% rmnames)], by="id_all",all.x=TRUE) 
oceaniadatax <- merge(hldata, oceaniadata[,!(names(oceaniadata) %in% rmnames)], by="id_all",all.x=TRUE) 
africadatax <- merge(hldata, africadata[,!(names(africadata) %in% rmnames)], by="id_all",all.x=TRUE) 

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
   rmnames1,rmnames2,rmnames)

ls()


##################################
## Additional Monthly Variables ##
##################################

## Total Coverage (TC)
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

## Tone of Coverage (PNC) *Denominator = Country TC. NOT USED.
usmonth$statetone_w_both_per<-usmonth$statetone_w_both/usmonth$statecount_w_both*100
chnmonth$statetone_w_both_per<-chnmonth$statetone_w_both/chnmonth$statecount_w_both*100
skormonth$statetone_w_both_per<-skormonth$statetone_w_both/skormonth$statecount_w_both*100
nkormonth$statetone_w_both_per<-nkormonth$statetone_w_both/nkormonth$statecount_w_both*100
chnmonth$statetone_w_both_per[is.na(chnmonth$statetone_w_both_per)]<-0
skormonth$statetone_w_both_per[is.na(skormonth$statetone_w_both_per)]<-0
nkormonth$statetone_w_both_per[is.na(nkormonth$statetone_w_both_per)]<-0

## Framed TC (Economy) *Denominator = Country TC. NOT USED.
usmonth$econ_w_both_per<-usmonth$econ_w_both/usmonth$statecount_w_both*100
chnmonth$econ_w_both_per<-chnmonth$econ_w_both/chnmonth$statecount_w_both*100
skormonth$econ_w_both_per<-skormonth$econ_w_both/skormonth$statecount_w_both*100
nkormonth$econ_w_both_per<-nkormonth$econ_w_both/nkormonth$statecount_w_both*100
chnmonth$econ_w_both_per[is.na(chnmonth$econ_w_both_per)]<-0
skormonth$econ_w_both_per[is.na(skormonth$econ_w_both_per)]<-0
nkormonth$econ_w_both_per[is.na(nkormonth$econ_w_both_per)]<-0

## Framed TC (Defense) *Denominator = Country TC. NOT USED.
usmonth$defense_w_both_per<-usmonth$defense_w_both/usmonth$statecount_w_both*100
chnmonth$defense_w_both_per<-chnmonth$defense_w_both/chnmonth$statecount_w_both*100
skormonth$defense_w_both_per<-skormonth$defense_w_both/skormonth$statecount_w_both*100
nkormonth$defense_w_both_per<-nkormonth$defense_w_both/nkormonth$statecount_w_both*100
chnmonth$defense_w_both_per[is.na(chnmonth$defense_w_both_per)]<-0
skormonth$defense_w_both_per[is.na(skormonth$defense_w_both_per)]<-0
nkormonth$defense_w_both_per[is.na(nkormonth$defense_w_both_per)]<-0

## Framed TC (Elite Communication) *Denominator = Country TC.
# usmonth$polcom_w_both_per<-usmonth$polcom_w_both/usmonth$statecount_w_both*100
# chnmonth$polcom_w_both_per<-chnmonth$polcom_w_both/chnmonth$statecount_w_both*100
# skormonth$polcom_w_both_per<-skormonth$polcom_w_both/skormonth$statecount_w_both*100
# nkormonth$polcom_w_both_per<-nkormonth$polcom_w_both/nkormonth$statecount_w_both*100
# chnmonth$polcom_w_both_per[is.na(chnmonth$polcom_w_both_per)]<-0
# skormonth$polcom_w_both_per[is.na(skormonth$polcom_w_both_per)]<-0
# nkormonth$polcom_w_both_per[is.na(nkormonth$polcom_w_both_per)]<-0

## Tone of Coverage (PNC) *Denominator = ALL. USED.
usmonth$statetone_w_both_per2<-usmonth$statetone_w_both/usmonth$wcount*100
chnmonth$statetone_w_both_per2<-chnmonth$statetone_w_both/chnmonth$wcount*100
skormonth$statetone_w_both_per2<-skormonth$statetone_w_both/skormonth$wcount*100
nkormonth$statetone_w_both_per2<-nkormonth$statetone_w_both/nkormonth$wcount*100

## Framed TC (Economy) *Denominator = ALL. USED.
usmonth$econ_w_both_per2<-usmonth$econ_w_both/usmonth$wcount*100
chnmonth$econ_w_both_per2<-chnmonth$econ_w_both/chnmonth$wcount*100
skormonth$econ_w_both_per2<-skormonth$econ_w_both/skormonth$wcount*100
nkormonth$econ_w_both_per2<-nkormonth$econ_w_both/nkormonth$wcount*100

## Framed TC (Defense) *Denominator = ALL. USED.
usmonth$defense_w_both_per2<-usmonth$defense_w_both/usmonth$wcount*100
chnmonth$defense_w_both_per2<-chnmonth$defense_w_both/chnmonth$wcount*100
skormonth$defense_w_both_per2<-skormonth$defense_w_both/skormonth$wcount*100
nkormonth$defense_w_both_per2<-nkormonth$defense_w_both/nkormonth$wcount*100

## Framed TC (Elite Communication) *Denominator = ALL.
# usmonth$polcom_w_both_per2<-usmonth$polcom_w_both/usmonth$wcount*100
# chnmonth$polcom_w_both_per2<-chnmonth$polcom_w_both/chnmonth$wcount*100
# skormonth$polcom_w_both_per2<-skormonth$polcom_w_both/skormonth$wcount*100
# nkormonth$polcom_w_both_per2<-nkormonth$polcom_w_both/nkormonth$wcount*100

## Framed PNC (Economy) *Denominator = ALL. USED.
usmonth$toneecon_w_both_per2<-usmonth$toneecon_w_both/usmonth$wcount*100
chnmonth$toneecon_w_both_per2<-chnmonth$toneecon_w_both/chnmonth$wcount*100
skormonth$toneecon_w_both_per2<-skormonth$toneecon_w_both/skormonth$wcount*100
nkormonth$toneecon_w_both_per2<-nkormonth$toneecon_w_both/nkormonth$wcount*100

## Framed PNC (Defense) *Denominator = ALL. USED.
usmonth$tonedefense_w_both_per2<-usmonth$tonedefense_w_both/usmonth$wcount*100
chnmonth$tonedefense_w_both_per2<-chnmonth$tonedefense_w_both/chnmonth$wcount*100
skormonth$tonedefense_w_both_per2<-skormonth$tonedefense_w_both/skormonth$wcount*100
nkormonth$tonedefense_w_both_per2<-nkormonth$tonedefense_w_both/nkormonth$wcount*100

usmonth$time<-seq(1:nrow(usmonth))
chnmonth$time<-seq(1:nrow(chnmonth))
skormonth$time<-seq(1:nrow(skormonth))
nkormonth$time<-seq(1:nrow(nkormonth))
rusmonth$time<-seq(1:nrow(rusmonth))
euromonth$time<-seq(1:nrow(euromonth))
mneastmonth$time<-seq(1:nrow(mneastmonth))
#indiamonth$time<-seq(1:nrow(indiamonth))
taiwanmonth$time<-seq(1:nrow(taiwanmonth))
seasiamonth$time<-seq(1:nrow(seasiamonth))
msamericamonth$time<-seq(1:nrow(msamericamonth))
oceaniamonth$time<-seq(1:nrow(oceaniamonth))
africamonth$time<-seq(1:nrow(africamonth))


########################
## Jiji Monthly Poll ##
########################

## Import Data ##
jijidata<-read.csv("data/jijidata_simple_8711to1503.csv")

## Create Additional Variables
jijidata$econbetter<-jijidata$econpast_better+jijidata$econpast_swbetter
jijidata$econworse<-jijidata$econpast_worse+jijidata$econpast_swworse
jijidata$lifeeasy<-jijidata$lifepast_easy+jijidata$lifepast_sweasy
jijidata$lifedifficult<-jijidata$lifepast_difficult+jijidata$lifepast_swdifficult

## Set the Time Variable (The Current Survey Month Matched with Previous Month Coverage/Trade)
jijidata[nrow(jijidata)+1,] <- NA
jijidata$time<-seq(1:nrow(jijidata))-1
jijidata <- jijidata[-1,]

head(jijidata)

################
## Trade Data ##
################

## Import Data 
tradedata<-read.csv("data/trade_gdp.csv")

## Total Trade Quantity
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

## Trade Balance
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

## Trade Quantity / GDP
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

## Trade Balance / GDP
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

## Time Variable for Trade
tradedata$time<-seq(1:nrow(tradedata))

head(tradedata)


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
save.image("data/v3_Data5_MonthlySubset.RData")
