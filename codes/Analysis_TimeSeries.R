#################################################################################
## File Name: Analysis_TimeSeries.R                                            ##
## Date: 27 APr 2016                                                           ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Conduct Time Series Analysis                                       ##
#################################################################################

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot); library(doBy); library(descr)

##################Prepare Packages##################
library(vars)#;detach("package:vars", unload=TRUE)
library(tseries);library(urca);
library(tsDyn)#;detach("package:tsDyn", unload=TRUE)
#library(MSBVAR);detach("package:MSBVAR", unload=TRUE)
#library(FIAR);
####################################################
library(ggplot2)

## Set Working Directory (Automatically or Manually) ##
#projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")

## Load The Headline Subset Data ##
load("./data/Data_MonthlySubset_Simple_170427.Rdata")

#############################
## Agenda Setting Analysis ##
#############################

## Create Data ##
attach(usmonth[90:329,])
usimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_usimp<-cbind(imprel_opt_india)
detach(usmonth[90:329,])
attach(chnmonth[90:329,])
chnimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_chnimp<-cbind(imprel_opt_india)
detach(chnmonth[90:329,])
attach(skormonth[90:329,])
skorimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_skorimp<-cbind(imprel_opt_india)
detach(skormonth[90:329,])
attach(nkormonth[90:329,])
nkorimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_nkorimp<-cbind(imprel_opt_india)
detach(nkormonth[90:329,])
attach(rusmonth[90:329,])
rusimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_rusimp<-cbind(imprel_opt_india)
detach(rusmonth[90:329,])
attach(euromonth[90:329,])
euroimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_euroimp<-cbind(imprel_opt_india)
detach(euromonth[90:329,])
attach(mneastmonth[90:329,])
mneastimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_mneastimp<-cbind(imprel_opt_india)
detach(mneastmonth[90:329,])
attach(taiwanmonth[90:329,])
taiwanimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_taiwanimp<-cbind(imprel_opt_india)
detach(taiwanmonth[90:329,])
attach(seasiamonth[90:329,])
seasiaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_seasiaimp<-cbind(imprel_opt_india)
detach(seasiamonth[90:329,])
attach(msamericamonth[90:329,])
msamericaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_msamericaimp<-cbind(imprel_opt_india)
detach(msamericamonth[90:329,])
attach(oceaniamonth[90:329,])
oceaniaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_oceaniaimp<-cbind(imprel_opt_india)
detach(oceaniamonth[90:329,])
attach(africamonth[90:329,])
africaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
exovar_africaimp<-cbind(imprel_opt_india)
detach(africamonth[90:329,])
#attach(indiamonth[indiamonth$imprel_opt_india==1,])
#indiaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
#detach(indiamonth[indiamonth$imprel_opt_india==1,])

VARselect(usimp,lag.max=12) # 3
VARselect(chnimp,lag.max=12) # 8
VARselect(skorimp,lag.max=12) # 4
VARselect(nkorimp,lag.max=12) # 4
VARselect(rusimp,lag.max=12) # 3
VARselect(euroimp,lag.max=12) # 7
VARselect(mneastimp,lag.max=12) # 3
VARselect(taiwanimp,lag.max=12) # 4
VARselect(seasiaimp,lag.max=12) # 8
VARselect(msamericaimp,lag.max=12) # 3
VARselect(oceaniaimp,lag.max=12) # 7
VARselect(africaimp,lag.max=12) # 6

usvar<-VAR(usimp,p=3,exogen=exovar_usimp);summary(usvar)
chnvar<-VAR(chnimp,p=8,exogen=exovar_chnimp);summary(chnvar)
skorvar<-VAR(skorimp,p=4,exogen=exovar_skorimp);summary(skorvar)
nkorvar<-VAR(nkorimp,p=4,exogen=exovar_nkorimp);summary(nkorvar)
rusvar<-VAR(rusimp,p=3,exogen=exovar_rusimp);summary(rusvar)
eurovar<-VAR(euroimp,p=7,exogen=exovar_euroimp);summary(eurovar)
mneastvar<-VAR(mneastimp,p=3,exogen=exovar_mneastimp);summary(mneastvar)
taiwanvar<-VAR(taiwanimp,p=4,exogen=exovar_taiwanimp);summary(taiwanvar)
seasiavar<-VAR(seasiaimp,p=8,exogen=exovar_seasiaimp);summary(seasiavar)
msamericavar<-VAR(msamericaimp,p=3,exogen=exovar_msamericaimp);summary(msamericavar)
oceaniavar<-VAR(oceaniaimp,p=7,exogen=exovar_oceaniaimp);summary(oceaniavar)
africavar<-VAR(africaimp,p=6,exogen=exovar_africaimp);summary(africavar)
#indiavar<-VAR(indiaimp,p=12);summary(indiavar)

usirf<-irf(usvar,n.ahead=12,ci=0.95,runs=2000)
chnirf<-irf(chnvar,n.ahead=12,ci=0.95,runs=2000)
skorirf<-irf(skorvar,n.ahead=12,ci=0.95,runs=2000)
nkorirf<-irf(nkorvar,n.ahead=12,ci=0.95,runs=2000)
rusirf<-irf(rusvar,n.ahead=12,ci=0.95,runs=2000)
euroirf<-irf(eurovar,n.ahead=12,ci=0.95,runs=2000)
mneastirf<-irf(mneastvar,n.ahead=12,ci=0.95,runs=2000)
taiwanirf<-irf(taiwanvar,n.ahead=12,ci=0.95,runs=2000)
seasiairf<-irf(seasiavar,n.ahead=12,ci=0.95,runs=2000)
msamericairf<-irf(msamericavar,n.ahead=12,ci=0.95,runs=2000)
oceaniairf<-irf(oceaniavar,n.ahead=12,ci=0.95,runs=2000)
africairf<-irf(africavar,n.ahead=12,ci=0.95,runs=2000)
#indiairf<-irf(indiavar,n.ahead=12,ci=0.95,runs=2000)

#plot(usirf) # small
#plot(chnirf) # none
#plot(skorirf) # good short term
#plot(nkorirf) # good and persistent
#plot(rusirf) # good short term
#plot(euroirf) # small short term
#plot(mneastirf) # small short term
#plot(taiwanirf) # small short term
#plot(seasiairf) # no effect
#plot(msamericairf) # small
#plot(oceaniairf) # none
#plot(africairf) # none


################################
## Framing and Agenda-Setting ##
################################

attach(usmonth[90:329,])
usimp_frame<-data.frame(trade_gdp,econ_w_both_per2,defense_w_both_per2,imprel)
exovar_usimp_frame<-cbind(imprel_opt_india)
detach(usmonth[90:329,])
attach(chnmonth[90:329,])
chnimp_frame<-data.frame(trade_gdp,econ_w_both_per2,defense_w_both_per2,imprel)
exovar_chnimp_frame<-cbind(imprel_opt_india)
detach(chnmonth[90:329,])
attach(skormonth[90:329,])
skorimp_frame<-data.frame(trade_gdp,econ_w_both_per2,defense_w_both_per2,imprel)
exovar_skorimp_frame<-cbind(imprel_opt_india)
detach(skormonth[90:329,])
attach(nkormonth[90:329,])
nkorimp_frame<-data.frame(trade_gdp,econ_w_both_per2,defense_w_both_per2,imprel)
exovar_nkorimp_frame<-cbind(imprel_opt_india)
detach(nkormonth[90:329,])

VARselect(usimp_frame,lag.max=12) # 3
VARselect(chnimp_frame,lag.max=12) # 8
VARselect(skorimp_frame,lag.max=12) # 2
VARselect(nkorimp_frame,lag.max=12) # 4

usvar_frame<-VAR(usimp_frame,p=3,exogen=exovar_usimp_frame);summary(usvar_frame)
chnvar_frame<-VAR(chnimp_frame,p=8,exogen=exovar_chnimp_frame);summary(chnvar_frame)
skorvar_frame<-VAR(skorimp_frame,p=2,exogen=exovar_skorimp_frame);summary(skorvar_frame)
nkorvar_frame<-VAR(nkorimp_frame,p=4,exogen=exovar_nkorimp_frame);summary(nkorvar_frame)

usirf_frame<-irf(usvar_frame,n.ahead=12,ci=0.95,runs=2000)
chnirf_frame<-irf(chnvar_frame,n.ahead=12,ci=0.95,runs=2000)
skorirf_frame<-irf(skorvar_frame,n.ahead=12,ci=0.95,runs=2000)
nkorirf_frame<-irf(nkorvar_frame,n.ahead=12,ci=0.95,runs=2000)

###############################
## Persuasion (Like/Dislike) ##
###############################

attach(usmonth)
usldl<-data.frame(surplus_gdp,statepos_w_both_per2,stateneg_w_both_per2,ldlstate)
exovar_usldl<-cbind(econworse)
detach(usmonth)
attach(chnmonth)
chnldl<-data.frame(surplus_gdp,statepos_w_both_per2,stateneg_w_both_per2,ldlstate)
exovar_chnldl<-cbind(econworse)
detach(chnmonth)
attach(skormonth)
skorldl<-data.frame(surplus_gdp,statepos_w_both_per2,stateneg_w_both_per2,ldlstate)
exovar_skorldl<-cbind(econworse)
detach(skormonth)
attach(nkormonth)
nkorldl<-data.frame(surplus_gdp,statepos_w_both_per2,stateneg_w_both_per2,ldlstate)
exovar_nkorldl<-cbind(econworse)
detach(nkormonth)

VARselect(usldl,lag.max=12) # 12 (13)
VARselect(chnldl,lag.max=12) # 4
VARselect(skorldl,lag.max=12) # 7
VARselect(nkorldl,lag.max=12) # 4

usvar_ldl<-VAR(usldl,p=12,exogen=exovar_usldl);summary(usvar_ldl)
chnvar_ldl<-VAR(chnldl,p=4,exogen=exovar_chnldl);summary(chnvar_ldl)
skorvar_ldl<-VAR(skorldl,p=7,exogen=exovar_skorldl);summary(skorvar_ldl)
nkorvar_ldl<-VAR(nkorldl,p=4,exogen=exovar_nkorldl);summary(nkorvar_ldl)

usirf_ldl<-irf(usvar_ldl,n.ahead=12,ci=0.95,runs=2000)
chnirf_ldl<-irf(chnvar_ldl,n.ahead=12,ci=0.95,runs=2000)
skorirf_ldl<-irf(skorvar_ldl,n.ahead=12,ci=0.95,runs=2000)
nkorirf_ldl<-irf(nkorvar_ldl,n.ahead=12,ci=0.95,runs=2000)

#############################
## Persuasion (ldl, frame) ##
#############################

attach(usmonth)
usldl_frame<-data.frame(surplus_gdp,negecon_w_both_per2,negdefense_w_both_per2,ldlstate)
exovar_usldl_frame<-cbind(econworse)
detach(usmonth)
attach(chnmonth)
chnldl_frame<-data.frame(surplus_gdp,negecon_w_both_per2,negdefense_w_both_per2,ldlstate)
exovar_chnldl_frame<-cbind(econworse)
detach(chnmonth)
attach(skormonth)
skorldl_frame<-data.frame(surplus_gdp,negecon_w_both_per2,negdefense_w_both_per2,ldlstate)
exovar_skorldl_frame<-cbind(econworse)
detach(skormonth)
attach(nkormonth)
nkorldl_frame<-data.frame(surplus_gdp,negecon_w_both_per2,negdefense_w_both_per2,ldlstate)
exovar_nkorldl_frame<-cbind(econworse)
detach(nkormonth)

VARselect(usldl_frame,lag.max=12) # 12
VARselect(chnldl_frame,lag.max=12) # 3
VARselect(skorldl_frame,lag.max=12) # 3
VARselect(nkorldl_frame,lag.max=12) # 4

usvar_ldl_frame<-VAR(usldl_frame,p=12,exogen=exovar_usldl);summary(usvar_ldl_frame)
chnvar_ldl_frame<-VAR(chnldl_frame,p=3,exogen=exovar_chnldl);summary(chnvar_ldl_frame)
skorvar_ldl_frame<-VAR(skorldl_frame,p=3,exogen=exovar_skorldl);summary(skorvar_ldl_frame)
nkorvar_ldl_frame<-VAR(nkorldl_frame,p=4,exogen=exovar_nkorldl);summary(nkorvar_ldl_frame)

usirf_ldl_frame<-irf(usvar_ldl_frame,n.ahead=12,ci=0.95,runs=2000)
chnirf_ldl_frame<-irf(chnvar_ldl_frame,n.ahead=12,ci=0.95,runs=2000)
skorirf_ldl_frame<-irf(skorvar_ldl_frame,n.ahead=12,ci=0.95,runs=2000)
nkorirf_ldl_frame<-irf(nkorvar_ldl_frame,n.ahead=12,ci=0.95,runs=2000)

#plot(usirf_ldl_frame) # small
#plot(chnirf_ldl_frame) # none
#plot(skorirf_ldl_frame) # good short term
#plot(nkorirf_ldl_frame) # good and persistent

######################################
## Save Result (separate from data) ##
######################################

## Remove Data from Work Space ##
rm(usmonth, chnmonth, skormonth, nkormonth, rusmonth, euromonth, mneastmonth, #indiamonth,
   taiwanmonth, seasiamonth, msamericamonth, oceaniamonth, africamonth)

## Save the Remaining Analytical Result ##
save.image("./outputs/Analysis_TimeSeries_170427.RData")
