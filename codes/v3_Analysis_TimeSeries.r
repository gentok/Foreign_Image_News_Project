
#################################################################################
## File Name: v3_Analysis_TimeSeries.R                                         ##
## Date: 27 APr 2016                                                           ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Conduct Time Series Analysis                                       ##
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

## Suppress Warning
#options(warn=-1)
#options(warn=0) # put it back

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
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")


#######################
## Load Monthly Data ##
#######################

load("data/v3_Data5_MonthlySubset.Rdata")

ls()


#############################
## Agenda Setting Analysis ##
#############################

## Data #######################################

stv <- min(which(!is.na(usmonth$imprel)))
edv <- max(which(!is.na(usmonth$imprel)))

## Create Data ##
attach(usmonth[stv:edv,])
usimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(usmonth[stv:edv,])
attach(chnmonth[stv:edv,])
chnimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(chnmonth[stv:edv,])
attach(skormonth[stv:edv,])
skorimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(skormonth[stv:edv,])
attach(nkormonth[stv:edv,])
nkorimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(nkormonth[stv:edv,])
attach(rusmonth[stv:edv,])
rusimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(rusmonth[stv:edv,])
attach(euromonth[stv:edv,])
euroimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(euromonth[stv:edv,])
attach(mneastmonth[stv:edv,])
mneastimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(mneastmonth[stv:edv,])
attach(taiwanmonth[stv:edv,])
taiwanimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(taiwanmonth[stv:edv,])
attach(seasiamonth[stv:edv,])
seasiaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(seasiamonth[stv:edv,])
attach(msamericamonth[stv:edv,])
msamericaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(msamericamonth[stv:edv,])
attach(oceaniamonth[stv:edv,])
oceaniaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(oceaniamonth[stv:edv,])
attach(africamonth[stv:edv,])
africaimp<-data.frame(trade_gdp,statecount_w_both_per,imprel)
detach(africamonth[stv:edv,])


## ADF Test (Non-Stationary Variable Exists for All Except for US)
# US
summary(ur.df(usimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(usimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(usimp$trade_gdp,type="drift",selectlags="AIC")) ## Stationary
# China
summary(ur.df(chnimp$imprel,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(chnimp$imprel),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnimp$trade_gdp,type="trend",selectlags="AIC")) ## Stationary
# S.Korea
summary(ur.df(skorimp$imprel,type="drift",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(skorimp$imprel),type="none",selectlags="AIC")) ## Stationary
summary(ur.df(skorimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(skorimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (5%)
summary(ur.df(diff(skorimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# N.Korea
summary(ur.df(nkorimp$imprel,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(nkorimp$imprel),type="drift",selectlags="AIC")) # Stationary 
summary(ur.df(nkorimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(nkorimp$trade_gdp,type="trend",selectlags="AIC")) ## None-Stationary (5%)
summary(ur.df(diff(nkorimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# Russia
summary(ur.df(rusimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(rusimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(rusimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (5%)
summary(ur.df(diff(rusimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# Europe
summary(ur.df(euroimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(euroimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(euroimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (5%)
summary(ur.df(diff(euroimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary 
# Middle-Near East
summary(ur.df(mneastimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(mneastimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(mneastimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(mneastimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# Taiwan
summary(ur.df(taiwanimp$imprel,type="trend",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(taiwanimp$imprel),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(taiwanimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(taiwanimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(taiwanimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# South-East Asia
summary(ur.df(seasiaimp$imprel,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(seasiaimp$imprel),type="none",selectlags="AIC")) ## Stationary
summary(ur.df(seasiaimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(seasiaimp$trade_gdp,type="trend",selectlags="AIC")) ## Stationary
# Middle-South America
summary(ur.df(msamericaimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(msamericaimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(msamericaimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(msamericaimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# Oceania
summary(ur.df(oceaniaimp$imprel,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(oceaniaimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(oceaniaimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (1%)
summary(ur.df(diff(oceaniaimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationary
# Africa
summary(ur.df(africaimp$imprel,type="trend",selectlags="AIC")) ## Stationary
summary(ur.df(africaimp$statecount_w_both_per,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(africaimp$trade_gdp,type="trend",selectlags="AIC")) ## Non-Stationary (5%)
summary(ur.df(diff(africaimp$trade_gdp),type="drift",selectlags="AIC")) ## Stationry


# Optimal Lag Based on AIC
usIp <- VARselect(usimp,lag.max=12, type="both")$selection[1]; usIp # 3
chnIp <- VARselect(chnimp,lag.max=12, type="both")$selection[1]; chnIp # 12
skorIp <- VARselect(skorimp,lag.max=12, type="both")$selection[1]; skorIp # 4
nkorIp <- VARselect(nkorimp,lag.max=12, type="both")$selection[1]; nkorIp # 4
rusIp <- VARselect(rusimp,lag.max=12, type="both")$selection[1]; rusIp # 6
euroIp <- VARselect(euroimp,lag.max=12, type="both")$selection[1]; euroIp # 7
mneastIp <- VARselect(mneastimp,lag.max=12, type="both")$selection[1]; mneastIp # 3
taiwanIp <- VARselect(taiwanimp,lag.max=12, type="both")$selection[1]; taiwanIp # 5
seasiaIp <- VARselect(seasiaimp,lag.max=12, type="both")$selection[1]; seasiaIp # 7
msamericaIp <- VARselect(msamericaimp,lag.max=12, type="both")$selection[1]; msamericaIp # 3
oceaniaIp <- VARselect(oceaniaimp,lag.max=12, type="both")$selection[1]; oceaniaIp # 7
africaIp <- VARselect(africaimp,lag.max=12, type="both")$selection[1]; africaIp # 5


# VECM with Cointegration Trace Test (Cointegration Exists for All *This is Used)
usimp.vecm <- ca.jo(usimp, ecdet="const", type='trace', K=usIp, spec="transitory"); summary(usimp.vecm) # 2
chnimp.vecm <- ca.jo(chnimp, ecdet="const", type='trace', K=chnIp, spec="transitory"); summary(chnimp.vecm) # 2
skorimp.vecm <- ca.jo(skorimp, ecdet="const", type='trace', K=skorIp, spec="transitory"); summary(skorimp.vecm) # 1
nkorimp.vecm <- ca.jo(nkorimp, ecdet="const", type='trace', K=nkorIp, spec="transitory"); summary(nkorimp.vecm) # 2
rusimp.vecm <- ca.jo(rusimp, ecdet="const", type='trace', K=rusIp, spec="transitory"); summary(rusimp.vecm) # 1
euroimp.vecm <- ca.jo(euroimp, ecdet="const", type='trace', K=euroIp, spec="transitory"); summary(euroimp.vecm) # 1
mneastimp.vecm <- ca.jo(mneastimp, ecdet="const", type='trace', K=mneastIp, spec="transitory"); summary(mneastimp.vecm) # 2
taiwanimp.vecm <- ca.jo(taiwanimp, ecdet="const", type='trace', K=taiwanIp, spec="transitory"); summary(taiwanimp.vecm) #1
seasiaimp.vecm <- ca.jo(seasiaimp, ecdet="const", type='trace', K=seasiaIp, spec="transitory"); summary(seasiaimp.vecm) # 1
msamericaimp.vecm <- ca.jo(msamericaimp, ecdet="const", type='trace', K=msamericaIp, spec="transitory"); summary(msamericaimp.vecm) # 2
oceaniaimp.vecm <- ca.jo(oceaniaimp, ecdet="const", type='trace', K=oceaniaIp, spec="transitory"); summary(oceaniaimp.vecm) # 1
africaimp.vecm <- ca.jo(africaimp, ecdet="const", type='trace', K=africaIp, spec="transitory"); summary(africaimp.vecm) # 1


# Cointegration  Maximal Eigenvalue Test (Result Identical: not used for the final analysis)
# summary(ca.jo(usimp, ecdet="const", type='eigen', K=usIp, spec="transitory")) # 2
# summary(ca.jo(chnimp, ecdet="const", type='eigen', K=chnIp, spec="transitory")) # 2
# summary(ca.jo(skorimp, ecdet="const", type='eigen', K=skorIp, spec="transitory")) # 1
# summary(ca.jo(nkorimp, ecdet="const", type='eigen', K=nkorIp, spec="transitory")) # 2
# summary(ca.jo(rusimp, ecdet="const", type='eigen', K=rusIp, spec="transitory")) # 1
# summary(ca.jo(euroimp, ecdet="const", type='eigen', K=euroIp, spec="transitory")) # 1
# summary(ca.jo(mneastimp, ecdet="const", type='eigen', K=mneastIp, spec="transitory")) # 2
# summary(ca.jo(taiwanimp, ecdet="const", type='eigen', K=taiwanIp, spec="transitory")) # 1
# summary(ca.jo(seasiaimp, ecdet="const", type='eigen', K=seasiaIp, spec="transitory")) # 1
# summary(ca.jo(msamericaimp, ecdet="const", type='eigen', K=msamericaIp, spec="transitory")) # 2
# summary(ca.jo(oceaniaimp, ecdet="const", type='eigen', K=oceaniaIp, spec="transitory")) # 1
# summary(ca.jo(africaimp, ecdet="const", type='eigen', K=africaIp, spec="transitory")) # 1


## Restrictions for Short Run Matrix
SR <- matrix(NA,nrow=3,ncol=3); SR[1,2:3] <- 0; SR[2,3] <- 0
## NO Restrictions for Long Run Matrix
LR <- matrix(NA,nrow=3,ncol=3)

## Structural VECM Result
usimp.vecm.res <- SVEC(usimp.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE,boot=FALSE,runs=500)
chnimp.vecm.res <- SVEC(chnimp.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE,boot=FALSE,runs=500)
skorimp.vecm.res <- SVEC(skorimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
nkorimp.vecm.res <- SVEC(nkorimp.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE,boot=FALSE,runs=500)
rusimp.vecm.res <- SVEC(rusimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
euroimp.vecm.res <- SVEC(euroimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
mneastimp.vecm.res <- SVEC(mneastimp.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE,boot=FALSE,runs=500)
taiwanimp.vecm.res <- SVEC(taiwanimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
seasiaimp.vecm.res <- SVEC(seasiaimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
msamericaimp.vecm.res <- SVEC(msamericaimp.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE,boot=FALSE,runs=500)
oceaniaimp.vecm.res <- SVEC(oceaniaimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)
africaimp.vecm.res <- SVEC(africaimp.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE,boot=FALSE,runs=500)


## IRF
usirf<-irf(usimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
chnirf<-irf(chnimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
skorirf<-irf(skorimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
nkorirf<-irf(nkorimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
rusirf<-irf(rusimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
euroirf<-irf(euroimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
mneastirf<-irf(mneastimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
taiwanirf<-irf(taiwanimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
seasiairf<-irf(seasiaimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
msamericairf<-irf(msamericaimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
oceaniairf<-irf(oceaniaimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)
africairf<-irf(africaimp.vecm.res,n.ahead=12,ci=0.95,runs=1000)

# plot(usirf) # small
# plot(chnirf) # none
# plot(skorirf) # good short term
# plot(nkorirf) # good and persistent
# plot(rusirf) # good short term
# plot(euroirf) # small short term
# plot(mneastirf) # small short term
# plot(taiwanirf) # small short term
# plot(seasiairf) # no effect
# plot(msamericairf) # small
# plot(oceaniairf) # none
# plot(africairf) # none


################################
## Framing and Agenda-Setting ##
################################

attach(usmonth[stv:edv,])
usimp_frame<-data.frame(trade_gdp,defense_w_both_per2,econ_w_both_per2,imprel)
detach(usmonth[stv:edv,])
attach(chnmonth[stv:edv,])
chnimp_frame<-data.frame(trade_gdp,defense_w_both_per2,econ_w_both_per2,imprel)
detach(chnmonth[stv:edv,])
attach(skormonth[stv:edv,])
skorimp_frame<-data.frame(trade_gdp,defense_w_both_per2,econ_w_both_per2,imprel)
detach(skormonth[stv:edv,])
attach(nkormonth[stv:edv,])
nkorimp_frame<-data.frame(trade_gdp,defense_w_both_per2,econ_w_both_per2,imprel)
detach(nkormonth[stv:edv,])


## ADF Test (All new Variables are stationary)
# US
summary(ur.df(usimp_frame$econ_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(usimp_frame$defense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# China
summary(ur.df(chnimp_frame$econ_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnimp_frame$defense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# S.Korea
summary(ur.df(skorimp_frame$econ_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(skorimp_frame$defense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# N.Korea
summary(ur.df(nkorimp_frame$econ_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(nkorimp_frame$defense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary


# Optimal Lag Based on AIC
usIFp <- VARselect(usimp_frame,lag.max=12, type="both")$selection[1]; usIFp; usIFp=2 # 1
chnIFp <- VARselect(chnimp_frame,lag.max=12, type="both")$selection[1]; chnIFp # 2
skorIFp <- VARselect(skorimp_frame,lag.max=12, type="both")$selection[1]; skorIFp # 2
nkorIFp <- VARselect(nkorimp_frame,lag.max=12, type="both")$selection[1]; nkorIFp # 4


# VECM with Cointegration Trace Test (Cointegration Exists for All *This is Used)
usimpF.vecm <- ca.jo(usimp_frame, ecdet="const", type='trace', K=usIFp, spec="transitory"); summary(usimpF.vecm) # 3
chnimpF.vecm <- ca.jo(chnimp_frame, ecdet="const", type='trace', K=chnIFp, spec="transitory"); summary(chnimpF.vecm) # 2
skorimpF.vecm <- ca.jo(skorimp_frame, ecdet="const", type='trace', K=skorIFp, spec="transitory"); summary(skorimpF.vecm) # 2
nkorimpF.vecm <- ca.jo(nkorimp_frame, ecdet="const", type='trace', K=nkorIFp, spec="transitory"); summary(nkorimpF.vecm) # 2


## Restrictions for Short Run Matrix
SR <- matrix(NA,nrow=4,ncol=4); SR[1,2:4]<-0; SR[2,3:4]<-0; SR[3,4]<-0; #SR[3,2]<-0
## NO Restrictions for Long Run Matrix
LR <- matrix(NA,nrow=4,ncol=4)

## VECM Result
usimpF.vecm.res <- SVEC(usimpF.vecm,LR=LR,SR=SR,r=3,lrtest=FALSE)
chnimpF.vecm.res <- SVEC(chnimpF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500
skorimpF.vecm.res <- SVEC(skorimpF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500
nkorimpF.vecm.res <- SVEC(nkorimpF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500


## IRF
usirf_frame<-irf(usimpF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
chnirf_frame<-irf(chnimpF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
skorirf_frame<-irf(skorimpF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
nkorirf_frame<-irf(nkorimpF.vecm.res,n.ahead=12,ci=0.95,runs=1000)

# plot(usirf_frame)
# plot(chnirf_frame)
# plot(skorirf_frame)
# plot(nkorirf_frame)


###############################
## Persuasion (Like/Dislike) ##
###############################

attach(usmonth)
usldl<-data.frame(surplus_gdp,statetone_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(usmonth)
attach(chnmonth)
chnldl<-data.frame(surplus_gdp,statetone_w_both_per2,ldlstate)[-nrow(chnmonth),]
detach(chnmonth)
attach(skormonth)
skorldl<-data.frame(surplus_gdp,statetone_w_both_per2,ldlstate)[-nrow(skormonth),]
detach(skormonth)
attach(nkormonth)
nkorldl<-data.frame(surplus_gdp,statetone_w_both_per2,ldlstate)[-nrow(nkormonth),]
detach(nkormonth)


## ADF Test
# US
summary(ur.df(usldl$ldlstate,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(usldl$ldlstate),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(usldl$statetone_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(usldl$surplus_gdp,type="trend",selectlags="AIC")) ## Stationary
# China
summary(ur.df(chnldl$ldlstate,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(chnldl$ldlstate),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnldl$statetone_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnldl$surplus_gdp,type="trend",selectlags="AIC")) ## Stationary
# S.Korea
summary(ur.df(skorldl$ldlstate,type="trend",selectlags="AIC")) ## Non-Stationary (10%)
summary(ur.df(diff(skorldl$ldlstate),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(skorldl$statetone_w_both_per2,type="none",selectlags="AIC")) ## Stationary
summary(ur.df(skorldl$surplus_gdp,type="trend",selectlags="AIC")) ## Stationary
# N.Korea
summary(ur.df(nkorldl$ldlstate,type="trend",selectlags="AIC")) ## Non-Stationary (5%)
summary(ur.df(diff(nkorldl$ldlstate),type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(nkorldl$statetone_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(nkorldl$surplus_gdp,type="none",selectlags="AIC")) ## Stationary


# Optimal Lag Based on AIC
usTp <- VARselect(usldl,lag.max=12, type="both")$selection[1]; usTp # 12
chnTp <- VARselect(chnldl,lag.max=12, type="both")$selection[1]; chnTp # 12
skorTp <- VARselect(skorldl,lag.max=12, type="both")$selection[1]; skorTp # 12
nkorTp <- VARselect(nkorldl,lag.max=12, type="both")$selection[1]; nkorTp # 4


# VECM with Cointegration Trace Test (Cointegration Exists for All *This is Used)
usldl.vecm <- ca.jo(usldl, ecdet="const", type='trace', K=usTp, spec="transitory"); summary(usldl.vecm) # 1
chnldl.vecm <- ca.jo(chnldl, ecdet="const", type='trace', K=chnTp, spec="transitory"); summary(chnldl.vecm) # 1
skorldl.vecm <- ca.jo(skorldl, ecdet="const", type='trace', K=skorTp, spec="transitory"); summary(skorldl.vecm) # 1
nkorldl.vecm <- ca.jo(nkorldl, ecdet="const", type='trace', K=nkorTp, spec="transitory"); summary(nkorldl.vecm) # 2


## Restrictions for Short Run Matrix
SR <- matrix(NA,nrow=3,ncol=3); SR[1,2:3] <- 0; SR[2,3] <- 0
## NO Restrictions for Long Run Matrix
LR <- matrix(NA,nrow=3,ncol=3)

## Structural VECM Result
usldl.vecm.res <- SVEC(usldl.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE)
chnldl.vecm.res <- SVEC(chnldl.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE) #,boot=TRUE,runs=500
skorldl.vecm.res <- SVEC(skorldl.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE) #,boot=TRUE,runs=500
nkorldl.vecm.res <- SVEC(nkorldl.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500


## IRF
usirf_ldl<-irf(usldl.vecm.res,n.ahead=12,ci=0.95,runs=1000)
chnirf_ldl<-irf(chnldl.vecm.res,n.ahead=12,ci=0.95,runs=1000)
skorirf_ldl<-irf(skorldl.vecm.res,n.ahead=12,ci=0.95,runs=1000)
nkorirf_ldl<-irf(nkorldl.vecm.res,n.ahead=12,ci=0.95,runs=1000)

# plot(usirf_ldl)
# plot(chnirf_ldl)
# plot(skorirf_ldl)
# plot(nkorirf_ldl)


#############################
## Persuasion (ldl, frame) ##
#############################

attach(usmonth)
usldl_frame<-data.frame(surplus_gdp,tonedefense_w_both_per2,toneecon_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(usmonth)
attach(chnmonth)
chnldl_frame<-data.frame(surplus_gdp,tonedefense_w_both_per2,toneecon_w_both_per2,ldlstate)[-nrow(chnmonth),]
detach(chnmonth)
attach(skormonth)
skorldl_frame<-data.frame(surplus_gdp,tonedefense_w_both_per2,toneecon_w_both_per2,ldlstate)[-nrow(skormonth),]
detach(skormonth)
attach(nkormonth)
nkorldl_frame<-data.frame(surplus_gdp,tonedefense_w_both_per2,toneecon_w_both_per2,ldlstate)[-nrow(nkormonth),]
detach(nkormonth)


## ADF Test (All new Variables are stationary)
# US
summary(ur.df(usldl_frame$toneecon_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(usldl_frame$tonedefense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# China
summary(ur.df(chnldl_frame$toneecon_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(chnldl_frame$tonedefense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# S.Korea
summary(ur.df(skorldl_frame$toneecon_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(skorldl_frame$tonedefense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
# N.Korea
summary(ur.df(nkorldl_frame$toneecon_w_both_per2,type="drift",selectlags="AIC")) ## Stationary
summary(ur.df(nkorldl_frame$tonedefense_w_both_per2,type="drift",selectlags="AIC")) ## Stationary


# Optimal Lag Based on AIC
usTFp <- VARselect(usldl_frame,lag.max=12, type="both")$selection[1]; usTFp # 12
chnTFp <- VARselect(chnldl_frame,lag.max=12, type="both")$selection[1]; chnTFp # 3
skorTFp <- VARselect(skorldl_frame,lag.max=12, type="both")$selection[1]; skorTFp; skorTFp=2 # 1
nkorTFp <- VARselect(nkorldl_frame,lag.max=12, type="both")$selection[1]; nkorTFp # 4


# VECM with Cointegration Trace Test (Cointegration Exists for All *This is Used)
usldlF.vecm <- ca.jo(usldl_frame, ecdet="const", type='trace', K=usTFp, spec="transitory"); summary(usldlF.vecm) # 2
chnldlF.vecm <- ca.jo(chnldl_frame, ecdet="const", type='trace', K=chnTFp, spec="transitory"); summary(chnldlF.vecm) # 1
skorldlF.vecm <- ca.jo(skorldl_frame, ecdet="const", type='trace', K=skorTFp, spec="transitory"); summary(skorldlF.vecm) # 2
nkorldlF.vecm <- ca.jo(nkorldl_frame, ecdet="const", type='trace', K=nkorTFp, spec="transitory"); summary(nkorldlF.vecm) # 3


## Restrictions for Short Run Matrix
SR <- matrix(NA,nrow=4,ncol=4); SR[1,2:4]<-0; SR[2,3:4]<-0; SR[3,4]<-0; #SR[3,2]<-0
## NO Restrictions for Long Run Matrix
LR <- matrix(NA,nrow=4,ncol=4)

## VECM Result
usldlF.vecm.res <- SVEC(usldlF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE)
chnldlF.vecm.res <- SVEC(chnldlF.vecm,LR=LR,SR=SR,r=1,lrtest=FALSE) #,boot=TRUE,runs=500
skorldlF.vecm.res <- SVEC(skorldlF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500
nkorldlF.vecm.res <- SVEC(nkorldlF.vecm,LR=LR,SR=SR,r=3,lrtest=FALSE) #,boot=TRUE,runs=500


## IRF
usirf_ldl_frame<-irf(usldlF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
chnirf_ldl_frame<-irf(chnldlF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
skorirf_ldl_frame<-irf(skorldlF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
nkorirf_ldl_frame<-irf(nkorldlF.vecm.res,n.ahead=12,ci=0.95,runs=1000)

# plot(usirf_ldl_frame) # small
# plot(chnirf_ldl_frame) # none
# plot(skorirf_ldl_frame) # good short term
# plot(nkorirf_ldl_frame) # good and persistent


###########################################
## Persuasion (ldl, frame by frequency ) ##
###########################################

# Create Datasets
attach(usmonth)
usldlI_frame<-data.frame(surplus_gdp,defense_w_both_per2,econ_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(usmonth)
attach(chnmonth)
chnldlI_frame<-data.frame(surplus_gdp,defense_w_both_per2,econ_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(chnmonth)
attach(skormonth)
skorldlI_frame<-data.frame(surplus_gdp,defense_w_both_per2,econ_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(skormonth)
attach(nkormonth)
nkorldlI_frame<-data.frame(surplus_gdp,defense_w_both_per2,econ_w_both_per2,ldlstate)[-nrow(usmonth),]
detach(nkormonth)


# Optimal Lag Based on AIC
usTIFp <- VARselect(usldlI_frame,lag.max=12, type="both")$selection[1]; usTIFp # 12
chnTIFp <- VARselect(chnldlI_frame,lag.max=12, type="both")$selection[1]; chnTIFp # 12
skorTIFp <- VARselect(skorldlI_frame,lag.max=12, type="both")$selection[1]; skorTIFp; skorTIFp=2 # 1 (Changed to 2)
nkorTIFp <- VARselect(nkorldlI_frame,lag.max=12, type="both")$selection[1]; nkorTIFp # 3


# VECM with Cointegration Trace Test (Cointegration Exists for All *This is Used)
usldlIF.vecm <- ca.jo(usldlI_frame, ecdet="const", type='trace', K=usTIFp, spec="transitory"); summary(usldlIF.vecm) # 2
chnldlIF.vecm <- ca.jo(chnldlI_frame, ecdet="const", type='trace', K=chnTIFp, spec="transitory"); summary(chnldlIF.vecm) # 2
skorldlIF.vecm <- ca.jo(skorldlI_frame, ecdet="const", type='trace', K=skorTIFp, spec="transitory"); summary(skorldlIF.vecm) # 2
nkorldlIF.vecm <- ca.jo(nkorldlI_frame, ecdet="const", type='trace', K=nkorTIFp, spec="transitory"); summary(nkorldlIF.vecm) # 3


## Restrictions for Short Run Matrix
SR <- matrix(NA,nrow=4,ncol=4); SR[1,2:4]<-0; SR[2,3:4]<-0; SR[3,4]<-0; #SR[3,2]<-0
## NO Restrictions for Long Run Matrix
LR <- matrix(NA,nrow=4,ncol=4)

## SVECM Result
usldlIF.vecm.res <- SVEC(usldlIF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE)
chnldlIF.vecm.res <- SVEC(chnldlIF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500
skorldlIF.vecm.res <- SVEC(skorldlIF.vecm,LR=LR,SR=SR,r=2,lrtest=FALSE) #,boot=TRUE,runs=500
nkorldlIF.vecm.res <- SVEC(nkorldlIF.vecm,LR=LR,SR=SR,r=3,lrtest=FALSE) #,boot=TRUE,runs=500


usirf_ldlI_frame<-irf(usldlIF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
chnirf_ldlI_frame<-irf(chnldlIF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
skorirf_ldlI_frame<-irf(skorldlIF.vecm.res,n.ahead=12,ci=0.95,runs=1000)
nkorirf_ldlI_frame<-irf(nkorldlIF.vecm.res,n.ahead=12,ci=0.95,runs=1000)

# plot(usirf_ldlI_frame) # small
# plot(chnirf_ldlI_frame) # none
# plot(skorirf_ldlI_frame) # good short term
# plot(nkorirf_ldlI_frame) # good and persistent


######################################
## Save Result (separate from data) ##
######################################

## Remove Data from Work Space ##
rm(usmonth, chnmonth, skormonth, nkormonth, rusmonth, euromonth, mneastmonth, #indiamonth,
   taiwanmonth, seasiamonth, msamericamonth, oceaniamonth, africamonth)

## Save the Remaining Analytical Result ##
save.image("./outputs/v3_Analysis_TimeSeries.RData")
#load("./outputs/v3_Analysis_TimeSeries.RData")

