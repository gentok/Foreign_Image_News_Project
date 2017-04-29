#################################################################################
## File Name: Figures_TimeSeries.R                                             ##
## Creation Date: 27 APr 2016                                                  ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Visualizer Result of Time Series Analysis                          ##
#################################################################################

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot);
library(knitr);library(ggplot2);library(gridExtra);library(grid);library(xtable)
source("C:/GoogleDrive/Stat_Soft_Helps/R/Packages/ggplot2/multiplot.R")

## Set Working Directory (Automatically or Manually) ##
#projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project/codes")

## Load The Headline Subset Data ##
load("../new_data/Data_MonthlySubset_170427.Rdata")
load("./outputs/Analysis_TimeSeries_170427.Rdata")

#######################
## Plot Descriptives ##
#######################

## Average Level of Importance ##
imprel_us_median<-median(jijidata$imprel_us,na.rm=TRUE); imprel_us_median
imprel_chn_median<-median(jijidata$imprel_china,na.rm=TRUE);imprel_chn_median
imprel_seasia_median<-median(jijidata$imprel_seasia,na.rm=TRUE); imprel_seasia_median
imprel_skor_median<-median(jijidata$imprel_skorea,na.rm=TRUE); imprel_skor_median
imprel_euro_median<-median(jijidata$imprel_europe,na.rm=TRUE); imprel_euro_median
imprel_rus_median<-median(jijidata$imprel_rus,na.rm=TRUE); imprel_rus_median
imprel_nkor_median<-median(jijidata$imprel_nkorea,na.rm=TRUE); imprel_nkor_median
imprel_mneast_median<-median(jijidata$imprel_mneast,na.rm=TRUE); imprel_mneast_median
imprel_taiwan_median<-median(jijidata$imprel_taiwan,na.rm=TRUE); imprel_taiwan_median
imprel_msamerica_median<-median(jijidata$imprel_csamerica,na.rm=TRUE); imprel_msamerica_median
imprel_africa_median<-median(jijidata$imprel_africa,na.rm=TRUE); imprel_africa_median
imprel_oceania_median<-median(jijidata$imprel_oceania,na.rm=TRUE); imprel_oceania_median

time<-rep(seq(1:329),12)
impreldata<-data.frame(time)
impreldata$id<-c(rep(1,329),rep(2,329),rep(3,329),rep(4,329),
                 rep(5,329),rep(6,329),rep(7,329),rep(8,329),
                 rep(9,329),rep(10,329),rep(11,329),rep(12,329))
impreldata$imprel<-c(jijidata$imprel_us,
                         jijidata$imprel_china,
                         jijidata$imprel_seasia,
                         jijidata$imprel_skor,
                         jijidata$imprel_euro,
                         jijidata$imprel_rus,
                         jijidata$imprel_nkor,
                         jijidata$imprel_mneast,
                         jijidata$imprel_taiwan,
                         jijidata$imprel_csamerica,
                         jijidata$imprel_africa,
                         jijidata$imprel_oceania)
impreldata$statecount_w_both_per<-c(usmonth$statecount_w_both_per,
     chnmonth$statecount_w_both_per,
     seasiamonth$statecount_w_both_per,
     skormonth$statecount_w_both_per,
     euromonth$statecount_w_both_per,
     rusmonth$statecount_w_both_per,
     nkormonth$statecount_w_both_per,
     mneastmonth$statecount_w_both_per,
     taiwanmonth$statecount_w_both_per,
     msamericamonth$statecount_w_both_per,
     africamonth$statecount_w_both_per,
     oceaniamonth$statecount_w_both_per)

impreldatax<-impreldata[impreldata$time>=90,]

gktheme <-
  theme(axis.text=element_text(size=10, colour="black"),
        axis.title.x=element_text(size=12,face="bold", vjust=-1.5),
        axis.title.y=element_text(size=12,face="bold", vjust=1.5),
        plot.title=element_text(size=13,face="bold", vjust=2),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.background = element_rect(fill=NA, colour="black",
                                        size=0.5, linetype=1))
## R Code ##
p1<- ggplot(impreldatax, aes(as.factor(id),imprel)) + gktheme +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_boxplot(colour="black",fill="grey80",lwd=0.5)+
  scale_y_continuous(limits=c(0,80),breaks=c(0,20,40,60,80)) +
  scale_x_discrete(labels=c("United States","China","South East Asia",
                            "South Korea","Europe","Russia","North Korea",
                            "Middle/Near East","Taiwan",
                            "Central/South \nAmerica","Africa","Oceania"))+
  xlab("Foreign States and Regions") + ylab("% Answered Important") +
  ggtitle("Foreign Importance Perceptions (April 1995 - March 2015)")
p1

p2<- ggplot(impreldatax, aes(as.factor(id),statecount_w_both_per)) + gktheme +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_boxplot(colour="black",fill="grey80",lwd=0.5)+
  scale_y_continuous(limits=c(0,20),breaks=c(0,5,10,15,20)) +
  scale_x_discrete(labels=c("United States","China","South East Asia",
                            "South Korea","Europe","Russia","North Korea",
                            "Middle/Near East","Taiwan",
                            "Central/South \nAmerica","Africa","Oceania"))+
  xlab("Foreign States and Regions") + ylab("% in All Monthly Headlines (Words)") +
  ggtitle("Monthly Total Foreign News Coverage (April 1995 - March 2015)")
p2

## Persuasion ##

time<-rep(seq(1:329),4)
ldldata<-data.frame(time)
ldldata$id<-c(rep(1,329),rep(2,329),rep(3,329),rep(4,329))
ldldata$ldl<-c(usmonth$ldlstate,
               chnmonth$ldlstate,
               skormonth$ldlstate,
               nkormonth$ldlstate)

p3<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=usmonth,aes(time,ldlstate,color="1"),lwd=1,linetype=1)+
  geom_line(data=chnmonth,aes(time,ldlstate,colour="2"),lwd=1,linetype=1)+
  geom_line(data=skormonth,aes(time,ldlstate,colour="3"),lwd=1,linetype=1)+
  geom_line(data=nkormonth,aes(time,ldlstate,colour="4"),lwd=1,linetype=1)+
  scale_y_continuous(limits=c((-100),50),breaks=c(-100,-75,-50,-25,0,25,50))+
  scale_x_continuous(limits=c(1,329),breaks=c(3,27,87,147,207,267,327),
                     labels=c("Jan 1988","Jan 1990","Jan1995","Jan2000",
                              "Jan 2005","Jan 2010","Jan 2015"))+
  xlab("Time") + ylab("% Positive - % Negative") +
  ggtitle("Monthly Foreign Directional Perceptions (Nov. 1987 - March 2015)")+
  scale_colour_manual(name = 'States',
                      values =c("1"='black',"2"='red',"3"='green',"4"='blue'),
                      labels = c('United States','China','South Korea','North Korea'))
p3

########################
## Frequency of Frame ##
########################

p4a<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=usmonth,aes(time,econ_w_both_per2),colour='black',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Economy (United States)")
p4a
p4b<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=usmonth,aes(time,defense_w_both_per2),colour='black',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Defence (United Staes)")
p4b
multiplot(p4a,p4b)

p5a<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=chnmonth,aes(time,econ_w_both_per2),colour='red',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Economy (China)")
p5a
p5b<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=chnmonth,aes(time,defense_w_both_per2),colour='red',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Defence (China)")
p5b
multiplot(p5a,p5b)

p6a<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=skormonth,aes(time,econ_w_both_per2),colour='green',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Economy (S.Korea)")
p6a
p6b<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=skormonth,aes(time,defense_w_both_per2),colour='green',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Defence (S.Korea)")
p6b
multiplot(p6a,p6b)

p7a<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=nkormonth,aes(time,econ_w_both_per2),colour='blue',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Economy (N.Korea)")
p7a
p7b<-ggplot()+gktheme+
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=nkormonth,aes(time,defense_w_both_per2),colour='blue',lwd=1,linetype=1)+
  scale_y_continuous(limits=c(0,16),breaks=c(0,4,8,12,16))+
  scale_x_continuous(limits=c(1,329),breaks=c(27,87,147,207,267,327),
                     labels=c("Jan90","Jan95","Jan00","Jan05","Jan10","Jan15"))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("Defence (N.Korea)")
p7b
multiplot(p7a,p7b)

p4to7<-arrangeGrob(p4a,p4b,p5a,p5b,p6a,p6b,p7a,p7b,ncol=2,nrow=4,
             left="Percent in All Monthly Headlines",
             bottom="Month of the Coverage")
grid.arrange(p4to7)

###########################
## Agenda-setting Effect ##
###########################

stdiv<-sd(usimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-usirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-usirf[[3]]$statecount_w_both_per[,3]/stdiv
px1<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("United States")
px1

stdiv<-sd(chnimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-chnirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-chnirf[[3]]$statecount_w_both_per[,3]/stdiv
px2<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("China")
px2

stdiv<-sd(seasiaimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-seasiairf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-seasiairf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-seasiairf[[3]]$statecount_w_both_per[,3]/stdiv
px3<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("SE Asia")
px3

stdiv<-sd(skorimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-skorirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-skorirf[[3]]$statecount_w_both_per[,3]/stdiv
px4<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("South Korea")
px4

stdiv<-sd(euroimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-euroirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-euroirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-euroirf[[3]]$statecount_w_both_per[,3]/stdiv
px5<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Europe")
px5

stdiv<-sd(rusimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-rusirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-rusirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-rusirf[[3]]$statecount_w_both_per[,3]/stdiv
px6<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Russia")
px6

stdiv<-sd(nkorimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-nkorirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-nkorirf[[3]]$statecount_w_both_per[,3]/stdiv
px7<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("North Korea")
px7

stdiv<-sd(mneastimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-mneastirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-mneastirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-mneastirf[[3]]$statecount_w_both_per[,3]/stdiv
px8<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Mid. Near East")
px8

stdiv<-sd(taiwanimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-taiwanirf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-taiwanirf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-taiwanirf[[3]]$statecount_w_both_per[,3]/stdiv
px9<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Taiwan")
px9

stdiv<-sd(msamericaimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-msamericairf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-msamericairf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-msamericairf[[3]]$statecount_w_both_per[,3]/stdiv
px10<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Mid. South Ame.")
px10

stdiv<-sd(africaimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-africairf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-africairf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-africairf[[3]]$statecount_w_both_per[,3]/stdiv
px11<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Africa")
px11

stdiv<-sd(oceaniaimp$statecount_w_both_per)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-oceaniairf[[1]]$statecount_w_both_per[,3]/stdiv
data$downci<-oceaniairf[[2]]$statecount_w_both_per[,3]/stdiv
data$upci<-oceaniairf[[3]]$statecount_w_both_per[,3]/stdiv
px12<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=2)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=2)+
  scale_y_continuous(limits=c(-1,2),breaks=c(-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) + ggtitle("Oceania")
px12

px<-arrangeGrob(px1,px2,px3,px4,px5,px6,px7,px8,px9,px10,px11,px12,nrow=4,ncol=3,
                 left="Impulse Response of Foreign Importance Perception (%)",
                 bottom="Month from 1 Percent Increase in TC")
grid.arrange(px)

#######################
## Persuasion Effect ##
#######################

## US Positive ##
stdiv<-sd(usldl$statepos_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_ldl[[1]]$statepos_w_both_per2[,4]/stdiv
data$downci<-usirf_ldl[[2]]$statepos_w_both_per2[,4]/stdiv
data$upci<-usirf_ldl[[3]]$statepos_w_both_per2[,4]/stdiv
py1a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (PC)")
py1a

## US Negative ##
stdiv<-sd(usldl$stateneg_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_ldl[[1]]$stateneg_w_both_per2[,4]/stdiv
data$downci<-usirf_ldl[[2]]$stateneg_w_both_per2[,4]/stdiv
data$upci<-usirf_ldl[[3]]$stateneg_w_both_per2[,4]/stdiv
py1b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (NC)")
py1b

## China Positive ##
stdiv<-sd(chnldl$statepos_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_ldl[[1]]$statepos_w_both_per2[,4]/stdiv
data$downci<-chnirf_ldl[[2]]$statepos_w_both_per2[,4]/stdiv
data$upci<-chnirf_ldl[[3]]$statepos_w_both_per2[,4]/stdiv
py2a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (PC)")
py2a

## China Negative ##
stdiv<-sd(chnldl$stateneg_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_ldl[[1]]$stateneg_w_both_per2[,4]/stdiv
data$downci<-chnirf_ldl[[2]]$stateneg_w_both_per2[,4]/stdiv
data$upci<-chnirf_ldl[[3]]$stateneg_w_both_per2[,4]/stdiv
py2b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (NC)")
py2b

## South Korea Positive ##
stdiv<-sd(skorldl$statepos_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_ldl[[1]]$statepos_w_both_per2[,4]/stdiv
data$downci<-skorirf_ldl[[2]]$statepos_w_both_per2[,4]/stdiv
data$upci<-skorirf_ldl[[3]]$statepos_w_both_per2[,4]/stdiv
py3a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("South Korea (PC)")
py3a

## South Korea Negative ##
stdiv<-sd(skorldl$stateneg_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_ldl[[1]]$stateneg_w_both_per2[,4]/stdiv
data$downci<-skorirf_ldl[[2]]$stateneg_w_both_per2[,4]/stdiv
data$upci<-skorirf_ldl[[3]]$stateneg_w_both_per2[,4]/stdiv
py3b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("South Korea (NC)")
py3b
multiplot(py3a,py3b)

## North Korea Positive ##
stdiv<-sd(nkorldl$statepos_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_ldl[[1]]$statepos_w_both_per2[,4]/stdiv
data$downci<-nkorirf_ldl[[2]]$statepos_w_both_per2[,4]/stdiv
data$upci<-nkorirf_ldl[[3]]$statepos_w_both_per2[,4]/stdiv
py4a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("North Korea (PC)")
py4a

## North Korea Negative ##
stdiv<-sd(nkorldl$stateneg_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_ldl[[1]]$stateneg_w_both_per2[,4]/stdiv
data$downci<-nkorirf_ldl[[2]]$stateneg_w_both_per2[,4]/stdiv
data$upci<-nkorirf_ldl[[3]]$stateneg_w_both_per2[,4]/stdiv
py4b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,2),breaks=c(-3,-2,-1,0,1,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("North Korea (NC)")
py4b
multiplot(py4a,py4b)

py<-arrangeGrob(py1a,py1b,py2a,py2b,py3a,py3b,py4a,py4b,nrow=4,ncol=2,
                 left="Impulse Response of Foreign Favorability Perception (%)",
                 bottom="Month from 1 Percent Increase in Directional News Coverage")
grid.arrange(py)

##############################
## Framing * Agenda-Setting ##
##############################

## United States Economy Frame ##
stdiv<-sd(usimp_frame$econ_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_frame[[1]]$econ_w_both_per2[,4]/stdiv
data$downci<-usirf_frame[[2]]$econ_w_both_per2[,4]/stdiv
data$upci<-usirf_frame[[3]]$econ_w_both_per2[,4]/stdiv
pz1a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (Economy)")
pz1a

## United States Defense Frame ##
stdiv<-sd(usimp_frame$defense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_frame[[1]]$defense_w_both_per2[,4]/stdiv
data$downci<-usirf_frame[[2]]$defense_w_both_per2[,4]/stdiv
data$upci<-usirf_frame[[3]]$defense_w_both_per2[,4]/stdiv
pz1b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (Defense))")
pz1b

## China Economy Frame ##
stdiv<-sd(chnimp_frame$econ_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_frame[[1]]$econ_w_both_per2[,4]/stdiv
data$downci<-chnirf_frame[[2]]$econ_w_both_per2[,4]/stdiv
data$upci<-chnirf_frame[[3]]$econ_w_both_per2[,4]/stdiv
pz2a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (Economy))")
pz2a

## China Defense Frame ##
stdiv<-sd(chnimp_frame$defense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_frame[[1]]$defense_w_both_per2[,4]/stdiv
data$downci<-chnirf_frame[[2]]$defense_w_both_per2[,4]/stdiv
data$upci<-chnirf_frame[[3]]$defense_w_both_per2[,4]/stdiv
pz2b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (Defense)")
pz2b

## South Korea Economy Frame ##
stdiv<-sd(skorimp_frame$econ_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_frame[[1]]$econ_w_both_per2[,4]/stdiv
data$downci<-skorirf_frame[[2]]$econ_w_both_per2[,4]/stdiv
data$upci<-skorirf_frame[[3]]$econ_w_both_per2[,4]/stdiv
pz3a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("S.Korea (Economy)")
pz3a

## South Korea Defense Frame ##
stdiv<-sd(skorimp_frame$defense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_frame[[1]]$defense_w_both_per2[,4]/stdiv
data$downci<-skorirf_frame[[2]]$defense_w_both_per2[,4]/stdiv
data$upci<-skorirf_frame[[3]]$defense_w_both_per2[,4]/stdiv
pz3b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.1),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("S.Korea (Defense)")
pz3b

## North Korea Economy Frame ##
stdiv<-sd(nkorimp_frame$econ_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_frame[[1]]$econ_w_both_per2[,4]/stdiv
data$downci<-nkorirf_frame[[2]]$econ_w_both_per2[,4]/stdiv
data$upci<-nkorirf_frame[[3]]$econ_w_both_per2[,4]/stdiv
pz4a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("N.Korea (Economy)")
pz4a

## North Korea Defense Frame ##
stdiv<-sd(nkorimp_frame$defense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_frame[[1]]$defense_w_both_per2[,4]/stdiv
data$downci<-nkorirf_frame[[2]]$defense_w_both_per2[,4]/stdiv
data$upci<-nkorirf_frame[[3]]$defense_w_both_per2[,4]/stdiv
pz4b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,5.2),breaks=c(-3,-1.5,0,1.5,3,4.5))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("N.Korea (Defense)")
pz4b

pz<-arrangeGrob(pz1a,pz1b,pz2a,pz2b,pz3a,pz3b,pz4a,pz4b,nrow=4,ncol=2,
                left="Impulse Response of Foreign Importance Perception (%)",
                bottom="Month from 1 Percent Increase in Framed News Coverage")
grid.arrange(pz)

##########################
## Framing * Persuasion ##
##########################

## United States Negative Economy Frame ##
stdiv<-sd(usldl_frame$negecon_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_ldl_frame[[1]]$negecon_w_both_per2[,4]/stdiv
data$downci<-usirf_ldl_frame[[2]]$negecon_w_both_per2[,4]/stdiv
data$upci<-usirf_ldl_frame[[3]]$negecon_w_both_per2[,4]/stdiv
pzz1a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-1.5,0,1.5,3))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (Economy)")
pzz1a

## United States Negative Defense Frame ##
stdiv<-sd(usldl_frame$negdefense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-usirf_ldl_frame[[1]]$negdefense_w_both_per2[,4]/stdiv
data$downci<-usirf_ldl_frame[[2]]$negdefense_w_both_per2[,4]/stdiv
data$upci<-usirf_ldl_frame[[3]]$negdefense_w_both_per2[,4]/stdiv
pzz1b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-1.5,0,1.5,3))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("United States (Defense))")
pzz1b

## China Negative Economy Frame ##
stdiv<-sd(chnldl_frame$negecon_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_ldl_frame[[1]]$negecon_w_both_per2[,4]/stdiv
data$downci<-chnirf_ldl_frame[[2]]$negecon_w_both_per2[,4]/stdiv
data$upci<-chnirf_ldl_frame[[3]]$negecon_w_both_per2[,4]/stdiv
pzz2a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-20,1),breaks=c(-20,-15,-10,5,0))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (Economy))")
pzz2a

## China Negative Defense Frame ##
stdiv<-sd(chnldl_frame$negdefense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-chnirf_ldl_frame[[1]]$negdefense_w_both_per2[,4]/stdiv
data$downci<-chnirf_ldl_frame[[2]]$negdefense_w_both_per2[,4]/stdiv
data$upci<-chnirf_ldl_frame[[3]]$negdefense_w_both_per2[,4]/stdiv
pzz2b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-20,1),breaks=c(-20,-15,-10,5,0))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("China (Defense)")
pzz2b

## South Korea Negative Economy Frame ##
stdiv<-sd(skorldl_frame$negecon_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_ldl_frame[[1]]$negecon_w_both_per2[,4]/stdiv
data$downci<-skorirf_ldl_frame[[2]]$negecon_w_both_per2[,4]/stdiv
data$upci<-skorirf_ldl_frame[[3]]$negecon_w_both_per2[,4]/stdiv
pzz3a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-18,12),breaks=c(-15,-10,-5,0,5,10))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("S.Korea (Economy)")
pzz3a

## South Korea Negative Defense Frame ##
stdiv<-sd(skorldl_frame$negdefense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-skorirf_ldl_frame[[1]]$negdefense_w_both_per2[,4]/stdiv
data$downci<-skorirf_ldl_frame[[2]]$negdefense_w_both_per2[,4]/stdiv
data$upci<-skorirf_ldl_frame[[3]]$negdefense_w_both_per2[,4]/stdiv
pzz3b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-18,12),breaks=c(-15,-10,-5,0,5,10))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("S.Korea (Defense)")
pzz3b

## North Korea Negative Economy Frame ##
stdiv<-sd(nkorldl_frame$negecon_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_ldl_frame[[1]]$negecon_w_both_per2[,4]/stdiv
data$downci<-nkorirf_ldl_frame[[2]]$negecon_w_both_per2[,4]/stdiv
data$upci<-nkorirf_ldl_frame[[3]]$negecon_w_both_per2[,4]/stdiv
pzz4a<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-5,2),breaks=c(-4,-2,0,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("N.Korea (Economy)")
pzz4a

## North Korea Negative Defense Frame ##
stdiv<-sd(nkorldl_frame$negdefense_w_both_per2)
time<-seq(0:12)
data<-data.frame(time)
data$time<-data$time-1
data$impulse<-nkorirf_ldl_frame[[1]]$negdefense_w_both_per2[,4]/stdiv
data$downci<-nkorirf_ldl_frame[[2]]$negdefense_w_both_per2[,4]/stdiv
data$upci<-nkorirf_ldl_frame[[3]]$negdefense_w_both_per2[,4]/stdiv
pzz4b<-ggplot()+gktheme+
  geom_abline(slope=0,intercept=0)+
  geom_line(data=data,aes(x=time,y=impulse),colour='black',lwd=1,linetype=1)+
  #geom_ribbon(data=data,aes(x=time, ymin=downci, ymax=upci), alpha=0.2)+
  geom_line(data=data,aes(x=time,y=downci),colour='black',lwd=1,linetype=5)+
  geom_line(data=data,aes(x=time,y=upci),colour='black',lwd=1,linetype=5)+
  scale_y_continuous(limits=c(-5,2),breaks=c(-4,-2,0,2))+
  scale_x_continuous(limits=c(0,12),breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  xlab(NULL) + ylab(NULL) +
  ggtitle("N.Korea (Defense)")
pzz4b

pzz<-arrangeGrob(pzz1a,pzz1b,pzz2a,pzz2b,pzz3a,pzz3b,pzz4a,pzz4b,nrow=4,ncol=2,
                left="Impulse Response of Foreign Favorability Perception (%)",
                bottom="Month from 1 Percent Increase in Framed Negative News Coverage")
grid.arrange(pzz)

#######################
## Store IRF Results ##
#######################

impirf<-matrix(nrow=12*3,ncol=13,
       rbind(usirf[[1]]$statecount_w_both_per[,3],
         usirf[[2]]$statecount_w_both_per[,3],
         usirf[[3]]$statecount_w_both_per[,3],
         chnirf[[1]]$statecount_w_both_per[,3],
         chnirf[[2]]$statecount_w_both_per[,3],
         chnirf[[3]]$statecount_w_both_per[,3],
         seasiairf[[1]]$statecount_w_both_per[,3],
         seasiairf[[2]]$statecount_w_both_per[,3],
         seasiairf[[3]]$statecount_w_both_per[,3],
         skorirf[[1]]$statecount_w_both_per[,3],
         skorirf[[2]]$statecount_w_both_per[,3],
         skorirf[[3]]$statecount_w_both_per[,3],
         euroirf[[1]]$statecount_w_both_per[,3],
         euroirf[[2]]$statecount_w_both_per[,3],
         euroirf[[3]]$statecount_w_both_per[,3],
         rusirf[[1]]$statecount_w_both_per[,3],
         rusirf[[2]]$statecount_w_both_per[,3],
         rusirf[[3]]$statecount_w_both_per[,3],
         nkorirf[[1]]$statecount_w_both_per[,3],
         nkorirf[[2]]$statecount_w_both_per[,3],
         nkorirf[[3]]$statecount_w_both_per[,3],
         mneastirf[[1]]$statecount_w_both_per[,3],
         mneastirf[[2]]$statecount_w_both_per[,3],
         mneastirf[[3]]$statecount_w_both_per[,3],
         taiwanirf[[1]]$statecount_w_both_per[,3],
         taiwanirf[[2]]$statecount_w_both_per[,3],
         taiwanirf[[3]]$statecount_w_both_per[,3],
         msamericairf[[1]]$statecount_w_both_per[,3],
         msamericairf[[2]]$statecount_w_both_per[,3],
         msamericairf[[3]]$statecount_w_both_per[,3],
         africairf[[1]]$statecount_w_both_per[,3],
         africairf[[2]]$statecount_w_both_per[,3],
         africairf[[3]]$statecount_w_both_per[,3],
         oceaniairf[[1]]$statecount_w_both_per[,3],
         oceaniairf[[2]]$statecount_w_both_per[,3],
         oceaniairf[[3]]$statecount_w_both_per[,3]))
namestate<-c("USA", "", "",
             "China", "", "",
             "S.E.Asia", "", "",
             "S.Korea", "", "",
             "Europe", "", "",
             "Russia", "", "",
             "N.Korea", "", "",
             "M.N.East", "", "",
             "Taiwan", "", "",
             "M.S.Ame.", "", "",
             "Africa", "", "",
             "Oceania", "", "" )
komoku<-rep(c("Response","Lo95CI","Up95CI"),12)


impirftable<-data.frame(namestate,komoku,impirf)
colnames(impirftable)<-c("Country","",seq(0,12))

## Export Table-as-Figure ##
print(xtable(impirftable, digits = 1,
             caption="IRF Analysis Results Table (Agenda-Setting)"),
      caption.placement = "top",booktab=TRUE,
      include.rownames=FALSE)

#################
## Save Result ##
#################

save.image("./outputs/Figure_TimeSeries_170427.RData")
