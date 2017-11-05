
#################################################################################
## File Name: v3_Data2_HeadlineWrdMat.R                                        ##
## Creation Date: 4 Nov 2016                                                   ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Conduct Isomorphic Analysis and Create RMeCab Dataset              ##
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
library(RMeCab); library(rprojroot); library(xtable)

## Set Working Directory (Automatically or Manually) ##
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd(../) #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project")

#########################
## Data Import & Merge ##
#########################

# Read Headline Level Data
hldata<-read.csv("data/allheadline.csv",fileEncoding = "CP932")

# Read Coding Data
sample_selection <- read.csv("data/sample_selection.csv",fileEncoding = "CP932")
coding_data <- read.csv("data/trainingcode.csv",fileEncoding = "CP932")

ls()

##################################################
## Merge Original Coding Data into Full Dataset ##
##################################################

#### 1. Use the Proportion Within Coders (with Fixed Data) ####

## Create Mean Coding (Negative/Positive Probability)
uscode_neg <- as.data.frame((coding_data[,grep("_us",names(coding_data))]==1)*1)
chncode_neg <- as.data.frame((coding_data[,grep("_chn",names(coding_data))]==1)*1)
skocode_neg <- as.data.frame((coding_data[,grep("_sko",names(coding_data))]==1)*1)
nkocode_neg <- as.data.frame((coding_data[,grep("_nko",names(coding_data))]==1)*1)
uscode_pos <- as.data.frame((coding_data[,grep("_us",names(coding_data))]==3)*1)
chncode_pos <- as.data.frame((coding_data[,grep("_chn",names(coding_data))]==3)*1)
skocode_pos <- as.data.frame((coding_data[,grep("_sko",names(coding_data))]==3)*1)
nkocode_pos <- as.data.frame((coding_data[,grep("_nko",names(coding_data))]==3)*1)

coding_data$prneg_us <- NA
coding_data$prneg_chn <- NA
coding_data$prneg_sko <- NA
coding_data$prneg_nko <- NA
coding_data$prpos_us <- NA
coding_data$prpos_chn <- NA
coding_data$prpos_sko <- NA
coding_data$prpos_nko <- NA
for (i in 1:1000){
  coding_data$prneg_us[i] <- mean(as.numeric(uscode_neg[i,]))
  coding_data$prneg_chn[i] <- mean(as.numeric(chncode_neg[i,]))
  coding_data$prneg_sko[i] <- mean(as.numeric(skocode_neg[i,]))
  coding_data$prneg_nko[i] <- mean(as.numeric(nkocode_neg[i,]))
  coding_data$prpos_us[i] <- mean(as.numeric(uscode_pos[i,]))
  coding_data$prpos_chn[i] <- mean(as.numeric(chncode_pos[i,]))
  coding_data$prpos_sko[i] <- mean(as.numeric(skocode_pos[i,]))
  coding_data$prpos_nko[i] <- mean(as.numeric(nkocode_pos[i,]))
}

## Set rows for the sampling population
samplepoprows <- which(hldata$id_all %in% sample_selection$id_all)

## Merge Coding Values into Full Dataset
hldata$prneg_us <- NA
hldata$prneg_chn <- NA
hldata$prneg_sko <- NA
hldata$prneg_nko <- NA
hldata$prpos_us <- NA
hldata$prpos_chn <- NA
hldata$prpos_sko <- NA
hldata$prpos_nko <- NA

hldata[samplepoprows,]$prneg_us[order(sample_selection$US_SID)][1:1000] <- 
  coding_data$prneg_us[order(coding_data$id)]
hldata[samplepoprows,]$prneg_chn[order(sample_selection$CHN_SID)][1:1000] <- 
  coding_data$prneg_chn[order(coding_data$id)]
hldata[samplepoprows,]$prneg_sko[order(sample_selection$KOR_SID)][1:1000] <- 
  coding_data$prneg_sko[order(coding_data$id)]
hldata[samplepoprows,]$prneg_nko[order(sample_selection$NKOR_SID)][1:1000] <- 
  coding_data$prneg_nko[order(coding_data$id)]
hldata[samplepoprows,]$prpos_us[order(sample_selection$US_SID)][1:1000] <- 
  coding_data$prpos_us[order(coding_data$id)]
hldata[samplepoprows,]$prpos_chn[order(sample_selection$CHN_SID)][1:1000] <- 
  coding_data$prpos_chn[order(coding_data$id)]
hldata[samplepoprows,]$prpos_sko[order(sample_selection$KOR_SID)][1:1000] <- 
  coding_data$prpos_sko[order(coding_data$id)]
hldata[samplepoprows,]$prpos_nko[order(sample_selection$NKOR_SID)][1:1000] <- 
  coding_data$prpos_nko[order(coding_data$id)]

hldata$train_us <-  (!is.na(hldata$prneg_us))*1
hldata$train_chn <-  (!is.na(hldata$prneg_chn))*1
hldata$train_sko <-  (!is.na(hldata$prneg_sko))*1
hldata$train_nko <-  (!is.na(hldata$prneg_nko))*1

hldata$cdneg_us <- (hldata$prneg_us >= 0.5)*1
hldata$cdneg_chn <- (hldata$prneg_chn >= 0.5)*1
hldata$cdneg_sko <- (hldata$prneg_sko >= 0.5)*1
hldata$cdneg_nko <- (hldata$prneg_nko >= 0.5)*1

hldata$cdpos_us <- (hldata$prpos_us >= 0.5)*1
hldata$cdpos_chn <- (hldata$prpos_chn >= 0.5)*1
hldata$cdpos_sko <- (hldata$prpos_sko >= 0.5)*1
hldata$cdpos_nko <- (hldata$prpos_nko >= 0.5)*1


tx <- rbind(t(table(hldata$cdneg_us)),
            t(table(hldata$cdneg_chn)),
            t(table(hldata$cdneg_sko)),
            t(table(hldata$cdneg_nko)),
            t(table(hldata$cdpos_us)),
            t(table(hldata$cdpos_chn)),
            t(table(hldata$cdpos_sko)),
            t(table(hldata$cdpos_nko)))
row.names(tx) <- c("US (Negative)","China (Negative)","South Korea (Negative)","North Korea (Negative)",
                   "US (Positive)","China (Positive)","South Korea (Positive)","North Korea (Positive)")
xtable(tx)

#### 2. Use the Consistent Coding Scheme (Stata File) ####

## Set rows for the sampling population
samplepoprows <- which(hldata$id_all %in% sample_selection$id_all)

## Create Variables
hldata$cdneg_us2 <- NA
hldata$cdneg_chn2 <- NA
hldata$cdneg_sko2 <- NA
hldata$cdneg_nko2 <- NA
hldata$cdpos_us2 <- NA
hldata$cdpos_chn2 <- NA
hldata$cdpos_sko2 <- NA
hldata$cdpos_nko2 <- NA

## Merge Data
hldata[samplepoprows,]$cdneg_us2[order(sample_selection$US_SID)][1:1000] <- 
  (coding_data$us_final[order(coding_data$id)]==1)*1
hldata[samplepoprows,]$cdneg_chn2[order(sample_selection$CHN_SID)][1:1000] <- 
  (coding_data$chn_final[order(coding_data$id)]==1)*1
hldata[samplepoprows,]$cdneg_sko2[order(sample_selection$KOR_SID)][1:1000] <- 
  (coding_data$sko_final[order(coding_data$id)]==1)*1
hldata[samplepoprows,]$cdneg_nko2[order(sample_selection$NKOR_SID)][1:1000] <- 
  (coding_data$nko_final[order(coding_data$id)]==1)*1
hldata[samplepoprows,]$cdpos_us2[order(sample_selection$US_SID)][1:1000] <- 
  (coding_data$us_final[order(coding_data$id)]==3)*1
hldata[samplepoprows,]$cdpos_chn2[order(sample_selection$CHN_SID)][1:1000] <- 
  (coding_data$chn_final[order(coding_data$id)]==3)*1
hldata[samplepoprows,]$cdpos_sko2[order(sample_selection$KOR_SID)][1:1000] <- 
  (coding_data$sko_final[order(coding_data$id)]==3)*1
hldata[samplepoprows,]$cdpos_nko2[order(sample_selection$NKOR_SID)][1:1000] <- 
  (coding_data$nko_final[order(coding_data$id)]==3)*1


tx <- rbind(t(table(hldata$cdneg_us2)),
            t(table(hldata$cdneg_chn2)),
            t(table(hldata$cdneg_sko2)),
            t(table(hldata$cdneg_nko2)),
            t(table(hldata$cdpos_us2)),
            t(table(hldata$cdpos_chn2)),
            t(table(hldata$cdpos_sko2)),
            t(table(hldata$cdpos_nko2)))
row.names(tx) <- c("US (Negative)","China (Negative)","South Korea (Negative)","North Korea (Negative)",
                   "US (Positive)","China (Positive)","South Korea (Positive)","North Korea (Positive)")
xtable(tx)


#########################################
## Create New Variables & Data Subsets ##
#########################################

## Date Data Add Variables ##
hldata$Asahi[is.na(hldata$Asahi)]<-0
hldata$Yomiuri[is.na(hldata$Yomiuri)]<-0
hldata$Both <- hldata$Asahi*(4/9)+hldata$Yomiuri*(5/9) ## Headline Count
hldata$Asahi_w <- hldata$Asahi*hldata$wcount # Word count for Asahi
hldata$Yomiuri_w <- hldata$Yomiuri*hldata$wcount # Word count Yomiuri
hldata$Both_w <- hldata$Asahi_w*(4/9)+hldata$Yomiuri_w*(5/9)
## Note that Weighting in "Both" variable is made on the fact that 
## Actual circulation of two newspapers are roughly 4:5

# Create Subset Data
usdata<-subset(hldata, us==1)
chndata<-subset(hldata, chn==1)
skordata<-subset(hldata, kor==1)
nkordata<-subset(hldata, nkor==1)


#########################
## Isomorphic Analysis ##
#########################

# Conduct Isomorphic Analysis (Use User Dictionary for user-defined words)
MecabRes<-RMeCabDF(dataf=hldata,coln="Headline",
                   mypref=1,
                   dic="./codes/userdictionary/user.dic")
# Results by Country 
usMecabRes <- MecabRes[which(hldata$us==1)]
chnMecabRes<- MecabRes[which(hldata$chn==1)]
skorMecabRes<- MecabRes[which(hldata$kor==1)]
nkorMecabRes<- MecabRes[which(hldata$nkor==1)]


usMecabRes[[1]]

# Create Word Matrix by Country
createWrdMat <- function(dt) docMatrixDF(dt[,"Headline"], 
                                         pos = c("名詞","形容詞","動詞","助動詞","接頭詞","副詞"), 
                                         #"助動詞","助詞","接続詞","接頭詞","副詞","感動詞","記号", "フィラー", "その他"
                                         dic="./codes/userdictionary/user.dic",
                                         minFreq = 2)
usWrdMat <- as.data.frame(t(createWrdMat(usdata)))
chnWrdMat <- as.data.frame(t(createWrdMat(chndata)))
skorWrdMat <- as.data.frame(t(createWrdMat(skordata)))
nkorWrdMat <- as.data.frame(t(createWrdMat(nkordata)))


head(usWrdMat)

################
## Save Image ##
################

#load("data_heavy/v3_Data_HeadlineWrdMat.Rdata")

save.image("data/v3_Data2_HeadlineWrdMat.Rdata")

## Save Country Subsets
write.csv(usdata,"data/usdata.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(chndata,"data/chndata.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(skordata,"data/skordata.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(nkordata,"data/nkordata.csv", fileEncoding = "CP932",row.names=FALSE)

## Save Word Appearance Matrix
write.csv(usWrdMat,"data/usWrdMat.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(chnWrdMat,"data/chnWrdMat.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(skorWrdMat,"data/skorWrdMat.csv", fileEncoding = "CP932",row.names=FALSE)
write.csv(nkorWrdMat,"data/nkorWrdMat.csv", fileEncoding = "CP932",row.names=FALSE)

