
#################################################################################
## File Name: v3_Data1_ManualCodingTests.R                                     ##
## Creation Date: 4 Nov 2017                                                   ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Test Reliability of Manual Coding and Export Final Coding Dataset  ##
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
library(rprojroot); library(readstata13);library(xtable);library(irr)

## Set Working Directory (Automatically or Manually) ##
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd(../) #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project")


#################
## Import Data ##
#################

# Read Manual Coding Data
mc <- read.csv("data/manual_codes.csv")

head(mc)

#############################
## Missing Recoded Dataset ##
#############################

# Copy Data
mc1 <- mc

# Add 1 to Count Scales of Neutral if coding==8 
for (i in grep("_us",names(mc))) {
  mc1[mc1[,i]==8,]$us2count <- mc1[mc1[,i]==8,]$us2count+1
}
for (i in grep("_chn",names(mc))) {
  mc1[mc1[,i]==8,]$chn2count <- mc1[mc1[,i]==8,]$chn2count+1
}
for (i in grep("_sko",names(mc))) {
  mc1[mc1[,i]==8,]$sko2count <- mc1[mc1[,i]==8,]$sko2count+1
}
for (i in grep("_nko",names(mc))) {
  mc1[mc1[,i]==8,]$nko2count <- mc1[mc1[,i]==8,]$nko2count+1
}

# 8 to Neutral
for (i in c(grep("_us",names(mc)),grep("_chn",names(mc)),
            grep("_sko",names(mc)),grep("_nko",names(mc)))) {
  mc1[mc1[,i]==8,i] <- 2
}

# 9 to Missing
for (i in c(grep("_us",names(mc)),grep("_chn",names(mc)),
           grep("_sko",names(mc)),grep("_nko",names(mc)))) {
  mc1[mc1[,i]==9,i] <- NA
}

# Missing if Any other codes of the same country is missing
for (i in grep("_us",names(mc))) {
  mc1[is.na(mc1[,i]),grep("_us",names(mc))] <- rep(NA,4)
}
for (i in grep("_chn",names(mc))) {
  mc1[is.na(mc1[,i]),grep("_chn",names(mc))] <- rep(NA,4)
}
for (i in grep("_sko",names(mc))) {
  mc1[is.na(mc1[,i]),grep("_sko",names(mc))] <- rep(NA,4)
}
for (i in grep("_nko",names(mc))) {
  mc1[is.na(mc1[,i]),grep("_nko",names(mc))] <- rep(NA,4)
}

head(mc1)

#########################################################
## Overy Extreme Coding to Neutral (Triple 2 to All 2) ##
#########################################################

# Copy the Previous Data
mc2 <- mc1

## Replace Overly Extreme Code to Neutral
mc2[mc1$us2count==3,grep("_us",names(mc))] <- rep(2,4)
mc2[mc1$chn2count==3,grep("_chn",names(mc))] <- rep(2,4)
mc2[mc1$sko2count==3,grep("_sko",names(mc))] <- rep(2,4)
mc2[mc1$nko2count==3,grep("_nko",names(mc))] <- rep(2,4)

## Put NA Values Back
mc2[is.na(mc1$c1_us),grep("_us",names(mc))] <- rep(NA,4)
mc2[is.na(mc1$c3_chn),grep("_chn",names(mc))] <- rep(NA,4)
mc2[is.na(mc1$c1_sko),grep("_sko",names(mc))] <- rep(NA,4)
mc2[is.na(mc1$c2_nko),grep("_nko",names(mc))] <- rep(NA,4)

head(mc2)

##############################################
## Overy Neutral Coding to Directional Code ##
##############################################

# Copy Data
mc3 <- mc2

## Replace Overly Neutral Code to Directional
mc3[mc1$us1count==3,grep("_us",names(mc))] <- rep(1,4)
mc3[mc1$chn1count==3,grep("_chn",names(mc))] <- rep(1,4)
mc3[mc1$sko1count==3,grep("_sko",names(mc))] <- rep(1,4)
mc3[mc1$nko1count==3,grep("_nko",names(mc))] <- rep(1,4)
mc3[mc1$us3count==3,grep("_us",names(mc))] <- rep(3,4)
mc3[mc1$chn3count==3,grep("_chn",names(mc))] <- rep(3,4)
mc3[mc1$sko3count==3,grep("_sko",names(mc))] <- rep(3,4)
mc3[mc1$nko3count==3,grep("_nko",names(mc))] <- rep(3,4)

## Put NA Values Back
mc3[is.na(mc1$c1_us),grep("_us",names(mc))] <- rep(NA,4)
mc3[is.na(mc1$c3_chn),grep("_chn",names(mc))] <- rep(NA,4)
mc3[is.na(mc1$c1_sko),grep("_sko",names(mc))] <- rep(NA,4)
mc3[is.na(mc1$c2_nko),grep("_nko",names(mc))] <- rep(NA,4)

head(mc3)

#########################
## Krippendorf's Alpha ##
#########################

## Missing Recoded Data
ka1us <- kripp.alpha(t(mc1[,grep("_us",names(mc1))]),"ordinal")
ka1chn <- kripp.alpha(t(mc1[,grep("_chn",names(mc1))]),"ordinal")
ka1sko <- kripp.alpha(t(mc1[,grep("_sko",names(mc1))]),"ordinal")
ka1nko <- kripp.alpha(t(mc1[,grep("_nko",names(mc1))]),"ordinal")
ka1 <- c(ka1us$value,ka1chn$value,ka1sko$value,ka1nko$value)

## Overly Extreme to Neutral
ka2us <- kripp.alpha(t(mc2[,grep("_us",names(mc2))]),"ordinal")
ka2chn <- kripp.alpha(t(mc2[,grep("_chn",names(mc2))]),"ordinal")
ka2sko <- kripp.alpha(t(mc2[,grep("_sko",names(mc2))]),"ordinal")
ka2nko <- kripp.alpha(t(mc2[,grep("_nko",names(mc2))]),"ordinal")
ka2 <- c(ka2us$value,ka2chn$value,ka2sko$value,ka2nko$value)

## Overly Neutral to Directional
ka3us <- kripp.alpha(t(mc3[,grep("_us",names(mc3))]),"ordinal")
ka3chn <- kripp.alpha(t(mc3[,grep("_chn",names(mc3))]),"ordinal")
ka3sko <- kripp.alpha(t(mc3[,grep("_sko",names(mc3))]),"ordinal")
ka3nko <- kripp.alpha(t(mc3[,grep("_nko",names(mc3))]),"ordinal")
ka3 <- c(ka3us$value,ka3chn$value,ka3sko$value,ka3nko$value)

## Summary Table
katab <- rbind(ka1,ka2,ka3)
rownames(katab) <- c("Original Coding","Overly Directional Codes Recoded",
                      "Overly Neutral Codes Recoded")
colnames(katab) <- c("US","China","S.Korea","N.Korea")
round(katab,3)

########################################################################
## Generate Training Codes by Consistent & Conservative Coding Scheme ##
########################################################################

# Copy Data
mc4 <- mc1

## Consistent Coding Scheme ##

# US Coding
mc4$us_final <- 999
mc4[is.na(mc1$c1_us),c("us1count","us2count","us3count")] <- NA
mc4$us_final[which(mc4$us1count>=3)] <- 1
mc4$us_final[which(mc4$us2count>=3)] <- 2
mc4$us_final[which(mc4$us3count>=3)] <- 3
mc4$us_final[which(mc4$us1count==2 & mc4$us2count==2)] <- 1
mc4$us_final[which(mc4$us2count==2 & mc4$us3count==2)] <- 3
mc4$us_final[which(mc4$us1count==2 & mc4$us3count==2)] <- 2
mc4$us_final[which(mc4$us1count==1 & mc4$us2count==2 & mc4$us3count==1)] <- 2
mc4$us_final[which(mc4$us1count==2 & mc4$us2count==1 & mc4$us3count==1)] <- 2
mc4$us_final[which(mc4$us1count==1 & mc4$us2count==1 & mc4$us3count==2)] <- 2
mc4$us_final[which(mc4$us1count==1 & mc4$us2count==1 & mc4$us3count==2)] <- 2
mc4$us_final[is.na(mc4$us1count)] <- NA
usfin <- t(table(mc4$us_final,useNA="always"))

# China Coding
mc4$chn_final <- 999
mc4[is.na(mc1$c3_chn),c("chn1count","chn2count","chn3count")] <- NA
mc4$chn_final[which(mc4$chn1count>=3)] <- 1
mc4$chn_final[which(mc4$chn2count>=3)] <- 2
mc4$chn_final[which(mc4$chn3count>=3)] <- 3
mc4$chn_final[which(mc4$chn1count==2 & mc4$chn2count==2)] <- 1
mc4$chn_final[which(mc4$chn2count==2 & mc4$chn3count==2)] <- 3
mc4$chn_final[which(mc4$chn1count==2 & mc4$chn3count==2)] <- 2
mc4$chn_final[which(mc4$chn1count==1 & mc4$chn2count==2 & mc4$chn3count==1)] <- 2
mc4$chn_final[which(mc4$chn1count==2 & mc4$chn2count==1 & mc4$chn3count==1)] <- 2
mc4$chn_final[which(mc4$chn1count==1 & mc4$chn2count==1 & mc4$chn3count==2)] <- 2
mc4$chn_final[which(mc4$chn1count==1 & mc4$chn2count==1 & mc4$chn3count==2)] <- 2
mc4$chn_final[is.na(mc4$chn1count)] <- NA
chnfin <- t(table(mc4$chn_final,useNA="always"))

# South Korea Coding
mc4$sko_final <- 999
mc4[is.na(mc1$c1_sko),c("sko1count","sko2count","sko3count")] <- NA
mc4$sko_final[which(mc4$sko1count>=3)] <- 1
mc4$sko_final[which(mc4$sko2count>=3)] <- 2
mc4$sko_final[which(mc4$sko3count>=3)] <- 3
mc4$sko_final[which(mc4$sko1count==2 & mc4$sko2count==2)] <- 1
mc4$sko_final[which(mc4$sko2count==2 & mc4$sko3count==2)] <- 3
mc4$sko_final[which(mc4$sko1count==2 & mc4$sko3count==2)] <- 2
mc4$sko_final[which(mc4$sko1count==1 & mc4$sko2count==2 & mc4$sko3count==1)] <- 2
mc4$sko_final[which(mc4$sko1count==2 & mc4$sko2count==1 & mc4$sko3count==1)] <- 2
mc4$sko_final[which(mc4$sko1count==1 & mc4$sko2count==1 & mc4$sko3count==2)] <- 2
mc4$sko_final[which(mc4$sko1count==1 & mc4$sko2count==1 & mc4$sko3count==2)] <- 2
mc4$sko_final[is.na(mc4$sko1count)] <- NA
skofin <- t(table(mc4$sko_final,useNA="always"))

# North Korea Coding
mc4$nko_final <- 999
mc4[is.na(mc1$c2_nko),c("nko1count","nko2count","nko3count")] <- NA
mc4$nko_final[which(mc4$nko1count>=3)] <- 1
mc4$nko_final[which(mc4$nko2count>=3)] <- 2
mc4$nko_final[which(mc4$nko3count>=3)] <- 3
mc4$nko_final[which(mc4$nko1count==2 & mc4$nko2count==2)] <- 1
mc4$nko_final[which(mc4$nko2count==2 & mc4$nko3count==2)] <- 3
mc4$nko_final[which(mc4$nko1count==2 & mc4$nko3count==2)] <- 2
mc4$nko_final[which(mc4$nko1count==1 & mc4$nko2count==2 & mc4$nko3count==1)] <- 2
mc4$nko_final[which(mc4$nko1count==2 & mc4$nko2count==1 & mc4$nko3count==1)] <- 2
mc4$nko_final[which(mc4$nko1count==1 & mc4$nko2count==1 & mc4$nko3count==2)] <- 2
mc4$nko_final[which(mc4$nko1count==1 & mc4$nko2count==1 & mc4$nko3count==2)] <- 2
mc4$nko_final[is.na(mc4$nko1count)] <- NA
nkofin <- t(table(mc4$nko_final,useNA="always"))

## Conservative Coding Scheme ##

# US
mc4$us_final2 <- 2
mc4$us_final2[which(mc4$us1count>=3)] <- 1
mc4$us_final2[which(mc4$us3count>=3)] <- 3
mc4$us_final2[is.na(mc4$us1count)] <- NA
usfin2 <- t(table(mc4$us_final2,useNA="always"))

# China
mc4$chn_final2 <- 2
mc4$chn_final2[which(mc4$chn1count>=3)] <- 1
mc4$chn_final2[which(mc4$chn3count>=3)] <- 3
mc4$chn_final2[is.na(mc4$chn1count)] <- NA
chnfin2 <- t(table(mc4$chn_final2,useNA="always"))

# South Korea
mc4$sko_final2 <- 2
mc4$sko_final2[which(mc4$sko1count>=3)] <- 1
mc4$sko_final2[which(mc4$sko3count>=3)] <- 3
mc4$sko_final2[is.na(mc4$sko1count)] <- NA
skofin2 <- t(table(mc4$sko_final2,useNA="always"))

# North Korea
mc4$nko_final2 <- 2
mc4$nko_final2[which(mc4$nko1count>=3)] <- 1
mc4$nko_final2[which(mc4$nko3count>=3)] <- 3
mc4$nko_final2[is.na(mc4$nko1count)] <- NA
nkofin2 <- t(table(mc4$nko_final2,useNA="always"))

fintab <- rbind(usfin,chnfin,skofin,nkofin,
                usfin2,chnfin2,skofin2,nkofin2)
rownames(fintab) <- c("US Code (Consistent)","China Code (Consistent)",
                      "S.Korea Code (Consistent)","N.Korea Code (Consistent)",
                      "US Code (Conservative)","China Code (Conservative)",
                      "S.Korea Code (Conservative)","N.Korea Code (Conservative)")
fintab

###############
## Save Data ##
###############

# All Data
save.image("data/v3_Data1_ManualCodingTests.RData")
# Training Codes Dataset
write.csv(mc4,"data/trainingcode.csv",fileEncoding = "CP932",row.names=FALSE)                    
