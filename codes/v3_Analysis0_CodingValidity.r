
#################################################################################
## File Name: v3_Analysis0_CodingValidity.R                                    ##
## Creation Date: 4 Nov 2017                                                   ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Check the Validity of Machine-Coding                               ##
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
options(warn=-1)
#options(warn=0) # put it back


#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot); library(xtable);library(repr)

## Set Working Directory (Automatically or Manually) ##
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project")

uspred<- read.csv("data/uspred_test.csv")
chnpred<- read.csv("data/chnpred_test.csv")
skopred<- read.csv("data/skopred_test.csv")
nkopred<- read.csv("data/nkopred_test.csv")


###########################################################
## Function to Assess Aggregate Level RF Coding Validity ##
###########################################################

simubysampleRF <- function(dtpred,N,seedset=5689){
dtpred_simu <- data.frame(by10true_neg=rep(NA,N),by10rfp_neg=NA,by10difp_neg=NA,by10rfd_neg=NA,by10difd_neg=NA,
                          by50true_neg=NA,by50rfp_neg=NA,by50difp_neg=NA,by50rfd_neg=NA,by50difd_neg=NA,
                          by100true_neg=NA,by100rfp_neg=NA,by100difp_neg=NA,by100rfd_neg=NA,by100difd_neg=NA,
                          by10true_pos=rep(NA,N),by10rfp_pos=NA,by10difp_pos=NA,by10rfd_pos=NA,by10difd_pos=NA,
                          by50true_pos=NA,by50rfp_pos=NA,by50difp_pos=NA,by50rfd_pos=NA,by50difd_pos=NA,
                          by100true_pos=NA,by100rfp_pos=NA,by100difp_pos=NA,by100rfd_pos=NA,by100difd_pos=NA)
for (i in 1:N){
set.seed(i+seedset)
nrs10 <- sample(seq(1,nrow(dtpred),by=1),10)
nrs50 <- sample(seq(1,nrow(dtpred),by=1),50)
nrs100 <- sample(seq(1,nrow(dtpred),by=1),100)
dtpred_simu$by10true_neg[i] <- mean(dtpred[nrs10,]$y_test_neg)
dtpred_simu$by10rfp_neg[i] <- mean(dtpred[nrs10,]$proby_test_rf_neg)
dtpred_simu$by10rfd_neg[i] <- mean((dtpred[nrs10,]$proby_test_rf_neg>=0.5)*1)
dtpred_simu$by50true_neg[i] <- mean(dtpred[nrs50,]$y_test_neg)
dtpred_simu$by50rfp_neg[i] <- mean(dtpred[nrs50,]$proby_test_rf_neg)
dtpred_simu$by50rfd_neg[i] <- mean((dtpred[nrs50,]$proby_test_rf_neg>=0.5)*1)
dtpred_simu$by100true_neg[i] <- mean(dtpred[nrs100,]$y_test_neg)
dtpred_simu$by100rfp_neg[i] <- mean(dtpred[nrs100,]$proby_test_rf_neg)
dtpred_simu$by100rfd_neg[i] <- mean((dtpred[nrs100,]$proby_test_rf_neg>=0.5)*1)
dtpred_simu$by10true_pos[i] <- mean(dtpred[nrs10,]$y_test_pos)
dtpred_simu$by10rfp_pos[i] <- mean(dtpred[nrs10,]$proby_test_rf_pos)
dtpred_simu$by10rfd_pos[i] <- mean((dtpred[nrs10,]$proby_test_rf_pos>=0.5)*1)
dtpred_simu$by50true_pos[i] <- mean(dtpred[nrs50,]$y_test_pos)
dtpred_simu$by50rfp_pos[i] <- mean(dtpred[nrs50,]$proby_test_rf_pos)
dtpred_simu$by50rfd_pos[i] <- mean((dtpred[nrs50,]$proby_test_rf_pos>=0.5)*1)
dtpred_simu$by100true_pos[i] <- mean(dtpred[nrs100,]$y_test_pos)
dtpred_simu$by100rfp_pos[i] <- mean(dtpred[nrs100,]$proby_test_rf_pos)
dtpred_simu$by100rfd_pos[i] <- mean((dtpred[nrs100,]$proby_test_rf_pos>=0.5)*1)
}

dtpred_simu$by10difp_neg <- dtpred_simu$by10rfp_neg - dtpred_simu$by10true_neg 
dtpred_simu$by50difp_neg <- dtpred_simu$by50rfp_neg - dtpred_simu$by50true_neg 
dtpred_simu$by100difp_neg <- dtpred_simu$by100rfp_neg - dtpred_simu$by100true_neg 
dtpred_simu$by10difp_pos <- dtpred_simu$by10rfp_pos - dtpred_simu$by10true_pos 
dtpred_simu$by50difp_pos <- dtpred_simu$by50rfp_pos - dtpred_simu$by50true_pos 
dtpred_simu$by100difp_pos <- dtpred_simu$by100rfp_pos - dtpred_simu$by100true_pos 

dtpred_simu$by10difd_neg <- dtpred_simu$by10rfd_neg - dtpred_simu$by10true_neg 
dtpred_simu$by50difd_neg <- dtpred_simu$by50rfd_neg - dtpred_simu$by50true_neg 
dtpred_simu$by100difd_neg <- dtpred_simu$by100rfd_neg - dtpred_simu$by100true_neg 
dtpred_simu$by10difd_pos <- dtpred_simu$by10rfd_pos - dtpred_simu$by10true_pos 
dtpred_simu$by50difd_pos <- dtpred_simu$by50rfd_pos - dtpred_simu$by50true_pos 
dtpred_simu$by100difd_pos <- dtpred_simu$by100rfd_pos - dtpred_simu$by100true_pos 

return(dtpred_simu)
}


###############################
## Assess RF Coding Validity ##
###############################

uspred_simuRF <- simubysampleRF(uspred,1000)
chnpred_simuRF <- simubysampleRF(chnpred,1000)
skopred_simuRF <- simubysampleRF(skopred,1000)
nkopred_simuRF <- simubysampleRF(nkopred,1000)


codesimutab <- data.frame(
  "Metric" = NA,
  "Tone" = NA,
  "Country" = NA,
  "by10p" = rep(NA,24), 
  "by10d" = NA,
  "by50p" = NA, 
  "by50d" = NA,
  "by100p" = NA,
  "by100d" = NA)

codesimutab$Metric <- c("Correlation",rep(NA,7),
                        "Av. Distance",rep(NA,7),
                        "Distance SD",rep(NA,7))
codesimutab$Tone <- c("Negative",rep(NA,3),
                      "Positive",rep(NA,3),
                      "Negative",rep(NA,3),
                      "Positive",rep(NA,3),
                      "Negative",rep(NA,3),
                      "Positive",rep(NA,3))
codesimutab$Country <- c(rep(c("US","China","S.Korea","N.Korea"),6))

## Correlation (Negative)
codesimutab[1,-c(1,2,3)] <- 
c(cor(uspred_simuRF$by10true_neg,uspred_simuRF$by10rfp_neg),
  cor(uspred_simuRF$by10true_neg,uspred_simuRF$by10rfd_neg),
  cor(uspred_simuRF$by50true_neg,uspred_simuRF$by50rfp_neg),
  cor(uspred_simuRF$by50true_neg,uspred_simuRF$by50rfd_neg),
  cor(uspred_simuRF$by100true_neg,uspred_simuRF$by100rfp_neg),
  cor(uspred_simuRF$by100true_neg,uspred_simuRF$by100rfd_neg))
codesimutab[2,-c(1,2,3)] <- 
  c(cor(chnpred_simuRF$by10true_neg,chnpred_simuRF$by10rfp_neg),
    cor(chnpred_simuRF$by10true_neg,chnpred_simuRF$by10rfd_neg),
    cor(chnpred_simuRF$by50true_neg,chnpred_simuRF$by50rfp_neg),
    cor(chnpred_simuRF$by50true_neg,chnpred_simuRF$by50rfd_neg),
    cor(chnpred_simuRF$by100true_neg,chnpred_simuRF$by100rfp_neg),
    cor(chnpred_simuRF$by100true_neg,chnpred_simuRF$by100rfd_neg))
codesimutab[3,-c(1,2,3)] <- 
  c(cor(skopred_simuRF$by10true_neg,skopred_simuRF$by10rfp_neg),
    cor(skopred_simuRF$by10true_neg,skopred_simuRF$by10rfd_neg),
    cor(skopred_simuRF$by50true_neg,skopred_simuRF$by50rfp_neg),
    cor(skopred_simuRF$by50true_neg,skopred_simuRF$by50rfd_neg),
    cor(skopred_simuRF$by100true_neg,skopred_simuRF$by100rfp_neg),
    cor(skopred_simuRF$by100true_neg,skopred_simuRF$by100rfd_neg))
codesimutab[4,-c(1,2,3)] <- 
  c(cor(nkopred_simuRF$by10true_neg,nkopred_simuRF$by10rfp_neg),
    cor(nkopred_simuRF$by10true_neg,nkopred_simuRF$by10rfd_neg),
    cor(nkopred_simuRF$by50true_neg,nkopred_simuRF$by50rfp_neg),
    cor(nkopred_simuRF$by50true_neg,nkopred_simuRF$by50rfd_neg),
    cor(nkopred_simuRF$by100true_neg,nkopred_simuRF$by100rfp_neg),
    cor(nkopred_simuRF$by100true_neg,nkopred_simuRF$by100rfd_neg))
## Correlation (Positive)
codesimutab[5,-c(1,2,3)] <- 
  c(cor(uspred_simuRF$by10true_pos,uspred_simuRF$by10rfp_pos),
    cor(uspred_simuRF$by10true_pos,uspred_simuRF$by10rfd_pos),
    cor(uspred_simuRF$by50true_pos,uspred_simuRF$by50rfp_pos),
    cor(uspred_simuRF$by50true_pos,uspred_simuRF$by50rfd_pos),
    cor(uspred_simuRF$by100true_pos,uspred_simuRF$by100rfp_pos),
    cor(uspred_simuRF$by100true_pos,uspred_simuRF$by100rfd_pos))
codesimutab[6,-c(1,2,3)] <- 
  c(cor(chnpred_simuRF$by10true_pos,chnpred_simuRF$by10rfp_pos),
    cor(chnpred_simuRF$by10true_pos,chnpred_simuRF$by10rfd_pos),
    cor(chnpred_simuRF$by50true_pos,chnpred_simuRF$by50rfp_pos),
    cor(chnpred_simuRF$by50true_pos,chnpred_simuRF$by50rfd_pos),
    cor(chnpred_simuRF$by100true_pos,chnpred_simuRF$by100rfp_pos),
    cor(chnpred_simuRF$by100true_pos,chnpred_simuRF$by100rfd_pos))
codesimutab[7,-c(1,2,3)] <- 
  c(cor(skopred_simuRF$by10true_pos,skopred_simuRF$by10rfp_pos),
    cor(skopred_simuRF$by10true_pos,skopred_simuRF$by10rfd_pos),
    cor(skopred_simuRF$by50true_pos,skopred_simuRF$by50rfp_pos),
    cor(skopred_simuRF$by50true_pos,skopred_simuRF$by50rfd_pos),
    cor(skopred_simuRF$by100true_pos,skopred_simuRF$by100rfp_pos),
    cor(skopred_simuRF$by100true_pos,skopred_simuRF$by100rfd_pos))
codesimutab[8,-c(1,2,3)] <- 
  c(cor(nkopred_simuRF$by10true_pos,nkopred_simuRF$by10rfp_pos),
    cor(nkopred_simuRF$by10true_pos,nkopred_simuRF$by10rfd_pos),
    cor(nkopred_simuRF$by50true_pos,nkopred_simuRF$by50rfp_pos),
    cor(nkopred_simuRF$by50true_pos,nkopred_simuRF$by50rfd_pos),
    cor(nkopred_simuRF$by100true_pos,nkopred_simuRF$by100rfp_pos),
    cor(nkopred_simuRF$by100true_pos,nkopred_simuRF$by100rfd_pos))
## Average Distance (Negative)
codesimutab[9,-c(1,2,3)] <-
  c(mean(abs(uspred_simuRF$by10difp_neg)),
    mean(abs(uspred_simuRF$by10difd_neg)),
    mean(abs(uspred_simuRF$by50difp_neg)),
    mean(abs(uspred_simuRF$by50difd_neg)),
    mean(abs(uspred_simuRF$by100difp_neg)),
    mean(abs(uspred_simuRF$by100difd_neg)))
codesimutab[10,-c(1,2,3)] <-
  c(mean(abs(chnpred_simuRF$by10difp_neg)),
    mean(abs(chnpred_simuRF$by10difd_neg)),
    mean(abs(chnpred_simuRF$by50difp_neg)),
    mean(abs(chnpred_simuRF$by50difd_neg)),
    mean(abs(chnpred_simuRF$by100difp_neg)),
    mean(abs(chnpred_simuRF$by100difd_neg)))
codesimutab[11,-c(1,2,3)] <-
  c(mean(abs(skopred_simuRF$by10difp_neg)),
    mean(abs(skopred_simuRF$by10difd_neg)),
    mean(abs(skopred_simuRF$by50difp_neg)),
    mean(abs(skopred_simuRF$by50difd_neg)),
    mean(abs(skopred_simuRF$by100difp_neg)),
    mean(abs(skopred_simuRF$by100difd_neg)))
codesimutab[12,-c(1,2,3)] <-
  c(mean(abs(nkopred_simuRF$by10difp_neg)),
    mean(abs(nkopred_simuRF$by10difd_neg)),
    mean(abs(nkopred_simuRF$by50difp_neg)),
    mean(abs(nkopred_simuRF$by50difd_neg)),
    mean(abs(nkopred_simuRF$by100difp_neg)),
    mean(abs(nkopred_simuRF$by100difd_neg)))
## Average Distance (Positive)
codesimutab[13,-c(1,2,3)] <-
  c(mean(abs(uspred_simuRF$by10difp_pos)),
    mean(abs(uspred_simuRF$by10difd_pos)),
    mean(abs(uspred_simuRF$by50difp_pos)),
    mean(abs(uspred_simuRF$by50difd_pos)),
    mean(abs(uspred_simuRF$by100difp_pos)),
    mean(abs(uspred_simuRF$by100difd_pos)))
codesimutab[14,-c(1,2,3)] <-
  c(mean(abs(chnpred_simuRF$by10difp_pos)),
    mean(abs(chnpred_simuRF$by10difd_pos)),
    mean(abs(chnpred_simuRF$by50difp_pos)),
    mean(abs(chnpred_simuRF$by50difd_pos)),
    mean(abs(chnpred_simuRF$by100difp_pos)),
    mean(abs(chnpred_simuRF$by100difd_pos)))
codesimutab[15,-c(1,2,3)] <-
  c(mean(abs(skopred_simuRF$by10difp_pos)),
    mean(abs(skopred_simuRF$by10difd_pos)),
    mean(abs(skopred_simuRF$by50difp_pos)),
    mean(abs(skopred_simuRF$by50difd_pos)),
    mean(abs(skopred_simuRF$by100difp_pos)),
    mean(abs(skopred_simuRF$by100difd_pos)))
codesimutab[16,-c(1,2,3)] <-
  c(mean(abs(nkopred_simuRF$by10difp_pos)),
    mean(abs(nkopred_simuRF$by10difd_pos)),
    mean(abs(nkopred_simuRF$by50difp_pos)),
    mean(abs(nkopred_simuRF$by50difd_pos)),
    mean(abs(nkopred_simuRF$by100difp_pos)),
    mean(abs(nkopred_simuRF$by100difd_pos)))
## Distance SD (Negative)
codesimutab[17,-c(1,2,3)] <-
  c(sd(abs(uspred_simuRF$by10difp_neg)),
    sd(abs(uspred_simuRF$by10difd_neg)),
    sd(abs(uspred_simuRF$by50difp_neg)),
    sd(abs(uspred_simuRF$by50difd_neg)),
    sd(abs(uspred_simuRF$by100difp_neg)),
    sd(abs(uspred_simuRF$by100difd_neg)))
codesimutab[18,-c(1,2,3)] <-
  c(sd(abs(chnpred_simuRF$by10difp_neg)),
    sd(abs(chnpred_simuRF$by10difd_neg)),
    sd(abs(chnpred_simuRF$by50difp_neg)),
    sd(abs(chnpred_simuRF$by50difd_neg)),
    sd(abs(chnpred_simuRF$by100difp_neg)),
    sd(abs(chnpred_simuRF$by100difd_neg)))
codesimutab[19,-c(1,2,3)] <-
  c(sd(abs(skopred_simuRF$by10difp_neg)),
    sd(abs(skopred_simuRF$by10difd_neg)),
    sd(abs(skopred_simuRF$by50difp_neg)),
    sd(abs(skopred_simuRF$by50difd_neg)),
    sd(abs(skopred_simuRF$by100difp_neg)),
    sd(abs(skopred_simuRF$by100difd_neg)))
codesimutab[20,-c(1,2,3)] <-
  c(sd(abs(nkopred_simuRF$by10difp_neg)),
    sd(abs(nkopred_simuRF$by10difd_neg)),
    sd(abs(nkopred_simuRF$by50difp_neg)),
    sd(abs(nkopred_simuRF$by50difd_neg)),
    sd(abs(nkopred_simuRF$by100difp_neg)),
    sd(abs(nkopred_simuRF$by100difd_neg)))
## Distance SD (Positive)
codesimutab[21,-c(1,2,3)] <-
  c(sd(abs(uspred_simuRF$by10difp_pos)),
    sd(abs(uspred_simuRF$by10difd_pos)),
    sd(abs(uspred_simuRF$by50difp_pos)),
    sd(abs(uspred_simuRF$by50difd_pos)),
    sd(abs(uspred_simuRF$by100difp_pos)),
    sd(abs(uspred_simuRF$by100difd_pos)))
codesimutab[22,-c(1,2,3)] <-
  c(sd(abs(chnpred_simuRF$by10difp_pos)),
    sd(abs(chnpred_simuRF$by10difd_pos)),
    sd(abs(chnpred_simuRF$by50difp_pos)),
    sd(abs(chnpred_simuRF$by50difd_pos)),
    sd(abs(chnpred_simuRF$by100difp_pos)),
    sd(abs(chnpred_simuRF$by100difd_pos)))
codesimutab[23,-c(1,2,3)] <-
  c(sd(abs(skopred_simuRF$by10difp_pos)),
    sd(abs(skopred_simuRF$by10difd_pos)),
    sd(abs(skopred_simuRF$by50difp_pos)),
    sd(abs(skopred_simuRF$by50difd_pos)),
    sd(abs(skopred_simuRF$by100difp_pos)),
    sd(abs(skopred_simuRF$by100difd_pos)))
codesimutab[24,-c(1,2,3)] <-
  c(sd(abs(nkopred_simuRF$by10difp_pos)),
    sd(abs(nkopred_simuRF$by10difd_pos)),
    sd(abs(nkopred_simuRF$by50difp_pos)),
    sd(abs(nkopred_simuRF$by50difd_pos)),
    sd(abs(nkopred_simuRF$by100difp_pos)),
    sd(abs(nkopred_simuRF$by100difd_pos)))

codesimutab <- as.matrix(cbind(codesimutab[,1:3],round(codesimutab[,-c(1,2,3)],3)))
colnames(codesimutab)[-c(1,2,3)] <- c("$\\overline{p(c|x)}$ by 10","$d(c|x)$ by 10",
                                     "$\\overline{p(c|x)}$ by 50","$d(c|x)$ by 50",
                                     "$\\overline{p(c|x)}$ by 100","$d(c|x)$ by 100")
# display value
xtable(codesimutab)
cat('No cases for US-positive have predicted probability larger than 0.5.')

## For the Paper ##
colnames(codesimutab)[-c(1,2,3)] <- rep(c("$\\overline{p(c|x)}$","$d(c|x)$"),3)

addtorow_cst <- list()
addtorow_cst$pos <- list(-1,nrow(codesimutab[1:8,]))
addtorow_cst$command <- 
  c("\\toprule \n \\multicolumn{3}{l}{\\it Aggregation Size:} & \\multicolumn{2}{c}{By 10} & \\multicolumn{2}{c}{By 50}  & \\multicolumn{2}{c}{By 100} \\\\ \n",
"\\bottomrule \n \\multicolumn{9}{r}{\\scriptsize No cases for US-positive have predicted probability larger than 0.5.}")

# print(xtable(codesimutab[1:8,]),add.to.row=addtorow_cst,
#       hline.after = c(0),sanitize.colnames.function = identity,
#       floating=FALSE,booktabs=TRUE,include.rownames=F)

##############################################################
## Function to Assess Aggregate Level Logit Coding Validity ##
##############################################################

simubysamplelogit <- function(dtpred,N,seedset=5689){
dtpred_simu <- data.frame(by10true_neg=rep(NA,N),by10logitp_neg=NA,by10difp_neg=NA,by10logitd_neg=NA,by10difd_neg=NA,
                          by50true_neg=NA,by50logitp_neg=NA,by50difp_neg=NA,by50logitd_neg=NA,by50difd_neg=NA,
                          by100true_neg=NA,by100logitp_neg=NA,by100difp_neg=NA,by100logitd_neg=NA,by100difd_neg=NA,
                          by10true_pos=rep(NA,N),by10logitp_pos=NA,by10difp_pos=NA,by10logitd_pos=NA,by10difd_pos=NA,
                          by50true_pos=NA,by50logitp_pos=NA,by50difp_pos=NA,by50logitd_pos=NA,by50difd_pos=NA,
                          by100true_pos=NA,by100logitp_pos=NA,by100difp_pos=NA,by100logitd_pos=NA,by100difd_pos=NA)
for (i in 1:N){
set.seed(i+seedset)
nrs10 <- sample(seq(1,nrow(dtpred),by=1),10)
nrs50 <- sample(seq(1,nrow(dtpred),by=1),50)
nrs100 <- sample(seq(1,nrow(dtpred),by=1),100)
dtpred_simu$by10true_neg[i] <- mean(dtpred[nrs10,]$y_test_neg)
dtpred_simu$by10logitp_neg[i] <- mean(dtpred[nrs10,]$proby_test_logit_neg)
dtpred_simu$by10logitd_neg[i] <- mean((dtpred[nrs10,]$proby_test_logit_neg>=0.5)*1)
dtpred_simu$by50true_neg[i] <- mean(dtpred[nrs50,]$y_test_neg)
dtpred_simu$by50logitp_neg[i] <- mean(dtpred[nrs50,]$proby_test_logit_neg)
dtpred_simu$by50logitd_neg[i] <- mean((dtpred[nrs50,]$proby_test_logit_neg>=0.5)*1)
dtpred_simu$by100true_neg[i] <- mean(dtpred[nrs100,]$y_test_neg)
dtpred_simu$by100logitp_neg[i] <- mean(dtpred[nrs100,]$proby_test_logit_neg)
dtpred_simu$by100logitd_neg[i] <- mean((dtpred[nrs100,]$proby_test_logit_neg>=0.5)*1)
dtpred_simu$by10true_pos[i] <- mean(dtpred[nrs10,]$y_test_pos)
dtpred_simu$by10logitp_pos[i] <- mean(dtpred[nrs10,]$proby_test_logit_pos)
dtpred_simu$by10logitd_pos[i] <- mean((dtpred[nrs10,]$proby_test_logit_pos>=0.5)*1)
dtpred_simu$by50true_pos[i] <- mean(dtpred[nrs50,]$y_test_pos)
dtpred_simu$by50logitp_pos[i] <- mean(dtpred[nrs50,]$proby_test_logit_pos)
dtpred_simu$by50logitd_pos[i] <- mean((dtpred[nrs50,]$proby_test_logit_pos>=0.5)*1)
dtpred_simu$by100true_pos[i] <- mean(dtpred[nrs100,]$y_test_pos)
dtpred_simu$by100logitp_pos[i] <- mean(dtpred[nrs100,]$proby_test_logit_pos)
dtpred_simu$by100logitd_pos[i] <- mean((dtpred[nrs100,]$proby_test_logit_pos>=0.5)*1)
}

dtpred_simu$by10difp_neg <- dtpred_simu$by10logitp_neg - dtpred_simu$by10true_neg 
dtpred_simu$by50difp_neg <- dtpred_simu$by50logitp_neg - dtpred_simu$by50true_neg 
dtpred_simu$by100difp_neg <- dtpred_simu$by100logitp_neg - dtpred_simu$by100true_neg 
dtpred_simu$by10difp_pos <- dtpred_simu$by10logitp_pos - dtpred_simu$by10true_pos 
dtpred_simu$by50difp_pos <- dtpred_simu$by50logitp_pos - dtpred_simu$by50true_pos 
dtpred_simu$by100difp_pos <- dtpred_simu$by100logitp_pos - dtpred_simu$by100true_pos 

dtpred_simu$by10difd_neg <- dtpred_simu$by10logitd_neg - dtpred_simu$by10true_neg 
dtpred_simu$by50difd_neg <- dtpred_simu$by50logitd_neg - dtpred_simu$by50true_neg 
dtpred_simu$by100difd_neg <- dtpred_simu$by100logitd_neg - dtpred_simu$by100true_neg 
dtpred_simu$by10difd_pos <- dtpred_simu$by10logitd_pos - dtpred_simu$by10true_pos 
dtpred_simu$by50difd_pos <- dtpred_simu$by50logitd_pos - dtpred_simu$by50true_pos 
dtpred_simu$by100difd_pos <- dtpred_simu$by100logitd_pos - dtpred_simu$by100true_pos 

return(dtpred_simu)
}


##################################
## Assess Logit Coding Validity ##
##################################

uspred_simulogit <- simubysamplelogit(uspred,1000)
chnpred_simulogit <- simubysamplelogit(chnpred,1000)
skopred_simulogit <- simubysamplelogit(skopred,1000)
nkopred_simulogit <- simubysamplelogit(nkopred,1000)

codesimutabLT <- data.frame(
  "Metric" = NA,
  "Tone" = NA,
  "Country" = NA,
  "by10p" = rep(NA,24), 
  "by10d" = NA,
  "by50p" = NA, 
  "by50d" = NA,
  "by100p" = NA,
  "by100d" = NA)

codesimutabLT$Metric <- c("Correlation",rep(NA,7),
                        "Av. Distance",rep(NA,7),
                        "Distance SD",rep(NA,7))
codesimutabLT$Tone <- c("Negative",rep(NA,3),
                      "Positive",rep(NA,3),
                      "Negative",rep(NA,3),
                      "Positive",rep(NA,3),
                      "Negative",rep(NA,3),
                      "Positive",rep(NA,3))
codesimutabLT$Country <- c(rep(c("US","China","S.Korea","N.Korea"),6))

## Correlation (Negative)
codesimutabLT[1,-c(1,2,3)] <- 
c(cor(uspred_simulogit$by10true_neg,uspred_simulogit$by10logitp_neg),
  cor(uspred_simulogit$by10true_neg,uspred_simulogit$by10logitd_neg),
  cor(uspred_simulogit$by50true_neg,uspred_simulogit$by50logitp_neg),
  cor(uspred_simulogit$by50true_neg,uspred_simulogit$by50logitd_neg),
  cor(uspred_simulogit$by100true_neg,uspred_simulogit$by100logitp_neg),
  cor(uspred_simulogit$by100true_neg,uspred_simulogit$by100logitd_neg))
codesimutabLT[2,-c(1,2,3)] <- 
  c(cor(chnpred_simulogit$by10true_neg,chnpred_simulogit$by10logitp_neg),
    cor(chnpred_simulogit$by10true_neg,chnpred_simulogit$by10logitd_neg),
    cor(chnpred_simulogit$by50true_neg,chnpred_simulogit$by50logitp_neg),
    cor(chnpred_simulogit$by50true_neg,chnpred_simulogit$by50logitd_neg),
    cor(chnpred_simulogit$by100true_neg,chnpred_simulogit$by100logitp_neg),
    cor(chnpred_simulogit$by100true_neg,chnpred_simulogit$by100logitd_neg))
codesimutabLT[3,-c(1,2,3)] <- 
  c(cor(skopred_simulogit$by10true_neg,skopred_simulogit$by10logitp_neg),
    cor(skopred_simulogit$by10true_neg,skopred_simulogit$by10logitd_neg),
    cor(skopred_simulogit$by50true_neg,skopred_simulogit$by50logitp_neg),
    cor(skopred_simulogit$by50true_neg,skopred_simulogit$by50logitd_neg),
    cor(skopred_simulogit$by100true_neg,skopred_simulogit$by100logitp_neg),
    cor(skopred_simulogit$by100true_neg,skopred_simulogit$by100logitd_neg))
codesimutabLT[4,-c(1,2,3)] <- 
  c(cor(nkopred_simulogit$by10true_neg,nkopred_simulogit$by10logitp_neg),
    cor(nkopred_simulogit$by10true_neg,nkopred_simulogit$by10logitd_neg),
    cor(nkopred_simulogit$by50true_neg,nkopred_simulogit$by50logitp_neg),
    cor(nkopred_simulogit$by50true_neg,nkopred_simulogit$by50logitd_neg),
    cor(nkopred_simulogit$by100true_neg,nkopred_simulogit$by100logitp_neg),
    cor(nkopred_simulogit$by100true_neg,nkopred_simulogit$by100logitd_neg))
## Correlation (Positive)
codesimutabLT[5,-c(1,2,3)] <- 
  c(cor(uspred_simulogit$by10true_pos,uspred_simulogit$by10logitp_pos),
    cor(uspred_simulogit$by10true_pos,uspred_simulogit$by10logitd_pos),
    cor(uspred_simulogit$by50true_pos,uspred_simulogit$by50logitp_pos),
    cor(uspred_simulogit$by50true_pos,uspred_simulogit$by50logitd_pos),
    cor(uspred_simulogit$by100true_pos,uspred_simulogit$by100logitp_pos),
    cor(uspred_simulogit$by100true_pos,uspred_simulogit$by100logitd_pos))
codesimutabLT[6,-c(1,2,3)] <- 
  c(cor(chnpred_simulogit$by10true_pos,chnpred_simulogit$by10logitp_pos),
    cor(chnpred_simulogit$by10true_pos,chnpred_simulogit$by10logitd_pos),
    cor(chnpred_simulogit$by50true_pos,chnpred_simulogit$by50logitp_pos),
    cor(chnpred_simulogit$by50true_pos,chnpred_simulogit$by50logitd_pos),
    cor(chnpred_simulogit$by100true_pos,chnpred_simulogit$by100logitp_pos),
    cor(chnpred_simulogit$by100true_pos,chnpred_simulogit$by100logitd_pos))
codesimutabLT[7,-c(1,2,3)] <- 
  c(cor(skopred_simulogit$by10true_pos,skopred_simulogit$by10logitp_pos),
    cor(skopred_simulogit$by10true_pos,skopred_simulogit$by10logitd_pos),
    cor(skopred_simulogit$by50true_pos,skopred_simulogit$by50logitp_pos),
    cor(skopred_simulogit$by50true_pos,skopred_simulogit$by50logitd_pos),
    cor(skopred_simulogit$by100true_pos,skopred_simulogit$by100logitp_pos),
    cor(skopred_simulogit$by100true_pos,skopred_simulogit$by100logitd_pos))
codesimutabLT[8,-c(1,2,3)] <- 
  c(cor(nkopred_simulogit$by10true_pos,nkopred_simulogit$by10logitp_pos),
    cor(nkopred_simulogit$by10true_pos,nkopred_simulogit$by10logitd_pos),
    cor(nkopred_simulogit$by50true_pos,nkopred_simulogit$by50logitp_pos),
    cor(nkopred_simulogit$by50true_pos,nkopred_simulogit$by50logitd_pos),
    cor(nkopred_simulogit$by100true_pos,nkopred_simulogit$by100logitp_pos),
    cor(nkopred_simulogit$by100true_pos,nkopred_simulogit$by100logitd_pos))
## Average Distance (Negative)
codesimutabLT[9,-c(1,2,3)] <-
  c(mean(abs(uspred_simulogit$by10difp_neg)),
    mean(abs(uspred_simulogit$by10difd_neg)),
    mean(abs(uspred_simulogit$by50difp_neg)),
    mean(abs(uspred_simulogit$by50difd_neg)),
    mean(abs(uspred_simulogit$by100difp_neg)),
    mean(abs(uspred_simulogit$by100difd_neg)))
codesimutabLT[10,-c(1,2,3)] <-
  c(mean(abs(chnpred_simulogit$by10difp_neg)),
    mean(abs(chnpred_simulogit$by10difd_neg)),
    mean(abs(chnpred_simulogit$by50difp_neg)),
    mean(abs(chnpred_simulogit$by50difd_neg)),
    mean(abs(chnpred_simulogit$by100difp_neg)),
    mean(abs(chnpred_simulogit$by100difd_neg)))
codesimutabLT[11,-c(1,2,3)] <-
  c(mean(abs(skopred_simulogit$by10difp_neg)),
    mean(abs(skopred_simulogit$by10difd_neg)),
    mean(abs(skopred_simulogit$by50difp_neg)),
    mean(abs(skopred_simulogit$by50difd_neg)),
    mean(abs(skopred_simulogit$by100difp_neg)),
    mean(abs(skopred_simulogit$by100difd_neg)))
codesimutabLT[12,-c(1,2,3)] <-
  c(mean(abs(nkopred_simulogit$by10difp_neg)),
    mean(abs(nkopred_simulogit$by10difd_neg)),
    mean(abs(nkopred_simulogit$by50difp_neg)),
    mean(abs(nkopred_simulogit$by50difd_neg)),
    mean(abs(nkopred_simulogit$by100difp_neg)),
    mean(abs(nkopred_simulogit$by100difd_neg)))
## Average Distance (Positive)
codesimutabLT[13,-c(1,2,3)] <-
  c(mean(abs(uspred_simulogit$by10difp_pos)),
    mean(abs(uspred_simulogit$by10difd_pos)),
    mean(abs(uspred_simulogit$by50difp_pos)),
    mean(abs(uspred_simulogit$by50difd_pos)),
    mean(abs(uspred_simulogit$by100difp_pos)),
    mean(abs(uspred_simulogit$by100difd_pos)))
codesimutabLT[14,-c(1,2,3)] <-
  c(mean(abs(chnpred_simulogit$by10difp_pos)),
    mean(abs(chnpred_simulogit$by10difd_pos)),
    mean(abs(chnpred_simulogit$by50difp_pos)),
    mean(abs(chnpred_simulogit$by50difd_pos)),
    mean(abs(chnpred_simulogit$by100difp_pos)),
    mean(abs(chnpred_simulogit$by100difd_pos)))
codesimutabLT[15,-c(1,2,3)] <-
  c(mean(abs(skopred_simulogit$by10difp_pos)),
    mean(abs(skopred_simulogit$by10difd_pos)),
    mean(abs(skopred_simulogit$by50difp_pos)),
    mean(abs(skopred_simulogit$by50difd_pos)),
    mean(abs(skopred_simulogit$by100difp_pos)),
    mean(abs(skopred_simulogit$by100difd_pos)))
codesimutabLT[16,-c(1,2,3)] <-
  c(mean(abs(nkopred_simulogit$by10difp_pos)),
    mean(abs(nkopred_simulogit$by10difd_pos)),
    mean(abs(nkopred_simulogit$by50difp_pos)),
    mean(abs(nkopred_simulogit$by50difd_pos)),
    mean(abs(nkopred_simulogit$by100difp_pos)),
    mean(abs(nkopred_simulogit$by100difd_pos)))
## Distance SD (Negative)
codesimutabLT[17,-c(1,2,3)] <-
  c(sd(abs(uspred_simulogit$by10difp_neg)),
    sd(abs(uspred_simulogit$by10difd_neg)),
    sd(abs(uspred_simulogit$by50difp_neg)),
    sd(abs(uspred_simulogit$by50difd_neg)),
    sd(abs(uspred_simulogit$by100difp_neg)),
    sd(abs(uspred_simulogit$by100difd_neg)))
codesimutabLT[18,-c(1,2,3)] <-
  c(sd(abs(chnpred_simulogit$by10difp_neg)),
    sd(abs(chnpred_simulogit$by10difd_neg)),
    sd(abs(chnpred_simulogit$by50difp_neg)),
    sd(abs(chnpred_simulogit$by50difd_neg)),
    sd(abs(chnpred_simulogit$by100difp_neg)),
    sd(abs(chnpred_simulogit$by100difd_neg)))
codesimutabLT[19,-c(1,2,3)] <-
  c(sd(abs(skopred_simulogit$by10difp_neg)),
    sd(abs(skopred_simulogit$by10difd_neg)),
    sd(abs(skopred_simulogit$by50difp_neg)),
    sd(abs(skopred_simulogit$by50difd_neg)),
    sd(abs(skopred_simulogit$by100difp_neg)),
    sd(abs(skopred_simulogit$by100difd_neg)))
codesimutabLT[20,-c(1,2,3)] <-
  c(sd(abs(nkopred_simulogit$by10difp_neg)),
    sd(abs(nkopred_simulogit$by10difd_neg)),
    sd(abs(nkopred_simulogit$by50difp_neg)),
    sd(abs(nkopred_simulogit$by50difd_neg)),
    sd(abs(nkopred_simulogit$by100difp_neg)),
    sd(abs(nkopred_simulogit$by100difd_neg)))
## Distance SD (Positive)
codesimutabLT[21,-c(1,2,3)] <-
  c(sd(abs(uspred_simulogit$by10difp_pos)),
    sd(abs(uspred_simulogit$by10difd_pos)),
    sd(abs(uspred_simulogit$by50difp_pos)),
    sd(abs(uspred_simulogit$by50difd_pos)),
    sd(abs(uspred_simulogit$by100difp_pos)),
    sd(abs(uspred_simulogit$by100difd_pos)))
codesimutabLT[22,-c(1,2,3)] <-
  c(sd(abs(chnpred_simulogit$by10difp_pos)),
    sd(abs(chnpred_simulogit$by10difd_pos)),
    sd(abs(chnpred_simulogit$by50difp_pos)),
    sd(abs(chnpred_simulogit$by50difd_pos)),
    sd(abs(chnpred_simulogit$by100difp_pos)),
    sd(abs(chnpred_simulogit$by100difd_pos)))
codesimutabLT[23,-c(1,2,3)] <-
  c(sd(abs(skopred_simulogit$by10difp_pos)),
    sd(abs(skopred_simulogit$by10difd_pos)),
    sd(abs(skopred_simulogit$by50difp_pos)),
    sd(abs(skopred_simulogit$by50difd_pos)),
    sd(abs(skopred_simulogit$by100difp_pos)),
    sd(abs(skopred_simulogit$by100difd_pos)))
codesimutabLT[24,-c(1,2,3)] <-
  c(sd(abs(nkopred_simulogit$by10difp_pos)),
    sd(abs(nkopred_simulogit$by10difd_pos)),
    sd(abs(nkopred_simulogit$by50difp_pos)),
    sd(abs(nkopred_simulogit$by50difd_pos)),
    sd(abs(nkopred_simulogit$by100difp_pos)),
    sd(abs(nkopred_simulogit$by100difd_pos)))

codesimutabLT <- as.matrix(cbind(codesimutabLT[,1:3],round(codesimutabLT[,-c(1,2,3)],3)))
colnames(codesimutabLT)[-c(1,2,3)] <- c("$\\overline{p(c|x)}$ by 10","$d(c|x)$ by 10",
                                     "$\\overline{p(c|x)}$ by 50","$d(c|x)$ by 50",
                                     "$\\overline{p(c|x)}$ by 100","$d(c|x)$ by 100")
# display value
xtable(codesimutabLT)
cat('No cases for US-positive have predicted probability larger than 0.5.')

## For the Paper
colnames(codesimutabLT)[-c(1,2,3)] <- rep(c("$\\overline{p(c|x)}$","$d(c|x)$"),3)

addtorow_cst <- list()
addtorow_cst$pos <- list(-1,nrow(codesimutabLT[1:8,]))
addtorow_cst$command <- 
  c("\\toprule \n \\multicolumn{3}{l}{\\it Aggregation Size:} & \\multicolumn{2}{c}{By 10} & \\multicolumn{2}{c}{By 50}  & \\multicolumn{2}{c}{By 100} \\\\ \n",
"\\bottomrule \n \\multicolumn{9}{r}{\\scriptsize No cases for US-positive have predicted probability larger than 0.5.}")

# print(xtable(codesimutabLT[1:8,]),add.to.row=addtorow_cst,
#       hline.after = c(0),sanitize.colnames.function = identity,
#       floating=FALSE,booktabs=TRUE,include.rownames=F)


####################
## Save the Table ##
####################

save(codesimutab,addtorow_cst,file="./outputs/codesimutab.RData")
save(codesimutabLT,addtorow_cst,file="./outputs/codesimutabLT.RData")

