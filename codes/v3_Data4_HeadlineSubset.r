
################################################################################# 
## File Name: v3_Data_HeadlineSubset.R                                         ##
## Creation Date: 4 Nov 2016                                                   ##
## Author: Gento Kato                                                          ##
## Project: Foreign Image News Project                                         ##
## Purpose: Search Relevant Texts and Create Country Data Subsets              ##
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
library(rprojroot); library(descr)

## Set Working Directory (Automatically or Manually) ##
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd("../") #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Agenda-Setting Persuasion Framing/Foreign_Image_News_Project")


## Load The RMeCab Data ##
load("data/v3_Data2_HeadlineWrdMat.Rdata")

## Replace Old Subset Data with Machine Coded Data
usdata <- read.csv("data/usdata_codepred.csv",fileEncoding = "CP932")
chndata <- read.csv("data/chndata_codepred.csv",fileEncoding = "CP932")
skordata <- read.csv("data/skodata_codepred.csv",fileEncoding = "CP932")
nkordata <- read.csv("data/nkodata_codepred.csv",fileEncoding = "CP932")

## Add Coding Variables (as Random Forest Probability)
# Negative
usdata$code_us_neg <- usdata$rfpr_neg
chndata$code_chn_neg <- chndata$rfpr_neg
skordata$code_sko_neg <- skordata$rfpr_neg
nkordata$code_nko_neg <- nkordata$rfpr_neg
# Positive
usdata$code_us_pos <- usdata$rfpr_pos
chndata$code_chn_pos <- chndata$rfpr_pos
skordata$code_sko_pos <- skordata$rfpr_pos
nkordata$code_nko_pos <- nkordata$rfpr_pos
# Tone: Positive - Negative
usdata$code_us <- usdata$rfpr_pos - usdata$rfpr_neg 
chndata$code_chn <- chndata$rfpr_pos - chndata$rfpr_neg
skordata$code_sko <- skordata$rfpr_pos - skordata$rfpr_neg
nkordata$code_nko <- nkordata$rfpr_pos - nkordata$rfpr_neg

ls()


###########################
## Text Search Function ##
###########################

inclwrd<-function(target,search){ ##target=Mecab List, search=set of words to search
n<-length(target) # Define the length of exporting vector
countres<-rep(NA,n) # create the exporting vector
for(i in 1:n){ 
  sample<-as.factor(target[[i]]) # Each element in data value
  levels(sample)[levels(sample) %in%  search]<-"ifindit" # Mark the searching words
  countres[i]<-sum(sample=="ifindit") # word count of searching words
  }
countres[countres>0]<-1 # make it a dummy variable
return(countres) # return the vector
}


###########################################
## Searching State/Region Relevant Cases ##
###########################################

## New Country Search #########################################################
target<-MecabRes

## Russia ##
search<-c("ロシア","露","米露","米ロ","日露","日ロ",
          "エリツィン","プーチン","メドヴェージェフ",
          "メドベージェフ")
hldata$rus<-inclwrd(target=target,search=search)
##freq(hldata$rus)
#head(hldata$Headline[hldata$rus==1],50)

## Europe ##
search<-c("欧州","欧","米欧","日欧","日米欧","ヨーロッパ",
          "ＥＵ","ドイツ","独","日独","イギリス","英","日英",
          "フランス","日仏","仏","イタリア","伊","日伊","スペイン",
          "ポルトガル","オランダ","アムステルダム","ベルギー",
          "ルクセンブルク","アイルランド","ギリシャ","マルタ",
          "クロアチア","ブルガリア","ポーランド","ハンガリー",
          "チェコ","チェコスロバキア","スロバキア","スロヴァキア",
          "スロベニア","オーストリア","オーストリー","ルーマニア",
          "デンマーク","スウェーデン","フィンランド","リトアニア",
          "エストニア","ラトビア","キプロス","セルビア",
          "モンテネグロ","アルバニア","マケドニア","コソボ",
          "コソヴォ","ボスニア","ヘルツェゴビナ","ユーゴスラビア",
          "ユーゴ","ユーゴスラヴィア","アイスランド","ノルウェー",
          "スイス","モナコ","リヒテンシュタイン","アンドラ",
          "サンマリノ","バチカン")
hldata$euro<-inclwrd(target=target,search=search)
##freq(hldata$euro)
#head(hldata$Headline[hldata$euro==1],50)

## Middle Near East ##
search<-c("中東","中近東","イラン","イラク","サウジ",
          "サウジアラビア","クウェート","バーレーン",
          "アラブ","ＵＡＥ","イエメン","オマーン",
          "カタール","イスラエル","ガザ","ヨルダン",
          "シリア","レバノン","トルコ")
hldata$mneast<-inclwrd(target=target,search=search)
wangan<-c("湾岸");wangan<-inclwrd(target=target,search=wangan) 
senso<-c("戦争");senso<-inclwrd(target=target,search=senso)
wangansenso<-wangan*senso
hldata$mneast[hldata$mneast==0]<-wangansenso[hldata$mneast==0]
rm(wangan,senso,wangansenso)
##freq(hldata$mneast)
#head(hldata$Headline[hldata$mneast==1],50)

## India ##
#search<-c("インド","デリー","ニューデリー","印","日印","米印")
#hldata$india<-inclwrd(target=target,search=search)
##freq(hldata$india)
#head(hldata$Headline[hldata$india==1],50)

## Taiwan ##
search<-c("台湾","日台","李登輝","登輝","陳水扁","水扁","馬英九")
hldata$taiwan<-inclwrd(target=target,search=search)
##freq(hldata$taiwan)
#head(hldata$Headline[hldata$taiwan==1],50)

## South East Asia ##
search<-c("東南アジア","ＡＳＥＡＮ","フィリピン","比","日比",
          "カンボジア","ベトナム","ラオス","タイ","ミャンマー",
          "ビルマ","シンガポール","マレーシア","インドネシア",
          "ブルネイ")
hldata$seasia<-inclwrd(target=target,search=search)
##freq(hldata$seasia)
#head(hldata$Headline[hldata$seasia==1],50)

## Central (Middle) South America ##
search<-c("中南米","南米","中米","ラテン","カリブ","メキシコ",
          "グアテマラ","ホンジュラス","ベリーズ","エルサルバドル",
          "ニカラグア","コスタリカ","パナマ","バミューダ",
          "バハマ","ジャマイカ","バルバドス","トリニダード",
          "トバゴ","トリニダッド","トリニダード・トバゴ",
          "トリニダッド・トバゴ","ハイチ","ドミニカ","プエルトリコ",
          "ケイマン","グレナダ","アンティグア","バーブーダ",
          "クリストファー","ネイビス","セントビンセント",
          "グレナディーン","コロンビア","ベネズエラ","ガイアナ",
          "スリナム","エクアドル","ペルー","ボリビア","チリ",
          "ブラジル","パラグアイ","ウルグアイ","アルゼンチン",
          "フォークランド")
hldata$msamerica<-inclwrd(target=target,search=search)
##freq(hldata$msamerica)
#head(hldata$Headline[hldata$msamerica==1],50)

## Oceania ##
search<-c("オセアニア","大洋州","オーストラリア","ニュージーランド",
          "ニュージ―","ＮＺ","パプア","ニューギニア","サモア",
          "バヌアツ","フィジー","ソロモン","トンガ","ナウル",
          "ニューカレドニア","ポリネシア","グアム","ツバル",
          "マーシャル諸島","ミクロネシア","マリアナ","パラオ")
hldata$oceania<-inclwrd(target=target,search=search)
##freq(hldata$oceania)
#head(hldata$Headline[hldata$oceania==1],50)

## Africa ##
search<-c("アフリカ","サハラ","モロッコ","アルジェリア","チュニジア",
          "リビア","エジプト","スーダン","西サハラ","モーリタニア",
          "セネガル","ガンビア","ギニア","ギニアビサウ","シエラレオネ",
          "リベリア","コートジボワール","コートジヴォワール",
          "ガーナ","トーゴ","ベニン","マリ","ブルキナファソ",
          "カーボベルデ","カーボヴェルデ","カボベルデ","ナイジェリア",
          "ニジェール","ルワンダ","カメルーン","チャド","中央アフリカ",
          "赤道ギニア","ガボン","コンゴ","ザイール","ブルンジ",
          "サントメ","プリンシペ","エチオピア","ジブチ","ソマリア",
          "ケニア","ケニヤ","ウガンダ","タンザニア","セイシェル",
          "モザンビーク","マダガスカル","モーリシャス","ジンバブエ",
          "ナミビア","南アフリカ","レソト","マラウイ","マラウィ",
          "ザンビア","ボツワナ","スワジランド","コモロ","エリトリア")
hldata$africa<-inclwrd(target=target,search=search)
##freq(hldata$africa)
#head(hldata$Headline[hldata$africa==1],50)


## Frequency Table ##
statefreq <- t(cbind(
      table(hldata$us),table(hldata$chn),
      table(hldata$kor),table(hldata$nkor),
      table(hldata$rus),table(hldata$euro),
      table(hldata$mneast),table(hldata$taiwan),
      table(hldata$seasia),table(hldata$msamerica),
      table(hldata$oceania),table(hldata$africa)))
rownames(statefreq) <- 
 c("US (by KH Coder)","China (by KH Coder)", "S.Korea (by KH Coder)", "N. Korea (by KH Coder)",
   "Russia","Europe", "Middle Near East", "Taiwan", "South East Asia", "Central South America",
   "Oceania", "Africa")
statefreq


########################
## Coding Issue Frame ##
########################

## Issue Frame Search ############################################################

## Economy Frame ##
search<-c("貿易","投資","ガット","関税","輸入","輸出","禁輸",
          "資本","現地生産","漁業協定","ＷＴＯ","ＦＴＡ","ＡＰＥＣ",
          "援助","支援","円借款","経済","株","相場","円安","円高",
          "終値","市場","赤字","黒字","公共事業","産業","人民元",
          "バブル","円","就業","ドル","ウォン","通商","社","関税","構造協議")
target<-usMecabRes
usdata$econ<-inclwrd(target=target,search=search)
##freq(usdata$econ)
target<-chnMecabRes
chndata$econ<-inclwrd(target=target,search=search)
##freq(chndata$econ)
target<-skorMecabRes
skordata$econ<-inclwrd(target=target,search=search)
##freq(skordata$econ)
target<-nkorMecabRes
nkordata$econ<-inclwrd(target=target,search=search)
##freq(nkordata$econ)

## Defense Frame ##
search<-c("制裁","武力","軍","核","国防","不安定","安定",
          "有事","軍拡","脅威","侵攻","防衛","安全保障","安保",
          "自衛隊","攻撃","交戦","爆撃","空爆","停戦","和平","平和",
          "同盟","自衛権","自衛","戦争","イラク","アフガン",
          "アフガニスタン","タリバン","尖閣","拉致","ら致","竹島",
          "ミサイル","迎撃","テロ")
target<-usMecabRes
usdata$defense<-inclwrd(target=target,search=search)
##freq(usdata$defense)
# Other candidates "米軍","基地","沖縄","在沖"
target<-chnMecabRes
chndata$defense<-inclwrd(target=target,search=search)
##freq(chndata$defense)
# Other candidates "米軍","基地","沖縄","在沖"
target<-skorMecabRes
skordata$defense<-inclwrd(target=target,search=search)
##freq(skordata$defense)
# Other candidates "米軍","基地","沖縄","在沖","普天間"
target<-nkorMecabRes
nkordata$defense<-inclwrd(target=target,search=search)
##freq(nkordata$defense)

# ## Political Communication ##
# search<-c("会談","訪問","訪米","訪中","訪韓","訪日","首脳",
#           "訪朝","歴訪","来日","交渉","合意","協議","会議")
# target<-usMecabRes
# usdata$polcom<-inclwrd(target=target,search=search)
# ##freq(usdata$polcom)
# target<-chnMecabRes
# chndata$polcom<-inclwrd(target=target,search=search)
# ##freq(chndata$polcom)
# target<-skorMecabRes
# skordata$polcom<-inclwrd(target=target,search=search)
# ##freq(skordata$polcom)
# target<-nkorMecabRes
# nkordata$polcom<-inclwrd(target=target,search=search)
# ##freq(nkordata$polcom)


## Frequency Table ##
framefreq <- t(cbind(
      table(usdata$econ),table(usdata$defense),
      table(chndata$econ),table(chndata$defense),
      table(skordata$econ),table(skordata$defense),
      table(nkordata$econ),table(nkordata$defense)))
rownames(framefreq) <- 
 c("Economy Frame (US)","Defense Frame (US)", "Economy Frame (China)", "Defense Frame (China)",
   "Economy Frame (S.Korea)","Defense Frame (S.Korea)", "Economy Frame (N.Korea)","Defense Frame (N.Korea)")
framefreq

#################################
## Create Word Count Variables ##
#################################

## United States ##
 #statecount_w
usdata$statecount_w<-usdata$us*usdata$wcount
usdata$statecount_w_asahi<-usdata$us*usdata$wcount*usdata$Asahi
usdata$statecount_w_yomiuri<-usdata$us*usdata$wcount*usdata$Yomiuri
usdata$statecount_w_both<-usdata$statecount_w_asahi*(4/9)+usdata$statecount_w_yomiuri*(5/9)
 #statetone_w
usdata$statetone_w<-usdata$code_us*usdata$wcount
usdata$statetone_w_asahi<-usdata$code_us*usdata$wcount*usdata$Asahi
usdata$statetone_w_yomiuri<-usdata$code_us*usdata$wcount*usdata$Yomiuri
usdata$statetone_w_both<-usdata$statetone_w_asahi*(4/9)+usdata$statetone_w_yomiuri*(5/9)
 #econ_w
usdata$econ_w<-usdata$econ*usdata$wcount
usdata$econ_w_asahi<-usdata$econ*usdata$wcount*usdata$Asahi
usdata$econ_w_yomiuri<-usdata$econ*usdata$wcount*usdata$Yomiuri
usdata$econ_w_both<-usdata$econ_w_asahi*(4/9)+usdata$econ_w_yomiuri*(5/9)
 #defense_w
usdata$defense_w<-usdata$defense*usdata$wcount
usdata$defense_w_asahi<-usdata$defense*usdata$wcount*usdata$Asahi
usdata$defense_w_yomiuri<-usdata$defense*usdata$wcount*usdata$Yomiuri
usdata$defense_w_both<-usdata$defense_w_asahi*(4/9)+usdata$defense_w_yomiuri*(5/9)
#  #polcom_w
# usdata$polcom_w<-usdata$polcom*usdata$wcount
# usdata$polcom_w_asahi<-usdata$polcom*usdata$wcount*usdata$Asahi
# usdata$polcom_w_yomiuri<-usdata$polcom*usdata$wcount*usdata$Yomiuri
# usdata$polcom_w_both<-usdata$polcom_w_asahi*(4/9)+usdata$polcom_w_yomiuri*(5/9)
#toneecon_w
usdata$toneecon_w<-usdata$econ*usdata$wcount*usdata$code_us
usdata$toneecon_w_asahi<-usdata$econ*usdata$wcount*usdata$Asahi*usdata$code_us
usdata$toneecon_w_yomiuri<-usdata$econ*usdata$wcount*usdata$Yomiuri*usdata$code_us
usdata$toneecon_w_both<-usdata$toneecon_w_asahi*(4/9)+usdata$toneecon_w_yomiuri*(5/9)
#tonedefense_w
usdata$tonedefense_w<-usdata$defense*usdata$wcount*usdata$code_us
usdata$tonedefense_w_asahi<-usdata$defense*usdata$wcount*usdata$Asahi*usdata$code_us
usdata$tonedefense_w_yomiuri<-usdata$defense*usdata$wcount*usdata$Yomiuri*usdata$code_us
usdata$tonedefense_w_both<-usdata$tonedefense_w_asahi*(4/9)+usdata$tonedefense_w_yomiuri*(5/9)

## China ##
#statecount_w
chndata$statecount_w<-chndata$chn*chndata$wcount
chndata$statecount_w_asahi<-chndata$chn*chndata$wcount*chndata$Asahi
chndata$statecount_w_yomiuri<-chndata$chn*chndata$wcount*chndata$Yomiuri
chndata$statecount_w_both<-chndata$statecount_w_asahi*(4/9)+chndata$statecount_w_yomiuri*(5/9)
#statetone_w
chndata$statetone_w<-chndata$code_chn*chndata$wcount
chndata$statetone_w_asahi<-chndata$code_chn*chndata$wcount*chndata$Asahi
chndata$statetone_w_yomiuri<-chndata$code_chn*chndata$wcount*chndata$Yomiuri
chndata$statetone_w_both<-chndata$statetone_w_asahi*(4/9)+chndata$statetone_w_yomiuri*(5/9)
#econ_w
chndata$econ_w<-chndata$econ*chndata$wcount
chndata$econ_w_asahi<-chndata$econ*chndata$wcount*chndata$Asahi
chndata$econ_w_yomiuri<-chndata$econ*chndata$wcount*chndata$Yomiuri
chndata$econ_w_both<-chndata$econ_w_asahi*(4/9)+chndata$econ_w_yomiuri*(5/9)
#defense_w
chndata$defense_w<-chndata$defense*chndata$wcount
chndata$defense_w_asahi<-chndata$defense*chndata$wcount*chndata$Asahi
chndata$defense_w_yomiuri<-chndata$defense*chndata$wcount*chndata$Yomiuri
chndata$defense_w_both<-chndata$defense_w_asahi*(4/9)+chndata$defense_w_yomiuri*(5/9)
# #polcom_w
# chndata$polcom_w<-chndata$polcom*chndata$wcount
# chndata$polcom_w_asahi<-chndata$polcom*chndata$wcount*chndata$Asahi
# chndata$polcom_w_yomiuri<-chndata$polcom*chndata$wcount*chndata$Yomiuri
# chndata$polcom_w_both<-chndata$polcom_w_asahi*(4/9)+chndata$polcom_w_yomiuri*(5/9)
#toneecon_w
chndata$toneecon_w<-chndata$econ*chndata$wcount*chndata$code_chn
chndata$toneecon_w_asahi<-chndata$econ*chndata$wcount*chndata$Asahi*chndata$code_chn
chndata$toneecon_w_yomiuri<-chndata$econ*chndata$wcount*chndata$Yomiuri*chndata$code_chn
chndata$toneecon_w_both<-chndata$toneecon_w_asahi*(4/9)+chndata$toneecon_w_yomiuri*(5/9)
#tonedefense_w
chndata$tonedefense_w<-chndata$defense*chndata$wcount*chndata$code_chn
chndata$tonedefense_w_asahi<-chndata$defense*chndata$wcount*chndata$Asahi*chndata$code_chn
chndata$tonedefense_w_yomiuri<-chndata$defense*chndata$wcount*chndata$Yomiuri*chndata$code_chn
chndata$tonedefense_w_both<-chndata$tonedefense_w_asahi*(4/9)+chndata$tonedefense_w_yomiuri*(5/9)

## South Korea ##
#statecount_w
skordata$statecount_w<-skordata$kor*skordata$wcount
skordata$statecount_w_asahi<-skordata$kor*skordata$wcount*skordata$Asahi
skordata$statecount_w_yomiuri<-skordata$kor*skordata$wcount*skordata$Yomiuri
skordata$statecount_w_both<-skordata$statecount_w_asahi*(4/9)+skordata$statecount_w_yomiuri*(5/9)
#statetone_w
skordata$statetone_w<-skordata$code_sko*skordata$wcount
skordata$statetone_w_asahi<-skordata$code_sko*skordata$wcount*skordata$Asahi
skordata$statetone_w_yomiuri<-skordata$code_sko*skordata$wcount*skordata$Yomiuri
skordata$statetone_w_both<-skordata$statetone_w_asahi*(4/9)+skordata$statetone_w_yomiuri*(5/9)
#econ_w
skordata$econ_w<-skordata$econ*skordata$wcount
skordata$econ_w_asahi<-skordata$econ*skordata$wcount*skordata$Asahi
skordata$econ_w_yomiuri<-skordata$econ*skordata$wcount*skordata$Yomiuri
skordata$econ_w_both<-skordata$econ_w_asahi*(4/9)+skordata$econ_w_yomiuri*(5/9)
#defense_w
skordata$defense_w<-skordata$defense*skordata$wcount
skordata$defense_w_asahi<-skordata$defense*skordata$wcount*skordata$Asahi
skordata$defense_w_yomiuri<-skordata$defense*skordata$wcount*skordata$Yomiuri
skordata$defense_w_both<-skordata$defense_w_asahi*(4/9)+skordata$defense_w_yomiuri*(5/9)
# #polcom_w
# skordata$polcom_w<-skordata$polcom*skordata$wcount
# skordata$polcom_w_asahi<-skordata$polcom*skordata$wcount*skordata$Asahi
# skordata$polcom_w_yomiuri<-skordata$polcom*skordata$wcount*skordata$Yomiuri
# skordata$polcom_w_both<-skordata$polcom_w_asahi*(4/9)+skordata$polcom_w_yomiuri*(5/9)
#toneecon_w
skordata$toneecon_w<-skordata$econ*skordata$wcount*skordata$code_sko
skordata$toneecon_w_asahi<-skordata$econ*skordata$wcount*skordata$Asahi*skordata$code_sko
skordata$toneecon_w_yomiuri<-skordata$econ*skordata$wcount*skordata$Yomiuri*skordata$code_sko
skordata$toneecon_w_both<-skordata$toneecon_w_asahi*(4/9)+skordata$toneecon_w_yomiuri*(5/9)
#tonedefense_w
skordata$tonedefense_w<-skordata$defense*skordata$wcount*skordata$code_sko
skordata$tonedefense_w_asahi<-skordata$defense*skordata$wcount*skordata$Asahi*skordata$code_sko
skordata$tonedefense_w_yomiuri<-skordata$defense*skordata$wcount*skordata$Yomiuri*skordata$code_sko
skordata$tonedefense_w_both<-skordata$tonedefense_w_asahi*(4/9)+skordata$tonedefense_w_yomiuri*(5/9)

## North Korea ##
#statecount_w
nkordata$statecount_w<-nkordata$nkor*nkordata$wcount
nkordata$statecount_w_asahi<-nkordata$nkor*nkordata$wcount*nkordata$Asahi
nkordata$statecount_w_yomiuri<-nkordata$nkor*nkordata$wcount*nkordata$Yomiuri
nkordata$statecount_w_both<-nkordata$statecount_w_asahi*(4/9)+nkordata$statecount_w_yomiuri*(5/9)
#statetone_w
nkordata$statetone_w<-nkordata$code_nko*nkordata$wcount
nkordata$statetone_w_asahi<-nkordata$code_nko*nkordata$wcount*nkordata$Asahi
nkordata$statetone_w_yomiuri<-nkordata$code_nko*nkordata$wcount*nkordata$Yomiuri
nkordata$statetone_w_both<-nkordata$statetone_w_asahi*(4/9)+nkordata$statetone_w_yomiuri*(5/9)
#econ_w
nkordata$econ_w<-nkordata$econ*nkordata$wcount
nkordata$econ_w_asahi<-nkordata$econ*nkordata$wcount*nkordata$Asahi
nkordata$econ_w_yomiuri<-nkordata$econ*nkordata$wcount*nkordata$Yomiuri
nkordata$econ_w_both<-nkordata$econ_w_asahi*(4/9)+nkordata$econ_w_yomiuri*(5/9)
#defense_w
nkordata$defense_w<-nkordata$defense*nkordata$wcount
nkordata$defense_w_asahi<-nkordata$defense*nkordata$wcount*nkordata$Asahi
nkordata$defense_w_yomiuri<-nkordata$defense*nkordata$wcount*nkordata$Yomiuri
nkordata$defense_w_both<-nkordata$defense_w_asahi*(4/9)+nkordata$defense_w_yomiuri*(5/9)
# #polcom_w
# nkordata$polcom_w<-nkordata$polcom*nkordata$wcount
# nkordata$polcom_w_asahi<-nkordata$polcom*nkordata$wcount*nkordata$Asahi
# nkordata$polcom_w_yomiuri<-nkordata$polcom*nkordata$wcount*nkordata$Yomiuri
# nkordata$polcom_w_both<-nkordata$polcom_w_asahi*(4/9)+nkordata$polcom_w_yomiuri*(5/9)
#toneecon_w
nkordata$toneecon_w<-nkordata$econ*nkordata$wcount*nkordata$code_nko
nkordata$toneecon_w_asahi<-nkordata$econ*nkordata$wcount*nkordata$Asahi*nkordata$code_nko
nkordata$toneecon_w_yomiuri<-nkordata$econ*nkordata$wcount*nkordata$Yomiuri*nkordata$code_nko
nkordata$toneecon_w_both<-nkordata$toneecon_w_asahi*(4/9)+nkordata$toneecon_w_yomiuri*(5/9)
#tonedefense_w
nkordata$tonedefense_w<-nkordata$defense*nkordata$wcount*nkordata$code_nko
nkordata$tonedefense_w_asahi<-nkordata$defense*nkordata$wcount*nkordata$Asahi*nkordata$code_nko
nkordata$tonedefense_w_yomiuri<-nkordata$defense*nkordata$wcount*nkordata$Yomiuri*nkordata$code_nko
nkordata$tonedefense_w_both<-nkordata$tonedefense_w_asahi*(4/9)+nkordata$tonedefense_w_yomiuri*(5/9)

## Russia ##
rusdata<-subset(hldata, rus==1)
rusdata$statecount_w<-rusdata$rus*rusdata$wcount
rusdata$statecount_w_asahi<-rusdata$rus*rusdata$wcount*rusdata$Asahi
rusdata$statecount_w_yomiuri<-rusdata$rus*rusdata$wcount*rusdata$Yomiuri
rusdata$statecount_w_both<-rusdata$statecount_w_asahi*(4/9)+rusdata$statecount_w_yomiuri*(5/9)

## Europe ##
eurodata<-subset(hldata, euro==1)
eurodata$statecount_w<-eurodata$euro*eurodata$wcount
eurodata$statecount_w_asahi<-eurodata$euro*eurodata$wcount*eurodata$Asahi
eurodata$statecount_w_yomiuri<-eurodata$euro*eurodata$wcount*eurodata$Yomiuri
eurodata$statecount_w_both<-eurodata$statecount_w_asahi*(4/9)+eurodata$statecount_w_yomiuri*(5/9)

## Middle Near East ##
mneastdata<-subset(hldata, mneast==1)
mneastdata$statecount_w<-mneastdata$mneast*mneastdata$wcount
mneastdata$statecount_w_asahi<-mneastdata$mneast*mneastdata$wcount*mneastdata$Asahi
mneastdata$statecount_w_yomiuri<-mneastdata$mneast*mneastdata$wcount*mneastdata$Yomiuri
mneastdata$statecount_w_both<-mneastdata$statecount_w_asahi*(4/9)+mneastdata$statecount_w_yomiuri*(5/9)

## India ##
#indiadata<-subset(hldata, india==1)
#indiadata$statecount_w<-indiadata$india*indiadata$wcount
#indiadata$statecount_w_asahi<-indiadata$india*indiadata$wcount*indiadata$Asahi
#indiadata$statecount_w_yomiuri<-indiadata$india*indiadata$wcount*indiadata$Yomiuri
#indiadata$statecount_w_both<-indiadata$statecount_w_asahi*(4/9)+indiadata$statecount_w_yomiuri*(5/9)

## Taiwan ##
taiwandata<-subset(hldata, taiwan==1)
taiwandata$statecount_w<-taiwandata$taiwan*taiwandata$wcount
taiwandata$statecount_w_asahi<-taiwandata$taiwan*taiwandata$wcount*taiwandata$Asahi
taiwandata$statecount_w_yomiuri<-taiwandata$taiwan*taiwandata$wcount*taiwandata$Yomiuri
taiwandata$statecount_w_both<-taiwandata$statecount_w_asahi*(4/9)+taiwandata$statecount_w_yomiuri*(5/9)

## South East Asia ##
seasiadata<-subset(hldata, seasia==1)
seasiadata$statecount_w<-seasiadata$seasia*seasiadata$wcount
seasiadata$statecount_w_asahi<-seasiadata$seasia*seasiadata$wcount*seasiadata$Asahi
seasiadata$statecount_w_yomiuri<-seasiadata$seasia*seasiadata$wcount*seasiadata$Yomiuri
seasiadata$statecount_w_both<-seasiadata$statecount_w_asahi*(4/9)+seasiadata$statecount_w_yomiuri*(5/9)

## Central (Middle) South America ##
msamericadata<-subset(hldata, msamerica==1)
msamericadata$statecount_w<-msamericadata$msamerica*msamericadata$wcount
msamericadata$statecount_w_asahi<-msamericadata$msamerica*msamericadata$wcount*msamericadata$Asahi
msamericadata$statecount_w_yomiuri<-msamericadata$msamerica*msamericadata$wcount*msamericadata$Yomiuri
msamericadata$statecount_w_both<-msamericadata$statecount_w_asahi*(4/9)+msamericadata$statecount_w_yomiuri*(5/9)

## Oceania ##
oceaniadata<-subset(hldata, oceania==1)
oceaniadata$statecount_w<-oceaniadata$oceania*oceaniadata$wcount
oceaniadata$statecount_w_asahi<-oceaniadata$oceania*oceaniadata$wcount*oceaniadata$Asahi
oceaniadata$statecount_w_yomiuri<-oceaniadata$oceania*oceaniadata$wcount*oceaniadata$Yomiuri
oceaniadata$statecount_w_both<-oceaniadata$statecount_w_asahi*(4/9)+oceaniadata$statecount_w_yomiuri*(5/9)

## Africa ##
africadata<-subset(hldata, africa==1)
africadata$statecount_w<-africadata$africa*africadata$wcount
africadata$statecount_w_asahi<-africadata$africa*africadata$wcount*africadata$Asahi
africadata$statecount_w_yomiuri<-africadata$africa*africadata$wcount*africadata$Yomiuri
africadata$statecount_w_both<-africadata$statecount_w_asahi*(4/9)+africadata$statecount_w_yomiuri*(5/9)


####################
## Save Full Data ##
####################

## Remove Objects from v2_Data2_HeadlineWrdMat.RData ##
rm(inclwrd, target, search,i,createWrdMat,
   coding_data,sample_selection,samplepoprows,
   uscode_neg,chncode_neg,skocode_neg,nkocode_neg,
   uscode_pos,chncode_pos,skocode_pos,nkocode_pos,
   usWrdMat,chnWrdMat,skorWrdMat,nkorWrdMat,
   MecabRes, usMecabRes, chnMecabRes, skorMecabRes, nkorMecabRes)

## Save Data Image ##
save.image("data/v3_Data4_HeadlineSubset.RData")

