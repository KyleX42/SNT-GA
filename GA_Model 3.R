library(dplyr) 
library(scales)
install.packages("fredr")
#devtools::install_github("sboysel/fredr")
library(fredr)
library(readr) 
library(glmnet) 
library(readxl)
library(xts) 
library(xlsx)
library(plyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


##########################
#Pollster~BardwellModel+Macro Economic+Gtrends
#2008-2020
#########################

#Retrieve Data from FRED in logarithmic figures
FredGeneratorlog <- function(code){
  a = NULL
  a = fredr(series_id = code,units = 'log') %>%
    filter(!is.na(value))
  a = (a[-2])
  colnames(a) = c("date",code)
  
  tail(a)
  return(a)
}

FredGenerator<- function(code){
  a = NULL
  a = fredr(series_id = code) %>%
    filter(!is.na(value))
  a = (a[-2])
  colnames(a) = c("date",code)
  
  tail(a)
  return(a)
}

#Merge function
MGE<-function(a,b){
  DF<- merge(x=a, y=b,by.x=c("year"),by.y=c("year" ))
  return(DF)
}

#Filter and select the Max
FTmx <- function(x) {
  a = NULL
  a = rbind(
    apply(x[which(x[1]>="2008-08-01" & x[1]<"2008-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2010-08-01" & x[1]<"2012-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2014-08-01" & x[1]<"2014-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2016-08-01" & x[1]<"2018-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2020-08-01" & x[1]<"2020-10-31"),2], 2, max,na.rm=TRUE))
  year = c('2008','2010','2014','2016','2020')
  a = cbind(year,a)
  return(a)
}
# If only annual data is available
FTJan <- function(x) {
  a = NULL
  a = rbind(
    apply(x[which(x[1]>="2008-01-01" & x[1]<"2008-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2010-01-01" & x[1]<"2012-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2014-01-01" & x[1]<"2014-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2016-01-01" & x[1]<"2018-10-31"),2], 2, max,na.rm=TRUE),
    apply(x[which(x[1]>="2020-01-01" & x[1]<"2020-10-31"),2], 2, max,na.rm=TRUE))
  year = c('2008','2010','2014','2016','2020')
  a = cbind(year,a)
  return(a)
}

#Calculate the Growth
FTG <- function(x) {
  a = NULL
  a = rbind(
    apply(x[which(x[1]>="2008-01-01" & x[1]<"2008-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2010-01-01" & x[1]<"2012-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2014-01-01" & x[1]<"2014-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2016-01-01" & x[1]<"2018-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2020-01-01" & x[1]<"2020-3-31"),2], 2, mean,na.rm=TRUE))
  b = NULL
  b = rbind(
    apply(x[which(x[1]>="2007-01-01" & x[1]<"2007-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2009-01-01" & x[1]<"2009-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2013-01-01" & x[1]<"2013-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2015-01-01" & x[1]<"2015-3-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2019-01-01" & x[1]<"2019-3-31"),2], 2, mean,na.rm=TRUE))
  year = c('2008','2010','2014','2016','2020')
  c = (a-b)/b
  z = cbind(year,c)
  return(z)
}

FTmb <- function(x) {
  a = NULL
  a = rbind(
    apply(x[which(x[1]>="2008-08-01" & x[1]<"2008-10-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2010-08-01" & x[1]<"2012-10-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2014-08-01" & x[1]<"2014-10-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2016-08-01" & x[1]<"2018-10-31"),2], 2, mean,na.rm=TRUE),
    apply(x[which(x[1]>="2020-08-01" & x[1]<"2020-10-31"),2], 2, mean,na.rm=TRUE))
  year = c('2008','2010','2014','2016','2020')
  a = cbind(year,a)
  return(a)
}

#A larger Combination Function
CBN<-function(a,b,c,d,e,f){
  x = MGE(a,b)
  y = MGE(c,d)
  z = MGE(e,f)
  k = MGE(x,y)
  l = MGE(k,z)
  
  return(l)
}


fredr_set_key("e850e3d306e0e2c8517f4afe9564b69e")

#Input Bardwell model
GA = read_excel("GA.xlsx")

#Input Google Trends and Polls
GA_Polls_Trends <- read_csv("GA_Polls+Trends.csv")


#Input Polls
Polls = read_excel("PollsGA.xlsx")

#Merge them
DF<- merge(x=Polls, y=GA_Polls_Trends,by.x=c("Poll_Index"),by.y=c("Poll_Index" ))
DF= merge(x=GA, y=DF, by.x = c("year"),by.y = c("year"))

head(DF)

#Apply POTUS multiplier
POTUS <- read_excel("POTUS_Multiplier.xlsx")

# Import Housing 
HousePriceIndex <-FredGenerator("GASTHPI")
tail(HousePriceIndex)


HousePriceIndexSVA <- FredGenerator("ATNHPIUS42340Q")
tail(HousePriceIndexSVA)

HousePriceIndexFTN = FredGenerator("ATNHPIUS13121A")
head(HousePriceIndexFTN)

HousePriceIndexCTG = FredGenerator("ATNHPIUS16860Q")
head(HousePriceIndexCTG)


HousePriceIndexCLN = FredGenerator("ATNHPIUS13063A")
head(HousePriceIndexCLN)

HousePriceIndexAUG = FredGenerator("ATNHPIUS12260Q")
head(HousePriceIndexAUG)
###
HousePriceIndexOco = FredGenerator("ATNHPIUS13219A")
head(HousePriceIndexOco)

HousePriceIndexMus = FredGenerator("ATNHPIUS13215A")
head(HousePriceIndexMus)

HousePriceIndexCob = FredGenerator("ATNHPIUS13067A")
head(HousePriceIndexCob)

HousePriceIndexMcB = FredGenerator("ATNHPIUS31420Q")
head(HousePriceIndexMcB)

HousePriceIndexAtC = FredGenerator("ATNHPIUS12020Q")
head(HousePriceIndexAtC)

HomeownerhipRate = FredGenerator("GAHOWN")
head(HomeownerhipRate)

Housing1=CBN(FTG(HousePriceIndex),FTG(HousePriceIndexSVA),
             FTG(HousePriceIndexFTN),FTG(HousePriceIndexCTG),
             FTG(HousePriceIndexCLN),FTG(HousePriceIndexAUG))

Housing2=CBN(FTG(HousePriceIndexOco),FTG(HousePriceIndexMus),
             FTG(HousePriceIndexCob),FTG(HousePriceIndexMcB),
             FTG(HousePriceIndexAtC),FTG(HomeownerhipRate))



Housing = MGE(Housing1,Housing2)
Housing <- as.data.frame(sapply(Housing, as.numeric)) 
#Housing[-1]=Housing[-1]*unlist(POTUS$POTUS)

# Import Income

GDP=FredGenerator("GANGSP")
GDPIND = FredGenerator("GANQGSP")
CoincEconAct=FredGenerator("GAPHCI")
LeadingIndex=FredGenerator("GASLIND")

HourlyEarningsAll = FredGenerator("SMU13000000500000003SA")
HourlyEarningsCst = FredGenerator("SMU13000002000000003SA")
HourlyEarningsOth = FredGenerator("SMU13000008000000003SA")
HourlyEarningsFin = FredGenerator("SMU13000005500000003SA")
HourlyEarningsLnH = FredGenerator("SMU13000007000000003SA")
HourlyEarningsGP = FredGenerator("SMU13000000600000003SA")
HourlyEarningsEnH = FredGenerator("SMU13000006500000003SA")
HourlyEarningsTTU = FredGenerator("SMU13000004000000003SA")
HourlyEarningsPnB = FredGenerator("SMU13000006000000003SA")
HourlyEarningsMnf = FredGenerator("SMU13000003000000003SA")
HourlyEarningsNDG = FredGenerator("SMU13000003200000008SA")


TotalNonFarm = FredGenerator("GANA")
Income = FredGenerator("GAPCPI")

MinWage = FredGenerator("STTMINWGGA")
ConstructionWage=FredGenerator("GAWCON")

Income1=CBN(FTG(GDP),FTG(GDPIND),
            FTG(CoincEconAct),FTG(LeadingIndex),
            FTG(HourlyEarningsAll),FTG(HourlyEarningsCst))
Income2=CBN(FTG(HourlyEarningsOth),FTG(HourlyEarningsFin),
            FTG(HourlyEarningsLnH),FTG(HourlyEarningsGP),
            FTG(HourlyEarningsEnH),FTG(HourlyEarningsTTU))
Income3=CBN(FTG(HourlyEarningsPnB),FTG(HourlyEarningsMnf),
            FTG(HourlyEarningsNDG),FTG(TotalNonFarm),
            FTG(Income),FTG(MinWage))
Income4=MGE(Income1,FTG(ConstructionWage))
Income5=MGE(Income2,Income3)

Income=MGE(Income4,Income5)

Income <- as.data.frame(sapply(Income, as.numeric)) 

#Income[-1]=Income[-1]*unlist(POTUS$POTUS)

head(Income)
Sys.sleep(10)

# Import Labor force and Unemployment 
UN = FredGenerator("GAUR")
UNASSR = FredGenerator("ATLA013URN")
UNFLN = FredGenerator("GAFULT1URN")
UNGWN = FredGenerator("GAGWIN7URN")
UNCob = FredGenerator("GACOBB0URN")
UNChat = FredGenerator("CHAT847URN")

UNGLYN = FredGenerator("GAGLYN5URN")
UNCHM = FredGenerator("GACHAT9URN")
UNHALL = FredGenerator("GAHALL0URN")
UNATHE = FredGenerator("ATHE013URN")
UNWhit = FredGenerator("GAWHIT5URN")
UNDeKalb = FredGenerator("GADEKA9URN")

UNSVN = FredGenerator("SAVA313URN")
UNFys = FredGenerator("GAFORS7URN")
UNHRY = FredGenerator("GAHENR1URN")
UNAUG = FredGenerator("AUGU213URN")
UNBULL = FredGenerator("GABULL1URN")
UNCTS = FredGenerator("GACATO7URN")

UNWKL = FredGenerator("GAWALK0URN")
UNRMA = FredGenerator("ROME613URN")
UNCRL = FredGenerator("GACARR0URN")
UNCRK = FredGenerator("GACHER5URN")
UNPAL = FredGenerator("GAPAUL3URN")
UNMTC = FredGenerator("GAMITC5URN")

UNFYT = FredGenerator("GAFAYE3URN")
UNMCN = FredGenerator("MACO413URN")
UNMTV = FredGenerator("GACLAR5URN")
UNWRE = FredGenerator("GAWARE9URN")
UNCOL = FredGenerator("GACOLQ1URN")
UNTBS = FredGenerator("GATOOM9URN")

UNCLT = FredGenerator("GACLAY5URN")
UNWLT = FredGenerator("GAWALT7URN")
UNCWT = FredGenerator("GACOWE7URN")
UNNTN= FredGenerator("GANEWT7URN")
UNWHE = FredGenerator("GAWHEE9URN")
LaborForce = FredGenerator("LBSSA13")

UNe1 = CBN(FTmb(UN),FTmb(UNASSR),
           FTmb(UNFLN),FTmb(UNGWN),
           FTmb(UNCob),FTmb(UNChat))
  
UNe2 = CBN(FTmb(UNGLYN),FTmb(UNCHM),
           FTmb(UNHALL),FTmb(UNATHE),
           FTmb(UNWhit),FTmb(UNDeKalb))
UNe3 = CBN(FTmb(UNSVN),FTmb(UNFys),
           FTmb(UNHRY),FTmb(UNAUG),
           FTmb(UNBULL),FTmb(UNCTS))
UNe4 = CBN(FTmb(UNWKL),FTmb(UNRMA),
           FTmb(UNCRL),FTmb(UNCRK),
           FTmb(UNPAL),FTmb(UNMTC))

UNe5 = CBN(FTmb(UNFYT),FTmb(UNMCN),
          FTmb(UNMTV),FTmb(UNWRE),
          FTmb(UNCOL),FTmb(UNTBS))

UNe6 = CBN(FTmb(UNCLT),FTmb(UNWLT),
          FTmb(UNCWT),FTmb(UNNTN),
          FTmb(UNWHE),FTmb(LaborForce))

Unemployment = CBN(UNe1,UNe2,UNe3,UNe4,UNe5,UNe6)

Unemployment <- as.data.frame(sapply(Unemployment, as.numeric)) 

#Unemployment[-1]=Unemployment[-1]*unlist(POTUS$POTUS)
Unemployment=Unemployment[!duplicated(Unemployment),]

tail(Unemployment)


# Import Population

ResidentPopulation = FredGeneratorlog("GAPOP")
PMDFl = FredGeneratorlog("CDC20N2UAA013121")
PMDkb = FredGeneratorlog("CDC20N2U013089")
PMDCu = FredGeneratorlog("CDC20N2U037051")
PMDHl = FredGeneratorlog("CDC20N2U013139")
PopATL = FredGeneratorlog("ATLPOP")

PopD = FredGeneratorlog("MACPOP")
PopMB= FredGeneratorlog("RCYPOP")
PopFTN = FredGeneratorlog("GAFULT1POP")
PopVL = FredGeneratorlog("VLDPOP")
PopRMD = FredGeneratorlog("GARICH5POP")
PopCHAT = FredGeneratorlog("GACHAT9POP")

PopAUG = FredGeneratorlog("AUGPOP")
PopTWG = FredGeneratorlog("GATWIG9POP")
PopPDG = FredGeneratorlog("GAPAUL3POP")
PopCOBB = FredGeneratorlog("GACOBB0POP")
PopGWN = FredGeneratorlog("GAGWIN7POP")
PopFYT = FredGeneratorlog("GAFAYE3POP")

PopBTR = FredGeneratorlog("GABART5POP")
PopCOW = FredGeneratorlog("GACOWE7POP")
PopDKB = FredGeneratorlog("GADEKA9POP")
PopWLX = FredGeneratorlog("GAWILC5POP")
PopWLR = FredGeneratorlog("GAWHEE9POP")
PopTNS = FredGeneratorlog("GATOWN1POP")

PopTMS = FredGeneratorlog("GATHOM5POP")
PopTLR = FredGeneratorlog("GATAYL9POP")
PopSTR = FredGeneratorlog("GASUMT1POP")
PopSCY = FredGeneratorlog("GASCHL9POP")
PopRBN = FredGeneratorlog("GARABU1POP")
PopPLSK = FredGeneratorlog("GAPULA5POP")

PopPRS = FredGeneratorlog("GAPIER9POP")
PopMGN = FredGeneratorlog("GAMORG1POP")
PopMTL = FredGeneratorlog("GAMACO3POP")
PopJHN = FredGeneratorlog("GAJOHN7POP")
PopHRT = FredGeneratorlog("GAHART7POP")
PopHANC = FredGeneratorlog("GAHANC1POP")


Pop1 = CBN(FTJan(ResidentPopulation),FTJan(PMDFl),
           FTJan(PMDkb),FTJan(PMDCu),
           FTJan(PMDHl),FTJan(PopATL))

Pop2 = CBN(FTJan(PopD),FTJan(PopMB),
           FTJan(PopFTN),FTJan(PopVL),
           FTJan(PopRMD),FTJan(PopCHAT))

Pop3 = CBN(FTJan(PopBTR),FTJan(PopCOW),
           FTJan(PopDKB),FTJan(PopWLX),
           FTJan(PopWLR),FTJan(PopTNS))

Pop4 = CBN(FTJan(PopTMS),FTJan(PopTLR),
           FTJan(PopSTR),FTJan(PopSCY),
           FTJan(PopRBN),FTJan(PopPLSK))

Pop5 = CBN(FTJan(PopPRS),FTJan(PopMGN),
           FTJan(PopMTL),FTJan(PopJHN),
           FTJan(PopHRT),FTJan(PopHANC))

Pop6 = CBN(FTJan(PopAUG),FTJan(PopTWG),
           FTJan(PopPDG),FTJan(PopCOBB),
           FTJan(PopGWN),FTJan(PopFYT))

Population = CBN(Pop1,Pop2,Pop3,Pop4,Pop5,Pop6)



Population <- as.data.frame(sapply(Population, as.numeric)) 




# Import Employees


SNAP = FredGeneratorlog("BRGA13M647NCEN")
EmC = FredGeneratorlog("GACONS")
EmLH = FredGeneratorlog("GALEIH")
EmND = FredGeneratorlog("SMU13000002023800001SA")
EmInfo = FredGeneratorlog("GAINFO")
EmOs = FredGeneratorlog("GASRVO")

EmG = FredGeneratorlog("GAGOVT")
EmFIN = FredGeneratorlog("GAFIRE")
EmSv = FredGeneratorlog("SMS13000000700000001")
EmPv = FredGeneratorlog("SMS13000000500000001")
EmU = FredGeneratorlog("SMU13000004322000001A")
EmRs = FredGeneratorlog("SMU13000005553000001A")

EmMLg = FredGeneratorlog("SMS13000001000000001")
EmHs = FredGeneratorlog("SMU13000006562200001A")
EmTk = FredGeneratorlog("SMU13000004348400001A")
EmEdu = FredGeneratorlog("SMU13000006561000001A")
EmGd = FredGeneratorlog("SMS13000000600000001")
EmPB = FredGeneratorlog("GAPBSV")

Emp1 = CBN(FTmb(SNAP),FTmb(EmC),FTmb(EmLH),FTJan(EmND),FTmb(EmInfo),FTmb(EmOs))
Emp2 = CBN(FTmb(EmG),FTmb(EmFIN),FTmb(EmSv),FTmb(EmPv),FTJan(EmU),FTJan(EmRs))
Emp3 = MGE(Emp1,Emp2)
Emp4 = CBN(FTmb(EmMLg),FTJan(EmHs),FTJan(EmTk),FTJan(EmEdu),FTmb(EmGd),FTmb(EmPB))
Employee = MGE(Emp4,Emp3)

head(Employee)

v1 = MGE(Housing,Income)
v2 = MGE(Unemployment,Population)
Macro = MGE(MGE(MGE(Housing,Income),MGE(Unemployment,Population)),Employee)




GA = MGE(DF,Macro)
InP = GA["IncumbentPoll"] 
year = GA["year"]

GA = GA[-c(8:12)]
GA = GA[-1]
GA = cbind(year,InP,GA)

#######################################




#Run Regressions

alphaValue = 1 # Lasso 
alphaValue = 0.5 # Enet 
GA <- as.data.frame(sapply(GA, as.factor)) 

GA[is.na(GA)] <- 0
fortify.zoo(GA) %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x))  
GA[GA=="-Inf"]<-0
GA[GA=="Inf"]<-0

y = GA$IncumbentPoll
x = GA[,-c(1:2)] 

head(y)
head(x)
y1 = as.numeric(y>0.5)
GA$year[42]

#Enet-til early 2020
fitEnet = glmnet(data.matrix(x[-c(44:60),]), y[-c(44:60)], alpha = 0.5) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[44,-c(1:2)]), type="response") 
pred
#Enet-til the latest
AfitEnet = glmnet(data.matrix(x[-60,]), y[-60], alpha = 0.5) 
pred = predict(AfitEnet, s=0.01, newx=data.matrix(GA[60,-c(1:2)]), type="response") 
pred

#Enet-CV-til the latest
cvfit = cv.glmnet(data.matrix(x[-60,]), y1[-60], alpha = 0.5)
plot(cvfit)
cvfit$lambda.min 
pred = predict(fitEnet, s=cvfit$lambda.min, newx=data.matrix(GA[60,-c(1:2)]), type="response") 
pred

#LASSO-til early 2020
fitEnet = glmnet(data.matrix(x[-c(44:60),]), y[-c(44:60)], alpha = 1) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[41,-c(1:2)]), type="response") 
pred
#Enet-til the latest
fitEnet = glmnet(data.matrix(x[-60,]), y[-60], alpha = 1) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[60,-c(1:2)]), type="response") 
pred

#LASSO-CV
cvfit = cv.glmnet(data.matrix(x[-60,]), y1[-60], alpha = 1)
plot(cvfit)
cvfit$lambda.min 
pred = predict(fitEnet, s=cvfit$lambda.min, newx=data.matrix(GA[60,-c(1:2)]), type="response") 
pred

#
coef2020 = coef(AfitEnet,s=0.01) 
write.csv(coef2020[,1],"coefficient2020GA.csv")


# Create word clouds 

coeff = read.csv("coefficient2020GA.csv") 

set.seed(1234)
wordcloud(words = coeff$X[-1], freq = abs(coeff$x[-1]), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#######################################
##Predict 2016
#Enet
GA$year[42]
fitEnet = glmnet(data.matrix(x[-c(40:60),]), y[-c(40:60)], alpha = 0.5) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[40,-c(1:2)]), type="response") 
pred
#Lasso
fitEnet = glmnet(data.matrix(x[-c(40:60),]), y[-c(40:60)], alpha = 1) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[40,-c(1:2)]), type="response") 
pred
#######################################

#######################################
##Predict 2014
GA$year[26]
fitEnet = glmnet(data.matrix(x[-c(29:60),]), y[-c(29:60)], alpha = 0.5) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[29,-c(1:2)]), type="response") 
pred
#Lasso
fitEnet = glmnet(data.matrix(x[-c(29:60),]), y[-c(29:60)], alpha = 1) 
pred = predict(fitEnet, s=0.01, newx=data.matrix(GA[29,-c(1:2)]), type="response") 
pred
#######################################







