# Example preprocessing script.


library(dplyr)
library(stringr)
library(here)
library(EndoMineR)
library(readxl)
library(tidyr)
library(PhysiMineR)
library(finalfit)
library(knitr)
library(kableExtra)
library(cutpointr)
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)
library(gtsummary)
library(gt)
library(ggpubr)
library(flextable)
library(knitr)
library(here)




##### Clean ####################################################################################################################
## @knitr dataClean
#Data cleaning
#Use after PhysiData_Acq
#Clean HRM
#Clean up the HRM swallows:

HRMImportMainAperi <- read_excel(here::here("inst/Projects/MotilityConditionQuestions/Aperistalsis/data/HRMImportMain.xlsx"))
HRMImportSwallowsAperi <- read_excel(here::here("inst/Projects/MotilityConditionQuestions/Aperistalsis/data/HRMImportSwallows.xlsx"))
HRMImportSwallowsAperi<-HRMImportSwallowsAperi[!is.na(HRMImportSwallowsAperi$panesophagealpressurizationMapSwallowsNum8),]
AllFundos <- read_excel(here::here("inst/Projects/MotilityConditionQuestions/Aperistalsis/data/AllFundoplication.xlsx"))


##### IntraTestMergeAndClean ########################################################################################
## @knitr dataIntraTestMerge



#Diag<-dataDiagClean(Diag)


#Merge the HRM results together (swallows and Main)
AllHRMAperi<-merge(HRMImportMainAperi,HRMImportSwallowsAperi,by="HRM_Id",all=TRUE)

#Get rid of the swallows for now as using the whole HRM dataset makes things slow:
AllHRMAperi<-AllHRMAperi[,!grepl("Num\\d+",colnames(AllHRMAperi))]
#Roll up the repeats in to one:
AllHRMAperiTwo<-AllHRMAperi %>%
  dplyr::group_by(HospNum_Id,DistalLESfromnarescm)%>%
  dplyr::summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))


#Clean Up the main HRM
AllHRMAperiTwo<-HRMCleanUp1(AllHRMAperiTwo)

#Extrapolate so you get the aperistaltic patients:

AperiAll<-PhysiMineR::HRMDiagnoses(AllHRMAperiTwo)
#Subset the data so yuo only get the aperistaltic patients:

AperiAllOnly<-AperiAll%>%filter(grepl("Absent",dx,ignore.case = T))


#Now just need to cross reference with Galaxy to see who got fundoplications (obtained via remote server STH Analytics)
AperiAllOnly<-AperiAllOnly%>%rename(TRUSTID=HospNum_Id)

#Merge the two data sets together:
FInalAperisFundos<-AperiAllOnly%>%inner_join(AllFundos,by="TRUSTID")

writexl::write_xlsx(FInalAperisFundos,here("inst","Projects","MotilityConditionQuestions","Aperistalsis","reports","FinalAperiFundos.xlsx"))
