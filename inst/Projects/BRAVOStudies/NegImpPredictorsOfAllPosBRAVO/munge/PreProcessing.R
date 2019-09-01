library(dplyr)
library(stringr)
library(here)
library(EndoMineR)
library(DiagrammeR)
library(readxl)
library(tidyr)
library(PhysiMineR)
library(DataExplorer)
library(psych)
library(finalfit)
library(knitr)
library(kableExtra)
library(cutpointr)
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)

##### Import ####
## @knitr dataImport

oldw <- getOption("warn")
options(warn = -1)

#Get this from the data folder for PhysiMineR-  just click on the RData to load into
#the global environment

#Or if using the local file system

BravoDayOneAndTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BravoDay1And2.xls"))
BravoDayThreeAndFour <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BravoDay3And4.xls"))
BRAVOTotal <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BRAVOTotal.xls"))
Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Diag.xlsx"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
ImpedanceTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Impedance2.xls"))

options(warn = oldw)




##### Clean ####################################################################################################################
## @knitr dataClean
#Data cleaning
#Use after PhysiData_Acq
#Clean HRM
#Clean up the HRM swallows:
HRMImportSwallows<-HRMImportSwallows[!is.na(HRMImportSwallows$panesophagealpressurizationMapSwallowsNum8),]

#Clean Diag
Diag$HospNum_Id<-toupper(Diag$HospNum_Id)
Diag$FileCreationDate<-as.Date(gsub("(\\d{4}-\\d{2}-\\d{2}).*","\\1",Diag$FileCreationDate))
#Convert VisitDate into a Date format and extract the year (the month and day are unreliable here as sometimes
#American and sometimes British dates are used)

#Convert the dates to date format
Diag$VisitDate2<-as.Date(Diag$VisitDate,format="%d_%m_%Y")
#For NA dates make sure it is not because the dates are the wrong way around
Diag$VisitDate3<-as.Date(Diag$VisitDate,format="%m_%d_%Y")

#Merge the dates if there are separate HRM and Diag results which are reporting the same thing
Diag$VisitDate4<-as.Date(ifelse(is.na(Diag$VisitDate2),Diag$VisitDate3,Diag$VisitDate2),origin = "1970-01-01")
#If still NA then use the file creation date as the visit date
Diag$VisitDate4<-as.Date(ifelse(is.na(Diag$VisitDate4),Diag$FileCreationDate,Diag$VisitDate4),origin = "1970-01-01")

#Extract the Distal LES as is likely to be in both the final and the HRM report so can be merged on this
Diag$DistalLESnares<-gsub(".*Distal LES from nares.*?(\\d+).*","\\1",Diag$WholeReport)
#Get rid of whole reports that are copied over for some reason
Diag$DistalLESnares<-gsub(".*[A-Za-z].*","\\1",Diag$DistalLESnares)

Diag<-data.frame(Diag, stringsAsFactors = FALSE)













##### Merge ########################################################################################
## @knitr dataIntraTestMerge


AllImpedance<-merge(ImpedanceTwo,Imp_Symp,by="Imp_Id", all = TRUE)

#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)

#Merge the BRAVO results together:
AllBravo<-merge(BravoDayOneAndTwo,BravoDayThreeAndFour,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")
AllBravo<-dataBRAVOSymptoms(AllBravo)













##### Clean2 ############################################################################################################
## @knitr dataClean2


#Remove duplicate reports on the basis of being the same Hospital Number and date range here
#Also need to get rid of the duplicates from the Diag table:


Diag<-Diag %>%
  arrange(DistalLESnares,HospNum_Id) %>%
  group_by(DistalLESnares,HospNum_Id,lubridate::year(VisitDate4)) %>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))

#Take only the first report date here:
Diag$VisitDate4<-gsub(":.*","",Diag$VisitDate4)
Diag$VisitDate4<-as.Date(Diag$VisitDate4)

#Get rid of extra columns:
Diag$VisitDate<-NULL
Diag$VisitDate2<-NULL
Diag$VisitDate3<-NULL
Diag<-Diag%>% rename(VisitDate=VisitDate4)


#Clean up the HRM HRMImportMain (to get rid of duplicates mainly)
#This looks for all the duplicates by hospital number and DistalLESfromnarescm
#(the latter so that duplicates means same test rather than same patient on two different days)

#Get rid of the swallows for now as using the whole HRM dataset makes things slow:
AllHRM<-AllHRM[,!grepl("Num\\d+",colnames(AllHRM))]

HRMImportMainTwo<-AllHRM %>%
  group_by(HospNum_Id,DistalLESfromnarescm)%>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))

#Clean Up the main HRM
HRMImportMainTwo<-HRMCleanUp1(HRMImportMainTwo)
#Not sure why I did this.
HRMImportMainTwo$LOS_relax<-as.factor(HRMImportMainTwo$LOS_relax)
HRMImportMainTwo$LowerOesoph<-as.factor(HRMImportMainTwo$LowerOesoph)


# Get the whole impedance dataset
#You will need to re-clean the merged dataImpSympImpedance as cleaning function needed to be fixed
ImpedanceThree<-dataImpClean(AllImpedance)
#Extract all the symptoms
ImpAll<-dataImpSymptoms(ImpedanceThree)
ImpAll<-GORD_AcidImp(ImpAll)

#Need to change this so that is part of the PhysiMineR codebase (when I can get AWS to stop crashing of course)
AllBravo<-GORD_AcidBRAVO(AllBravo)













##### Merge ########################################################################################
## @knitr dataCrossTestMerge


#Merge all HRM with AllBravo to get the full dataset
#This merges the tests so that the nearest hrm to the BRAVO is chosen to avoid replication
#Note that a lot of Bravo's do not have HRM as may not have been requested by outside referrers



#Impedance Merges:
ImpAndhrm <- HRMImportMainTwo %>% inner_join(ImpAll, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

ImpAndBravo <- AllBravo %>% inner_join(ImpAll, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

ImpAnddiag <- Diag %>% inner_join(ImpAll, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)


#BRAVO Merges:
BravoAnd_hrm <- HRMImportMainTwo %>% inner_join(AllBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

BravoAnd_diag <- Diag %>% inner_join(AllBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)


#Diag Merges:
HRMAnd_diag <- HRMImportMainTwo %>% inner_join(Diag, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)


#Three test merges:
ImpAndBravoWithHRM <- HRMImportMainTwo %>% inner_join(ImpAndBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)













##### Extrapolate ############################################################################################################
## @knitr dataForking
#Extract the negative impedance



#Recoding the impedance reflux columns:

ImpAndBravoWithHRM$AcidReflux_Imp<-gsub("NoAcid",0,ImpAndBravoWithHRM$AcidReflux_Imp)
ImpAndBravoWithHRM$AcidReflux_Imp<-gsub(".*Acid",1,ImpAndBravoWithHRM$AcidReflux_Imp)
ImpAndBravoWithHRM$AcidReflux_Imp<-as.numeric(ImpAndBravoWithHRM$AcidReflux_Imp)

#Recoding the BRAVO rewflux columns:
ImpAndBravoWithHRM$AcidRefluxBRAVO<-gsub("NoAcid",0,ImpAndBravoWithHRM$AcidRefluxBRAVO)
ImpAndBravoWithHRM$AcidRefluxBRAVO<-gsub(".*Acid",1,ImpAndBravoWithHRM$AcidRefluxBRAVO)
ImpAndBravoWithHRM$AcidRefluxBRAVO<-as.numeric(ImpAndBravoWithHRM$AcidRefluxBRAVO)

#Get the age of the patient at the time of doing the test:
library(eeptools)
ImpAndBravoWithHRM$ageInYears<-age_calc(na.omit(ImpAndBravoWithHRM$MainPtDataDateofBirth),ImpAndBravoWithHRM$MainPtDataDateofAdmission,precise=TRUE)/12


#Get all the Negative Impedance patients (with both positive and negative BRAVO results-#AcidRefluxBRAVO is the outcome variable we will use;)
NegImp_FromImpWithBRavoAndHRM<-ImpAndBravoWithHRM %>% filter(AcidReflux_Imp==0)

#Store the results so can do BRAVO studies demographics later in the BRAVO description part:
ForBRAVODescriptionLater<-NegImp_FromImpWithBRavoAndHRM


#Get rid of all the BRAVO columns as we don't need these as predictive variables
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Dd]ay" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Ss]wallo" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]



##### BRAVO description #############################################
## @knitr BRAVOdescription
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)

#need to replicate Sweis paper and show 1. SAP worst day =63.2% 2. SI Worst day =61% 3. pH worst day <4 (>4.2%) =47% and most in the first 24 hours 4. 45% GERD based on worst day SAP or pH <4

#Use Fraction <pH4 as the analysis:
MyBravoDays<-ForBRAVODescriptionLater[,grepl("FractionTimepHLessThan4Total|AcidRefluxBRAVO|SAP|SI",names(ForBRAVODescriptionLater))]
MyBravoDays<-MyBravoDays[,!grepl("SxMain|MainSx",names(MyBravoDays))]

MyBravoDays<-MyBravoDays[,!grepl("ReflDayTotal",names(MyBravoDays))]

#Only select the GORD patients by pH<4
MyBravoDays$SIDay1Heartburn<-as.numeric(MyBravoDays$SIDay1Heartburn)
MyBravoDays$average<-rowMeans(select_if(MyBravoDays, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), na.rm = TRUE)
MyBravoDays$worst<-do.call(pmax, c(select_if(MyBravoDays, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), na.rm = TRUE))

#Need to get which day was the worst:
MyBravoDays$worstDaypH<-names(select_if(MyBravoDays, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")))[max.col(replace(select_if(MyBravoDays, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), is.na(select_if(MyBravoDays, is.numeric)%>%select(contains("FractionTimepHLessThan4Total"))),0),ties.method = "first")]
#Clean up the days:
MyBravoDays$worstDaypH<-gsub("FractionTimepHLessThan4Total","",MyBravoDays$worstDaypH)
MyBravoDays$worstDaypH<-gsub("ReflDay","",MyBravoDays$worstDaypH)
MyBravoDays$worstDaypH<-gsub("1_2","3",MyBravoDays$worstDaypH)
MyBravoDays$worstDaypH<-gsub("2_2","4",MyBravoDays$worstDaypH)
MyBravoDays$worstDaypH<-gsub("3_2","5",MyBravoDays$worstDaypH)

MyBravoDays$worstDaypH<-as.numeric(MyBravoDays$worstDaypH)
MyBravoDaysAnydayPositiveWDA<-MyBravoDays %>%select(contains("2FractionTimepHLessThan4Total")) %>%filter_all(any_vars(. >4.9))

MyBravoDays$Day2Pos<-ifelse(MyBravoDays$ReflDay2FractionTimepHLessThan4Total>4.2,"Day2Pos","Day2Neg")

#Positive GORD by pH<4 in 4.2% Worst day overall analysis-
pHWDA<-(nrow(MyBravoDays[MyBravoDays$worst>4.9,])/nrow(MyBravoDays))*100

#Positive GORD by SAP Worst day overall analysis-
SAPWDA<-(nrow(MyBravoDays %>%select(contains("SAP")) %>%filter_all(any_vars(. >94.9)))/nrow(MyBravoDays))*100

#Positive GORD by SI Worst day overall analysis-
SIWDA<-(nrow(MyBravoDays %>%select(contains("SI")) %>%filter_all(any_vars(. >50)))/nrow(MyBravoDays))*100


MyBravoDaysPo<-MyBravoDays[MyBravoDays$AcidRefluxBRAVO==1,]
#Plot for the GORD Positive BRAVOs only

a1<-ggplot(MyBravoDaysPo, aes(x=log(average))) +
  geom_density()


a2<-ggplot(MyBravoDaysPo,aes(worstDaypH))+geom_histogram(binwidth = 1)+stat_bin()


a3<-ggplot(MyBravoDaysPo, aes(x = "",y=average)) +
  geom_boxplot(outlier.shape = NA)+geom_beeswarm(size=2,priority='descending',cex = 2)


grid.arrange(a1,a2,a3,nrow=1)









##### Clean columns with minimal data for data quality ############################################
## @knitr missingClean



#Remove columns that are not meaningful:
#Impedance symptoms:
#Get all the MainAcidExposure/MainSx columns as your main impedance dataset. Note this also gets rid of composite score(=DeMeester) as it is derived from MainAcidExp so likely to correlate too much for regression
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRM[,grepl("ageInYears|MainAcidEx|MainSx|SxMain|HospNum_Id|DistalLESfromnarescm|Gender|DOBAge|Height|LESlengthcm|PIPfromnarescm|Hiatalhernia|BasalrespiratoryminmmHg|BasalrespiratorymeanmmHg||DistalcontractileintegralmeanmmHgcms|Contractilefrontvelocitycms|IntraboluspressureATLESRmmHg|Distallatency|failedChicagoClassification|panesophagealpressurization|largebreaks|Simultaneous|prematurecontraction|rapidcontraction|smallbreaks|VisitDate|Age|AcidReflux|Upright|Recumbent",names(NegImp_FromImpWithBRavoAndHRM))]

#Also get rid of all columns containing unrelated symptoms and Nonacid and only look at the acid related things (so not "AllReflux")
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("Unrelated|Nonacid|AllReflux",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Remove RSSI as dont know what it is and SxCorr as it is just a number rather than a symptom score (RSI and SAP derived from it anyway)
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("RSSI|SxCorr|^SAP|^SI|ChannelTime|Duration|ClearanceTime|Composite|PP",names(NegImp_FromImpWithBRavoAndHRMMinSet))]


#Combine columns so they are more meaningful. Use the SI and SAP as potential parameters but only if significantly associated
#Combine symptoms so have only three categories- LPR (Throat/cough/Globus)/Oesophageal(Chest pain/regurgitation/belch/vomiting/heartburn)/Abdominal (Stomachpain)
#Also just code the symptoms as being present or not so if any of the members of the category are SAP >95% or SI >50% then it is a positive finding for that category

NegImp_FromImpWithBRavoAndHRMMinSet <- NegImp_FromImpWithBRavoAndHRMMinSet %>%

  mutate(
  SAPHeartburn = case_when(
    SxMainRSAPAcidHeartburn  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPChestPain = case_when(
    SxMainRSAPAcidChestPain  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPVomiting = case_when(
    SxMainRSAPAcidVomiting  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPNausea = case_when(
    SxMainRSAPAcidNausea  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPRegurgitation = case_when(
    SxMainRSAPAcidRegurgitation  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPBelch = case_when(
    SxMainRSAPAcidBelch  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
  SAPStomachPain = case_when(
    SxMainRSAPAcidStomachPain  >94 ~ 1,
    TRUE ~ 0))%>%




  mutate(
    SIHeartburn = case_when(
      MainSxRSIAcidHeartburn  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    SIChestPain = case_when(
      MainSxRSIAcidChestPain  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    SIVomiting = case_when(
      MainSxRSIAcidVomiting  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    SINausea = case_when(
      MainSxRSIAcidNausea  >94 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    SIRegurgitation = case_when(
     MainSxRSIAcidRegurgitation  >94 ~ 1,
      TRUE ~ 0
    )) %>%
  mutate(
    SIBelch = case_when(
      MainSxRSIAcidBelch  >94 ~ 1,
      TRUE ~ 0
    )) %>%
  mutate(
    SIStomachPain = case_when(
      MainSxRSIAcidStomachPain   >94 ~ 1,
      TRUE ~ 0
    )) %>%

  mutate(
    SAPOesophageal = case_when(
  SxMainRSAPAcidHeartburn  >94 ~ 1,
  SxMainRSAPAcidChestPain  >94 ~ 1,
  TRUE ~ 0
)) %>%
  mutate(
    SAPOther = case_when(
  SxMainRSAPAcidVomiting >94 ~ 1,
  SxMainRSAPAcidNausea  >94 ~ 1,
  SxMainRSAPAcidRegurgitation  >94 ~ 1,
  SxMainRSAPAcidBelch  >94 ~ 1,
  SxMainRSAPAcidStomachPain >94 ~ 1,
  TRUE ~ 0
))%>%
  mutate(
    SIOesophageal = case_when(
      MainSxRSIAcidHeartburn  >49 ~ 1,
      MainSxRSIAcidChestPain  >49 ~ 1,
      TRUE ~ 0
    ))%>%
mutate(
    SIOther = case_when(
      MainSxRSIAcidVomiting >49 ~ 1,
      MainSxRSIAcidNausea  >49 ~ 1,
      MainSxRSIAcidRegurgitation  >49 ~ 1,
      MainSxRSIAcidBelch  >49 ~ 1,
      MainSxRSIAcidStomachPain >49 ~ 1,
      TRUE ~ 0
    ))%>%
  mutate(
    SAPLPR = case_when(
      SxMainRSAPAcidThroat >94 ~ 1,
      SxMainRSAPAcidCough  >94 ~ 1,
      TRUE ~ 0
)) %>%
  mutate(
    SILPR = case_when(
      MainSxRSIAcidThroat >49 ~ 1,
      MainSxRSIAcidCough  >49 ~ 1,
      TRUE ~ 0
    ))



NegImp_FromImpWithBRavoAndHRMMinSet$SAPOesophageal<-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPOesophageal)
NegImp_FromImpWithBRavoAndHRMMinSet$SIOesophageal<-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIOesophageal)

NegImp_FromImpWithBRavoAndHRMMinSet$SAPLPR<-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPLPR)
NegImp_FromImpWithBRavoAndHRMMinSet$SILPR<-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SILPR)

NegImp_FromImpWithBRavoAndHRMMinSet$SAPOther <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPOther)
NegImp_FromImpWithBRavoAndHRMMinSet$SIOther <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIOther)

NegImp_FromImpWithBRavoAndHRMMinSet$SAPHeartburn <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPHeartburn)
NegImp_FromImpWithBRavoAndHRMMinSet$SAPChestPain <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPChestPain )
NegImp_FromImpWithBRavoAndHRMMinSet$SAPVomiting <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPVomiting)
NegImp_FromImpWithBRavoAndHRMMinSet$SAPNausea <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPNausea)
NegImp_FromImpWithBRavoAndHRMMinSet$SAPRegurgitation <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPRegurgitation)
NegImp_FromImpWithBRavoAndHRMMinSet$SAPBelch <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPBelch)
NegImp_FromImpWithBRavoAndHRMMinSet$SAPStomachPain <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SAPStomachPain)

NegImp_FromImpWithBRavoAndHRMMinSet$SIHeartburn <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIHeartburn)
NegImp_FromImpWithBRavoAndHRMMinSet$SIChestPain <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIChestPain)
NegImp_FromImpWithBRavoAndHRMMinSet$SIVomiting <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIVomiting)
NegImp_FromImpWithBRavoAndHRMMinSet$SINausea <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SINausea)
NegImp_FromImpWithBRavoAndHRMMinSet$SIRegurgitation <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIRegurgitation)
NegImp_FromImpWithBRavoAndHRMMinSet$SIBelch <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIBelch)
NegImp_FromImpWithBRavoAndHRMMinSet$SIStomachPain <-as.factor(NegImp_FromImpWithBRavoAndHRMMinSet$SIStomachPain)


#Now get rid of BRAVO columns you dont need:
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("HospNum_Id|HRM_Id|Physician|ReferringPhysician|Operator|BravoID|VisitDate|Stats|Procedure|FileCreationDate|TimeToNext|TimeSinceLast|BravoID|Imp_Id|^id$|PatientScore|MainPtData|GastricChannel|MainProcProcedureStart|MainProcProcedureDuration",names(NegImp_FromImpWithBRavoAndHRMMinSet))]


#Tells you which columns have the most missing data so they can be excluded

exclude_missing<-function(df){
  missing<-profile_missing(df)
  missing<-data.frame(missing,stringsAsFactors = FALSE)
  #Only <10% for each row missing data is accepted
  df_m<-missing[!grepl("Remove|Bad",missing$group),]
  #Pick the columns with good data in them from this set
  df<-df[,match(as.character(df_m$feature),names(df))]
  df<-data.frame(df,stringsAsFactors = FALSE)
  #3. Get some basic descriptive stats:
  #rr<-describe(df)
  #Select only the numeric variables
  #df<-dplyr::select_if(df, is.numeric)
  return(df)
}

#Only use columns with good quality data:
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-exclude_missing(NegImp_FromImpWithBRavoAndHRMMinSet)


######################  Univariate Anaylsis OR ########################################
## @knitr HRM_UnivariateAnalysisOR


NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO<-factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO,
       levels=c(0,1),
       labels=c("Negative", # Reference
                "Positive"))



#Create labels
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$ageInYears, "label") <- "Age (years)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$LESlengthcm, "label") <- "LOS length (cm)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$BasalrespiratoryminmmHg, "label") <- "Basal resp. min (mmHg)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$ResidualmeanmmHg, "label") <- "Mean Residual Pressure (mmHg)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$DistalcontractileintegralmeanmmHgcms, "label") <- "DCI (mean mmHg/cm/s)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$Contractilefrontvelocitycms, "label") <- "CFV (m/s)"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$Distallatency, "label") <- "DL"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$failedChicagoClassification, "label") <- "% Failed swallows"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$panesophagealpressurization, "label") <- "% Panoesophageal pressurization"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$largebreaks, "label") <- "% Large Breaks"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$prematurecontraction, "label") <- "% Premature contractions"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$rapidcontraction, "label") <- "% Rapid contraction"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$smallbreaks, "label") <- "Number of small breaks"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelNumberofAcidEpisodes, "label") <- "Number of Acid Episodes"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelPercentTime, "label") <- "Percentage Time pH <4"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelMeanAcidClearanceTime, "label") <- "Mean Acid Clearance Time"
# attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelLongestEpisode, "label") <- "Longest acid episode"


#Rearrange columns so can do the univariate analysis more efficiently:
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% select(AcidRefluxBRAVO,1:12, everything())

#For the table get rid of variables you dont want to see
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% select(-LOS_relax,-DOBAge,-LowerOesoph,-AllSymps_BRAVOcompartment,-AllSymps_BRAVOgrouped,-AcidRefluxBRAVOTotalOnly,-AllSymps_Impgrouped,-contains("SAP"),-contains("SI"))

#Make sure the group to compare is the first column and then list the last column as the final variable to compare for each group:
library(compareGroups)
resOR <- compareGroups(AcidRefluxBRAVO ~ . - ageInYears, NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2, compute = TRUE)
restabOR <- createTable(resOR, show.ratio = TRUE, show.p.overall = TRUE,
                        hide.no = "no", type=1)
export2md(restabOR)
detach("package:compareGroups", unload=TRUE)


########### Multiple Logistic Regression1#############################################
## @knitr HRM_MultipleLogRegression1


#Choose variables automatically from the univariate analysis:

dff<-lapply(resOR,function(x) x$p.mul)
ssd<-unlist(Filter(is.numeric, dff))
ssddf<-data.frame(ssd)
ssddf$att<-row.names(ssddf)
ToFilter<-ssddf%>%filter(ssd < 0.5e-01)
ToFilter$att<-gsub("\\.p\\..*","",ToFilter$att)



#Do some imputation for missing variables so can run regression:
library(missForest)
as<-missForest(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal[as.numeric(which(sapply(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal,class)=="numeric"))])
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal[as.numeric(which(sapply(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal,class)=="numeric"))]<-as$ximp


#Need to choose more variables here from the univariate above based on the p--value <0.25 (I did this manually)
chosen<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal%>% select(AcidRefluxBRAVO,as.character(ToFilter$att))
chosen$AcidRefluxBRAVOTotalOnly<-NULL

########### Univariate predictors graph #############################################
## @knitr UnivariatePredictorsGraph
library(ggplot2)
p1<-ggplot(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal, aes(x = AcidRefluxBRAVO, y = DistalcontractileintegralmeanmmHgcms)) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='descending',cex = 2)

p2<-ggplot(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal, aes(x = AcidRefluxBRAVO, y = Distallatency)) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='descending',cex = 2)

p3<-ggplot(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal, aes(x = AcidRefluxBRAVO, y = MainAcidExpTotalClearanceChannelPercentTime)) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='descending',cex = 2)

p4<-ggplot(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal, aes(x = AcidRefluxBRAVO, y = BasalrespiratorymeanmmHg)) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='descending',cex = 2)

library(gridExtra)
grid.arrange(p1, p2,p3,p4, nrow = 1)






# attr(chosen$BasalrespiratoryminmmHg, "label") <- "Basal resp. min (mmHg)"
# attr(chosen$DistalcontractileintegralmeanmmHgcms, "label") <- "DCI (mean mmHg/cm/s)"
# attr(chosen$Distallatency, "label") <- "DL"
# attr(chosen$MainAcidExpTotalClearanceChannelNumberofAcidEpisodes, "label") <- "Number of Acid Episodes"
# attr(chosen$MainAcidExpTotalClearanceChannelPercentTime, "label") <- "Percentage Time pH <4"



#To get this as a decent table for publication see here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#table_output_for_word_and_rmarkdown_documents


# Stepwise regression model
full.model <- glm(as.numeric(chosen$AcidRefluxBRAVO) ~ ., data = dplyr::select_if(chosen, is.numeric))
step.model <- MASS::stepAIC(full.model, direction = "both", trace = FALSE)

library(Publish)
kable(publish(step.model,pvalue.stars=TRUE,print = FALSE)$regressionTable)




########### Multiple Logistic Regression2 #############################################
## @knitr HRM_MultipleLogRegression2


#To get univariate and multivariate odds ratios:
 library(finalfit)
 NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO<-as.factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)
 #Note for some reason I cant get Age and SAP ones in here
 explanatory = c("MainAcidExpUprightClearanceChannelPercentTime","IntraabdominalLESlengthcm","BasalrespiratorymeanmmHg","DistalcontractileintegralmeanmmHgcms","Distallatency")
 dependent = 'AcidRefluxBRAVO'

 kable(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>%
   finalfit(dependent, explanatory))%>% kable_styling("striped", position = "left", font_size = 11)


########## Multiple Logistic RegressionOR #############################################
## @knitr HRM_MultipleLogRegressionOR



 NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>%
 or_plot(dependent, explanatory,column_space = c(-1, 0, 1), table_text_size = 4)




########## ROC Plot #############################################
## @knitr HRM_MultipleLogRegressionROC_plot

 #Get a ROC for your model:
 library(pROC)
 predpr <- predict(full.model,type=c("response"))
 roccurve <- roc(as.factor(AcidRefluxBRAVO) ~ predpr, data = chosen)
 plot(roccurve)
 #plot.roc(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelPercentTime,NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)



########## AUC #############################################
## @knitr HRM_MultipleLogRegressionROC_AUC


auc(roccurve)



########## Cut Point For pH <4 #############################################
## @knitr CutPointForAcid


 #Determine optimal cutpoints for the Percent time <4
library(cutpointr)
vb<-cutpointr(x = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$MainAcidExpTotalClearanceChannelPercentTime, class = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO,na.rm=TRUE)
kable(t(data.frame(vb[1:13])))













 ########## Hypersensitive oesophagus #############################################
 ## @knitr Hypersensitiveoesophagus

#
# hypersensitiveoesoph2<-NegImp_FromImpWithBRavoAndHRM %>%
#    select(HospNum_Id,SxMainRSAPAcidHeartburn,SxMainRSAPAcidChestPain,AcidReflux_Imp,AcidRefluxBRAVO,MainAcidExpTotalClearanceChannelPercentTime) %>%
#    filter_all(any_vars(. >94.9))
#
#    hypersensitiveoesoph2<-data.frame(hypersensitiveoesoph2)
#
#    (nrow(hypersensitiveoesoph2[hypersensitiveoesoph2$AcidRefluxBRAVO==1,])/nrow(hypersensitiveoesoph2))*100
#
#    #Does the anticipatory AET predict positive BRAVO in the hypersensitive population?
#
#    library(missForest)
#    as<-missForest(hypersensitiveoesoph2[as.numeric(which(sapply(hypersensitiveoesoph2,class)=="numeric"))])
#    hypersensitiveoesoph2[as.numeric(which(sapply(hypersensitiveoesoph2,class)=="numeric"))]<-as$ximp
#
#    full.model2 <- glm(as.factor(AcidRefluxBRAVO) ~ MainAcidExpTotalClearanceChannelPercentTime, data = hypersensitiveoesoph2,family=binomial(),na.action=na.omit)
#    step.model2 <- MASS::stepAIC(full.model2, direction = "both", trace = FALSE)
#    dd<-summary(step.model2)



########## Likelihood of second day positive if first day >2% but < 5% on BRAVO #############################################
## @knitr Day1NegDay2Pos

#Get all the BRAVOs and determine with >2<4.2 range and then see the percentage day 2 positive vs >4.2% day1 and <2% day 2

#Get relevant Bravo columns from the main Bravo results:
AllBravoAET<-AllBravo[,grepl("FractionTimepHLessThan4Total",names(AllBravo))]

#Get the named days only
AllBravoAET<-AllBravoAET[,!grepl("ReflDayTotal",names(AllBravoAET))]

#Only select the GORD patients by pH<4
AllBravoAET$average<-rowMeans(select_if(AllBravoAET, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), na.rm = TRUE)
AllBravoAET$worst<-do.call(pmax, c(select_if(AllBravoAET, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), na.rm = TRUE))

#Need to get which day was the worst:
AllBravoAET$worstDaypH<-names(select_if(AllBravoAET, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")))[max.col(replace(select_if(AllBravoAET, is.numeric)%>%select(contains("FractionTimepHLessThan4Total")), is.na(select_if(AllBravoAET, is.numeric)%>%select(contains("FractionTimepHLessThan4Total"))),0),ties.method = "first")]
#Clean up the days:
AllBravoAET$worstDaypH<-gsub("FractionTimepHLessThan4Total","",AllBravoAET$worstDaypH)
AllBravoAET$worstDaypH<-gsub("ReflDay","",AllBravoAET$worstDaypH)
AllBravoAET$worstDaypH<-gsub("1_2","3",AllBravoAET$worstDaypH)
AllBravoAET$worstDaypH<-gsub("2_2","4",AllBravoAET$worstDaypH)
AllBravoAET$worstDaypH<-gsub("3_2","5",AllBravoAET$worstDaypH)


AllBravoAET$Day2Pos<-ifelse(AllBravoAET$ReflDay2FractionTimepHLessThan4Total>4.9,"ADay2Pos","Day2Neg")

#Get rid of all day1 positive BRAVOs
AllBravoAETmodel<-AllBravoAET[AllBravoAET$ReflDay1FractionTimepHLessThan4Total<5,]
#Choose the predictive variables:
AllBravoAETmodel<-data.frame(AllBravoAETmodel$Day2Pos,AllBravoAETmodel$ReflDay1FractionTimepHLessThan4Total)


#Does Day 1 (normal)  predict day 2
full.model2 <- glm(as.factor(AllBravoAETmodel$AllBravoAETmodel.Day2Pos) ~ AllBravoAETmodel.ReflDay1FractionTimepHLessThan4Total, data = AllBravoAETmodel,family=binomial(),na.action=na.omit)
step.model2 <- MASS::stepAIC(full.model2, direction = "both", trace = FALSE)
dd<-summary(step.model2)


#Determine optimal cutpoints for the Percent time <4
vb<-cutpointr(x =AllBravoAETmodel$AllBravoAETmodel.ReflDay1FractionTimepHLessThan4Total , class = AllBravoAETmodel$AllBravoAETmodel.Day2Pos,na.rm=TRUE)

########## Score creation based on multivariate model #############################################
## @knitr score

#Get the positive variables from multivariate (p<0.05) and create a simple score of one point for each positive variable and then see if that is predictive




########## diagrammeR #############################################
## @knitr diagrammeR



#Other analyses:

#Day by day analysis to see if patients more likely to have intermittent symptoms especially for reflux and heartburn using the BRAVO results
#Compare BRAVO positive with BRAVO negative
#Are the hypersensitives also the ones with high AET. ie do HOvsHighAET NoHovs HighAET HOvsLowAET and NoHOvsLowAET and compare the groups (high AET is > optimal cutpoint)
