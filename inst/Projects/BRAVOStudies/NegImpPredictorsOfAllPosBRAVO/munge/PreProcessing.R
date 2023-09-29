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
library(matrixStats)
library(ggpubr)
library(flextable)
library(knitr)
library(here)
library(reshape)
#library(checkpoint)


#checkpoint("2020-08-14")


##### Import ####
## @knitr dataImport

oldw <- getOption("warn")

options(warn = -1)

#Get this from the data folder for PhysiMineR-  just click on the RData to load into
#the global environment

#Or if using the local file system

BravoDayOneAndTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BravoDay1And2.xls"))
BravoDayThreeAndFour <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BravoDay3And4.xls"))
BRAVOTotal <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BRAVOTotal.xls"))
#Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Diag.xls"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
ImpedanceTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Impedance2.xls"))
Procs <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/ProcByHospitalNumbers.xls"))
Procs<-Procs%>%dplyr::rename(Grade='Oesophagitis Grade')
PSPW<-readxl::read_xlsx((here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Seb_PSPW_list (003).xlsx")))
PSPW<-PSPW%>%dplyr::rename(HospitalNumber=HospitalNum)
MNBI<-read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/MNBI.xlsx"))
MNBI2<-read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/MNBI2.xlsx"))

MNBI<-rbind(MNBI,MNBI2)

MNBI<-merge(MNBI,PSPW,by="HospitalNumber",all.x=TRUE)

#Make compatibile with Hospital Number column below:
MNBI<-MNBI%>%dplyr::rename(HospNum_Id=HospitalNumber)
options(warn = oldw)




##### Clean ####################################################################################################################
## @knitr dataClean
#Data cleaning
#Use after PhysiData_Acq
#Clean HRM
#Clean up the HRM swallows:
HRMImportSwallows<-HRMImportSwallows[!is.na(HRMImportSwallows$panesophagealpressurizationMapSwallowsNum8),]


##### IntraTestMergeAndClean ########################################################################################
## @knitr dataIntraTestMerge

# Get the whole impedance dataset
#You will need to re-clean the merged dataImpSympImpedance as cleaning function needed to be fixed
AllImpedance<-merge(ImpedanceTwo,Imp_Symp,by="Imp_Id", all = TRUE)
ImpedanceThree<-dataImpClean(AllImpedance)
ImpAll<-dataImpSymptoms(ImpedanceThree)
ImpAll<-GORD_AcidImp(ImpAll)

#Diag<-dataDiagClean(Diag)


#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)

#Get rid of the swallows for now as using the whole HRM dataset makes things slow:
AllHRM<-AllHRM[,!grepl("Num\\d+",colnames(AllHRM))]
#Roll up the repeats in to one:
HRMImportMainTwo<-AllHRM %>%
  dplyr::group_by(HospNum_Id,DistalLESfromnarescm)%>%
  dplyr::summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x), x)), collapse = ":"))


#Clean Up the main HRM
HRMImportMainTwo<-HRMCleanUp1(HRMImportMainTwo)



#Merge the BRAVO results together:
AllBravo<-merge(BravoDayOneAndTwo,BravoDayThreeAndFour,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")
AllBravo<-dataBRAVOSymptoms(AllBravo)
#AllBravo<-GORD_AcidBRAVO(AllBravo)
AllBravo<-GORD_BravoWDAAndAverage(AllBravo,4.3)



#Get the BRAVO procedures


Procs<-data.frame(Procs,stringsAsFactors = FALSE)
Procs$DATEOFPROCEDURE<-as.Date(Procs$DATEOFPROCEDURE)

names(Procs)<-c("HospNum_Id","VisitDate.x","Findings","Oesophagitis","Grade")
Procs<-data.frame(Procs,stringsAsFactors = FALSE)



#MNBI<-MNBI%>%rename(HospNum_Id=HospitalNumber)
MNBI<-MNBI%>%dplyr::select(HospNum_Id,MethodOne_average,pspw_index,comments)%>%dplyr::rename(M1_av=MethodOne_average)


#Only select the MNBI final outputs from the two methods:
#MNBI$M2_noct<-as.numeric(MNBI$M2_noct)
MNBI$M1_av<-as.numeric(MNBI$M1_av)



##### CrossTestMerge ########################################################################################
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

#ImpAnddiag <- Diag %>% inner_join(ImpAll, by ="HospNum_Id") %>%
#  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
#  arrange(HospNum_Id, Date_ABS_Diff) %>%
#  group_by(HospNum_Id) %>%
#  slice(1)


#BRAVO Merges:
BravoAnd_hrm <- HRMImportMainTwo %>% inner_join(AllBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)

#BravoAnd_diag <- Diag %>% inner_join(AllBravo, by ="HospNum_Id") %>%
#  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
#  arrange(HospNum_Id, Date_ABS_Diff) %>%
#  group_by(HospNum_Id) %>%
#  slice(1)


#Diag Merges:
#HRMAnd_diag <- HRMImportMainTwo %>% inner_join(Diag, by ="HospNum_Id") %>%
#  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
#  arrange(HospNum_Id, Date_ABS_Diff) %>%
#  group_by(HospNum_Id) %>%
#  slice(1)


#Three test merges:
ImpAndBravoWithHRM <- HRMImportMainTwo %>% inner_join(ImpAndBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)





#Now filter for only those tests where there is a year or less between the BRAVO and the pH impedance (in either direction)
ImpAndBravoWithHRM$diffImpAndBravoDays<-difftime(ImpAndBravoWithHRM$VisitDate.x,ImpAndBravoWithHRM$VisitDate.y.y,tz,units = c("days"))

#These are the ones excluded mostly as too big a time difference:
#View(NoImpAndBravoWithHRM_Orig%>%select(HospNum_Id,VisitDate.x,VisitDate.y.y,diffImpAndBravoDays))



ImpAndBravoWithHRM<-ImpAndBravoWithHRM%>%filter(diffImpAndBravoDays<365,diffImpAndBravoDays>-365)





##### Extrapolate ############################################################################################################
## @knitr dataForking
#Extract the negative impedance

#Get the age of the patient at the time of doing the test:
library(eeptools)
ImpAndBravoWithHRM$ageInYears<-age_calc(na.omit(ImpAndBravoWithHRM$MainPtDataDateofBirth),ImpAndBravoWithHRM$MainPtDataDateofAdmission,precise=TRUE)/12

#Get all the Negative Impedance patients (with both positive and negative BRAVO results-#AcidRefluxBRAVOAv is the outcome variable we will use)
NegImp_FromImpWithBRavoAndHRM<-ImpAndBravoWithHRM %>% filter(AcidReflux_Imp==0)
#Also need to get rid of the non-acid reflux patients defined as >80 reflux episodes in a patient with normal AET.
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM%>% filter((MainRflxEpisodeTotalAcid+MainRflxEpisodeTotalNonacid)<80)

#Get rid of all the BRAVO columns as we don't need these as predictive variables
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "^t[Dd]ay" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Ss]wallo" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]

#Add the BRAVO procedure information:
NegImp_FromImpWithBRavoAndHRM<-merge(NegImp_FromImpWithBRavoAndHRM, Procs,by=c("HospNum_Id","VisitDate.x"),all.x=TRUE)


NegImp_FromImpWithBRavoAndHRM$Oesophagitis<-ifelse(grepl("Y",NegImp_FromImpWithBRavoAndHRM$Oesophagitis),"Y","N")


#Impute the gender
NegImp_FromImpWithBRavoAndHRM$Gender<-ifelse(NegImp_FromImpWithBRavoAndHRM$Gender=="null",sample(c("Male","Female"), 1),NegImp_FromImpWithBRavoAndHRM$Gender)




##### BRAVO description #############################################
## @knitr BRAVOdescription
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)

#Need to replicate Sweis paper and show 1. SAP worst day =63.2% 2. SI Worst day =61% 3. pH worst day <4 (>4.2%) =47% and most in the first 24 hours 4. 45% GERD based on worst day SAP or pH <4

#Positive GORD by pH<4 in 4.3% Worst day overall analysis-***
#pHWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM[NegImp_FromImpWithBRavoAndHRM$worstt>=7.1,])/nrow(NegImp_FromImpWithBRavoAndHRM))*100

#Positive GORD by SAP Worst day overall analysis-
SAPWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM %>%select(contains("SAP")) %>%filter_all(any_vars(. >94.9)))/nrow(NegImp_FromImpWithBRavoAndHRM))*100

#Positive GORD by SI Worst day overall analysis-
#SIWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM %>%select(contains("SI")) %>%filter_all(any_vars(. >50)))/nrow(NegImp_FromImpWithBRavoAndHRM))*100

#NegImp_FromImpWithBRavoAndHRM<-merge(NegImp_FromImpWithBRavoAndHRM,MNBI,by="HospNum_Id",all.x=TRUE)
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM %>% left_join(MNBI, by = "HospNum_Id")





NegImp_FromImpWithBRavoAndHRM<-as_tibble(NegImp_FromImpWithBRavoAndHRM)

#Get rid of the patients Ismail identified as being poor quality:
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[is.na(NegImp_FromImpWithBRavoAndHRM$comments),]

#Some some extra diagnoses for the paper

NegImp_FromImpWithBRavoAndHRM$Hypersensitive_vs_FD<-ifelse(NegImp_FromImpWithBRavoAndHRM$SAPTotalHeartburn>94.9&
                                                       !is.na(NegImp_FromImpWithBRavoAndHRM$SAPTotalHeartburn),"HypersensitiveOesophagus",
                                                            ifelse(NegImp_FromImpWithBRavoAndHRM$SAPTotalHeartburn<94.9&
                                                              !is.na(NegImp_FromImpWithBRavoAndHRM$SAPTotalHeartburn),"Functional Heartburn","NotHypersens"))
# DEFINITION OF POSITIVE WPM FOR GORD HERE
#How many days positive of the 4? (ie how many days with pH>5.3 on any one day)
#Using the average day analysis definition:
NegImp_FromImpWithBRavoAndHRMOnlyGORD<-NegImp_FromImpWithBRavoAndHRM[NegImp_FromImpWithBRavoAndHRM$AcidRefluxBRAVOAv==1,]

#Average day analysis definition:
#NegImp_FromImpWithBRavoAndHRMOnlyGORD<-NegImp_FromImpWithBRavoAndHRM[NegImp_FromImpWithBRavoAndHRM$average>5.2,]

#Need to divide the number of positive results by the number for each day that actually have a result
#(from ReflDay1FractionTimepHLessThan4Total",
#"ReflDay2FractionTimepHLessThan4Total","ReflDay1_2FractionTimepHLessThan4Total","ReflDay2_2FractionTimepHLessThan4Total"
#"ReflDay3_2FractionTimepHLessThan4Total" and use table(NegImp_FromImpWithBRavoAndHRM$ReflDay2_2FractionTimepHLessThan4Total,exclude = NULL))

#Get table with the actual day to day figures only for the positive patients:
DayToDayVar<-NegImp_FromImpWithBRavoAndHRMOnlyGORD%>%select(ReflDay1FractionTimepHLessThan4Total,ReflDay2FractionTimepHLessThan4Total,ReflDay1_2FractionTimepHLessThan4Total,ReflDay2_2FractionTimepHLessThan4Total,ReflDay3_2FractionTimepHLessThan4Total,ReflDay4_2FractionTimepHLessThan4Total,worstDaypH,averageAll)

#--------------------------------
library(matrixStats)
#Get rid of outliers:
DayToDayVar$row_std = rowSds(as.matrix(DayToDayVar[,c(1,2,3,4,5,6)]),na.rm=TRUE)
DayToDayVar$CV = 100*(DayToDayVar$row_std /DayToDayVar$averageAll)
#DayToDayVar<-DayToDayVar%>%filter(averageAll<20)%>%select(-averageAll)
#DayToDayVar<-DayToDayVar%>%filter(ReflDay2_2FractionTimepHLessThan4Total<20|NA)



DayToDayVar<-data.frame(DayToDayVar)
names(DayToDayVar)<-c("Day1","Day2","Day3","Day4","Day5","Day6","worstDay","averageAll","stdev","CV")

#For a quick ANOVA to see if there is any variability in the days with WOM:

data_long <- tidyr::gather(DayToDayVar, condition, measurement, Day1:Day6, factor_key=TRUE)


AOV<-aov(measurement~condition,data=data_long)
summary(AOV)
#See if can get some groupings:
#Graphs where there is a drop from day 1 to day 2:
DayToDayVar_earlydrop<-DayToDayVar%>%filter((Day2-Day1)<0)%>%select(-worstDay,-averageAll,-stdev,-CV)
DayToDayVar_earlyrise<-DayToDayVar%>%filter((Day2-Day1)>0)%>%select(-worstDay,-averageAll,-stdev,-CV)
DayToDayVar_day1_3drop<-DayToDayVar%>%filter((Day3-Day1)<0)%>%select(-worstDay,-averageAll,-stdev,-CV)
DayToDayVar_day1_3rise<-DayToDayVar%>%filter((Day3-Day1)>0)%>%select(-worstDay,-averageAll,-stdev,-CV)
DayToDayVar_day1_3rise<-DayToDayVar%>%filter((Day3-Day1)>0)%>%select(-worstDay,-averageAll,-stdev,-CV)

#Get the worst days
DayToDayVar_worst_is_Day1<-DayToDayVar%>%filter(worstDay==1)%>%select(-worstDay,-averageAll,-stdev)
DayToDayVar_worst_is_Day1_CV<-mean(DayToDayVar_worst_is_Day1$CV)
DayToDayVar_worst_is_Day2<-DayToDayVar%>%filter(worstDay==2)%>%select(-worstDay,-averageAll,-stdev)
DayToDayVar_worst_is_Day2_CV<-mean(DayToDayVar_worst_is_Day2$CV)
DayToDayVar_worst_is_Day3<-DayToDayVar%>%filter(worstDay==3)%>%select(-worstDay,-averageAll,-stdev)
DayToDayVar_worst_is_Day3_CV<-mean(DayToDayVar_worst_is_Day3$CV)
DayToDayVar_worst_is_Day4<-DayToDayVar%>%filter(worstDay==4)%>%select(-worstDay,-averageAll,-stdev)
DayToDayVar_worst_is_Day4_CV<-mean(DayToDayVar_worst_is_Day4$CV)
#DayToDayVar %>% mutate(mak=max(Day1,Day2,Day3,Day4,Day5,Day6))
library(reshape)
df_WorstDayTBB<-data.frame(x = seq_along(DayToDayVar[,1]),DayToDayVar)
df_WorstDayTBB <- melt(df_WorstDayTBB, id.vars = "x")
mydf<-df_WorstDayTBB%>%filter(grepl("worstDay",variable))
df_WorstDayTBB<-df_WorstDayTBB%>%left_join(mydf, by = "x")
df_WorstDayTBB<-df_WorstDayTBB%>%filter(!grepl("worst|averageAll|stdev|CV",variable.x))
df_WorstDayTBB$variable.x<-as.numeric(gsub("Day","",df_WorstDayTBB$variable.x))


library(ggplot2)
#Get rid of outliers and >day 4
df_WorstDayTBB<-df_WorstDayTBB%>%filter(df_WorstDayTBB$value.x<20)
df_WorstDayTBB<-df_WorstDayTBB%>%filter(df_WorstDayTBB$value.y<5)

WorstDaysPlot <- ggplot(df_WorstDayTBB, aes(x = variable.x, y = value.x))+
geom_line(aes(group = x, color = x))+
facet_grid(cols = vars(value.y))+
geom_smooth(method = "lm", formula = y ~ x, size = 1,aes(group=variable.y),color="red")+  xlab("Day") + ylab("AET (%)")








#Get the comparison of the lines:


WorstDaysPlot<-WorstDaysPlot+
  theme_Publication()+
  theme(legend.position='none')+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 0,vjust=1,hjust = 1,size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))+
  theme(strip.background = element_blank(), strip.placement = "outside",legend.title = element_blank())
#To Do- get the average of the line:



myworstDayTable<-data.frame(table(NegImp_FromImpWithBRavoAndHRMOnlyGORD$worstDaypH))
Day1TotNumPts<-myworstDayTable$Freq[1]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay1FractionTimepHLessThan4Total),])*100
Day2TotNumPts<-myworstDayTable$Freq[2]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay2FractionTimepHLessThan4Total),])*100
Day3TotNumPts<-myworstDayTable$Freq[3]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay1_2FractionTimepHLessThan4Total),])*100
Day4TotNumPts<-myworstDayTable$Freq[4]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay2_2FractionTimepHLessThan4Total),])*100
Day5TotNumPts<-myworstDayTable$Freq[5]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay3_2FractionTimepHLessThan4Total),])*100
myworstDayTable$Percentage<-c(Day1TotNumPts,Day2TotNumPts,Day3TotNumPts,Day4TotNumPts,Day5TotNumPts)
names(myworstDayTable)<-c("Day","Freq","Percentage")


a2<-ggplot(myworstDayTable,aes(x=Day,y=Percentage))+
  geom_histogram(stat="identity")+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("Worst Day AET pH<4 ")+
  ylab("% of studies")+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))+
  labs(fill = "GERD")+
  scale_fill_discrete(name = "GERD", labels = c("No", "Yes"))


#This graph looks at the worst pH for WPM-GORD positive and WPM-GORD negative patients
#Cant seem to get rid of the outliers so have set the average limit to 25.
a3<-ggplot(NegImp_FromImpWithBRavoAndHRM%>%filter(worstt<25), aes(x = as.factor(AcidRefluxBRAVOAv),y=worstt,fill=as.factor(AcidRefluxBRAVOAv))) +
  geom_violin(opacity="50%")+
  geom_boxplot(outlier.shape = NA,width=0.4)+geom_beeswarm(size=0.3,priority='density',cex=1.5)+
  scale_x_discrete(labels=c("Negative","Positive"))+
  scale_colour_Publication()+
  theme_Publication()+
  theme(legend.position='none')+
  xlab("GERD")+
  ylab("Worst day AET")+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))+
  labs(fill = "GERD") +
  scale_fill_grey(name = "GERD", labels = c("No", "Yes"),start = 0.5, end = 0.8)

myNumDayspositiveTable<-data.frame(table(NegImp_FromImpWithBRavoAndHRMOnlyGORD$NumDaysBravoPositive))

Day1TotNumPosPts<-myNumDayspositiveTable$Freq[1]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay1FractionTimepHLessThan4Total),])*100
Day2TotNumPosPts<-myNumDayspositiveTable$Freq[2]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay1FractionTimepHLessThan4Total),])*100
Day3TotNumPosPts<-myNumDayspositiveTable$Freq[3]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay2FractionTimepHLessThan4Total),])*100
Day4TotNumPosPts<-myNumDayspositiveTable$Freq[4]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay1_2FractionTimepHLessThan4Total),])*100
Day5TotNumPosPts<-myNumDayspositiveTable$Freq[5]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay2_2FractionTimepHLessThan4Total),])*100
Day6TotNumPosPts<-myNumDayspositiveTable$Freq[6]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay3_2FractionTimepHLessThan4Total),])*100
Day7TotNumPosPts<-myNumDayspositiveTable$Freq[7]/nrow(NegImp_FromImpWithBRavoAndHRMOnlyGORD[!is.na(NegImp_FromImpWithBRavoAndHRMOnlyGORD$ReflDay4_2FractionTimepHLessThan4Total),])*100

myNumDayspositiveTable$Percentage<-c(Day1TotNumPosPts,Day2TotNumPosPts,Day3TotNumPosPts,Day4TotNumPosPts,Day5TotNumPosPts,Day6TotNumPosPts)
names(myNumDayspositiveTable)<-c("Day","Freq","Percentage")




a4<-ggplot(myNumDayspositiveTable[2:nrow(myNumDayspositiveTable),],aes(x=Day,y=Percentage))+
  geom_histogram(stat="identity")+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("Number of days of GERD")+
  ylab("% of studies")+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))+
  labs(fill = "GERD") + scale_fill_discrete(name = "GORD", labels = c("No", "Yes"))
#+
#  theme(text=element_text(size=10, family="Arial"))+
 # ggsave("Figure1a.jpeg", width = 20, height = 20, dpi = 300, units="cm")


#Correlating the worst Day vs the number of positive days with BRAVO:

NumDays_vs_WorstDay<-merge(myNumDayspositiveTable[2:nrow(myNumDayspositiveTable),],myworstDayTable,by="Day")

NumDays_vs_WorstDayCorr<-cor.test(NumDays_vs_WorstDay$Percentage.x,NumDays_vs_WorstDay$Percentage.y)

############## Get the symptoms of the patients: ###########


interim<-strsplit(NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO,",")
interim<-lapply(interim,function(x)unique(x,","))
interim<-lapply(interim,function(x) sort(x))
new<-unlist(lapply(interim,function(x) paste0(x,collapse=",")))
NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO<-gsub("Epigastric","StomachPain",new)

BelchSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Belch",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
ChestPainSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("ChestPain",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
CoughSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Cough",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
HeartburnSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Heartburn",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
RegurgitationSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Regurgitation",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
ThroatSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Throat",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
VomitingSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Vomiting",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
NauseaSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("Nausea",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])
StomachPainSx<-nrow(NegImp_FromImpWithBRavoAndHRM[grepl("StomachPain",NegImp_FromImpWithBRavoAndHRM$AllSymps_BRAVO),])


Symps<-data.frame(BelchSx,ChestPainSx,CoughSx,HeartburnSx,RegurgitationSx,ThroatSx,VomitingSx,NauseaSx,StomachPainSx)



Sympdf<-gather(Symps, "Symptom", "Frequency", 1:ncol(Symps))
Sympdf$Percentage<-((Sympdf$Frequency/nrow(NegImp_FromImpWithBRavoAndHRM))*100)
Sympdf$Symptom<-gsub("Sx","",Sympdf$Symptom)



a5<-ggplot(Sympdf,aes(x=Symptom,y=Frequency)) +
  geom_histogram(stat="identity")+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("Symptom")+
  ylab("% of studies")+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 30,vjust=1,hjust = 1,size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))
#+
  #theme(text=element_text(size=10, family="Arial"))+
  #ggsave("Figure1d.jpeg", width = 20, height = 20, dpi = 300, units="cm")

df_WorstDayTBB2<-df_WorstDayTBB%>%dplyr::rename(DayPlot=variable.x,AET=value.x,WorstDayGroup=value.y)%>%select(DayPlot,AET,WorstDayGroup)
my_means<-df_WorstDayTBB2%>%group_by(WorstDayGroup,DayPlot)%>%dplyr::summarize(Mean = mean(AET, na.rm=FALSE))
my_means<-data.frame(my_means)
my_means$WorstDayGroup<-as.character(my_means$WorstDayGroup)


compLines<-ggplot(data=my_means,aes(x=DayPlot,y=Mean,group=WorstDayGroup))+
  geom_smooth(method = "lm", formula = y ~ x, size = 1,aes(group=WorstDayGroup),color="red")+  xlab("Day") + ylab("AET (%)")+
  theme_Publication()+
  theme(axis.text.y = element_text(angle = 0,hjust = 1,size = 10))+
  theme(axis.text.x = element_text(angle = 0,vjust=1,hjust = 1,size = 8))+
  theme(axis.title.y = element_text(size = 8))+
  theme(axis.title.x = element_text(size = 12))+
  xlim(1,5)

library(patchwork)
patch<-(a4+a3)/(a2+a5)/(WorstDaysPlot+compLines)
patchwork<-patch & theme(legend.position='none')
#patchwork
patchwork+plot_annotation(tag_levels = c('A', '1'),
                 tag_suffix = '')+
theme(plot.tag.position = c(0, 0),
         plot.tag = element_text(size = 8, hjust = 0, vjust = 0))
#+
 # ggsave(here::here("inst","Projects","BRAVOStudies","NegImpPredictorsOfAllPosBRAVO","reports","Figure1_Complete.jpeg"), width = 20, height = 20, dpi = 300, units="cm")




##### Select columns with minimal data for data quality ############################################
## @knitr missingClean

#attr(NegImp_FromImpWithBRavoAndHRM$M2_noct, "label") <- "MethodTwo Nocturnal"

#Remove columns that are not meaningful:
#Impedance symptoms:
#Get all the MainAcidExposure/MainSx columns as your main impedance dataset. Note this also gets rid of composite score(=DeMeester) as it is derived from MainAcidExp so likely to correlate too much for regression

#NegImp_FromImpWithBRavoAndHRM$WorstD_ayAnalysisGORDPositive
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRM[,grepl("ageInYears|MainAcidEx|MainSx|SxMain|HospNum_Id|DistalLESfromnarescm|Gender|DOBAge|Height|LESlengthcm|PIPfromnarescm|Hiatalhernia|BasalrespiratoryminmmHg|ResidualmeanmmHg|BasalrespiratorymeanmmHg|DistalcontractileintegralmeanmmHgcms|Contractilefrontvelocitycms|IntraboluspressureATLESRmmHg|Distallatency|failedChicagoClassification|panesophagealpressurization|largebreaks|Simultaneous|prematurecontraction|rapidcontraction|smallbreaks|VisitDate|Age|AcidReflux|Upright|Recumbent|SAP|M2_noct|M1_av|pspw|WorstD_|AllSymps_BRAVOgrouped|Oesophagitis|Grade|LESmidpointfromnarescm|MainRflxEpisodeTotalNonacid",names(NegImp_FromImpWithBRavoAndHRM))]

#attr(NegImp_FromImpWithBRavoAndHRMMinSet$M2_noct, "label") <- "MethodTwo Nocturnal"


#Also get rid of all columns containing unrelated symptoms and Nonacid and only look at the acid related things (so not "AllReflux")
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("Unrelated|AllReflux",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Remove RSSI as dont know what it is and SxCorr as it is just a number rather than a symptom score (RSI and SAP derived from it anyway)
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("^SITotal|^SIDay|RSSI|SxCorr|ChannelTime|Duration|ClearanceTime|Composite|PP",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Combine columns so they are more meaningful. Use the SI and SAP as potential parameters but only if significantly associated
#Combine symptoms so have only three categories- LPR (Throat/cough/Globus)/Oesophageal(Chest pain/regurgitation/belch/vomiting/heartburn)/Abdominal (Stomachpain)
#Also just code the symptoms as being present or not so if any of the members of the category are SAP >95% or SI >50% then it is a positive finding for that category


#Now get rid of BRAVO columns you dont need:
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("Meds|UESMeanResidLocationcenterfrnarescm|ResidMeanbasalpressuremmHg|ResidMeanresidualpressuremmHg|MainProc|Catheter|NumDaysBravoPositive|Migration|HospNum_Id|HRM_Id|Physician|ReferringPhysician|Operator|BravoID|VisitDate|Stats|Procedure|FileCreationDate|TimeToNext|TimeSinceLast|BravoID|Imp_Id|^id$|PatientScore|MainPtData|GastricChannel|MainProcProcedureStart|MainProcProcedureDuration",names(NegImp_FromImpWithBRavoAndHRMMinSet))]


#Tells you which columns have the most missing data so they can be excluded
library(DataExplorer)
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
#NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-exclude_missing(NegImp_FromImpWithBRavoAndHRMMinSet)

NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-NegImp_FromImpWithBRavoAndHRMMinSet



############### Consort diagram ###############

library(DiagrammeR)
library(DiagrammeRsvg) #Needed if you want to export the image
#library(rsvg) #Needed if you want to export the image

#Set the values which will go into each label.
a1 <- paste0("a1: Number of  impedance studies =(",nrow(ImpAndhrm), ") \n Patients =",length(unique(ImpAndhrm$HospNum_Id)))
b1 <- ''
c1 <- ''
d1 <- paste0("d1: Number of Impedance studies \n negative for GORD=",nrow(ImpAndhrm[ImpAndhrm$AcidReflux_Imp==0,]), ")","\n Patients:",length(unique(ImpAndhrm[ImpAndhrm$AcidReflux_Imp==0,]$HospNum_Id)))
e1 <- paste0("e1: Number patients \n with negative impedance and subsequent BRAVO: " ,nrow(NegImp_FromImpWithBRavoAndHRM))


a2 <- ''
b2 <- ''
c2 <- ''
d2 <- ''
e2 <- ''

a3 <- ''
b3 <- ''
c3 <- ''
d3 <- ''
e3 <- ''

#Create a node dataframe
ndf <- create_node_df(
  n = 15,
  label = c(a1, b1, c1, d1, e1, #Column 1
            a2, b2, c2, d2, e2, #Column 2
            a3, b3, c3, d3, e3), #Column 2
  style = c('solid', 'invis', 'invis', 'solid', 'solid', #Column 1
            'invis', 'invis', 'invis', 'invis', 'invis', #Column 2
            'invis', 'invis', 'invis', 'invis', 'invis'), #Column 3
  shape = c('box', 'box', 'point', 'box', 'box', #Column 1
            'box', 'point', 'point', 'box', 'point', #Column 2
            'point', 'point', 'point', 'point', 'point'), #Column 3
  width = c(3, 0.001, 0.001, 3, 3, #Column 1
            0.001, 0.001, 0.001, 0.001, 0.001, #Column 2
            0.001, 0.001, 0.001, 0.001, 0.001), #Column 3
  height = c(1, 0.001, 0.001, 1, 1, #Column 1
             0.001, 0.001, 0.001, 0.001, 0.001, #Column 2
             0.001, 0.001, 0.001, 0.001, 0.001), #Column 3
  fontsize = c(rep(6, 1)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true')

#Create an edge dataframe
edf <- create_edge_df(
  from = c(1,4#Horizontals
  ),
  to = c(4,5 #Horizontals
  ),
  arrowhead = c('normal', 'normal', 'none', 'none','none','none', #Column 1
                'none', 'none', 'normal', 'none', #Column 2
                'none', 'none' #Horizontals
  ),
  color = c('black', '#black', '#00000000', '#00000000', '#00000000', '#00000000', #Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', #Column 2
            '#00000000', '#00000000' #Horizontals
  ),
  constraint = c(rep('true', 8), #Columns
                 rep('false', 2) #Horizontals
  )
)

g <- create_graph(ndf,
                  edf,
                  attr_theme = NULL)

render_graph(g)



######################  Day to Day ANOVA ########################################
## @knitr Day2DayANOVA

library(ez)
#res.aov <- anova_test(data = my_means, dv = score, wid = id, within = time)
#get_anova_table(res.aov)
my_means=my_means[my_means$DayPlot!=6,]
repeat1<-ezANOVA(data=my_means ,dv=.(Mean),wid=.(WorstDayGroup),within=.(DayPlot),type=3)





#library(lsmeans)

#m.interaction <- lm(Mean~WorstDayGroup*DayPlot,data=my_means)
#myDayToDayAnova<-anova(m.interaction)
#m.interaction$coefficients
#m.lst <- lstrends(m.interaction, "WorstDayGroup", var="DayPlot")
#comparisonRegressionLines<-pairs(m.lst)

#myDayToDayAnovadf<-data.frame(myDayToDayAnova)
#dayToDayAnovaTable<-flextable(myDayToDayAnovadf %>% tibble::rownames_to_column("column name"))%>%
#  set_caption(caption = "Table 1: Example") %>%
  #font(fontname = "Calibri (Body)", part = "all") %>%
  #fontsize(size = 10, part = "body") %>%
  # add footer if you want
  # add_footer_row(values = "* p < 0.05. ** p < 0.01. *** p < 0.001.",
  #                colwidths = 4) %>%
#  theme_booktabs() %>% # default theme
#  autofit()

#dayToDayAnovaTable

######################  Univariate Anaylsis OR ########################################
## @knitr HRM_UnivariateAnalysisOR




NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVOAv<-factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVOAv,
                                                                        levels=c(0,1),
                                                                        labels=c("Negative", # Reference
                                                                                 "Positive"))

NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$WorstD_ayAnalysisGORDPositive<-factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$WorstD_ayAnalysisGORDPositive,
                                                                          levels=c(0,1),
                                                                          labels=c("Negative", # Reference
                                                                                   "Positive"))



#Rearrange columns so can do the univariate analysis more efficiently:
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% select(AcidRefluxBRAVOAv,1:12, everything())


#Create labels
#MNBI$M2_noct<-as.numeric(MNBI$M2_noct)
#MNBI$M1_av<-as.numeric(MNBI$M1_av)

#For the table get rid of variables you dont want to see
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% dplyr::select(
  #-LOS_relax,
  -DOBAge,
  #-LowerOesoph,
 # -AllSymps_BRAVOcompartment,
 # -EsophageallengthLESUEScenterscm,
  #-ProximalLESfromnarescm,
  -DistalLESfromnarescm,
  -LESlengthcm,
  -Height,
  #-MainRflxEpisodeTotalAcid,
  -Simultaneous,
  -prematurecontraction,
  -rapidcontraction,
   -PIPfromnarescm,
   dplyr::contains("Oesophagitis"),
# dplyr::contains("M2_noct"),
 dplyr::contains("M1_av"),
   dplyr::contains("Method")
  -dplyr::contains("TotalAcid"),
  -dplyr::contains("SAPDay"),
  -dplyr::contains("SAP",ignore.case = FALSE),
  -dplyr::contains("SI",ignore.case = FALSE),
  -dplyr::contains("SAPTotal"),
  -dplyr::contains("SxMain"),
  -dplyr::contains("MainSx"),
  -IntraabdominalLESlengthcm,
  -Hiatalhernia,
 # -Findings,
  -BasalrespiratorymeanmmHg,
  -IntraboluspressureATLESRmmHg,
  # -DistalcontractileintegralhighestmmHgcms,
  # -AcidRefluxBRAVOTotalOnly,
  #-AllSymps_Impgrouped,
  #-AllSymps_BRAVO,
  #-AllImpSymptom,
  -AcidReflux_Imp,
  -Age,
   matches("SAPTotalHeartburn|SAPTotalChestPain|SAPTotalRegurgitation|SAPTotalBelch|SAPTotalCough|SAPTotalThroat|SAPTotalEpigastric|SAPTotalVomiting|SAPTotalNausea"),
  -matches("SAPTotalOther|SAPTotalPostPr|SAPTotalMeal"),
  -SAPOesophageal,
  -SAPOther,
  -SAPLPR,
  #-SIOesophageal,
  #-SIOther,
  #-SILPR,
  -contains("SAP"),
  -contains(".y"),
  -matches("worstt|worstDaypH|Day2|ReflD|DeMeester|Recumbent|Prox|Upright|Day1Pos|Day2Pos|Day3Pos|Day4Pos|Day5Pos|Day6Pos|Trajec")
  #-average,
  #-averageAll
 )

#Do some imputation for missing data:
#library(mice)

#imputed<-mice(data = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2, m = 5, method = "pmm", maxit = 50, seed = 500)
#NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-complete(imputed)

#attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$M2_noct, "label") <- "MNBI Nocturnal"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$M1_av, "label") <- "MNBI"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$ageInYears, "label") <- "Age (years)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Oesophagitis, "label") <- "Oesophagitis"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$DistalcontractileintegralmeanmmHgcms, "label") <- "DCI (mean mmHg/cm/s)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Contractilefrontvelocitycms, "label") <- "Contractile front velocity (m/s)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Distallatency, "label") <- "Distal latency (s)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$panesophagealpressurization, "label") <- "% Panoesophageal pressurization"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$largebreaks, "label") <- "% Large Breaks"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$smallbreaks, "label") <- "% Small breaks"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainRflxEpisodeTotalNonacid, "label") <- "Number of Non acid episodes"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelNumberofAcidEpisodes, "label") <- "Number of Acid Episodes"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelPercentTime, "label") <- "AET"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelLongestEpisode, "label") <- "Longest acid episode"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$LESmidpointfromnarescm, "label") <- "LES midpoint from nares (cm)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$BasalrespiratoryminmmHg, "label") <- "Basal resp. min. (mmHg)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$ResidualmeanmmHg, "label") <- "Residual mean (mmHg)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$failedChicagoClassification, "label") <- "% Failed peristalsis"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$AllSymps_BRAVOgrouped, "label") <- "All symptoms grouped"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Oesophagitis, "label") <- "Endoscopic Findings"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$pspw_index, "label") <- "PSPWI"


#Symptom cleaning for analysis: Need to have as yes no answers rather than actual SAPs
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalHeartburn<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalHeartburn),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalChestPain<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalChestPain),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalRegurgitation<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalRegurgitation),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalBelch<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalBelch),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalCough<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalCough),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalThroat<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalThroat),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalEpigastric<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalEpigastric),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalVomiting<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPTotalVomiting),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPOesophageal<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPOesophageal),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPOther<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPOther),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPLPR<-ifelse(!is.na(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$SAPLPR),"Y","N")
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MethodOne_average<-as.numeric(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MethodOne_average)
# NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MethodTwo_WholeNocturnalPeriod<-as.numeric(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MethodTwo_WholeNocturnalPeriod)



#Get rid of rows where the impedance data is missing following the merge:

#Make sure the group to compare is the first column and then list the last column as the final variable to compare for each group:
library(compareGroups)
resOR <- compareGroups(AcidRefluxBRAVOAv ~ . , NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2, compute = TRUE)


########### Univariate predictors value #############################################
## @knitr UnivariatePredictorsValue
#Rearrange columns so age and gender are together
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2 %>% select(AcidRefluxBRAVOAv,ageInYears,1:12, everything())


#Remove some columns that arent for this table but are for the WDA analysis
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal3<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2%>%select(-contains("worst"))


library(gtsummary)
so<-tbl_summary(
  data = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal3,
  by = AcidRefluxBRAVOAv,missing = "no",
  type = list(all_numeric() ~ "continuous"),
  # change statistics printed in table
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = list("marker" ~ c("Y", "N"))
)
# add p-values, report t-test, round large pvalues to two decimal place
so<-add_p(so,test = list(vars(marker) ~ "t.test"),
          pvalue_fun = function(x) style_pvalue(x, digits = 2))

# bold variable labels, italicize levels
so<- bold_labels(so)
so<-italicize_levels(so)
# bold p-values under a given threshold (default is 0.05)
so<-bold_p(so,t = 0.05)
# include percent in headers
so<-modify_header(so,stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")
as_gt(so)

#Choose variables automatically from the univariate analysis:


sodf<-data.frame(so$meta_data,stringsAsFactors = FALSE)
sodf<-sodf%>%select(var_label,p.value)%>%filter(p.value < 0.5e-01)



NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal4<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2%>%select(-"AcidRefluxBRAVOAv")


soWDA<-tbl_summary(

  data = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal4,
  by = WorstD_ayAnalysisGORDPositive,missing = "no",
  type = list(all_numeric() ~ "continuous"),
  # change statistics printed in table
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = list("marker" ~ c("Y", "N"))
)
# add p-values, report t-test, round large pvalues to two decimal place
soWDA<-add_p(soWDA,test = list(vars(marker) ~ "t.test"),
          pvalue_fun = function(x) style_pvalue(x, digits = 2))

# bold variable labels, italicize levels
soWDA<- bold_labels(soWDA)
soWDA<-italicize_levels(soWDA)
# bold p-values under a given threshold (default is 0.05)
soWDA<-bold_p(soWDA,t = 0.05)
# include percent in headers
soWDA<-modify_header(soWDA,stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")
as_gt(soWDA)

#Choose variables automatically from the univariate analysis:
soWDA_df<-data.frame(soWDA$meta_data,stringsAsFactors = FALSE)
soWDA_df<-soWDA_df%>%select(var_label,p.value)%>%filter(p.value < 0.5e-01)


options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'x-small', data_row.padding = gt::px(1))")

soWDAandAV<-tbl_merge(list(so,soWDA),tab_spanner = c("Average day analysis", "Worst day analysis"))
soWDAandAV




#Need to choose more variables here from the univariate above based on the p--value <0.05
chosen<-data.frame(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$AcidRefluxBRAVOAv, NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2[, Hmisc::label(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2) %in% sodf$var_label])
chosen<-chosen %>% dplyr::rename(AcidRefluxBRAVOAv = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2.AcidRefluxBRAVOAv)

#Need to choose more variables here from the univariate above using average day analysis based on the p--value <0.05
chosenWDA<-data.frame(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$WorstD_ayAnalysisGORDPositive, NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2[, Hmisc::label(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2) %in% soWDA_df$var_label])
chosenWDA<-chosenWDA %>% dplyr::rename(WorstD_ayAnalysisGORDPositive = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2.WorstD_ayAnalysisGORDPositive)





########### Univariate predictors graph #############################################
## @knitr UnivariatePredictorsGraph


library(ggplot2)
p1<-ggplot(chosen, aes(x = AcidRefluxBRAVOAv, y = chosen[,2])) +
  geom_violin(outlier.shape = NA,fill='red',alpha=0.3)+geom_beeswarm(size=0.5,priority='density',cex = 2)+
  geom_boxplot(width=0.1,alpha=0.4)+
  scale_colour_Publication()+
  theme_Publication()+
  ylab(Hmisc::label(chosen[2]))+
  xlab("WPM-GERD")
#+
 # ggsave("Figure2a.jpeg", width = 20, height = 20, dpi = 300, units="cm")

p2<-ggplot(chosen, aes(x = AcidRefluxBRAVOAv, y = chosen[,3])) +
  geom_violin(outlier.shape = NA,fill='red',alpha=0.3)+geom_beeswarm(size=0.5,priority='density',cex = 2)+
  geom_boxplot(width=0.1,alpha=0.4)+
  scale_colour_Publication()+
  theme_Publication()+
  ylab(Hmisc::label(chosen[3]))+
  xlab("WPM-GERD")
#+
 # ggsave("Figure2b.jpeg", width = 20, height = 20, dpi = 300, units="cm")

p3<-ggplot(chosen, aes(x = AcidRefluxBRAVOAv, y = chosen[,4])) +
  geom_violin(outlier.shape = NA,fill='red',alpha=0.3)+geom_beeswarm(size=0.5,priority='density',cex = 2)+
  geom_boxplot(width=0.1,alpha=0.4)+
  scale_colour_Publication()+
  theme_Publication()+
  ylab(Hmisc::label(chosen[4]))+
  xlab("WPM-GERD")
#+
 # ggsave("Figure2c.jpeg", width = 20, height = 20, dpi = 300, units="cm")

#

  p4<-ggplot(chosen, aes(y = AcidRefluxBRAVOAv)) +
    geom_bar(aes(fill = chosen[,5]),position="fill") +
    scale_colour_Publication()+
    theme_Publication()+
    xlab("Number of studies")+
    ylab("GERD")+
    labs(fill = "Oesophagitis") + scale_fill_discrete(name = "Oesophagitis", labels = c("No", "Yes"))


   #theme(text=element_text(size=10, family="Arial")) +
   #ggsave("Figure2c.jpeg", width = 20, height = 20, dpi = 300, units="cm")


ggarrange(p1,p2,p3,p4,
          labels = c("a)","b)","c)"),
          nrow = 2,
          ncol = 2)+
  theme(text=element_text(size=6, family="Arial"))
 #ggsave("Figure2_Complete.jpeg", width = 20, height = 20, dpi = 300, units="cm")


########### Multiple Logistic Regression2 #############################################
## @knitr HRM_MultipleLogRegression2


#To get univariate and multivariate odds ratios:
library(finalfit)
chosen$AcidRefluxBRAVOAv<-as.factor(chosen$AcidRefluxBRAVOAv)

#Note for some reason I cant get Age and SAP ones in here
explanatory = names(chosen[,!grepl("AcidRefluxBRAVOAv",names(chosen))])
dependent = 'AcidRefluxBRAVOAv'


mytable<-chosen %>%
  finalfit.glm(dependent, explanatory)


#convert oesophagitis to numeric
chosen$Oesophagitis<-ifelse(grepl("Y",chosen$Oesophagitis),1,0)

mod1 <- glm(chosen$AcidRefluxBRAVOAv ~ ., data = dplyr::select_if(chosen, is.numeric), family = binomial(link = "logit"))
AvReg<-tbl_regression(mod1, exponentiate = TRUE)%>%bold_p(t = 0.05)


#Choose variables automatically from the univariate analysis:
AvReg_df<-data.frame(AvReg$table_body,stringsAsFactors = FALSE)
AvReg_df<-AvReg_df%>%select(variable,p.value)%>%filter(p.value < 0.5e-01)


####### For WDA:
#convert oesophagitis to numeric
chosenWDA$Oesophagitis<-ifelse(grepl("Y",chosenWDA$Oesophagitis),1,0)
chosenWDA$WorstD_ayAnalysisGORDPositive<-as.factor(chosenWDA$WorstD_ayAnalysisGORDPositive)

#Note for some reason I cant get Age and SAP ones in here
explanatory = names(chosenWDA[,!grepl("WorstD_ayAnalysisGORDPositive",names(chosenWDA))])
dependent = 'WorstD_ayAnalysisGORDPositive'


mytableWDA<-chosenWDA %>%
  finalfit.glm(dependent, explanatory)


#convert oesophagitis to numeric
#chosenWDA$Oesophagitis<-ifelse(grepl("Y",chosenWDA$Oesophagitis),1,0)

mod1WDA <- glm(chosenWDA$WorstD_ayAnalysisGORDPositive ~ ., data = dplyr::select_if(chosen, is.numeric), family = binomial(link = "logit"))
WDAReg<-tbl_regression(mod1WDA, exponentiate = TRUE)%>%bold_p(t = 0.051)
tbl_merge(list(AvReg,WDAReg),tab_spanner = c("Average day analysis", "Worst day analysis"))



########## Cut Point For pH <4 #############################################
## @knitr CutPointForAcid

chosen$AcidRefluxBRAVOAv<-as.character(chosen$AcidRefluxBRAVOAv)
#Determine optimal cutpoints for the Percent time <4 (the last column in the chosen table)
library(cutpointr)
vb<-cutpointr(x = chosen[,4], class = chosen$AcidRefluxBRAVOAv,na.rm=TRUE,method = maximize_metric, metric = sum_sens_spec)
dens1<-plot_x(vb)+geom_density(color="white", fill="grey")+
  xlab(Hmisc::label(names(chosen[ncol(chosen)])))+
  ggtitle(paste0(""))+
  labs(subtitle = "")+
theme(text=element_text(size=10, family="Arial"))
#ggsave("Figure3a.jpeg", width = 20, height = 20, dpi = 300, units="cm")
roc1<-plot_roc(vb)+ ggtitle(paste0(""))+
theme(text=element_text(size=10, family="Arial"))
#ggsave("Figure3b.jpeg", width = 20, height = 20, dpi = 300, units="cm")

# Look at the difference between the diagnoses of reflux with worst and average day analysis:
newout<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal
newout$WorstD_ayAnalysisGORDPositive<-gsub("Negative",0,newout$WorstD_ayAnalysisGORDPositive)
newout$WorstD_ayAnalysisGORDPositive<-gsub("Positive",1,newout$WorstD_ayAnalysisGORDPositive)
newout$WorstD_ayAnalysisGORDPositive<-as.numeric(newout$WorstD_ayAnalysisGORDPositive)

newout$AcidRefluxBRAVOAv<-gsub("Negative",0,newout$AcidRefluxBRAVOAv)
newout$AcidRefluxBRAVOAv<-gsub("Positive",1,newout$AcidRefluxBRAVOAv)
newout$AcidRefluxBRAVOAv<-as.numeric(newout$AcidRefluxBRAVOAv)
newout$DiagnosticDifference<-newout$WorstD_ayAnalysisGORDPositive-newout$AcidRefluxBRAVOAv

#Just pick the ones that are different:
DiffDxPatients<-newout[newout$DiagnosticDifference!=0,]

DiffDxPatients<-DiffDxPatients%>%select(AcidRefluxBRAVOAv,DistalLESfromnarescm,Gender,DOBAge,
                        AllSymps_BRAVOgrouped,MainAcidExpTotalClearanceChannelPercentTime,
                        MainAcidExpTotalClearanceChannelNumberofAcidEpisodes,AcidReflux_Imp,
                        ageInYears,Oesophagitis,Grade,M1_av,pspw_index,DiagnosticDifference)



########## diagrammeR #############################################
## @knitr diagrammeR



#Other analyses:

#Day by day analysis to see if patients more likely to have intermittent symptoms especially for reflux and heartburn using the BRAVO results
#Compare BRAVO positive with BRAVO negative
#Are the hypersensitives also the ones with high AET. ie do HOvsHighAET NoHovs HighAET HOvsLowAET and NoHOvsLowAET and compare the groups (high AET is > optimal cutpoint)

