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





##################### Inform the feature extraction by firstly extracting the predictors of a positive BRAVO in all comers ###########################

#Define positive BRAVOs as being as per the positive findings on the report on any of those days

#Extract all the positive symptoms for all BRAVOs as any column which is not NA in the SAPTotal-NOTE THIS SHOULD NOW BE A FUNCTION
#SO THE NEXT TIME THE SYMPTOMS SHOULD BE EXTRACTED FROM THE ALLBRAVO DATASET. YOU NEED TO REINTEGRATE THE FUNCTION INTO
#PHYSIMINER AND THEN MAKE THIS HAPPEN INSTEAD OF THIS CODE

myframe<-ImpAndBravoWithHRM[,grepl("SITotal",colnames(ImpAndBravoWithHRM))]
mine<-data.frame(apply(myframe,1,function(y) unlist(paste0(colnames(myframe),y))))
AllSymps<-as.character(apply(mine, 2, paste, collapse=","))

#Now extract the symptoms that have a value associated with them.
AllSymps2<-stringr::str_extract_all(AllSymps,"SITotal[A-Za-z]*\\d+")
ImpAndBravoWithHRM$AllSymps<-unlist(lapply(AllSymps2,function(x) paste0(unlist(x),collapse=",")))

#Now Clean it up
ImpAndBravoWithHRM$AllSymps<-gsub("SITotal","",ImpAndBravoWithHRM$AllSymps)
ImpAndBravoWithHRM$AllSymps<-gsub("\\d*","",ImpAndBravoWithHRM$AllSymps)


#Then extract all the relevant HRM findings for all positive BRAVOs









##### Extrapolate ############################################################################################################
## @knitr dataForking
#Extract the negative impedance


#2.Recoding the reflux columns:
ImpAndBravoWithHRM$AcidReflux_Imp<-gsub("NoAcid",0,ImpAndBravoWithHRM$AcidReflux_Imp)
ImpAndBravoWithHRM$AcidReflux_Imp<-gsub(".*Acid",1,ImpAndBravoWithHRM$AcidReflux_Imp)
ImpAndBravoWithHRM$AcidReflux_Imp<-as.numeric(ImpAndBravoWithHRM$AcidReflux_Imp)


ImpAndBravoWithHRM$AcidRefluxBRAVO<-gsub("NoAcid",0,ImpAndBravoWithHRM$AcidRefluxBRAVO)
ImpAndBravoWithHRM$AcidRefluxBRAVO<-gsub(".*Acid",1,ImpAndBravoWithHRM$AcidRefluxBRAVO)
ImpAndBravoWithHRM$AcidRefluxBRAVO<-as.numeric(ImpAndBravoWithHRM$AcidRefluxBRAVO)


#Get all the Negative Impedance patients
NegImp_FromImpWithBRavoAndHRM<-ImpAndBravoWithHRM %>% filter(AcidReflux_Imp==0)
#Get rid of all the BRAVO columns as we don't need these as predictive variables
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Dd]ay" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Ss]wallo" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]

#AcidRefluxBRAVO is the outcome variable we will use;






##### Clean columns with minimal data for data quality ############################################

## @knitr missingClean

#1. More cleaning....Get rid of bad columns with missing data
#Tells you which columns have the most missing data so they can be excluded


#Only include certain meaningful columns from the impedance data and from the HRM data

#Impedance symptoms:
#Get all the MainAcidExposure/MainSx columns as your main impedance dataset. Note this also gets rid of composite score(=DeMeester) as it is derived from MainAcidExp so likely to correlate too much for regression
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRM[,grepl("MainAcidEx|MainSx|SxMain|HospNum_Id|DistalLESfromnarescm|Gender|DOBAge|Height|LESlengthcm|PIPfromnarescm|Hiatalhernia|BasalrespiratoryminmmHg|BasalrespiratorymeanmmHg||DistalcontractileintegralmeanmmHgcms|Contractilefrontvelocitycms|IntraboluspressureATLESRmmHg|Distallatency|failedChicagoClassification|panesophagealpressurization|largebreaks|Simultaneous|prematurecontraction|rapidcontraction|smallbreaks|VisitDate|Age|AcidReflux",names(NegImp_FromImpWithBRavoAndHRM))]

#Also get rid of all columns containing unrelated symptoms and Nonacid and only look at the acid related things (so not "AllReflux")
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("Unrelated|Nonacid|AllReflux",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Remove RSSI as dont know what it is and SxCorr as it is just a number rather than a symptom score (RSI and SAP derived from it anyway)
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("RSSI|SxCorr|^SAP|^SI",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Combine symptoms so have only three categories- LPR (Throat/cough/Globus)/Oesophageal(Chest pain/regurgitation/belch/vomiting/heartburn)/Abdominal (Stomachpain)
#Also just code the symptoms as being present or not so if any of the members of the category are SAP >95% or SI >50% then it is a positive finding for that category


NegImp_FromImpWithBRavoAndHRMMinSet <- NegImp_FromImpWithBRavoAndHRMMinSet %>%
  mutate(
    SAPOesophageal = case_when(
  SxMainRSAPAcidVomiting >94 ~ 1,
  SxMainRSAPAcidNausea  >94 ~ 1,
  SxMainRSAPAcidChestPain  >94 ~ 1,
  SxMainRSAPAcidHeartburn  >94 ~ 1,
  SxMainRSAPAcidRegurgitation  >94 ~ 1,
  SxMainRSAPAcidBelch  >94 ~ 1,
  TRUE ~ 0
)) %>%
  mutate(
    SIOesophageal = case_when(
      MainSxRSIAcidVomiting >49 ~ 1,
      MainSxRSIAcidNausea  >49 ~ 1,
      MainSxRSIAcidChestPain  >49 ~ 1,
      MainSxRSIAcidHeartburn  >49 ~ 1,
      MainSxRSIAcidRegurgitation  >49 ~ 1,
      MainSxRSIAcidBelch  >49 ~ 1,
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
    )) %>%
  mutate(
    SAPAbdo = case_when(
      SxMainRSAPAcidStomachPain >94 ~ 1,
      TRUE ~ 0
    )) %>%
  mutate(
    SIAbdo = case_when(
      MainSxRSIAcidStomachPain >49 ~ 1,
      TRUE ~ 0
    ))

#Now get rid of the component symptoms as you dont need them anymore as they have been aggregate with the case_when above:
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("SxMain|MainSx",names(NegImp_FromImpWithBRavoAndHRMMinSet))]



#Now get rid of BRAVO columns you dont need:
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("BravoID|VisitDate|Stats|Procedure|FileCreationDate|TimeToNext|TimeSinceLast|BravoID|MainStats|PP|Imp_Id|^id$|PatientScore|Upright|Recumbent|GastricChannel|MainProcProcedureStart|MainProcProcedureDuration",names(NegImp_FromImpWithBRavoAndHRMMinSet))]









exclude_missing<-function(df){
  missing<-profile_missing(df)
  missing<-data.frame(missing,stringsAsFactors = FALSE)
  #Only <10% for each row missing data is accepted
  df_m<-missing[!grepl("Remove",missing$group),]
  #Pick the columns with good data in them from this set
  df<-df[,match(as.character(df_m$feature),names(df))]
  df<-data.frame(df,stringsAsFactors = FALSE)
  #3. Get some basic descriptive stats:
  #rr<-describe(df)
  #Select only the numeric variables
  df<-dplyr::select_if(df, is.numeric)
  return(df)
}

#Only use columns with good quality data:
NegImp_FromImpWithBRavoAndHRMMinSetQuality<-exclude_missing(NegImp_FromImpWithBRavoAndHRMMinSet)






# ##### Variable Selection #############################################
#
# ## @knitr VariableSelection
#
# #Variable selection- choose HRM variables only here. Got rid of BasalrespiratorymeanmmHg,ContractileFrontVelocity,HiatusHernia and most oesophageal length measurements because of colinearity concerns with other variables
#Refine further after checking multiple co-linearity
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQuality %>%
ungroup()%>%
select( LESlengthcm,
       BasalrespiratoryminmmHg,
       ResidualmeanmmHg,
       DistalcontractileintegralmeanmmHgcms,
       Contractilefrontvelocitycms,
       Distallatency,
       failedChicagoClassification,
       panesophagealpressurization,
       largebreaks,
       prematurecontraction,
       rapidcontraction,
       smallbreaks,
       AcidRefluxBRAVO,
       MainAcidExpTotalClearanceChannelNumberofAcidEpisodes,
       MainAcidExpTotalClearanceChannelPercentTime,
       MainAcidExpTotalClearanceChannelMeanAcidClearanceTime,
       MainAcidExpTotalClearanceChannelLongestEpisode,
       SAPOesophageal,
       SIOesophageal,
       SAPLPR,
       SILPR,
       SAPAbdo,
       SIAbdo)

#Deal with missing data using multiple imputation:
library(missForest)
as<-missForest(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal)
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-as$ximp

# #####  Demographics plotting #############################################
#
# ## @knitr Demographics

library(table1)

#from tutorial https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

#Get the p-values:

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)$p.value
    } else {
      p<-0
      p <- chisq.test(table(y, droplevels(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO<-
  factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO,
         levels=c(0,1,2),
         labels=c("Positive", # Reference
                  "Negative","P-value"))



# ###################### wip Univariate Anaylsis ########################################
# ## @knitr HRM_UnivariateAnalysis
# #Univariate analysis:
NegImpPosBRAVOFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal[NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO=="Positive",]
NegImpNegBRAVOFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal[NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO=="Negative",]
#
# #Univariate analysis
t.test(NegImpPosBRAVOFinal$DistalcontractileintegralmeanmmHgcms ,NegImpNegBRAVOFinal$DistalcontractileintegralmeanmmHgcms,paired=FALSE)
t.test(NegImpPosBRAVOFinal$SAPOesophageal,NegImpNegBRAVOFinal$SAPOesophageal,paired=FALSE)
t.test(NegImpPosBRAVOFinal$Distallatency,NegImpNegBRAVOFinal$Distallatency,paired=FALSE)
t.test(NegImpPosBRAVOFinal$largebreaks,NegImpNegBRAVOFinal$largebreaks,paired=FALSE)

Clin<-table(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$SAPOesophageal,NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)






#Some feature selection- need to tidy this up as confusing...
library(Boruta)
boruta_output <- Boruta(AcidRefluxBRAVO ~ ., data=na.omit(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")





#Check for multicolinearity
library("PerformanceAnalytics")
#chart.Correlation(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal, histogram=TRUE, pch=1)
#Exploratory data analysis:
#create_report(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal)

library(ggplot2)
library(GGally)
#ggpairs(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal,mapping = aes(color = AcidRefluxBRAVO))







# ########## Multiple Logistic Regression#############################################
# ## @knitr HRM_MultipleLogRegression
#
 library(MASS)
# # Fit the full model which doesnt give you the interactions just yet:
#Recode the variables:
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO <- ifelse(grepl("Positive",NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO),1,0)
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO<-as.numeric(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO)
 data<-na.omit(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal)


 full.model <- glm(AcidRefluxBRAVO ~ ., data = data)
 # Stepwise regression model
 step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
 summary(step.model)


 #To get this as a decent table for publication see here: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#table_output_for_word_and_rmarkdown_documents

 library(jtools)
 myCIplot<-plot_summs(step.model, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

 #To get into a word document:



 #To get the odds ratios:
 exp(cbind(OR = coef(step.model), confint(step.model)))





 #Get a ROC for your model:
 library(pROC)
 predpr <- predict(full.model,type=c("response"))
 roccurve <- roc(AcidRefluxBRAVO ~ predpr, data = data)
 myROC_plot<-plot(roccurve)
 myROC_AUC<-auc(roccurve)




# detach("package:MASS", unload=TRUE)

#1. How to interpret the model and especially the combination of factors- do I develop a scoring system around this?
#2. Need to clean the oesophageal data a bit more so that classic and non classic oesophageal symptoms are accounted for separately.
#3. Need to get the age of all patients at the time of the test and the genders
 # ########## diagrammeR #############################################
 # ## @knitr diagrammeR

