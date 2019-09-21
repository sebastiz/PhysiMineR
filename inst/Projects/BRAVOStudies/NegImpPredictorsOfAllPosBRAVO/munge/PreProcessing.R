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

#utility function for latex:




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
Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Diag.xlsx"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
ImpedanceTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Impedance2.xls"))

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





Diag<-dataDiagClean(Diag)





#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)
#Get rid of the swallows for now as using the whole HRM dataset makes things slow:
AllHRM<-AllHRM[,!grepl("Num\\d+",colnames(AllHRM))]
HRMImportMainTwo<-AllHRM %>%
  group_by(HospNum_Id,DistalLESfromnarescm)%>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))
#Clean Up the main HRM
HRMImportMainTwo<-HRMCleanUp1(HRMImportMainTwo)



#Merge the BRAVO results together:
AllBravo<-merge(BravoDayOneAndTwo,BravoDayThreeAndFour,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")
AllBravo<-dataBRAVOSymptoms(AllBravo)
AllBravo<-GORD_AcidBRAVO(AllBravo)
AllBravo<-GORD_BravoWDAAndAverage(AllBravo)






















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

#Get the age of the patient at the time of doing the test:
library(eeptools)
ImpAndBravoWithHRM$ageInYears<-age_calc(na.omit(ImpAndBravoWithHRM$MainPtDataDateofBirth),ImpAndBravoWithHRM$MainPtDataDateofAdmission,precise=TRUE)/12

#Get all the Negative Impedance patients (with both positive and negative BRAVO results-#AcidRefluxBRAVO is the outcome variable we will use;)
NegImp_FromImpWithBRavoAndHRM<-ImpAndBravoWithHRM %>% filter(AcidReflux_Imp==0)

#Get rid of all the BRAVO columns as we don't need these as predictive variables
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "^t[Dd]ay" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]
NegImp_FromImpWithBRavoAndHRM<-NegImp_FromImpWithBRavoAndHRM[ , !grepl( "[Ss]wallo" , names( NegImp_FromImpWithBRavoAndHRM ) ) ]


#Impute the gender
library(mice)





##### BRAVO description #############################################
## @knitr BRAVOdescription
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)

#need to replicate Sweis paper and show 1. SAP worst day =63.2% 2. SI Worst day =61% 3. pH worst day <4 (>4.2%) =47% and most in the first 24 hours 4. 45% GERD based on worst day SAP or pH <4


#Positive GORD by pH<4 in 4.2% Worst day overall analysis-
pHWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM[NegImp_FromImpWithBRavoAndHRM$worst>4.9,])/nrow(NegImp_FromImpWithBRavoAndHRM))*100

#Positive GORD by SAP Worst day overall analysis-
SAPWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM %>%select(contains("SAP")) %>%filter_all(any_vars(. >94.9)))/nrow(NegImp_FromImpWithBRavoAndHRM))*100

#Positive GORD by SI Worst day overall analysis-
SIWDA<-(nrow(NegImp_FromImpWithBRavoAndHRM %>%select(contains("SI")) %>%filter_all(any_vars(. >50)))/nrow(NegImp_FromImpWithBRavoAndHRM))*100




#Trajectory assessment
#How many days positive of the 4? (ie how many days with pH>5 on any one day)


a2<-ggplot(NegImp_FromImpWithBRavoAndHRM,aes(worstDaypH))+geom_bar(aes(fill = as.factor(AcidRefluxBRAVO)),colour="black")+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("Worst Day AET pH<4 ")+
  ylab("Number of studies")+
  labs(fill = "GORD") + scale_fill_discrete(name = "GORD", labels = c("No", "Yes"))

#Cant seem to get rid of the outliers so have set the average limit to 25
a3<-ggplot(NegImp_FromImpWithBRavoAndHRM%>%filter(average<25), aes(x = as.factor(AcidRefluxBRAVO),y=average,fill=as.factor(AcidRefluxBRAVO))) +
  geom_boxplot(outlier.shape = NA)+geom_beeswarm(size=1,priority='density',cex=1.5)+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("average AET")+
  ylab("Number of studies")+
  labs(fill = "GORD") + scale_fill_discrete(name = "GORD", labels = c("No", "Yes"))

a4<-ggplot(NegImp_FromImpWithBRavoAndHRM,aes(NumDaysBravoPositive,group=as.factor(AcidRefluxBRAVO)))+geom_histogram(binwidth = 1)+
  scale_colour_Publication()+
  theme_Publication()+
  xlab("Number of days GORD")


ggarrange(a2, a3,a4,
          labels = c("a)","b)","c)"),
          ncol = 2, nrow = 2)




##### Select columns with minimal data for data quality ############################################
## @knitr missingClean



#Remove columns that are not meaningful:
#Impedance symptoms:
#Get all the MainAcidExposure/MainSx columns as your main impedance dataset. Note this also gets rid of composite score(=DeMeester) as it is derived from MainAcidExp so likely to correlate too much for regression
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRM[,grepl("ageInYears|MainAcidEx|MainSx|SxMain|HospNum_Id|DistalLESfromnarescm|Gender|DOBAge|Height|LESlengthcm|PIPfromnarescm|Hiatalhernia|BasalrespiratoryminmmHg|BasalrespiratorymeanmmHg||DistalcontractileintegralmeanmmHgcms|Contractilefrontvelocitycms|IntraboluspressureATLESRmmHg|Distallatency|failedChicagoClassification|panesophagealpressurization|largebreaks|Simultaneous|prematurecontraction|rapidcontraction|smallbreaks|VisitDate|Age|AcidReflux|Upright|Recumbent|SAP",names(NegImp_FromImpWithBRavoAndHRM))]

#Also get rid of all columns containing unrelated symptoms and Nonacid and only look at the acid related things (so not "AllReflux")
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("Unrelated|Nonacid|AllReflux",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Remove RSSI as dont know what it is and SxCorr as it is just a number rather than a symptom score (RSI and SAP derived from it anyway)
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("RSSI|SxCorr|^SI|ChannelTime|Duration|ClearanceTime|Composite|PP",names(NegImp_FromImpWithBRavoAndHRMMinSet))]


#Remove RSSI as dont know what it is and SxCorr as it is just a number rather than a symptom score (RSI and SAP derived from it anyway)
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("RSSI|SxCorr|^SI|ChannelTime|Duration|ClearanceTime|Composite|PP",names(NegImp_FromImpWithBRavoAndHRMMinSet))]

#Combine columns so they are more meaningful. Use the SI and SAP as potential parameters but only if significantly associated
#Combine symptoms so have only three categories- LPR (Throat/cough/Globus)/Oesophageal(Chest pain/regurgitation/belch/vomiting/heartburn)/Abdominal (Stomachpain)
#Also just code the symptoms as being present or not so if any of the members of the category are SAP >95% or SI >50% then it is a positive finding for that category


#Now get rid of BRAVO columns you dont need:
NegImp_FromImpWithBRavoAndHRMMinSet<-NegImp_FromImpWithBRavoAndHRMMinSet[,!grepl("HospNum_Id|HRM_Id|Physician|ReferringPhysician|Operator|BravoID|VisitDate|Stats|Procedure|FileCreationDate|TimeToNext|TimeSinceLast|BravoID|Imp_Id|^id$|PatientScore|MainPtData|GastricChannel|MainProcProcedureStart|MainProcProcedureDuration",names(NegImp_FromImpWithBRavoAndHRMMinSet))]


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
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-exclude_missing(NegImp_FromImpWithBRavoAndHRMMinSet)


######################  Univariate Anaylsis OR ########################################
## @knitr HRM_UnivariateAnalysisOR


NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO<-factor(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal$AcidRefluxBRAVO,
       levels=c(0,1),
       labels=c("Negative", # Reference
                "Positive"))






#Rearrange columns so can do the univariate analysis more efficiently:
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% select(AcidRefluxBRAVO,1:12, everything())


#For the table get rid of variables you dont want to see
NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal %>% dplyr::select(-LOS_relax,
                                                                                                             -DOBAge,
                                                                                                             -LowerOesoph,
                                                                                                             -AllSymps_BRAVOcompartment,
                                                                                                             -AllSymps_BRAVOgrouped,
                                                                                                             -EsophageallengthLESUEScenterscm,
                                                                                                             -ProximalLESfromnarescm,LESlengthcm,
                                                                                                             -Height,
                                                                                                             -PIPfromnarescm,
                                                                                                             -dplyr::contains("TotalAcid"),
                                                                                                             -IntraabdominalLESlengthcm,
                                                                                                             -Hiatalhernia,
                                                                                                             -BasalrespiratoryminmmHg,
                                                                                                             -IntraboluspressureATLESRmmHg,
                                                                                                             -AcidRefluxBRAVOTotalOnly,
                                                                                                             -AllSymps_Impgrouped,
                                                                                                             -AllSymps_BRAVO,
                                                                                                             -AllImpSymptom,
                                                                                                             -AcidReflux_Imp,
                                                                                                             -Age,
                                                                                                             -matches("SI|worst|Day2|ReflD|DeMeester|average|Recumbent|Upright|Day1Pos|Day2Pos|Day3Pos|Day4Pos|Day5Pos|Day6Pos|Trajec"))

#Create labels
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$ageInYears, "label") <- "Age (years)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$LESlengthcm, "label") <- "LOS length (cm)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$DistalcontractileintegralmeanmmHgcms, "label") <- "DCI (mean mmHg/cm/s)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Contractilefrontvelocitycms, "label") <- "CFV (m/s)"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$Distallatency, "label") <- "DL"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$panesophagealpressurization, "label") <- "% Panoesophageal pressurization"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$largebreaks, "label") <- "% Large Breaks"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$prematurecontraction, "label") <- "% Premature contractions"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$rapidcontraction, "label") <- "% Rapid contraction"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$smallbreaks, "label") <- "Number of small breaks"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelNumberofAcidEpisodes, "label") <- "Number of Acid Episodes"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelPercentTime, "label") <- "Percentage Time pH <4"
attr(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$MainAcidExpTotalClearanceChannelLongestEpisode, "label") <- "Longest acid episode"

#Get rid of rows where the impedance data is missing following the merge:
#NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2<-NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2[complete.cases(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2),]


#Make sure the group to compare is the first column and then list the last column as the final variable to compare for each group:
library(compareGroups)
resOR <- compareGroups(AcidRefluxBRAVO ~ . , NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2, compute = TRUE)
restabOR <- createTable(resOR,show.ratio=TRUE,show.p.ratio =FALSE)
#export2md(restabOR, caption="",format='markdown',strip = TRUE, first.strip = TRUE)


# gtsummary::tbl_summary(
#   data = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2,
#   by = AcidRefluxBRAVO,missing = "no") %>%
#   add_p()





#detach("package:compareGroups", unload=TRUE)


########### Multiple Logistic Regression1#############################################
## @knitr HRM_MultipleLogRegression1


#Choose variables automatically from the univariate analysis:

dff<-lapply(resOR,function(x) x$p.mul)
ssd<-unlist(Filter(is.numeric, dff))
ssddf<-data.frame(ssd)
ssddf$att<-row.names(ssddf)
ToFilter<-ssddf%>%filter(ssd < 0.5e-01)
ToFilter$att<-gsub("\\.p\\..*","",ToFilter$att)



#Need to choose more variables here from the univariate above based on the p--value <0.25 (I did this manually)
chosen<-data.frame(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2$AcidRefluxBRAVO, NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2[, Hmisc::label(NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2) %in% ToFilter$att])
chosen<-chosen %>% dplyr::rename(AcidRefluxBRAVO = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2.AcidRefluxBRAVO)

########### Univariate predictors value #############################################
## @knitr UnivariatePredictorsValue


library(gtsummary)
so<-tbl_summary(

  data = NegImp_FromImpWithBRavoAndHRMMinSetQualityFinal2,
  by = AcidRefluxBRAVO,missing = "no",
  type = list(all_numeric() ~ "continuous"),
  # change statistics printed in table
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = list("marker" ~ c(1, 2))
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



########### Univariate predictors graph #############################################
## @knitr UnivariatePredictorsGraph


library(ggplot2)
p1<-ggplot(chosen, aes(x = AcidRefluxBRAVO, y = chosen[,2])) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='density',cex = 2)+
  scale_colour_Publication()+
  theme_Publication()+
  ylab(Hmisc::label(chosen[2]))+
  xlab("WPM pH<4 >6% time")

p2<-ggplot(chosen, aes(x = AcidRefluxBRAVO, y = chosen[,3])) +
  geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='density',cex = 2)+
  scale_colour_Publication()+
  theme_Publication()+
  ylab(Hmisc::label(chosen[3]))+
  xlab("WPM pH<4 >6% time")




ggarrange(p1, p2,
          labels = c("a)","b)"),
          ncol = 2, nrow = 2)






# Stepwise regression model
#full.model <- glm(as.numeric(chosen$AcidRefluxBRAVO) ~ ., data = dplyr::select_if(chosen, is.numeric))
#step.model <- MASS::stepAIC(full.model, direction = "both", trace = FALSE)



########### Multiple Logistic Regression2 #############################################
## @knitr HRM_MultipleLogRegression2


#To get univariate and multivariate odds ratios:
 library(finalfit)
 chosen$AcidRefluxBRAVO<-as.factor(chosen$AcidRefluxBRAVO)
 #Note for some reason I cant get Age and SAP ones in here
 explanatory = names(chosen[,!grepl("AcidRefluxBRAVO",names(chosen))])
 dependent = 'AcidRefluxBRAVO'


mytable<-chosen %>%
  finalfit.glm(dependent, explanatory)


#apa_table(apa_lm$table, caption = "Iris regression table.")
#my_lm <- lm(chosen$AcidRefluxBRAVO ~ ., data = dplyr::select_if(chosen, is.numeric), family = binomial(link = "logit"))
#apa_lm <- papaja::apa_print(my_lm)

mod1 <- glm(chosen$AcidRefluxBRAVO ~ ., data = dplyr::select_if(chosen, is.numeric), family = binomial(link = "logit"))
tbl_regression(mod1, exponentiate = TRUE)%>%bold_p(t = 0.05)


########## Cut Point For pH <4 #############################################
## @knitr CutPointForAcid


 #Determine optimal cutpoints for the Percent time <4
library(cutpointr)
vb<-cutpointr(x = chosen$MainAcidExpTotalClearanceChannelPercentTime, class = chosen$AcidRefluxBRAVO,na.rm=TRUE,method = maximize_metric, metric = sum_sens_spec)
dens1<-plot_x(vb)+geom_density(color="darkblue", fill="lightblue")+
  xlab(Hmisc::label(chosen$MainAcidExpTotalClearanceChannelPercentTime))+
  ggtitle(paste0(""))+
  labs(subtitle = "")
roc1<-plot_roc(vb)+ ggtitle(paste0(""))


vb2<-cutpointr(x = chosen$DistalcontractileintegralmeanmmHgcms, class = chosen$AcidRefluxBRAVO,na.rm=TRUE,method = maximize_metric, metric = sum_sens_spec)
dens2<-plot_x(vb2)+geom_density(color="darkblue", fill="lightblue")+
  xlab(Hmisc::label(chosen$DistalcontractileintegralmeanmmHgcms))+
  ggtitle(paste0(""))+
  labs(subtitle = "")
roc2<-plot_roc(vb2)+ ggtitle(paste0(""))

ggarrange(dens1,roc1,dens2,roc2,
          labels = c("a)","b)","c)","d)"),
          ncol = 2, nrow = 2)



















########## Likelihood of second day positive if first day >2% but < 5% on BRAVO #############################################
## @knitr Day1NegDay2PosBRAVO

#Get rid of all day1 positive BRAVOs
# AllBravoAETmodel<-AllBravo[AllBravo$ReflDay1FractionTimepHLessThan4Total<5.3,]
# #Choose the predictive variables:
# AllBravoAETmodel<-data.frame(AllBravoAETmodel$Day2Pos,AllBravoAETmodel$ReflDay1FractionTimepHLessThan4Total)
#
#
# #Does Day 1 (normal)  predict day 2
# full.model2 <- glm(as.factor(AllBravoAETmodel$AllBravoAETmodel.Day2Pos) ~ AllBravoAETmodel.ReflDay1FractionTimepHLessThan4Total, data = AllBravoAETmodel,family=binomial(),na.action=na.omit)
# step.model2 <- MASS::stepAIC(full.model2, direction = "both", trace = FALSE)
# dd<-summary(step.model2)
#
#
# #Determine optimal cutpoints for the Percent time <4
# vb2<-cutpointr(x =AllBravoAETmodel$AllBravoAETmodel.ReflDay1FractionTimepHLessThan4Total , class = AllBravoAETmodel$AllBravoAETmodel.Day2Pos,na.rm=TRUE)
# summary(vb2)
# plot(vb2)
#
#
# ########## Likelihood of second day positive if first day >2% but < 5% on BRAVO #############################################
# ## @knitr Day1NegDay2Pos48pH
#
# FourtyEightHourpHImp <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/48H final.xlsx"))
# #Get rid of all day1 positive BRAVOs
# FourtyEightHourpHImpAETmodel<-FourtyEightHourpHImp[FourtyEightHourpHImp$ReflDay1FractionTimepHLessThan4Total<4.2,]
# FourtyEightHourpHImpAETmodel$Day2Pos<-ifelse(FourtyEightHourpHImpAETmodel$ReflDay2FractionTimepHLessThan4Total>5.9,1,0)
# #Choose the predictive variables:
# FourtyEightHourpHImpAETmodel<-data.frame(FourtyEightHourpHImpAETmodel$Day2Pos,FourtyEightHourpHImpAETmodel$ReflDay1FractionTimepHLessThan4Total)
#
#
# #Does Day 1 (normal)  predict day 2
# full.model3 <- glm(as.factor(FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.Day2Pos) ~ FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.ReflDay1FractionTimepHLessThan4Total, data = AllBravoAETmodel,family=binomial(),na.action=na.omit)
# step.model3 <- MASS::stepAIC(full.model3, direction = "both", trace = FALSE)
# dd<-summary(step.model3)
#
# FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.Day2Pos<-as.factor(FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.Day2Pos)
# #Determine optimal cutpoints for the Percent time <4
# vb3<-cutpointr(x = FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.ReflDay1FractionTimepHLessThan4Total , class = FourtyEightHourpHImpAETmodel$FourtyEightHourpHImpAETmodel.Day2Pos,na.rm=TRUE)
# summary(vb3)
# plot(vb3)




########## Score creation based on multivariate model #############################################
## @knitr ClusterPositiveBravos

# chosen$IOM<-ifelse(chosen$DistalcontractileintegralmeanmmHgcms<550,"IOM","NoIOM")
# chosenClean<-(chosen[complete.cases(chosen),])
# chosenPosBravo<-chosenClean[chosenClean$AcidRefluxBRAVO=="Positive",]
#
# ggplot(chosenPosBravo, aes(x = IOM, y = MainAcidExpTotalClearanceChannelPercentTime)) +
#   geom_boxplot(outlier.shape = NA,fill="red")+geom_beeswarm(size=2,priority='density',cex = 2)+
#   scale_colour_Publication()+
#   theme_Publication()+
#   ylab(names(chosen[3]))+
#   xlab("pH<4 >6% time")
#
#
#
#
# library(cluster)
# #Exploratory data analysis:clustering
# fit <- kmeans(chosenPosBravo[2:3], 2)
# clusplot(chosenPosBravo[2:3], fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# library(ggbiplot)
# TrialEGOOSet.pca <- prcomp(chosenPosBravo[2:3], center = TRUE,scale. = TRUE)
# ggbiplot(TrialEGOOSet.pca,ellipse=TRUE,obs.scale = 5, var.scale = 5, labels=rownames(chosenPosBravo[2:3]))
#
# library(factoextra)
# fviz_screeplot(TrialEGOOSet.pca, ncp=10)



########## diagrammeR #############################################
## @knitr diagrammeR



#Other analyses:

#Day by day analysis to see if patients more likely to have intermittent symptoms especially for reflux and heartburn using the BRAVO results
#Compare BRAVO positive with BRAVO negative
#Are the hypersensitives also the ones with high AET. ie do HOvsHighAET NoHovs HighAET HOvsLowAET and NoHOvsLowAET and compare the groups (high AET is > optimal cutpoint)

