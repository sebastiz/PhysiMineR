

library(dplyr)
library(stringr)
library(here)
library(EndoMineR)
library(DiagrammeR)
library(readxl)
library(tidyr)
library(PhysiMineR)
library(DataExplorer)

##### Import ####
## @knitr dataImport

oldw <- getOption("warn")
options(warn = -1)

#Get this from the data folder for PhysiMineR-  just click on the RData to load into
#the global environment

#Or if using the local file system

BravoDay1And2 <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BravoDay1And2.xls"))
BravoDay3And4 <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BravoDay3And4.xls"))
BRAVOTotal <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/BRAVOTotal.xls"))
Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Diag.xlsx"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
Impedance2 <- read_excel(here::here("inst/Projects/BRAVOStudies/HRMPredictorsOfAllPosBRAVO/data/Impedance2.xls"))

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
#Convert VisitDate into a Date format and extract the year (the month and day are unreliable here as sometimes American and sometimes British dates are used)

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







##### Merge ########################################################################################
## @knitr dataMerge



AllImpedance<-merge(Impedance2,Imp_Symp,by="Imp_Id", all = TRUE)

#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)


AllBravo<-merge(BravoDay1And2,BravoDay3And4,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")









##### Clean2 ############################################################################################################
## @knitr dataClean2

#Remove duplicate reports on the basis of being the same Hospital Number and date range here
#Also need to get rid of the duplicates from the Diag table:


Diag2<-Diag %>%
  arrange(DistalLESnares,HospNum_Id) %>%
  group_by(DistalLESnares,HospNum_Id,lubridate::year(VisitDate4)) %>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))


#Clean up the HRM HRMImportMain (to get rid of duplicates mainly)
#This looks for all the duplicates by hospital number and DistalLESfromnarescm (the latter so that duplicates means same test rather than same patient on two different days)

HRMImportMain2<-AllHRM %>%
  group_by(HospNum_Id,DistalLESfromnarescm) %>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))

#Clean Up the main HRM
HRMImportMain<-HRMCleanUp1(HRMImportMain2)
#Not sure why I did this.
HRMImportMain$LOS_relax<-as.factor(HRMImportMain$LOS_relax)
HRMImportMain$LowerOesoph<-as.factor(HRMImportMain$LowerOesoph)


# Get the whole impedance dataset
#You will need to re-clean the merged dataImpSympImpedance as cleaning function needed to be fixed
Impedance2_Clean<-dataImpClean(AllImpedance)







#Merge all HRM with AllBravo to get the full dataset
#This merges the tests so that the nearest hrm to the BRAVO is chosen to avoid replication
#Note that a lot of Bravo's do not have HRM as may not have been requested by outside referrers
BravoAndHRM <- HRMImportMain %>%
  inner_join(AllBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%
  slice(1)















##### Extrapolate ############################################################################################################
## @knitr dataExtrapolate
#Extract and clean the meds (need to bind to the existing dataframe)
#Run after PhysiData_Acq
#Run after PhysiData_Clean


#Extract some diagnoses from the report (using the histopath extractor from EndoMineR)
Diag2$Dx<-str_extract_all(Diag2$WholeReport, paste0("upragas|neffecti|ackham|utcrack|[Aa]peris|[Ff]requent [Ff]ail" , simplify = FALSE))
Diag2$Dx <- sapply(Diag2$Dx, toString)

#Extract all the symptoms
ImpAll<-dataImpSymptoms(Impedance2_Clean)

#Extract who has reflux based on Demeester or on the Number of refluxes from both Impedance and BRAVO
dataImpSypmAndImpedanceMain<-GORD_AcidImp(ImpAll)

#Need to change this so that is part of the PhysiMineR codebase (when I can get AWS to stop crashing of course)
AllBravo<-GORD_AcidBRAVO(AllBravo)



#2.Recoding
BravoAndHRM$AcidRefluxBRAVO<-gsub("NoAcid",0,BravoAndHRM$AcidRefluxBRAVO)
BravoAndHRM$AcidRefluxBRAVO<-gsub(".*Acid",1,BravoAndHRM$AcidRefluxBRAVO)
BravoAndHRM$Hiatalhernia<-ifelse(BravoAndHRM$Hiatalhernia=="Yes",1,0)
BravoAndHRM$AcidRefluxBRAVO<-as.numeric(BravoAndHRM$AcidRefluxBRAVO)


##### Extrapolate2 ####
## @knitr dataExtrapolate2




##### Data exploration ####
## @knitr DataExploration




#Steps:
#1. Get rid of bad columns with missing data

#Tells you which columns have the most missing data so they can be excluded
missing<-profile_missing(BravoAndHRM)
missing<-data.frame(missing,stringsAsFactors = FALSE)
#Only <10% for each row missing data is accepted
BravoAndHRMNoMissing<-missing[!grepl("Bad|Remove",missing$group),]

#Pick the columns with good data in them from this set
BravoAndHRMNoMissing<-BravoAndHRM[,match(as.character(BravoAndHRMNoMissing$feature),names(BravoAndHRM))]



#3. Get some basic descriptive stats:
rr<-describe(BravoAndHRMNoMissing)





##### Preliminary Analysis ####
## @knitr PrelimAnalysisHRM

#Select HRM only. Also get rid of DistalLESfromnarescm,ProximalLESfromnarescm,EsophageallengthLESUEScenterscm as these are colinear and reflect length of oesophagus altogether)

#4. Variable selection- choose HRM variables only here. Got rid of BasalrespiratorymeanmmHg,ContractileFrontVelocity,HiatusHernia and most oesophageal length measurements because of colinearity concerns with other variables
BravoAndHRMNoMissingFinal<-BravoAndHRMNoMissing %>%
  ungroup()%>%
  select(IntraabdominalLESlengthcm,
         Hiatalhernia,
         LOS_relax,
         BasalrespiratoryminmmHg,
         ResidualmeanmmHg,
         DistalcontractileintegralmeanmmHgcms,
         IntraboluspressureATLESRmmHg,
         Distallatency,
         failedChicagoClassification,
         panesophagealpressurization,
         largebreaks,
         prematurecontraction,
         rapidcontraction,
         smallbreaks,
         AcidRefluxBRAVO)

#Check for multicolinearity
library("PerformanceAnalytics")
chart.Correlation(BravoAndHRMNoMissingFinal, histogram=TRUE, pch=1)
#Exploratory data analysis:
create_report(BravoAndHRMNoMissingFinal)

#Consider changing variables and repeat step 4






#
#
# #### Consort ####
# ## @knitr dataDiagrammeR
#
# nodes <- create_node_df(n=9,
#                         nodes=c("TheOGDReportFinal", "PathDataFrameFinal", "MyOGD","MyPath","FinalDataset","FinalDatasetBarr","LastTestsDone","FirstTestsDone","ff"),
#                         label=c(stringr::str_wrap(paste0("TheOGDReportFinal: ",nrow(TheOGDReportFinal)),5),
#                                 stringr::str_wrap(paste0("PathDataFrameFinal: ",nrow(PathDataFrameFinal)),5),
#                                 stringr::str_wrap(paste0("MyOGD: ",nrow(MyOGD)),5),
#                                 stringr::str_wrap(paste0("MyPath:",nrow(MyPath)),5),
#                                 stringr::str_wrap(paste0("FinalDataset:",nrow(FinalDataset)),5),
#                                 stringr::str_wrap(paste0("FinalDatasetBarr: ",nrow(FinalDatasetBarr)),5),
#                                 stringr::str_wrap(paste0("LastTestsDone: ",nrow(LastTestsDone)),5),
#                                 stringr::str_wrap(paste0("FirstTestsDone: ",nrow(FirstTestsDone)),5),
#                                 stringr::str_wrap(paste0("ff: ",nrow(ff)),5)),
#                         shape = "rectangle",
#                         fontsize=10)
#
# edges <-
#   create_edge_df(
#     from = c(1,2,3,4,5,6,6,6),
#     to =   c(3,4,5,5,6,7,8,9))
#
#
# g <- create_graph(nodes_df=nodes,
#                   edges_df=edges)%>%
#   add_global_graph_attrs(
#     attr = c("layout", "rankdir", "splines"),
#     value = c("dot", "TB", "false"),
#     attr_type = c("graph", "graph", "graph"))
# render_graph(g)










##### CodeDepends ####
## @knitr codeDepends


# sc = readScript(here("inst","TemplateProject","munge", "PreProcessing.R"))
# g = makeVariableGraph( info =getInputs(sc))
# require(Rgraphviz)
# edgemode(g) <- "directed"
# x <- layoutGraph(g, layoutType="neato")
# zz = layoutGraph(g)
# graph.par(list(nodes = list(fontsize = 25)))
# renderGraph(zz)
