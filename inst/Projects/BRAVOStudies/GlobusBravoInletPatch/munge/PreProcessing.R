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
Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Diag.xls"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
ImpedanceTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Impedance2.xls"))
Procs <- read_excel(here::here("inst/Projects/BRAVOStudies/GlobusBravoInletPatch/data/AllBravos.xlsx"))

options(warn = oldw)

#Ned to get the all the endoscopy reports here too.




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






#Get the BRAVO procedures

Procs<-data.frame(Procs,stringsAsFactors = FALSE)
Procs$dateofprocedure<-as.Date(Procs$dateofprocedure)
Procs<-Procs %>% rename(HospNum_Id=patientrecordnumber,VisitDate=dateofprocedure,Findings=findigs)
Procs<-data.frame(Procs,stringsAsFactors = FALSE)



##### CrossTestMerge ########################################################################################
## @knitr dataCrossTestMerge


#Merge all HRM with AllBravo to get the full dataset
#This merges the tests so that the nearest hrm to the BRAVO is chosen to avoid replication
#Note that a lot of Bravo's do not have HRM as may not have been requested by outside referrers


#Need to merge on the nearest matching date- cant figure out hw to do this ?using data table?
library(data.table)
Procs<-data.table(Procs)
AllBravo<-data.table(AllBravo)
Diag<-data.table(Diag)

#Get all the BRAVO reports that have an associated BRAVO by the nearest associated date
BravoEndoscopy<-Procs[AllBravo, on=.(HospNum_Id,VisitDate),roll='nearest']
#Get rid of duplicates
BravoEndoscopy<-distinct(BravoEndoscopy)

#Get the Diag for all the endoscopies by nearest date:
BravoAnd_diag<-Diag[BravoEndoscopy, on=.(HospNum_Id,VisitDate),roll='nearest']


#Filter for BRAVO studies doen after 2015 (ie when I arrived so that the template was in use)



###Need to extract the inlet patch from these patients:


library(purrr)
BravoAnd_diag <- BravoAnd_diag %>%
  mutate(
    InletPatchY_N = map(
      Findings, ~ case_when(
        grepl("Inlet patch y\\/n:\\s*N", .x, ignore.case=TRUE) ~ "NoInlet",
        grepl("Inlet patch y\\/n:\\s*Y", .x,ignore.case=TRUE) ~ "YesInlet",
        grepl("[Nn]o Inlet patch", .x,ignore.case=TRUE) ~ "NoInlet",
        grepl("Inlet patch", .x,ignore.case=TRUE) ~  "YesInlet",
        TRUE ~ "NoInlet"
      )
    )
  )

BravoAnd_diag$InletPatchY_N<-unlist(BravoAnd_diag$InletPatchY_N)

###Get the globus patients:


BravoAnd_diag <- BravoAnd_diag %>%
  mutate(
    GlobusY_N = map(
      WholeReport, ~ case_when(
        grepl("globus|lump|ball|throat", .x, ignore.case=TRUE) ~ "YesGlobus",
        TRUE ~ "NoGlobus"
      )
    )
  )

BravoAnd_diag$GlobusY_N<-unlist(BravoAnd_diag$GlobusY_N)


table(BravoAnd_diag$GlobusY_N,BravoAnd_diag$InletPatchY_N)

###~To do####
#Need to get all of the BRAVO results for all patients
#Then separate into inlet and non inlet patch patients

writexl::write_xlsx(BravoAnd_diag,"InletPatchBravo.xlsx",col_names = TRUE)


#To do: How to associate the nearest of one test with the nearest of another test for a patient without losing the other tests for that patient
