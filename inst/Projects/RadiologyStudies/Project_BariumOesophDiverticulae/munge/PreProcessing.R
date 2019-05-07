

##### Import ####
## @knitr dataImport


library(dplyr)
library(readxl)
library(EndoMineR)
library(stringr)
library(janitor)
library(lubridate)
library(PhysiMineR)

#Need to reimport this data
myBarium<-read_excel("/home/rstudio/PhysiMineR/inst/Projects/RadiologyStudies/Project_BariumOesophDiverticulae/data/FirstJan07To29Mar2019_BariumSwallow.XLSX")
MyHRM<-read_excel("/home/rstudio/PhysiMineR/inst/Projects/RadiologyStudies/Project_BariumOesophDiverticulae/data/HRMImportMain2.xls")


#Get the OGD_Divertic neatened upt o exclude all the duodenal divertic etc.
#This is the code that was used (with the old EndoscoChopper function)
MyOGDDivertic<-read_excel("/home/rstudio/PhysiMineR/inst/Projects/RadiologyStudies/Project_BariumOesophDiverticulae/data/OGD_Diverticulae.xlsx")

MyOGDDivertic$Endo_ResultText<-gsub("2nd Endoscopist:","Second Endoscopist:",MyOGDDivertic$Endo_ResultText)
#
# OGD_Divertic<-myOGD[grepl("[Gg]astroscopy",myOGD$Endo_ResultName),]
# OGD_Divertic<-OGD_Divertic[grepl("[Dd]ivertic",OGD_Divertic$Endo_ResultText),]
# OGD_Divertic<-data.frame(OGD_Divertic,stringsAsFactors=FALSE)
#
#
# #Bit of a tidy up
# source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
# OGD_Divertic<-as.data.frame(EndoscChopper(OGD_Divertic))
#
# #Lets get rid of the duodenal diverticulae:
# OGD_Divertic$Findings<-gsub("\n.*[Dd]uoden.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Dd]1.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Dd]2.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Bb]ulb.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Aa]mpulla.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Cc]olon.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Ss]igmoid.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Ss]tomach.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Gg]astric.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Aa]ntrum.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Aa]ntral.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Ff]undus.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic$Findings<-gsub("\n.*[Ff]undal.*\n","",OGD_Divertic$Findings,ignore.case=TRUE)
# OGD_Divertic<-OGD_Divertic[grepl("\n.*[Dd]ivertic",OGD_Divertic$Findings),]


##### Clean ####
## @knitr dataClean

#Get the headers all in place
myBarium<-myBarium%>%slice(3:n())
colnames(myBarium) = myBarium[1, ] # the first row will be the header
myBarium = myBarium[-1, ]
myBarium<-janitor::clean_names(myBarium)
myBarium$location<-tolower(myBarium$location)
myBarium$event_date<-excel_numeric_to_date(as.numeric(myBarium$event_date))
#Rename hospital number
myBarium<-myBarium%>%rename(HospNum_Id = hosp_no)

#Remove the ? oesopahgeal body as I think it is real
myBarium$location<-gsub("? oesophageal body","oesophageal body",myBarium$location,fixed=TRUE)
#Just select the oesophageal diverticulae:
myBarium<-myBarium[grepl("oesopha",myBarium$location),]



#For the OGD patients
#Rename hospital number

MyOGDDivertic<-janitor::clean_names(MyOGDDivertic)
MyOGDDivertic<-MyOGDDivertic%>%rename(HospNum_Id = patient_id)

#Clean Up and extrapolate the main HRM
HRMImportMain<-HRMCleanUp1(MyHRM)
HRMImportMain<-HRMDiagnoses(HRMImportMain) #Probably need to get this from the main dataset.

#Clean the dates for the HRM
HRMImportMain$VisitDate<-parse_date_time(HRMImportMain$VisitDate, orders = c("mdy", "dmy"))
HRMImportMain$VisitDate<-as.Date(HRMImportMain$VisitDate)


##### Merge ####
## @knitr dataMerg


#1. Merge barium with HRM results

#To get all the hospital numbers of HRMs done for these barium patients
BariumAndHRM <- HRMImportMain %>%
  inner_join(myBarium, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate - event_date)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id)

#2. Merge OGD with HRM results

OGDAndHRM <- HRMImportMain %>%
  inner_join(MyOGDDivertic, by ="HospNum_Id") %>%
  group_by(HospNum_Id)



##### Extrapolate ####
## @knitr dataExtrapolate
#Extract all the HRMs for the patients you have
#To do this you need to create the SQL query for the Diag table (as the table is too big to export)

AllTheHospNums<-unique(append(OGDAndHRM$HospNum_Id,BariumAndHRM$HospNum_Id))

SQL<-paste0("SELECT Diag.* FROM Diag WHERE HospNum_Id='0370333J' ",paste0(" OR  HospNum_Id='",AllTheHospNums,collapse="'"))

#Then put this into the database (it is saved as DiverticHRM in the database)


## @knitr dataAnalysis_Generic

#Input the Diag query result into the script:
myDiverticFullReport<-read_excel("/home/rstudio/PhysiMineR/inst/Projects/RadiologyStudies/Project_BariumOesophDiverticulae/data/DiverticHRM.xls")

#remerge Diag with the HRM results to get all the oesophageal diverticulae patients with Diagnostic reports

Barium_DiagAndHRM <- BariumAndHRM %>%
  full_join(myDiverticFullReport, by ="HospNum_Id") %>%
  group_by(HospNum_Id)

#Extrapolate a bit more:
Barium_DiagAndHRM$WholeReport<-gsub(".*Diagnosis","Diagnosis",Barium_DiagAndHRM$WholeReport)
Barium_DiagAndHRM$ExtractedDx<-str_extract(Barium_DiagAndHRM$WholeReport,"Diagnosis.*?(?:       |24)")
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"Comments.*?Dr"), Barium_DiagAndHRM$ExtractedDx)
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"Finding.*?Dr"), Barium_DiagAndHRM$ExtractedDx)
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"Finding.*?        "), Barium_DiagAndHRM$ExtractedDx)
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"Finding.*"), Barium_DiagAndHRM$ExtractedDx)
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"[Ss]ummary.*"), Barium_DiagAndHRM$ExtractedDx)
Barium_DiagAndHRM$ExtractedDx<-ifelse(is.na(Barium_DiagAndHRM$ExtractedDx),str_extract(Barium_DiagAndHRM$WholeReport,"[Cc]onclusion.*?Dr"), Barium_DiagAndHRM$ExtractedDx)


#Need further manual extrapolation for diagnoses:

#Get the number of patients you have HRM for:
length(unique(Barium_DiagAndHRM$HospNum_Id))

#Do some descriptive stats on them- see the Pandolfino paper
#Find out the number of patients with abnormalities on HRM
#Categorise the number of different diagnoses.

library(digest)
Barium_DiagAndHRM$HospNum_Id<-unlist(lapply(Barium_DiagAndHRM$HospNum_Id, function(x) digest(x,algo="sha1")))


#Patient demographics:

#Age: Get the DOB column in the correct order
library(psych)

demog<-unique(Barium_DiagAndHRM%>%select(HospNum_Id,sex,age_at_event))
Age<-describe(as.numeric(demog$age_at_event))
Sex<-table(demog$sex)


#Then decribe the hypermotility with divertics
#Have to exclude the ones who had surgery
#Also have to then describe the parameters of those with divertics
#Also give a table of the diagnoses of the remaining ones
#Maybe also give a table of symptoms of the patients
#?Need a trawl to see co-morbidities?



# #### Consort ####
# ## @knitr dataDiagrammeR
