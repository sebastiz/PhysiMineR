#+ setup, include=FALSE
knitr::opts_chunk$set(warning=FALSE,message=FALSE,dev = "svg",fig.ext = ".svg") #testline
library(ggplot2)
library(lattice)
library(compare)
library(grid)
library(reshape2)
library(plyr)
library(gtools)
library(dplyr)
library(lubridate)
library(openxlsx)
library(knitr)
library(xtable)
library(directlabels)
library(magrittr)
library(DiagrammeR)
local({
  hook_plot = knit_hooks$get('plot')
  knit_hooks$set(plot = function(x, options) {
    x = paste(x, collapse = '.')
    if (!grepl('\\.svg', x)) return(hook_plot(x, options))
    # read the content of the svg image and write it out without <?xml ... ?>
    paste(readLines(x)[-1], collapse = '\n')
    paste("<figure><img src=\"", opts_knit$get("base.url"), paste(x, collapse = "."),
          "\"><figcaption>", options$fig.cap, "</figcaption></figure>", sep = "")
  })
})


library(stringr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidyr)




MyBariumData<-read.csv("S:\\Gastroenterology\\Seb\\R\\Data\\Radiology\\1stJan08ToDec12016_BariumSwallow.csv",stringsAsFactors=F)
MyBariumData$Best.Hosp.No.<-as.character(MyBariumData$Best.Hosp.No.)
MyBariumData$Request.date<-as.character(MyBariumData$Request.date)
MyBariumData$Event.Date<-as.character(MyBariumData$Event.Date)
MyBariumData$Referring.Location<-as.character(MyBariumData$Referring.Location)
MyBariumData$Site<-as.character(MyBariumData$Site)
MyBariumData$RC<-as.character(MyBariumData$RC)
MyBariumData$Examination<-as.character(MyBariumData$Examination)
MyBariumData$Exam.Name<-as.character(MyBariumData$Exam.Name)
MyBariumData$Modality<-as.character(MyBariumData$Modality)
MyBariumData$Name<-as.character(MyBariumData$Name)
MyBariumData$Room<-as.character(MyBariumData$Room)
MyBariumData$Patient.Type.Des<-as.character(MyBariumData$Patient.Type.Des)
MyBariumData$Reported<-as.character(MyBariumData$Reported)
MyBariumData$Date.Reported<-as.character(MyBariumData$Date.Reported)
MyBariumData$Reported.By.Name<-as.character(MyBariumData$Reported.By.Name)
MyBariumData$Clinical.history<-as.character(MyBariumData$Clinical.history)
MyBariumData$Rep.Text<-as.character(MyBariumData$Rep.Text)
#MyBariumData<-loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\Radiology\\1stJan08ToDec12016_BariumSwallow.xlsx")
#MyBariumData = readWorksheet(MyBariumData, sheet="1stJan08ToDec12016_BariumSwallow",header=TRUE)
#For diagrammR
MyBariumDataAll<-MyBariumData

######Clean the data##########################################
############################################################################################################
############################################################################################################
############################################################################################################
MyBariumData$Request.date<-as.Date(MyBariumData$Request.date,format="%d/%m/%Y",origin="30/12/1899")
MyBariumData$Event.Date<-as.Date(MyBariumData$Event.Date,format="%d/%m/%Y",origin="30/12/1899")
MyBariumData$Rep.Text<-gsub("\\.","\\.\n",MyBariumData$Rep.Text)
#Make sure ENT and ENT-HEAD&NECK are the same
MyBariumData$Name<-gsub("ENT-HEAD&NECK","ENT",MyBariumData$Name)
MyBariumData$Name<-gsub("GENERAL MEDICAL PRACTICE","GEN MEDICINE",MyBariumData$Name)
#Only extract the barium swallows, not the barium swallow with meals
#For diagrammR
MyBariumDataSwall<-MyBariumData[MyBariumData$Examination=="FBASW",]
#For the data
MyBariumData<-MyBariumData[MyBariumData$Examination=="FBASW",]
############################################################################################################
############################################################################################################
############################################################################################################


###################################################################################################
###################################################################################################
#########Categorise The Text########################################################################
###################################################################################################
###################################################################################################
#Get rid of the negatives;

MyBariumData$Rep.Text<-gsub("[Nn]o .*[Tt]ertiary.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Dd]ysmotility.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Aa]chalasia.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Cc]orkscrew.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Bb]ead.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Tt]aper.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)
MyBariumData$Rep.Text<-gsub("[Nn]o .*[Ss]pasm.*?(\\.|$)","",MyBariumData$Rep.Text,perl=TRUE)


MyBariumData$Dysmotility <- ifelse(grepl("[Dd]ysmotility",MyBariumData$Rep.Text,perl=TRUE)&
                                  !grepl("[Nn]o .*[Dd]ysmotility.*?(\\.|$).",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Tertiary <- ifelse(grepl("[Tt]ertiary",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Tt]ertiary.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Bread <-str_extract(MyBariumData$Rep.Text,"[Bb]read")
MyBariumData$Achalasia <- ifelse(grepl("[Aa]chalasia",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Aa]chalasia.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Corkscrew <- ifelse(grepl("[Cc]orkscrew",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Cc]orkscrew.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Beaded <- ifelse(grepl("[Bb]eaded",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Bb]eaded.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Tapering <- ifelse(grepl("[Tt]apering",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Tt]apering.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Spasm <- ifelse(grepl("[Ss]pasm",MyBariumData$Rep.Text,perl=TRUE)&
                                   !grepl("[Nn]o .*[Ss]pasm.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")
MyBariumData$Osteophyte <- ifelse(grepl("[Pp]hyte",MyBariumData$Rep.Text,perl=TRUE)&
                               !grepl("[Nn]o .*[Pp]hyte.*?(\\.|$)",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")



MyBariumData$Any<- ifelse(MyBariumData$Dysmotility=="Yes"|MyBariumData$Tertiary=="Yes"|MyBariumData$Bread=="Yes"|MyBariumData$Achalasia=="Yes"|MyBariumData$Tapering=="Yes"| MyBariumData$Spasm=="Yes","Yes","NO")

MyBariumData$Requestor <- str_extract(MyBariumData$Clinical.history, 'Requested [Bb]y.*')
MyBariumData$Requestor <- str_replace(MyBariumData$Requestor,"Requested [Bb]y: ","")
MyBariumData$Requestor <- str_replace(MyBariumData$Requestor," : ","")
MyBariumData$Requestor<-str_replace(MyBariumData$Requestor,"Anderson, Simon","Simon Anderson")


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
MyBariumData<-Symptoms(MyBariumData,"Clinical.history")


###################################################################################################
###################################################################################################
###################################################################################################
############################# Osteophyte study as a sideline study ################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
Barium_Osteophytes<-MyBariumData[MyBariumData$Osteophyte=="Yes",]
Barium_Osteophytes$Rep.Text<-gsub("\\n","\n",Barium_Osteophytes$Rep.Text)
Barium_Osteophytes$OsteoLine<-str_extract(Barium_Osteophytes$Rep.Text,".*[Pp]hyte.*")
Barium_Osteophytes$Mild<-str_extract(Barium_Osteophytes$Rep.Text,"[Mm]ild|[Mm]inor.*|[Mm]ini.*")
Barium_Osteophytes$Obstruction<-str_extract(Barium_Osteophytes$OsteoLine,".*[Oo]bstr.*")
Barium_Osteophytes$OsteophyteLevel<-str_extract(Barium_Osteophytes$OsteoLine,"[CT]\\d")
Barium_Osteophytes$MultipleYN<-str_extract_all(Barium_Osteophytes$OsteoLine,"[CT]\\d")
Barium_OsteophytesLevel<-Barium_Osteophytes[!is.na(Barium_Osteophytes$OsteophyteLevel),]

Barium_Osteophytes$Dysphagic <- ifelse(grepl("[Hh]igh|[Uu]pper|[Oo]roph|[Dd]ysph|[Oo]donoph",Barium_Osteophytes$Clinical.history,perl=TRUE),"Yes","NO")

#Find the number who actually have dysphagia
MyOsteophyteLevels<-table(Barium_OsteophytesLevel$OsteophyteLevel)
barplot(MyOsteophyteLevels)







###################################################################################################
##########################  Do some easy demographic stuff#########################################
###################################################################################################
###################################################################################################
#Who is ordering all this Barium?
detach(package:plyr)
MyBariumGroups<- MyBariumData %>%
  group_by(Name) %>%
  dplyr::summarise (n=n())


MyBariumGroups<-data.frame(MyBariumGroups[MyBariumGroups$n>100,])


OrderByGroup<-ggplot(MyBariumGroups,aes(MyBariumGroups$Name,y=MyBariumGroups$n))+
  geom_bar(aes(Name),stat="identity")+
  theme(legend.position="none") +
  labs(title="Number of barium swallows filtered \nfor >100 Total since 1/1/2008)") +
  xlab("Speciality") +
  ylab("Number of barium swallows") +
  theme(axis.text.x=element_text(angle=-90))




#Do swallows over over time per speciality for the filtered group:
MyBariumByFilteredBySpecOrdeingOver100<-MyBariumData[MyBariumData$Name %in% MyBariumGroups$Name,]

#What are the indications for ordering all these tests?
#Organise by speciality broken down by indication if possible

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\Analytics.R")
NumOrderedBaSwallBySpec<-TimeLine(MyBariumByFilteredBySpecOrdeingOver100,"Event.Date","Name")

#Just extract gastroenterology for further analysis:

MyBariumData_Gastro<-MyBariumData[MyBariumData$Name=="GASTROENTEROLOGY",]

#Tidy the data by symptoms listed so can be grouped
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Dysphagia=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Hoarse=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Cough=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$StomachPain=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Nausea=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Vomiting=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Heartburn=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Regurgitation=="Yes",])
nrow(MyBariumData_Gastro[MyBariumData_Gastro$Globus=="Yes",])


MyBariumData_GastroTBB<-MyBariumData_Gastro %>%
  gather(variable, value,Dysphagia,Hoarse,Cough,StomachPain,Nausea,Vomiting,Heartburn,Regurgitation,Globus)

#Now determine which symptoms seem to be triggering all this barium swallow ordering:
MyBariumData_GastroTBB<-MyBariumData_GastroTBB %>%
  mutate(year=format(Event.Date, "%Y"))%>%
  mutate(month=format(Event.Date, "%m"))

mydf<-MyBariumData_GastroTBB %>%
  group_by(year,variable)

mydf<-dplyr::filter(mydf,value=="Yes")%>%
dplyr::summarize(n=n())

BaSwallowBySymptom<-ggplot(mydf,aes(year,n))+
  geom_line(aes(group=variable,colour=variable,size=12))+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = variable), method = list("smart.grid", cex = 1,hjust=-.5))+
  xlab("Year") +
  ylab("Freq")+
  scale_colour_discrete(guide = 'none')  +
  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  theme(axis.text.x=element_text(size=18)) +
  theme(axis.text.y=element_text(size=18)) +
  theme(axis.title=element_text(size=20))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position="top")


MyBariumData_GastroTBB$DatesMerge<-paste(MyBariumData_GastroTBB$year,MyBariumData_GastroTBB$month,sep=" ")
#



#Now just extract the one symptom type: Dysphagia and look at the indications more closely:

MyBariumData_GastroTBB_Dysphagia<-MyBariumData_GastroTBB %>%
  filter(variable=="Dysphagia")

#Now seperate into justified vs unjustified barium swallows:
MyBariumData_GastroTBB_Dysphagia$Justified<-ifelse(grepl("[Gg]lobus",MyBariumData_GastroTBB_Dysphagia$Clinical.history,perl=TRUE)|
                                                     grepl("[Hh]igh",MyBariumData_GastroTBB_Dysphagia$Clinical.history,perl=TRUE)|
                                                     grepl("[Uu]pper",MyBariumData_GastroTBB_Dysphagia$Clinical.history,perl=TRUE)|
                                                     grepl("[Oo]roph",MyBariumData_GastroTBB_Dysphagia$Clinical.history,perl=TRUE),
                                                   "Yes","No")
#Who is ordering all the unjustified barium swallows?
MyBariumData_GastroTBB_DysphagiaNotJust<-MyBariumData_GastroTBB_Dysphagia %>%
  filter(Justified=="No")

#Tabulate them:
NonJustBaSwallows<-data.frame(table(MyBariumData_GastroTBB_DysphagiaNotJust$Requestor))
NonJustBaSwallows<-tail(NonJustBaSwallows[order(NonJustBaSwallows$Freq),],15)

MyBariumData_GastroTBB_DysphagiaNotJust<-MyBariumData_GastroTBB_DysphagiaNotJust[MyBariumData_GastroTBB_DysphagiaNotJust$Requestor %in% NonJustBaSwallows$Var1,]

ggplot(MyBariumData_GastroTBB_DysphagiaNotJust,aes(Requestor))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=-90))

#Now plot the top 15 requestors over time to see if its any different and can explain the phenomenon:

MyBariumData_GastroTBB_DysphagiaNotJust<-MyBariumData_GastroTBB_DysphagiaNotJust %>%
  mutate(year=format(Event.Date, "%Y"))%>%
  mutate(month=format(Event.Date, "%m"))

mydf<-MyBariumData_GastroTBB_DysphagiaNotJust %>%
  group_by(year,Requestor)%>%
  dplyr::summarize(n=n())

#Need to do by a stacked barchart
NumBaSwallowByRequestorInGastro<-ggplot(mydf,aes(x=year,y=n,fill=Requestor))+geom_bar(stat="identity")
#What are the indications for ordering all these tests?
#Organise by speciality broken down by indication if possible


###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################


###################################################################################################
###################################################################################################
###################################################################################################
#######################  Import the HRM data ######################################################
###################################################################################################
###################################################################################################

.libPaths()
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")
data <- sqlQuery( channel , "SELECT  HRMImportMain.* FROM HRMImportMain")
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
#Some cleaning up
#Need to apply this to one column only
#data<-replace(data[1:ncol(data)], is.na(data[1:ncol(data)]), "Nought")
data$VisitDate<-as.character(data$VisitDate)
data$VisitDate<-as.Date(data$VisitDate,format="%m_%d_%Y")
data$VisitDate<-format(data$VisitDate,"%d_%m_%Y")

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Manometry\\MotilityFunctions.R")
data<-HRMCleanUp1(data)
data<-HRMCleanUp(data)

data$dx[is.na(data$dx)]=0



###################################################################################################
###################################################################################################
########################Cross reference HRM with barium data ######################################
###################################################################################################
###################################################################################################
###################################################################################################


##########Osteophyte Project- sideline####To cross reference with the HRM data
##############################################################################################################
##############################################################################################################
##############################################################################################################
Barium_Osteophytes2<-dplyr::rename(Barium_Osteophytes,HospNum_Id=Best.Hosp.No.)
Barium_Osteophytes2<-dplyr::inner_join(Barium_Osteophytes2,data,by="HospNum_Id")
Barium_Osteophytes2$dx<-gsub("0","Normal",Barium_Osteophytes2$dx)
print(paste("Number of patients with HRM and osteophytes:",nrow(Barium_Osteophytes2)))
print(paste("Number of patients with Normal HRM and osteophytes:",nrow(Barium_Osteophytes2[Barium_Osteophytes2$dx=="Normal",])))
print(paste("Number of patients with abnormal HRM and osteophytes:",nrow(Barium_Osteophytes2[!Barium_Osteophytes2$dx=="Normal",])))
##########Osteophyte Project- sideline######################################################################
##############################################################################################################
##############################################################################################################

#Rename barium HospNum so the columns can be merged
#MyBariumDatas<-MyBariumData
MyBariumData<-MyBariumDatas
MyBariumData<-dplyr::rename(MyBariumData,HospNum_Id=Best.Hosp.No.)

MyBariumDataWithHRM<-dplyr::inner_join(MyBariumData,data,by="HospNum_Id")

#For DiagrammR
MyBariumDataWithHRMAny<-MyBariumDataWithHRM
MyBariumDataWithHRM<-MyBariumDataWithHRMAny
#For the data
MyBariumDataWithHRM$DateDiff<-MyBariumDataWithHRM$Event.Date-MyBariumDataWithHRM$VisitDate
#For diagrammR
MyBariumDataWithHRMD<-subset(MyBariumDataWithHRM,MyBariumDataWithHRM$DateDiff<365)
MyBariumDataWithHRMD<-subset(MyBariumDataWithHRMD,MyBariumDataWithHRM$DateDiff>-365)

#For the data
MyBariumDataWithHRM<-subset(MyBariumDataWithHRM,MyBariumDataWithHRM$DateDiff<365)
MyBariumDataWithHRM<-subset(MyBariumDataWithHRM,MyBariumDataWithHRM$DateDiff>-365)

#Get rid of swallows where fundoplication is mentioned (gives flase IRP raised and therefore false achalsia diagnosis)

MyBariumDataWithHRM<-MyBariumDataWithHRM[!grepl(".*plicat.*",MyBariumDataWithHRM$Rep.Text),]



#Think about subsetting so that only the HRM done closest to time of the barium swallow is chosen for each barium swallow

#Convert NAs to 0's
MyBariumDataWithHRM$dx[is.na(MyBariumDataWithHRM$dx)]=0

#TO prevent having duplicate barium swallows for a patient, pick the HRM closesnt to the barium
#For DiagrammR:
MyBariumDataWithHRMNoDups<-MyBariumDataWithHRM%>%
  group_by(HospNum_Id,HospNum_Id)%>%
  slice(which.min(DateDiff))

#MyBariumDataWithHRMNoDups2<-as.data.frame(MyBariumDataWithHRMNoDups)

#For the real data
MyBariumDataWithHRM<-MyBariumDataWithHRM%>%
  group_by(HospNum_Id,HospNum_Id)%>%
  slice(which.min(DateDiff))

##########################################################################################
##########################################################################################
##########Sensitivity and Specificity Analysis############################################
##########################################################################################
##########################################################################################

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\Analytics.R")
#Get the motility Sens and Spec
MotilTableSensAndSpec<-function(x){
vecTertiary<-SensSpec(x,x$Tertiary)
vecDysmotility<-SensSpec(x,x$Dysmotility)
vecSpasm<-SensSpec(x,x$Spasm)
vecAchalasia<-SensSpec(x,x$Achalasia)
vecBeaded<-SensSpec(x,x$Beaded)
vecTapering<-SensSpec(x,x$Tapering)
vecAny<-SensSpec(x,x$Any)

SensAndSpecDatafr<-data.frame(t(cbind(vecTertiary,vecDysmotility,vecSpasm,vecAchalasia,vecBeaded,vecTapering)))
names(SensAndSpecDatafr)<-c("Sensitivity","Specificity","NPV","PPV","Accuracy","TotalNumberPosBarium","TotalNumberNegBarium","TP","FP","TN","FN")
return(SensAndSpecDatafr)
}

MyBariumDataWithHRM<-as.data.frame(MyBariumDataWithHRM)

#When comparing all HRM diagnoses with barium swallow
AllSensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRM)

#Now do for Spastic disorders only- leave out absent peristalsis as its an achalasia component
MyBariumDataWithHRMSpastic<-MyBariumDataWithHRM[!grepl("FrequentFailedPeristalsis|AbsentPeristalsis|WeakPeristalsis",MyBariumDataWithHRM$dx),]
SpasticSensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMSpastic)


#Now do for AChalasia/EGOO only from the spastic dataset- make sure that all the diagnoses that are not achalasia are called normal for this situation
MyBariumDataWithHRMAchalasia<-MyBariumDataWithHRMSpastic
MyBariumDataWithHRMAchalasia$dx<-ifelse(!grepl(".*EGOO.*|.*[Aa]chalasia.*",MyBariumDataWithHRMAchalasia$dx,perl=TRUE),"Normal","Achalasia")

AchaslasiaSensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMAchalasia)


#Now do only for type 1 achalasia
MyBariumDataWithHRMAchalasia_Type1<-MyBariumDataWithHRMSpastic
#This time need to subset the data to get Type1's only as barium doesn't subtype
MyBariumDataWithHRMAchalasia_Type1<-MyBariumDataWithHRMAchalasia_Type1[MyBariumDataWithHRMAchalasia_Type1$dx=="AchalasiaType1",]
MyBariumDataWithHRMAchalasia_Type1$dx<-ifelse(!grepl("AchalasiaType1",MyBariumDataWithHRMAchalasia_Type1$dx,perl=TRUE),"Normal","Achalasia")
#AchaslasiaSensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMAchalasia_Type1)

#Now do for all spastic disorders that are not achalasia:
MyBariumDataWithHRMSpasticNotAchalasia<-MyBariumDataWithHRM
MyBariumDataWithHRMSpasticNotAchalasia$dx<-ifelse(!grepl(".*DES.*|.*Jackhammer.*|.*Nutcracker.*|.*Rapid.*|.*Hypertensive.*",MyBariumDataWithHRMSpasticNotAchalasia$dx,perl=TRUE),"Normal","Spastic")
gfgffdgdfgs<-MotilTableSensAndSpec(MyBariumDataWithHRMSpasticNotAchalasia)


MyBariumDataWithHRMAbsent<-MyBariumDataWithHRM[grepl("Absent",MyBariumDataWithHRM$dx),]
AbsentSensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMAbsent)

#Now do for DES
MyBariumDataWithHRM_DES<-MyBariumDataWithHRM[grepl("DES",MyBariumDataWithHRM$dx),]
DES_SensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMSpastic)

#Now do Spastic and bread swallows only
MyBariumDataWithHRMSpastic_Bread<-MyBariumDataWithHRMSpastic[!is.na(MyBariumDataWithHRM$Bread),]
Spastic_Bread_SensAndSpecDatafr<-MotilTableSensAndSpec(MyBariumDataWithHRMSpastic_Bread)


MyBariumDataWithHRM_Achalsia<-MyBariumDataWithHRM[grepl("[A]chalasia",MyBariumDataWithHRM$dx),]
MyBariumDataWithHRM_Achalsia<-as.data.frame(MyBariumDataWithHRM_Achalsia)
MyBariumDataWithHRM_Achalsia_Yes<-MyBariumDataWithHRM_Achalsia[grepl("Yes",MyBariumDataWithHRM_Achalsia$Achalasia),]
MyBariumDataWithHRM_Achalsia_No<-MyBariumDataWithHRM_Achalsia[grepl("NO",MyBariumDataWithHRM_Achalsia$Achalasia),]


#
 ggplot(MyBariumDataWithHRM_Achalsia,aes(x=ResidualmeanmmHg,fill=Achalasia))+
   geom_histogram()
#

#Get the subsetted data
MyBariumDataWithHRMTertiary<-MyBariumDataWithHRM[MyBariumDataWithHRM$Tertiary=="Yes",]
MyBariumDataWithHRMTertiary$DateDiff<-MyBariumDataWithHRMTertiary$Event.Date-MyBariumDataWithHRMTertiary$VisitDate
MyBariumDataWithHRMCorkScrew<-MyBariumDataWithHRM[MyBariumDataWithHRM$Corkscrew=="Yes",]
MyBariumDataWithHRMBeaded<-MyBariumDataWithHRM[MyBariumDataWithHRM$Beaded=="Yes",]
MyBariumDataWithHRMTapering<-MyBariumDataWithHRM[MyBariumDataWithHRM$Tapering=="Yes",]
MyBariumDataWithHRMDysmotility<-MyBariumDataWithHRM[MyBariumDataWithHRM$Dysmotility=="Yes",]
MyBariumDataWithHRMAchalasia<-MyBariumDataWithHRM[MyBariumDataWithHRM$Achalasia=="Yes",]
MyBariumDataWithHRMSpasm<-MyBariumDataWithHRM[MyBariumDataWithHRM$Spasm=="Yes",]

#Sensitivity and specificity analysis for the detection of spasm and any dysmotility

##################The diagrammR graph to show consort patient selection stuff:####################################
##############################################################################################################################


##############################################################################################################################

AllIndexPreLabel<-paste("All Barium swallows \nwith meals",nrow(MyBariumDataAll), sep = ": ")
MyBariumDataSwallLab<-paste("All Barium swallows \nwithout meals",nrow(MyBariumDataSwall), sep = ": ")
MyBariumDataWithHRMAnyLab<-paste("All Barium swallows \nwith HRMs ",nrow(MyBariumDataWithHRMAny), sep = ": ")
MyBariumDataWithHRMDLab<-paste("All Barium swallows \nwith HRM <365 day difference",nrow(MyBariumDataWithHRMD), sep = ": ")
MyBariumDataWithHRMNoDupsLab<-paste("Closest HRM and Bariums only",nrow(MyBariumDataWithHRMNoDups), sep = ": ")


nodes <- create_nodes(nodes = c(AllIndexPreLabel, MyBariumDataSwallLab, MyBariumDataWithHRMAnyLab,MyBariumDataWithHRMDLab,MyBariumDataWithHRMNoDupsLab),
                      label = TRUE,
                      fontsize = 55,
                      fontcolour = "White",
                      type = "lower",
                      style = "filled",
                      color = "aqua",
                      shape = c("circle"),
                      x = c(0,0,0,0,0),
                      y = c(600,300,0,-300,-600,-900))

edges <- create_edges(from = c(AllIndexPreLabel, MyBariumDataSwallLab, MyBariumDataWithHRMAnyLab,MyBariumDataWithHRMDLab),
                      to = c(MyBariumDataSwallLab, MyBariumDataWithHRMAnyLab,MyBariumDataWithHRMDLab,MyBariumDataWithHRMNoDupsLab),
                      rel = c(nrow(MyBariumDataAll), nrow(MyBariumDataSwall), nrow(MyBariumDataWithHRMAny),
                              nrow(MyBariumDataWithHRMD)),
                      arrowhead = rep("normal", 60),
                      # color = c("red", "red", "red", "red", "red", "red"),
                      length = c(500,200,50,50),
                      fontsize = 55,
                      width=c(nrow(MyBariumDataAll)/100,nrow(MyBariumDataSwall)/100,nrow(MyBariumDataWithHRMAny)/100,nrow(MyBariumDataWithHRMD)/100))


graph <-
  create_graph(
    nodes_df = nodes,
    edges_df = edges,
    graph_attrs <-c("layout = visNetwork","overlap = FALSE","outputorder = edgesfirst"),
    edge_attrs = "color = white")

render_graph(graph, output = "vivagraph")
# View the graph


#' ---
#' author: Sebastian Zeki
#' date: November 12th, 2016
#'      St Thomas' Barium Swallow Requests
#' ---
#'

#+results='asis', echo=FALSE
knitr::kable(MyBariumGroups, digits = 2)
#'Table 1: Barium swallow rates (swallow only. Swallow+meal excluded)
#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 1: Number of barium swallows by requesting speciality since 2008 (filtered for >100)"
OrderByGroup
#'
#'
#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 2: Number of barium swallows by symptom requested by speciality over time"
NumOrderedBaSwallBySpec
#'
#'
#'
#'
#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 3: Number of barium swallows by symptom requested by gastroenterology"
BaSwallowBySymptom
#'
#'
#'
#'
#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 4: Number of barium swallows by requestor for unjustified 'symptoms'"
NumBaSwallowByRequestorInGastro
#'
#'
#'
#'
#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 5: Number of barium swallows by requestor for unjustified 'symptoms'"
#+results='asis', echo=FALSE
#'
#'
#'
#'Table 2: Sensitivity and Specificity of Barium Swallow for 'dysmotility' as measured by HRM within 1 year of the barium swallow
knitr::kable(AllSensAndSpecDatafr, digits = 2)
#'
#'
#'Table 3: Sensitivity and Specificity of Barium Swallow for 'dysmotility (spastic disorders only)' as measured by HRM within 1 year of the barium swallow
knitr::kable(SpasticSensAndSpecDatafr, digits = 2)
#'
#'
#'Table 4: Sensitivity and Specificity of bread-Barium Swallow for 'dysmotility (spastic disorders only)' as measured by HRM within 1 year of the barium swallow
knitr::kable(Spastic_Bread_SensAndSpecDatafr, digits = 2)
#'
render_graph(graph)
#'
#'
#'

#+mygraph='svg', dev='svg',echo=FALSE,fig.height=8, fig.width=8,out.width = "800px",out.height="800px",fig.cap="Figure 2: Adenoma Detection Rate by Instrument Number. Fentanyl usage and mean age by Instrument\also shown"
#Instruments

#To do
#Need to look at how the HRM diagnose are called eg DES etc.

#Presentation to include the reasons for a barium swallow, how it is done, literature review:
#Then demonstration of how we use it
#Then explanation of the data gathering, and the data processing
#Then show the various analyses
#Then the conclusion


writeWorksheetToFile("~\\MyBariumDataWithHRMNoDupsLab.xlsx",data=MyBariumDataWithHRMNoDupsLab,sheet="blabla",startRow=3,startCol=4)

