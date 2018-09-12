#Motility functions

#### Data acquisition ####
#This depends on forming a connection to the PhysiPop database. In windows the path is this:
#channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")

#' dataHRM
#' This acquires HRM data from the database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @importFrom RODBC sqlQuery
#' @keywords HRM data acquisition
#' @export
#' @examples #dataHRM(channel)

dataHRM<-function(channel){
  data <- sqlQuery(channel , "SELECT PatientData.*, HRMImportMain.*
    FROM PatientData INNER JOIN HRMImportMain ON PatientData.HospNum_Id = HRMImportMain.HospNum_Id")
  return(data)
}



#' HRMAndSwallows
#' This acquires HRMSwallow data from the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords HRMAndSwallows acquisition
#' @export
#' @examples #HRMAndSwallows(channel)

HRMAndSwallows<-function(channel){


  data <- sqlQuery( channel , "SELECT PatientData.*, HRMImportSwallows.*, HRMImportMain.*
                    FROM PatientData INNER JOIN (HRMImportMain INNER JOIN HRMImportSwallows ON HRMImportMain.HRM_Id = HRMImportSwallows.HRM_Id) ON PatientData.HospNum_Id = HRMImportMain.HospNum_Id
                    WHERE HRMImportMain.HRM_Id=HRMImportSwallows.HRM_Id")
  return(data)
}

#' HRMAndDiag
#' This acquires HRM and associated diagnosis data from the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords HRM and Diag
#' @export
#' @examples #HRMAndDiag(channel)

HRMAndDiag<-function(channel){

  data <- sqlQuery( channel , "SELECT DISTINCT HRMImportMain.*, Diag.IndicANDHx, Diag.*, PatientData.*
                    FROM (PatientData INNER JOIN Diag ON PatientData.HospNum_Id = Diag.HospNum_Id) INNER JOIN HRMImportMain ON PatientData.HospNum_Id = HRMImportMain.HospNum_Id
                    WHERE HRMImportMain.VisitDate=Diag.VisitDate")
  return(data)
}


########################### Clean the motility data #################################################################################
#' HRMCleanUp1
#' This cleans HRM data by making sure all the data is in the correct format
#' @param x dataframe
#' @keywords HRM CleanUp
#' @export
#' @examples #HRMCleanUp1(x)


HRMCleanUp1<-function(x){


  try(x$Gender.1<-gsub("(Male|Female).*","\\1",x$Gender.1))
  try(x$Gender<-gsub("(Male|Female).*","\\1",x$Gender))




  #Clean the demographics
  is.date <- function(x) inherits(x, 'Date')
  if(!is.date(x$VisitDate)){
    x$VisitDate<-as.character(x$VisitDate)
    x$VisitDate<-gsub("(^\\d{2}_\\d{2}_)(\\d{2}$)","\\120\\2",x$VisitDate)
    x$VisitDate<-as.Date(x$VisitDate,"%d_%m_%Y")
  }
  x$DOBAge<-as.character(x$DOBAge)
  x$DOBAge<-as.Date(x$DOBAge,"%Y-%m-%d")
  x$Age<-x$VisitDate-x$DOBAge
  x$Age<-difftime(x$VisitDate,x$DOBAge,units="days")/366.25
  x$Age<-as.numeric(x$Age)

  #Format the columns so they are correct
  #if contains mmHg or cm make sure they are as.numeric(as.character()).
  i1 <- grepl("cm|mmHg", names(x))
  x[i1] <- lapply(x[i1], as.numeric)


  #x[i1] <- lapply(x[grepl("cm|mmHg", names(x))], function(x) as.numeric(as.character(x)))
  x$Hiatalhernia<-as.character((x$Hiatalhernia))

  #Convert other columns to numeric
  x$Distallatency<-as.numeric(as.character((x$Distallatency)))
  x$DCI<-ifelse(!rowSums(is.na(x[c("DistalcontractileintegralhighestmmHgcms", "DistalcontractileintegralmeanmmHgcms")])), x$DistalcontractileintegralhighestmmHgcms, rowSums(x[c("DistalcontractileintegralhighestmmHgcms", "DistalcontractileintegralmeanmmHgcms")], na.rm=TRUE) )


  #Sort out the DCI, simultaneous contractions and LOS relaxation
  x$DistalcontractileintegralhighestmmHgcms[is.na(x$DistalcontractileintegralhighestmmHgcms)]=0
  x$DistalcontractileintegralmeanmmHgcms[is.na(x$DistalcontractileintegralmeanmmHgcms)]=0
  x$DistalcontractileintegralhighestmmHgcms<-NULL
  x$DistalcontractileintegralmeanmmHgcms<-NULL
  x$Simultaneous[is.na(x$Simultaneous)]=0
  x$LOS_relax<-ifelse(((x$ResidualmeanmmHg-x$BasalrespiratoryminmmHg/x$BasalrespiratorymeanmmHg)*100)<90,"NonRelaxLOS","NormalRelaxLOS")
  x$LowerOesoph<-ifelse(x$BasalrespiratoryminmmHg<4.7&x$Hiatalhernia=="Yes","HypotensiveLOSWithHH",
                           ifelse(x$BasalrespiratoryminmmHg<4.7,"HypotensiveLOS",
                                  ifelse(x$Hiatalhernia=="Yes","HHOnly","Normal")))

  x<-data.frame(x)
  return(x)
}


########################### HRM Symptom Subset Prepare #################################################################################


#'SymptomsExtraction
#'
#' SymptomsExtraction
#' This extracts the symptoms from a pre-specified column into its own column per symptom. Usually these are extracted from the
#' associated diagnosis table as the HRM main table doesnt keep free text stuff
#' @param x dataframe
#' @param y the column of interest
#' @keywords Symptom
#' @export
#' @examples #SymptomsExtraction(x,IndicANDHx)

SymptomsExtraction<-function(x,y){
  x$Dysphagia<-ifelse(grepl(".*[Dd]ysph.*",x[,y],perl=TRUE)|grepl(".*[Oo]dyn.*",x[,y],perl=TRUE)|grepl(".*[Ss]tuck.*",x[,y],perl=TRUE)|grepl(".*[Ss]tick.*",x[,y],perl=TRUE),"Dysphagia","No")
  x$Heartburn<-ifelse(grepl(".*[Hh]eart.*",x[,y],perl=TRUE)|grepl(".*[Rr]eflu.*",x[,y],perl=TRUE)|grepl(".*[Rr]etro.*",x[,y],perl=TRUE)|grepl(".*[Bb]urn.*",x[,y],perl=TRUE),"Heartburn","No")
  x$Throat<-ifelse(grepl(".*[Tt]hroat.*",x[,y],perl=TRUE)|grepl(".*[Nn]eck.*",x[,y],perl=TRUE),"Throat","No")
  x$Cough<-ifelse(grepl(".*[Cc]ough.*",x[,y],perl=TRUE)|grepl(".*[Cc]hok.*",x[,y],perl=TRUE),"Cough","No")
  x$ChestPain<-ifelse(grepl(".*[Ch]est.*",x[,y],perl=TRUE),"ChestPain","No")
  x$AbdoPain<-ifelse(grepl(".*[Ss]tomach.*",x[,y],perl=TRUE)|grepl(".*[Ee]piga.*",x[,y],perl=TRUE)|grepl(".*[Aa]bdom.*",x[,y],perl=TRUE),"AbdoPain","No")
  x$Hoarseness<-ifelse(grepl(".*[Hh]oarse.*",x[,y],perl=TRUE),"Hoarseness","No")
  x$Regurgitation<-ifelse(grepl(".*[Rr]egur.*",x[,y],perl=TRUE)|grepl(".*[Tt]aste.*",x[,y],perl=TRUE),"Regurgitation","No")
  x$Vomiting<-ifelse(grepl(".*[Vv]omit.*",x[,y],perl=TRUE),"Vomiting","No")
  x$Belch<-ifelse(grepl(".*[Bb]elch.*",x[,y],perl=TRUE)|grepl(".*[Bb]urp.*",x[,y],perl=TRUE),"Belch","No")

  x$AllSymptoms<-paste(x$Dysphagia,",",x$Heartburn,",",x$Throat,",",x$Cough,",",x$ChestPain,",",x$AbdoPain,",",x$Hoarseness,",",x$Regurgitation,",",x$Vomiting,",",x$Belch,",")
  x$AllSymptoms<-gsub("NO,","",x$AllSymptoms)
  x$AllSymptoms<-gsub(",NO","",x$AllSymptoms)
  x$AllSymptoms<-gsub("NO","",x$AllSymptoms)

  x$Dysphagia<-NULL
  x$Heartburn<-NULL
  x$Throat<-NULL
  x$Cough<-NULL
  x$ChestPain<-NULL
  x$AbdoPain<-NULL
  x$Hoarseness<-NULL
  x$Regurgitation<-NULL
  x$Vomiting<-NULL
  x$Belch<-NULL
  return (x)
  }


  ########################### Categorise the HRM diagnoses #################################################################################

#' HRMDiagnoses
#' This creates diagnoses from the HRM raw data based on the Chicago classification v4 (need to check)
#' @param x dataframe
#' @keywords HRM diagnoses
#' @export
#' @examples #HRMDiagnoses(x)

HRMDiagnoses<-function(x){
  x$dx<-ifelse(x$ResidualmeanmmHg>15&x$failedChicagoClassification==100&!is.na(x$ResidualmeanmmHg)&!is.na(x$failedChicagoClassification),"AchalasiaType1",
ifelse(x$ResidualmeanmmHg>=15&!is.na(x$ResidualmeanmmHg)&x$prematurecontraction>=20,"AchalasiaType2",
ifelse(x$ResidualmeanmmHg>=15&!is.na(x$ResidualmeanmmHg)&x$panesophagealpressurization>=20,"AchalasiaType3",
ifelse(x$ResidualmeanmmHg>=15&!is.na(x$ResidualmeanmmHg)&x$failedChicagoClassification>0,"EGOO",
ifelse(x$ResidualmeanmmHg<15&x$ResidualmeanmmHg>10&!is.na(x$ResidualmeanmmHg)&x$failedChicagoClassification==100&!is.na(x$failedChicagoClassification),"PossibleAchalasia",
ifelse(x$ResidualmeanmmHg>=15&!is.na(x$ResidualmeanmmHg),"AchalasiaType2or3orEGOO",
ifelse(x$ResidualmeanmmHg<=15&x$failedChicagoClassification==100&!is.na(x$ResidualmeanmmHg)&!is.na(x$failedChicagoClassification),"AbsentPeristalsis",
ifelse(x$ResidualmeanmmHg<=15&(x$prematurecontraction>=20|x$Simultaneous>=20|x$Distallatency<4.5)&x$DCI>=450&!is.na(x$ResidualmeanmmHg)&(!is.na(x$prematurecontraction)&!is.na(x$Simultaneous))&!is.na(x$DCI),"DES",
ifelse(x$ResidualmeanmmHg<=15&(x$DCI>=8000)|(x$DCI>=8000|x$DCI>=8000)&!is.na(x$ResidualmeanmmHg)&!is.na(x$DCI),"JackHammer",
       ifelse(x$ResidualmeanmmHg<=15&xfailedChicagoClassification<100&xfailedChicagoClassification>50&!is.na(x$ResidualmeanmmHg)&!is.na(x$DCI),"IOM",
#ifelse(x$ResidualmeanmmHg<15&x$Contractilefrontvelocitycms>9&x$Distallatency>=4.5&!is.na(x$ResidualmeanmmHg)&!is.na(x$Contractilefrontvelocitycms)&!is.na(x$Distallatency),"RapidContraction",
#ifelse(x$ResidualmeanmmHg<15&(x$DCI>=5000|x$DCI>=5000)&x$Distallatency>=4.5&!is.na(x$ResidualmeanmmHg)&!is.na(x$Distallatency)&!is.na(x$DCI),"HypertensivePeristalsis",
#ifelse(x$ResidualmeanmmHg<=15&x$smallbreaks>=30&x$largebreaks>=20&!is.na(x$ResidualmeanmmHg)&!is.na(x$smallbreaks)&!is.na(x$largebreaks),"WeakPeristalsis",
ifelse(x$ResidualmeanmmHg<15&largebreaks>=50&!is.na(x$ResidualmeanmmHg)&!is.na(x$failedChicagoClassification),"FrequentFailedPeristalsis","Normal")))))))))))
  return(x)
}


#Note Ineffective esophageal motility is diagnosed when >50% of swallows is ineffective, that is either failed (DCI <100mmHg cm s) or weak (DCI 100–450 mmHg cm s).
#Fragmented peristalsis is defined as >50% of swallows with a large break (>5 cm) and not matching criteria for ineffective esophageal motility




##### Merging with another table ####

#' Merge HRM and Impedance
#' This merges the overall impedance dataset with the HRM dataset
#' @param x the whole impedance dataset
#' @param y the whole HRM dataset
#' @keywords HRMAndSwallows acquisition
#' @export
#' @examples
#' #dataHRM<-dataHRM(channel)
#' #dataImp2<-dataImp2(channel)
#' #dataImp_Symp<-dataImp_Symp(channel)
#'
#' #dataImpWhole<-dataImpWhole(dataImp2,dataImp_Symp)
#' #MyImpedanceDataWithHRM(dataImpWhole,dataHRM)

MyImpedanceDataWithHRM<-function(x,y){
  MyImpedanceDataWithHRM<-merge(x,y,by=c("HospNum_Id"),all=TRUE)
  return(MyImpedanceDataWithHRM)
}


########################### Some metrics #################################################################################
#' MotilityTimeSeries
#' Plots the Tests over time. Probably redundant
#' @param x dataframe
#' @keywords HRM Motility
#' @export
#' @examples #MotilityTimeSeries(x)
#'
MotilityTimeSeries <- function(x) {
xTimePlot<-x %>%
  mutate(month=format(VisitDate,"%m"), year= format(VisitDate,"%Y")) %>%
  group_by(month,year)
xTimePlot$MergeCol<-paste("01",xTimePlot$month, xTimePlot$year, sep=" ")
xTimePlot<-data.frame(table(xTimePlot$MergeCol))
names(xTimePlot)<-c("Date","Freq")
xTimePlot<-subset(xTimePlot,!xTimePlot$Date=="NA/NA")
xTimePlot$Date<-as.Date(xTimePlot$Date,format="%d %m %Y")

PlotName<-deparse(substitute(x))
print(PlotName)

xTimePlot<-xTimePlot[order(xTimePlot$Date),]
myplot<-ggplot(xTimePlot) +
   geom_point(aes(Date, Freq, color = "red"))+
   labs(title=PlotName) +
   scale_color_manual("",labels = c("SVStart", "SVEnd"), values = c("blue", "red")) +
   xlab("Date") +
   ylab("Frequency") +
   theme(axis.text.x=element_text(angle=-90)) +
   theme(legend.position="top")
}



#' BasicBoxplots
#' This boxplots HRM measurements. Very likely redundant
#' @param x dataframe
#' @keywords HRM CleanUp
#' @examples #BasicBoxplots(x)


BasicBoxplots <- function(x) {
par(mar =rep(2,4))
par(mfrow=c(6,3))

#if the number of rows with na = the number of rows then ignore
if(sum(is.na(x$LESmidpointfromnarescm))!=nrow(x)){
  boxplot(x$LESmidpointfromnarescm,main="LESmid",xlim=c(0,3))
boxplot(Normal$LESmidpointfromnarescm,add=T)
}

if(sum(is.na(x$ProximalLESfromnarescm))!=nrow(x)){
  boxplot(x$ProximalLESfromnarescm,main="ProximalLES",xlim=c(0,3))
boxplot(Normal$ProximalLESfromnarescm,add=T)
}
if(sum(is.na(x$IntraabdominalLESlengthcm))!=nrow(x)){
  boxplot(x$IntraabdominalLESlengthcm,main="IntraabdoLESlength",xlim=c(0,3))
boxplot(Normal$IntraabdominalLESlengthcm,add=T)
}
if(sum(is.na(x$BasalrespiratoryminmmHg))!=nrow(x)){
  boxplot(x$BasalrespiratoryminmmHg,main="Basalresmin",xlim=c(0,3))
boxplot(Normal$BasalrespiratoryminmmHg,add=T)
}
if(sum(is.na(x$BasalrespiratorymeanmmHg))!=nrow(x)){
  boxplot(x$BasalrespiratorymeanmmHg,main="Basalrespmean",xlim=c(0,3))
boxplot(Normal$BasalrespiratorymeanmmHg,add=T)
}
if(sum(is.na(x$ResidualmeanmmHg))!=nrow(x)){
  boxplot(x$ResidualmeanmmHg,main="Residmean",xlim=c(0,3))
boxplot(Normal$ResidualmeanmmHg,add=T)
}
if(sum(is.na(x$DistalcontractileintegralhighestmmHgcms))!=nrow(x)){
boxplot(x$DistalcontractileintegralhighestmmHgcms,main="DCIhighest",xlim=c(0,3))
boxplot(Normal$DistalcontractileintegralhighestmmHgcms,add=T)
}
if(sum(is.na(x$DistalcontractileintegralmeanmmHgcms))!=nrow(x)){
  boxplot(x$DistalcontractileintegralmeanmmHgcms,main="DCImean",xlim=c(0,3))
boxplot(Normal$DistalcontractileintegralmeanmmHgcms,add=T)
}
if(sum(is.na(x$Contractilefrontvelocitycms))!=nrow(x)){
  boxplot(x$Contractilefrontvelocitycms,main="CFV",xlim=c(0,3))
boxplot(Normal$Contractilefrontvelocitycms,add=T)
}
if(sum(is.na(x$IntraboluspressureATLESRmmHg))!=nrow(x)){
  boxplot(x$IntraboluspressureATLESRmmHg,main="IBP_@LES",xlim=c(0,3))
boxplot(Normal$IntraboluspressureATLESRmmHg,add=T)
}
if(sum(is.na(x$Distallatency))!=nrow(x)){
  boxplot(x$Distallatency,main="DL",xlim=c(0,3))
boxplot(Normal$Distallatency,add=T)
}
if(sum(is.na(x$failedChicagoClassification))!=nrow(x)){
boxplot(x$failedChicagoClassification,main="failed",xlim=c(0,3))
boxplot(Normal$failedChicagoClassification,add=T)
}
if(sum(is.na(x$panesophagealpressurization))!=nrow(x)){
  boxplot(x$panesophagealpressurization,main="%panesoph",xlim=c(0,3))
boxplot(Normal$panesophagealpressurization,add=T)
}
if(sum(is.na(x$largebreaks))!=nrow(x)){
  boxplot(x$largebreaks,main="%Lbreaks",xlim=c(0,3))
boxplot(Normal$largebreaks,add=T)
}
if(sum(is.na(x$prematurecontraction))!=nrow(x)){
  boxplot(x$prematurecontraction,main="%Prem",xlim=c(0,3))
boxplot(Normal$prematurecontraction,add=T)
}
if(sum(is.na(x$rapidcontraction))!=nrow(x)){
  boxplot(x$rapidcontraction,main="%Rapid",xlim=c(0,3),na.rm=T)
boxplot(Normal$rapidcontraction,add=T)
}
if(sum(is.na(x$smallbreaks))!=nrow(x)){
  boxplot(x$smallbreaks,main="%Sbreaks",xlim=c(0,3))
boxplot(Normal$smallbreaks,add=T)
}
PlotName<-deparse(substitute(x))
title(paste(PlotName," vs Normal"),outer=T, line=-35)
}



#----------------------------Sandbox


  #Define multiple logistic regression and exploratory analysis first
  #see https://rcompanion.org/rcompanion/e_07.html

#  library(PerformanceAnalytics)

#chart.Correlation(Data.num, method="spearman",histogram=TRUE,pch=16)



##### Symptom vs Parameter
#eg. Question:What proportion of NERD is related to acid vs non acid. ie What proportion of {condition} is related to {parameter}
#1. Chronic Cough Is Associated With Long Breaks in Esophageal Peristaltic Integrity on High-resolution Manometry. J Neurogastroenterol Motil. 2018 Jul 30;24(3):387-394. doi: 10.5056/jnm17126.

#################{symptom} is associated with {parameter}

#2. Factors predictive of gastroesophageal reflux disease and esophageal motility disorders in patients with non-cardiac chest pain. Gomez Cifuentes J1, Lopez R2, Thota PN3. Scand J Gastroenterol. 2018 Jun;53(6):643-649. doi: 10.1080/00365521.2018.1452975.

###########{symptom subgroup} {parameter} predicts {condition}

##### Parameter vs Condition
#1. Swallow-induced esophageal shortening in patients without hiatal hernia is associated with gastroesophageal reflux. #Masuda T, Singhal S, Akimoto S, Bremner RM, Mittal SK. Dis Esophagus. 2018 May 1;31(5). doi: 10.1093/dote/dox152. PMID: 29293978

###########{{parameter} is associated with {condition}

#2. Comparison of esophageal motility in gastroesophageal reflux disease with and without globus sensation. Tang Y, Huang J, Zhu Y, Qian A, Xu B, Yao W.#Rev Esp Enferm Dig. 2017 Dec;109(12):850-855. doi: 10.17235/reed.2017.4449/2016.

###########{{symptom subgroup} {parameter} is associated with {condition}

#3. High-resolution manometry in patients with and without globus pharyngeus and/or symptoms of laryngopharyngeal reflux. #Ding H, Duan Z, Yang D, Zhang Z, Wang L, Sun X, Yao Y, Lin X, Yang H, Wang S, Chen JDZ. BMC Gastroenterol. 2017 Oct 23;17(1):109. doi: 10.1186/s12876-017-0666-x. PMID: 29061118 Free PMC Article{symptom subgroup} {parameter} is associated with {condition}
###########{{symptom subgroup} {parameter} is associated with {condition}

#4. Distribution of Esophageal Motor Disorders in Diabetic Patients With Dysphagia. George NS, Rangan V, Geng Z, Khan F, Kichler A, Gabbard S, Ganocy S, Fass R. J Clin Gastroenterol. 2017 Nov/Dec;51(10):890-895. doi: 10.1097/MCG.0000000000000894. PMID: 28746079
###########{{symptom subgroup} {parameter} is associated with {condition}

#5 Upper esophageal sphincter (UES) metrics on high-resolution manometry (HRM) differentiate achalasia subtypes.
###########{{condition subgroup} {parameter} is associated with {condition}



###### Parameter pre and post an intervention

#1. Esophageal Motility after Extensive Circumferential Endoscopic Submucosal Dissection for Superficial Esophageal Cancer.#Digestion. 2018 Jun 5;98(3):153-160. doi: 10.1159/000487751. [Epub ahead of print]

#2. Impact of thoracic surgery on esophageal motor function-Evaluation by high resolution manometry.Wäsche A, Kandulski A, Malfertheiner P, Riedel S, Zardo P, Hachenberg T, Schreiber J.J Thorac Dis. 2017 Jun;9(6):1557-1564. doi: 10.21037/jtd.2017.05.43.PMID: 28740669 Free PMC Article


###### Condition vs Parameter- mainly descriptive

#1.GERD: Presence and Size of Hiatal Hernia Influence Clinical Presentation, Esophageal Function, Reflux Profile, and Degree of Mucosal Injury. # Schlottmann F, Andolfi C, Herbella FA, Rebecchi F, Allaix ME, Patti MG.Am Surg. 2018 Jun 1;84(6):978-982.


#2.Esophageal Motor Disorders Are a Strong and Independant Associated Factor of Barrett's Esophagus. #Bazin C, Benezech A, Alessandrini M, Grimaud JC, Vitton V. #J Neurogastroenterol Motil. 2018 Apr 30;24(2):216-225. doi: 10.5056/jnm17090. #PMID: 29605977 Free PMC Article
###########{{condition} is associated with  {parameter}

#3. Esophagogastric junction outflow obstruction is often associated with coexistent abnormal esophageal body motility and abnormal bolus transit. #Zheng E, Gideon RM, Sloan J, Katz PO. Dis Esophagus. 2017 Oct 1;30(10):1-4. doi: 10.1093/dote/dox066.
###########{{parameter} is associated with {condition}


###########{Filtered subgroup

#Predictive variables
#Subclassifying algorithms

###########{function(subgroup,parameter OR symptom OR condition,parameter OR symptom OR condition ){
###########{}



# What {Parameter/condition/symptom} in this subgroup are associated with {Parameter/condition/symptom}?
#
# library(rattle)
#
#
#
# Filtered subgroup:Non cardiac_Chest_pain
#
# Is a associated with b
# eg Specific<-function(Non-subgroup,one Parameter, achalasia){
#
#
# }
#
# Is there a variable in  a subgroup that can predict b
# Multifac<-function(Non-cardiac_Chest_pain,all_device_parameters, achalasia){
#
#
#
# }
#
Symptom<-as.list(c("Non-cardiac chest pain",
                   "Heartburn",
                   "Regurgitation",
                   "Hoarse voice",
                   "Dysphagia",
                   "Cough",
                   "Globus",
                   "Throat clearing",
                   "abdominal pain"))

Parameterz<-as.list(c("DistalLESfromnarescm",
                     "LESmidpointfromnarescm",
                     "ProximalLESfromnarescm",
                     "LESlengthcm",
                     "EsophageallengthLESUEScenterscm",
                     "PIPfromnarescm",
                     "IntraabdominalLESlengthcm",
                     "Hiatalhernia",
                     "BasalrespiratoryminmmHg",
                     "BasalrespiratorymeanmmHg",
                     "ResidualmeanmmHg",
                     "UESMeanResidLocationcenterfrnarescm",
                     "ResidMeanbasalpressuremmHg",
                     "ResidMeanresidualpressuremmHg",
                     "Numberofswallowsevaluated",
                     "DistalcontractileintegralhighestmmHgcms",
                     "DistalcontractileintegralmeanmmHgcms",
                     "Contractilefrontvelocitycms",
                     "IntraboluspressureATLESRmmHg",
                     "Distallatency",
                     "failedChicagoClassification",
                     "panesophagealpressurization",
                     "largebreaks",
                     "Simultaneous",
                     "prematurecontraction",
                     "rapidcontraction",
                     "smallbreaks",
                     "VisitDate",
                     "DOBAge"))

Condition<-as.list(c("Achalasia type 1",
                     "Achalasia type 2",
                     "Achalasia type 3",
                     "EGOO",
                     "DES",
                     "Jackhammer",
                     "Aperistaltic",
                     "Frequent failed Peristalsis",
                     "Ineffective oesophageal motility"))

paste("Investigating the association between ",Condition," and ",Parameterz, "in patients with ",Symptom )
paste("Investigating the association between ",Symptom," and ",Parameterz, "in patients with ",Condition )
paste("Investigating the association between ",Symptom," and ",Condition, "in patients with ", Parameterz)
#
# #Patients with condition and a symptom analysis of parameters
# #ie which parameters are significantly elevated in patients with specific symptoms and a specific condition
#
#
# ###### Condition description (ie subclassification)
#
# #1. Clinical and manometric characteristics of patients with oesophagogastric outflow obstruction: towards a new classification George Triadafilopoulos, John O Clarke
#
# ##### Device vs device
# #
# #
# # Both vs future outcomes
# # Both vs future conditions



