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
  data <- sqlQuery(channel , "SELECT  HRMImportMain.* FROM HRMImportMain")
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

  #Clean the demographics
  is.date <- function(x) inherits(x, 'Date')
  if(!is.date(x$VisitDate)){
    data$VisitDate<-as.character(data$VisitDate)
    data$VisitDate<-as.Date(data$VisitDate,"%d_%m_%Y")
  }
  data$DOBAge<-as.character(data$DOBAge)
  data$DOBAge<-gsub("-","_",data$DOBAge)
  data$DOBAge<-gsub("(\\d+)_(\\d+)_(\\d{2}$)","\\1_\\2_19\\3",data$DOBAge)
  data$DOBAge<-as.Date(data$DOBAge,"%Y_%m_%d")
  data$Age<-data$VisitDate-data$DOBAge
  data$Age<-difftime(data$VisitDate,data$DOBAge,units="days")/366.25
  data$Age<-as.numeric(data$Age)

  #Format the columns so they are correct
  #if contains mmHg or cm make sure they are as.numeric(as.character()).
  data[i1] <- lapply(data[grepl("cm|mmHg", names(data))], function(x) as.numeric(as.character(x)))
  data$Hiatalhernia<-as.character((data$Hiatalhernia))

  #Convert other columns to numeric
  data$Distallatency<-as.numeric(as.character((data$Distallatency)))
  data$DCI<-ifelse(!rowSums(is.na(data[c("DistalcontractileintegralhighestmmHgcms", "DistalcontractileintegralmeanmmHgcms")])), data$DistalcontractileintegralhighestmmHgcms, rowSums(data[c("DistalcontractileintegralhighestmmHgcms", "DistalcontractileintegralmeanmmHgcms")], na.rm=TRUE) )


  #Sort out the DCI, simultaneous contractions and LOS relaxation
  data$DistalcontractileintegralhighestmmHgcms[is.na(data$DistalcontractileintegralhighestmmHgcms)]=0
  data$DistalcontractileintegralmeanmmHgcms[is.na(data$DistalcontractileintegralmeanmmHgcms)]=0
  data$DistalcontractileintegralhighestmmHgcms<-NULL
  data$DistalcontractileintegralmeanmmHgcms<-NULL
  data$Simultaneous[is.na(data$Simultaneous)]=0
  data$LOS_relax<-ifelse(((data$ResidualmeanmmHg-data$BasalrespiratoryminmmHg/data$BasalrespiratorymeanmmHg)*100)<90,"NonRelaxLOS","NormalRelaxLOS")
  data$LowerOesoph<-ifelse(data$BasalrespiratoryminmmHg<4.7&data$Hiatalhernia=="Yes","HypotensiveLOSWithHH",
                           ifelse(data$BasalrespiratoryminmmHg<4.7,"HypotensiveLOS",
                                  ifelse(data$Hiatalhernia=="Yes","HHOnly","Normal")))
  return(data)
}


########################### HRM Symptom Subset Prepare #################################################################################


SymptomsExtraction

#' SymptomsExtraction
#' This extracts the symptoms from a pre-specified column into its own column per symptom
#' @param x dataframe
#' @param y the column of interest
#' @keywords Symptom
#' @export
#' @examples #SymptomsExtraction(data,IndicANDHx)

SymptomsExtraction<-function(x,y){
  x$Dysphagia<-ifelse(grepl(".*[Dd]ysph.*",x$y,perl=TRUE)|grepl(".*[Oo]dyn.*",x$y,perl=TRUE)|grepl(".*[Ss]tuck.*",x$y,perl=TRUE)|grepl(".*[Ss]tick.*",x$y,perl=TRUE),"Yes","No")
  x$Heartburn<-ifelse(grepl(".*[Hh]eart.*",x$y,perl=TRUE)|grepl(".*[Rr]eflu.*",x$y,perl=TRUE)|grepl(".*[Rr]etro.*",x$y,perl=TRUE)|grepl(".*[Bb]urn.*",x$y,perl=TRUE),"Yes","No")
  x$Throat<-ifelse(grepl(".*[Tt]hroat.*",x$y,perl=TRUE)|grepl(".*[Nn]eck.*",x$y,perl=TRUE),"Yes","No")
  x$Cough<-ifelse(grepl(".*[Cc]ough.*",x$y,perl=TRUE)|grepl(".*[Cc]hok.*",x$y,perl=TRUE),"Yes","No")
  x$ChestPain<-ifelse(grepl(".*[Ch]est.*",x$y,perl=TRUE),"Yes","No")
  x$AbdoPain<-ifelse(grepl(".*[Ss]tomach.*",x$y,perl=TRUE)|grepl(".*[Ee]piga.*",x$y,perl=TRUE)|grepl(".*[Aa]bdom.*",x$y,perl=TRUE),"Yes","No")
  x$Hoarseness<-ifelse(grepl(".*[Hh]oarse.*",x$y,perl=TRUE),"Yes","No")
  x$Regurgitation<-ifelse(grepl(".*[Rr]egur.*",x$y,perl=TRUE)|grepl(".*[Tt]aste.*",x$y,perl=TRUE),"Yes","No")
  x$Vomiting<-ifelse(grepl(".*[Vv]omit.*",x$y,perl=TRUE),"Yes","No")
  x$Belch<-ifelse(grepl(".*[Bb]elch.*",x$y,perl=TRUE)|grepl(".*[Bb]urp.*",x$y,perl=TRUE),"Yes","No")
}


  ########################### Categorise the HRM diagnoses #################################################################################

#' HRMDiagnoses
#' This creates diagnoses from the HRM raw data based on the Chicago classification v4 (need to check)
#' @param x dataframe
#' @keywords HRM CleanUp
#' @export
#' @examples #HRMDiagnoses(x)

HRMDiagnoses<-function(x){
  data$dx<-ifelse(data$ResidualmeanmmHg>15&data$failedChicagoClassification==100&!is.na(data$ResidualmeanmmHg)&!is.na(data$failedChicagoClassification),"AchalasiaType1",
ifelse(data$ResidualmeanmmHg>=15&!is.na(data$ResidualmeanmmHg)&data$prematurecontraction>=20,"AchalasiaType2",
ifelse(data$ResidualmeanmmHg>=15&!is.na(data$ResidualmeanmmHg)&data$panesophagealpressurization>=20,"AchalasiaType3",
ifelse(data$ResidualmeanmmHg>=15&!is.na(data$ResidualmeanmmHg)&data$failedChicagoClassification>0,"EGOO",
ifelse(data$ResidualmeanmmHg<15&data$ResidualmeanmmHg>10&!is.na(data$ResidualmeanmmHg)&data$failedChicagoClassification==100&!is.na(data$failedChicagoClassification),"PossibleAchalasia",
ifelse(data$ResidualmeanmmHg>=15&!is.na(data$ResidualmeanmmHg),"AchalasiaType2or3orEGOO",

ifelse(data$ResidualmeanmmHg<=15&data$failedChicagoClassification==100&!is.na(data$ResidualmeanmmHg)&!is.na(data$failedChicagoClassification),"AbsentPeristalsis",
ifelse(data$ResidualmeanmmHg<=15&(data$prematurecontraction>=20|data$Simultaneous>=20|data$Distallatency<4.5)&data$DCI>=450&!is.na(data$ResidualmeanmmHg)&(!is.na(data$prematurecontraction)&!is.na(data$Simultaneous))&!is.na(data$DCI),"DES",
ifelse(data$ResidualmeanmmHg<=15&(data$DCI>=8000)|(data$DCI>=8000|data$DCI>=8000)&!is.na(data$ResidualmeanmmHg)&!is.na(data$DCI),"JackHammer",
ifelse(data$ResidualmeanmmHg<15&data$Contractilefrontvelocitycms>9&data$Distallatency>=4.5&!is.na(data$ResidualmeanmmHg)&!is.na(data$Contractilefrontvelocitycms)&!is.na(data$Distallatency),"RapidContraction",
ifelse(data$ResidualmeanmmHg<15&(data$DCI>=5000|data$DCI>=5000)&data$Distallatency>=4.5&!is.na(data$ResidualmeanmmHg)&!is.na(data$Distallatency)&!is.na(data$DCI),"HypertensivePeristalsis",
ifelse(data$ResidualmeanmmHg<=15&data$smallbreaks>=30&data$largebreaks>=20&!is.na(data$ResidualmeanmmHg)&!is.na(data$smallbreaks)&!is.na(data$largebreaks),"WeakPeristalsis",
ifelse(data$ResidualmeanmmHg<15&data$failedChicagoClassification>=30&data$failedChicagoClassification<=100&!is.na(data$ResidualmeanmmHg)&!is.na(data$failedChicagoClassification),"FrequentFailedPeristalsis","Normal")))))))))))))
  return(data)
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










##### Symptom vs Parameter
#eg. Question:What proportion of NERD is related to acid vs non acid. ie What proportion of {condition} is related to {parameter}
#1. Chronic Cough Is Associated With Long Breaks in Esophageal Peristaltic Integrity on High-resolution Manometry. J Neurogastroenterol Motil. 2018 Jul 30;24(3):387-394. doi: 10.5056/jnm17126.

#################{symptom} is associated with {parameter}

#2. Factors predictive of gastroesophageal reflux disease and esophageal motility disorders in patients with non-cardiac chest pain. Gomez Cifuentes J1, Lopez R2, Thota PN3. Scand J Gastroenterol. 2018 Jun;53(6):643-649. doi: 10.1080/00365521.2018.1452975.

{symptom subgroup} {parameter} predicts {condition}

##### Parameter vs Condition
#1. Swallow-induced esophageal shortening in patients without hiatal hernia is associated with gastroesophageal reflux. #Masuda T, Singhal S, Akimoto S, Bremner RM, Mittal SK. Dis Esophagus. 2018 May 1;31(5). doi: 10.1093/dote/dox152. PMID: 29293978

{parameter} is associated with {condition}

#2. Comparison of esophageal motility in gastroesophageal reflux disease with and without globus sensation. Tang Y, Huang J, Zhu Y, Qian A, Xu B, Yao W.#Rev Esp Enferm Dig. 2017 Dec;109(12):850-855. doi: 10.17235/reed.2017.4449/2016.

{symptom subgroup} {parameter} is associated with {condition}

#3. High-resolution manometry in patients with and without globus pharyngeus and/or symptoms of laryngopharyngeal reflux. #Ding H, Duan Z, Yang D, Zhang Z, Wang L, Sun X, Yao Y, Lin X, Yang H, Wang S, Chen JDZ. BMC Gastroenterol. 2017 Oct 23;17(1):109. doi: 10.1186/s12876-017-0666-x. PMID: 29061118 Free PMC Article{symptom subgroup} {parameter} is associated with {condition}
{symptom subgroup} {parameter} is associated with {condition}

#4. Distribution of Esophageal Motor Disorders in Diabetic Patients With Dysphagia. George NS, Rangan V, Geng Z, Khan F, Kichler A, Gabbard S, Ganocy S, Fass R. J Clin Gastroenterol. 2017 Nov/Dec;51(10):890-895. doi: 10.1097/MCG.0000000000000894. PMID: 28746079
{symptom subgroup} {parameter} is associated with {condition}

#5 Upper esophageal sphincter (UES) metrics on high-resolution manometry (HRM) differentiate achalasia subtypes.
{condition subgroup} {parameter} is associated with {condition}

###### Parameter pre and post an intervention

#1. Esophageal Motility after Extensive Circumferential Endoscopic Submucosal Dissection for Superficial Esophageal Cancer.#Digestion. 2018 Jun 5;98(3):153-160. doi: 10.1159/000487751. [Epub ahead of print]

#2. Impact of thoracic surgery on esophageal motor function-Evaluation by high resolution manometry.Wäsche A, Kandulski A, Malfertheiner P, Riedel S, Zardo P, Hachenberg T, Schreiber J.J Thorac Dis. 2017 Jun;9(6):1557-1564. doi: 10.21037/jtd.2017.05.43.PMID: 28740669 Free PMC Article


###### Condition vs Parameter

#1.GERD: Presence and Size of Hiatal Hernia Influence Clinical Presentation, Esophageal Function, Reflux Profile, and Degree of Mucosal Injury. # Schlottmann F, Andolfi C, Herbella FA, Rebecchi F, Allaix ME, Patti MG.Am Surg. 2018 Jun 1;84(6):978-982.



#2.Esophageal Motor Disorders Are a Strong and Independant Associated Factor of Barrett's Esophagus. #Bazin C, Benezech A, Alessandrini M, Grimaud JC, Vitton V. #J Neurogastroenterol Motil. 2018 Apr 30;24(2):216-225. doi: 10.5056/jnm17090. #PMID: 29605977 Free PMC Article
{condition} is associated with  {parameter}

#3. Esophagogastric junction outflow obstruction is often associated with coexistent abnormal esophageal body motility and abnormal bolus transit. #Zheng E, Gideon RM, Sloan J, Katz PO. Dis Esophagus. 2017 Oct 1;30(10):1-4. doi: 10.1093/dote/dox066.
{parameter} is associated with {condition}


Filtered subgroup

function(subgroup,parameter OR symptom OR condition,parameter OR symptom OR condition ){
}







Filtered subgroup:Non cardiac_Chest_pain

Is a associated with b
eg Specific<-function(Non-cardiac_Chest_pain,Age, achalasia){
}

Is there a variable in  a subgroup that can predict b
Multifac<-function(Non-cardiac_Chest_pain,all_device_parameters, achalasia){

}

###### Condition description (ie subclassification)

#1. Clinical and manometric characteristics of patients with oesophagogastric outflow obstruction: towards a new classification George Triadafilopoulos, John O Clarke

##### Device vs device
#
#
# Both vs future outcomes
# Both vs future conditions



