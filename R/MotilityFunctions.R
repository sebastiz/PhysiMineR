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
  if(!is.Date(x$VisitDate)){
    data$VisitDate<-as.character(data$VisitDate)
    data$VisitDate<-as.Date(data$VisitDate,"%d_%m_%Y")
  }
  data$DOBAge<-as.character(data$DOBAge)
  data$DOBAge<-gsub("(\\d+)_(\\d+)_(\\d{2}$)","\\1_\\2_19\\3",data$DOBAge)
  data$DOBAge<-as.Date(data$DOBAge,"%d_%m_%Y")
  data$Age<-data$VisitDate-data$DOBAge
  data$Age<-difftime(data$VisitDate,data$DOBAge,units="days")/366.25
  data$Age<-as.numeric(data$Age)

  #Format the columns so they are correct

  data$DistalLESfromnarescm<-as.numeric(as.character((data$DistalLESfromnarescm)))
  data$LESmidpointfromnarescm<-as.numeric(as.character((data$LESmidpointfromnarescm)))
  data$EsophageallengthLESUEScenterscm<-as.numeric(as.character((data$EsophageallengthLESUEScenterscm)))
  data$PIPfromnarescm<-as.numeric(as.character((data$PIPfromnarescm)))
  data$IntraabdominalLESlengthcm<-as.numeric(as.character((data$IntraabdominalLESlengthcm)))
  data$Hiatalhernia<-as.character((data$Hiatalhernia))
  data$BasalrespiratoryminmmHg<-as.numeric(as.character((data$BasalrespiratoryminmmHg)))
  data$BasalrespiratorymeanmmHg<-as.numeric(as.character((data$BasalrespiratorymeanmmHg)))
  data$UESMeanResidLocationcenterfrnarescm<-as.numeric(as.character((data$UESMeanResidLocationcenterfrnarescm)))
  data$ResidMeanbasalpressuremmHg<-as.numeric(as.character((data$ResidMeanbasalpressuremmHg)))
  data$ResidMeanresidualpressuremmHg<-as.numeric(as.character((data$ResidMeanresidualpressuremmHg)))
  data$DistalcontractileintegralhighestmmHgcms<-as.numeric(as.character((data$DistalcontractileintegralhighestmmHgcms)))
  data$DistalcontractileintegralmeanmmHgcms<-as.numeric(as.character((data$DistalcontractileintegralmeanmmHgcms)))
  data$Contractilefrontvelocitycms<-as.numeric(as.character((data$Contractilefrontvelocitycms)))
  data$IntraboluspressureATLESRmmHg<-as.numeric(as.character((data$IntraboluspressureATLESRmmHg)))
  data$Distallatency<-as.numeric(as.character((data$Distallatency)))
  data$ResidualmeanmmHg<-as.numeric(as.character((data$ResidualmeanmmHg)))
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


  ########################### Categorise the diagnoses #################################################################################

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
                                ifelse(data$ResidualmeanmmHg>=15&!is.na(data$ResidualmeanmmHg)&data$panesophagealpressurization<20&data$panesophagealpressurization<20,"EGOO",
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



#' SymptomsNoPlot
#' This get the number symptoms. Very likely redundant
#' @param x dataframe
#' @keywords HRM CleanUp
#' @export
#' @examples #SymptomsNoPlot(x)
#'
SymptomsNoPlot<- function (x){
  a<-nrow(subset(x,x$Dysphagia=="Yes"))
  b<-nrow(subset(x,x$Heartburn=="Yes"))
  c<-nrow(subset(x,x$Throat=="Yes"))
  d<-nrow(subset(x,x$Cough=="Yes"))
  e<-nrow(subset(x,x$ChestPain=="Yes"))
  f<-nrow(subset(x,x$AbdoPain=="Yes"))
  g<-nrow(subset(x,x$Hoarseness=="Yes"))
  h<-nrow(subset(x,x$Regurgitation=="Yes"))
  i<-nrow(subset(x,x$Vomiting=="Yes"))
  j<-nrow(subset(x,x$Belch=="Yes"))

  n = c(a,b,c,d,e,f,g,h,i,j)
  return(n)

}

#' Symptoms
#' This barcharts the symptoms
#' @param x dataframe
#' @keywords HRM CleanUp
#' @export
#' @examples #Symptoms(x)

Symptom_Plot<- function (x){
  a<-nrow(subset(x,x$Dysphagia=="Yes"))
  b<-nrow(subset(x,x$Heartburn=="Yes"))
  c<-nrow(subset(x,x$Throat=="Yes"))
  d<-nrow(subset(x,x$Cough=="Yes"))
  e<-nrow(subset(x,x$ChestPain=="Yes"))
  f<-nrow(subset(x,x$AbdoPain=="Yes"))
  g<-nrow(subset(x,x$Hoarseness=="Yes"))
  h<-nrow(subset(x,x$Regurgitation=="Yes"))
  i<-nrow(subset(x,x$Vomiting=="Yes"))
  j<-nrow(subset(x,x$Belch=="Yes"))

    n = c(a,b,c,d,e,f,g,h,i,j)

  s = c("Dysphagia", "Heartburn", "Throat","Cough","ChestPain","AbdoPain","Hoarseness","Regurgitation","Vomiting","Belch")
  Symp<-data.frame(s,n)
  PlotName<-deparse(substitute(x))

  mybarplot<-ggplot(Symp) +
    geom_bar(aes(s,n,color = "red"),stat="identity")+
    labs(title=PlotName) +
    xlab("Symptom") +
    ylab("Frequency") +
    theme(axis.text.x=element_text(angle=-45)) +
    theme(legend.position="top")
  return(mybarplot)
}


#' SymptomExtractor
#' This extracts the symptoms
#' @param x dataframe
#' @keywords HRM CleanUp
#' @export
#' @examples #SymptomExtractor(x)
#'
SymptomExtractor<-function(x){
  a<-nrow(subset(x,x$Dysphagia=="Yes"))
  b<-nrow(subset(x,x$Heartburn=="Yes"))
  c<-nrow(subset(x,x$Throat=="Yes"))
  d<-nrow(subset(x,x$Cough=="Yes"))
  e<-nrow(subset(x,x$ChestPain=="Yes"))
  f<-nrow(subset(x,x$AbdoPain=="Yes"))
  g<-nrow(subset(x,x$Hoarseness=="Yes"))
  h<-nrow(subset(x,x$Regurgitation=="Yes"))
  i<-nrow(subset(x,x$Vomiting=="Yes"))
  j<-nrow(subset(x,x$Belch=="Yes"))

}
