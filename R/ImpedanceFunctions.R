##### Data Acquisition ###############

#This depends on forming a connection to the PhysiPop database. In windows the path is this:
#channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")

#' dataImp2
#' This acquires impedance data from the upper GI database main impedance table called Impedance2
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords Imp CleanUp
#' @export
#' @examples #dataImp2(x)


dataImp2<-function(channel){
  dataImp2 <- sqlQuery( channel , "SELECT Impedance2.*FROM Impedance2")
}

#' dataImp_Symp
#' This acquires impedance symptoms data from the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords Imp_Symp Extraction
#' @export
#' @examples #dataImp_Symp(channel)

dataImp_Symp<-function(channel){
  dataImp_Symp <- sqlQuery( channel , "SELECT Imp_Symp.* FROM Imp_Symp")
}

#' dataImpWhole
#' This merges the main impedance and the impedance symptoms data frames together
#' It relies on the acquisition of the data from the functions dataImp2 and dataImp_Symp
#' @param dataImp2 the first table from physipop
#' @param dataImp_Symp the second table from physipop
#' @keywords ImpWhole CleanUp
#' @export
#' @examples #dataImp2<-dataImp2(channel)
#' #dataImp_Symp<-dataImp_Symp(channel)
#' #dataImpWhole(dataImp2,dataImp_Symp)

dataImpWhole<-function(dataImp2,dataImp_Symp){
  dataImpWhole<-dataImpClean(dataImp2,dataImp_Symp)
}

#' BRAVO
#' This acquires BRAVO data from the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords BRAVO CleanUp
#' @export
#' @examples #BRAVO(channel)

BRAVO<-function(channel){
  data <- sqlQuery( channel , "SELECT  BravoDay1And2.* FROM BravoDay1And2")
  return(data)
}


#' dataImpClean
#' This extracts the symptoms
#' @param x dataframe usually the standard impedance data
#' @param y the dataframe usually the symptom data
#' @keywords HRM CleanUp
#' @export
#' @examples #dataImpClean(dataImp2,dataImp_Symp)

dataImpClean<-function(x,y){
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("%", "", t)))
  x[,c(1:28,37:137)]<-as.data.frame(lapply(x[,c(1:28,37:137)], FUN = function(t) as.numeric(as.character(t))))
  y<-as.data.frame(lapply(y, FUN = function(t) as.numeric(gsub("%", "", t))))
  y<-as.data.frame(y)
  dataImpWhole<-merge(x,y,by=c("Imp_Id"),all=TRUE)
  dataImpWhole$HospNum_Id<-as.character(dataImpWhole$HospNum_Id)
  dataImpWhole$VisitDate<-as.Date(dataImpWhole$VisitDate,format="%d_%m_%Y",origin="30/12/1899")
  dataImpWhole$MainProcProcedureStart<-ymd_hms(dataImpWhole$MainProcProcedureStart,tz=Sys.timezone())
  dataImpWhole$MainPtDataDateofAdmission<-ymd(dataImpWhole$MainPtDataDateofAdmission,tz=Sys.timezone())
  dataImpWhole$MainProcProcedureStart<-as.Date(as.character(dataImpWhole$MainProcProcedureStart),format="%Y-%m-%d",origin="30/12/1899")
  dataImpWhole$MainPtDataDateofAdmission<-as.Date(dataImpWhole$MainPtDataDateofAdmission,format="%Y-%m-%d",origin="30/12/1899")
  dataImpWhole$VisitDate<-as.Date(ifelse(is.na(dataImpWhole$VisitDate),as.character(dataImpWhole$MainProcProcedureStart),as.character(dataImpWhole$VisitDate)),format="%Y-%m-%d",origin="30/12/1899")
  dataImpWhole$VisitDate<-as.Date(ifelse(is.na(dataImpWhole$VisitDate),as.character(dataImpWhole$MainPtDataDateofAdmission),as.character(dataImpWhole$VisitDate)),format="%Y-%m-%d",origin="30/12/1899")


  return(dataImpWhole)
}

#' dataImpSympClean
#' This extracts the symptoms from the ImpSymp table
#' @param x dataframe usually the standard impedance data
#' @keywords ImpSymp CleanUp
#' @export
#' @examples #dataImpSympClean(x)

dataImpSympClean<-function(x){
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("pcent", "", t)))
  x<-as.data.frame(lapply(x, FUN = function(t) as.numeric(t)))
  #Need to convert characters to minutes
  return(x)
}

#' dataBRAVOClean
#' This cleans the BRAVO Data
#' @param x dataframe usually the standard impedance data
#' @param y the dataframe usually the symptom data
#' @keywords HRM CleanUp
#' @export
#' @examples #dataImpClean(dataImp2,dataImp_Symp)

dataBRAVOClean<-function(x){
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("_", "", t)))
  i1 <- grepl("Duration", names(data))
  x[i1] <- lapply(x[i1], function(t) as.numeric(as.character(t))/60)
  return(x)
}



###################################### Impedance Symptom Subset Prepare ###################################################

#Get the symptoms in each row then into own dataset so that each dataset contains the findings for those symptoms
#To extract the symptoms into their own box:

#' Impedance symptom extractor function
#'
#' This extracts the relevant symptoms from the impedance dataset
#' @param x the impedance dataframe for extraction from RSAP as probably more accurate as patient entered (rather than from summary)
#' @keywords Impedance symptoms
#' @export
#' @examples
#' #dataImpSymptoms(x)

dataImpSymptoms<-function(x){
  x$Heartburn<-ifelse(!is.na(x$SxMainRSAPAcidHeartburn),"Heartburn","NO")
  x$Cough<-ifelse(!is.na(x$SxMainRSAPAcidCough),"Cough","NO")
  x$StomachPain<-ifelse(!is.na(x$SxMainRSAPAcidStomachPain),"StomachPain","NO")
  x$Nausea<-ifelse(!is.na(x$SxMainRSAPAcidNausea),"Nausea","NO")
  x$Vomiting<-ifelse(!is.na(x$SxMainRSAPAcidVomiting),"Vomiting","NO")
  x$Regurgitation<-ifelse(!is.na(x$SxMainRSAPAcidRegurgitation),"Regurgitation","NO")
  x$Throat<-ifelse(!is.na(x$SxMainRSAPAcidThroat),"Throat","NO")
  x$Belch<-ifelse(!is.na(x$SxMainRSAPAcidBelch),"Belch","NO")
  x$Chest<-ifelse(!is.na(x$SxMainRSAPAcidChestPain),"Chest","NO")
  x$AllImpSymptom<-paste(x$Heartburn,x$Cough,x$StomachPain,
                              x$Nausea,x$Vomiting
                              ,x$Regurgitation,x$Throat,x$Belch,x$Chest,sep=",")
  x$AllImpSymptom<-gsub("NO,","",x$AllImpSymptom)
  x$AllImpSymptom<-gsub(",NO","",x$AllImpSymptom)
  x$AllImpSymptom<-gsub("NO","",x$AllImpSymptom)
  x<-x[,colSums(is.na(x))<nrow(x)-5]
#TODO: Need to change the symptom extraction so that all the symptoms for each episode are recorded in one box
  dataImpWholeSymptomsPlotter<-x[nchar(x$AllImpSymptom)>0,]

  x$Heartburn<-NULL
  x$Throat<-NULL
  x$Cough<-NULL
  x$StomachPain<-NULL
  x$Throat<-NULL
  x$Nausea<-NULL
  x$Regurgitation<-NULL
  x$Vomiting<-NULL
  x$Belch<-NULL
  x$Chest<-NULL
  return (x)
}




###### Categorise the Impedance diagnoses ######
#' Acid subtype extractor
#' This creates the composite score and then subcategorises the reflux ie if acid reflux then it is recumbent vs upright vs postprandial etc. (postprandial to be done)
#' @param x the impedance dataset for extraction
#' @keywords Impedance acid
#' @export
#' @examples #AcidSubtypes(x)

AcidSubtypes<-function(x){
  #Calculate the composite score here:
  dataImpWhole$AcidRefluxScore<-dataImpWhole$MainAcidCompositeScorePatientScoreUprightTimeInReflux+
    dataImpWhole$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux+
    dataImpWhole$MainAcidCompositeScorePatientScoreTotalTimeInReflux+
    dataImpWhole$MainAcidCompositeScorePatientScoreEpisodesOver5min+
    dataImpWhole$MainAcidCompositeScorePatientScoreLongestEpisode+
    dataImpWhole$MainAcidCompositeScorePatientScoreTotalEpisodes


  #Need to classify whether the patient is predom acid vs non-acid reflux/recumbent vs upright reflux
  #Redo this one as it should be if any SAP >50% for Non-Acid reflux

  dataImpWhole$TypeOfAcid<-ifelse(dataImpWhole$AcidRefluxScore>14.7& rowSums(dataImpWhole[grepl("RSAPNonacid",names(dataImpWhole))]>=50,na.rm=T)>0,"Mixed",
                                  ifelse(dataImpWhole$AcidRefluxScore>14.7,"Acid",
                                         ifelse(rowSums(dataImpWhole[grepl("RSAPNonacid",names(dataImpWhole))]>=50,na.rm=T)>0,"NonAcid","Normal")))


  #Predom recumbent vs upright acid here

  dataImpWhole$PositionOfAcid<-ifelse(dataImpWhole$MainAcidCompositeScorePatientScoreUprightTimeInReflux>8.4&dataImpWhole$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux>3.5,"Upright&RecumbentAcid",
                                      ifelse(dataImpWhole$MainAcidCompositeScorePatientScoreUprightTimeInReflux>8.4&dataImpWhole$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux<3.5,"UprightAcid",
                                             ifelse(dataImpWhole$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux>3.5,"RecumbentAcid","NoPosition")))
  #Predom recumbent vs upright NonAcid here
  dataImpWhole$PositionOfNonAcid<-ifelse(dataImpWhole$MainRflxEpisodeUprightNonacid/dataImpWhole$MainRflxEpisodeUprightAllReflux>0.5&dataImpWhole$MainRflxEpisodeRecumbentNonacid/dataImpWhole$MainRflxEpisodeRecumbentAllReflux>0.5,"MixedNonAcid",
                                         ifelse(dataImpWhole$MainRflxEpisodeUprightNonacid/dataImpWhole$MainRflxEpisodeUprightAllReflux>0.5,"UprightNonAcid",
                                                ifelse(dataImpWhole$MainRflxEpisodeRecumbentNonacid/dataImpWhole$MainRflxEpisodeRecumbentAllReflux>0.5,"RecumbentNonAcid","Normal_NoNonAcid")))


  #Postprandial reflux to be done


  return(x)
}

#Will need to get this via natural language query from the text
#' Supragastric belching function
#'
#' Extracts all the patients with supragastric belchng
#' @param x the impedance dataset for extraction
#' @keywords belching
#' @export
#' @examples #SupragastricBelching(x)

SupragastricBelching<-function(x){

}

#Will need to get this via natural language query from the text I think..maybe
#' hypersensitive oesophagus function
#'
#' Extracts all the patients with hypersensitive oesophagus
#' @param x the impedance dataset for extraction
#' @keywords hypersensitive oesophagus
#' @export
#' @examples #HypersensitiveOesophagus(x)

HypersensitiveOesophagus<-function(x){

}

#FunctionalHeartburn
#' FunctionalHeartburn function
#'
#' Extracts all the patients with Functional Heartburn
#' @param x the impedance dataset for extraction
#' @keywords Functional Heartburn
#' @export
#' @examples #FunctionalHeartburn(x)

FunctionalHeartburn<-function(x){
}
