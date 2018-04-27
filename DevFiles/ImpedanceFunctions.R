library(lubridate)
#Impedance functions:

#Clean up the impedance scripts

#Get the merge done with symptoms and Imp_Symp
#Also tidy up the dates and then use this as the standard clean up script for the others

#Clean up percentages and 'mins' etc.

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

###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### Impedance Subset Preparer ################################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
#Get the symptoms in each row then into own dataset so that each dataset contains the findings for those symptoms
#To extract the symptoms into their own box:


dataImpWhole$Heartburn<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidHeartburn),"Heartburn","NO")
dataImpWhole$Cough<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidCough),"Cough","NO")
dataImpWhole$StomachPain<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidStomachPain),"StomachPain","NO")
dataImpWhole$Nausea<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidNausea),"Nausea","NO")
dataImpWhole$Vomiting<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidVomiting),"Vomiting","NO")
dataImpWhole$Regurgitation<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidRegurgitation),"Regurgitation","NO")
dataImpWhole$Throat<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidThroat),"Throat","NO")
dataImpWhole$Belch<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidBelch),"Belch","NO")
dataImpWhole$Chest<-ifelse(!is.na(dataImpWhole$SxMainRSAPAcidChestPain),"Chest","NO")
dataImpWhole$Symptom<-paste(dataImpWhole$Heartburn,dataImpWhole$Cough,dataImpWhole$StomachPain,
                            dataImpWhole$Nausea,dataImpWhole$Vomiting
                            ,dataImpWhole$Regurgitation,dataImpWhole$Throat,dataImpWhole$Belch,dataImpWhole$Chest,sep=",")
dataImpWhole$Symptom<-gsub("NO,","",dataImpWhole$Symptom)
dataImpWhole$Symptom<-gsub(",NO","",dataImpWhole$Symptom)
dataImpWhole$Symptom<-gsub("NO","",dataImpWhole$Symptom)
dataImpWhole<-dataImpWhole[,colSums(is.na(dataImpWhole))<nrow(dataImpWhole)-5]
########Need to change the symptom extraction so that all the symptoms for each episode are recorded in one box################################
dataImpWholeSymptomsPlotter<-dataImpWhole[nchar(dataImpWhole$Symptom)>0,]


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


return(dataImpWhole)
}