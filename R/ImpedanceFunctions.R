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
  dataImp2 <- sqlQuery( channel , "SELECT Impedance2.*, PatientData.*
FROM PatientData INNER JOIN Impedance2 ON PatientData.HospNum_Id = Impedance2.HospNum_Id")
}

#' dataImp_Symp
#' This acquires impedance symptoms data from the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords Imp_Symp Extraction
#' @export
#' @examples #dataImp_Symp(channel)

dataImp_Symp<-function(channel){
  dataImp_Symp <- sqlQuery( channel , "SELECT Impedance2.*, Imp_Symp.*, PatientData.*
FROM PatientData INNER JOIN (Impedance2 INNER JOIN Imp_Symp ON Impedance2.Imp_Id = Imp_Symp.Imp_Id) ON PatientData.HospNum_Id = Impedance2.HospNum_Id")
}

#' dataBRAVOTotal
#' Extracts from the BRAVOTotal table
#' This acquires the first two BRAVO days data frmo the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords BRAVO total Extraction
#' @export
#' @examples #dataBRAVOTotal(channel)


dataBRAVOTotal<-function(channel){
dataBRAVOTotal <- sqlQuery( channel , "SELECT BravoDay1And2.*, PatientData.* FROM PatientData INNER JOIN BravoDay1And2 ON PatientData.HospNum_Id = BravoDay1And2.HospNum_Id")
}

#' dataBRAVO
#' Extracts from the BRAVO table
#' This acquires the first two BRAVO days data frmo the upper GI database
#' @param channel odbcConnectAccess connection defined at the start of this file from RODBC function (for windows)
#' @keywords BRAVO Extraction
#' @export
#' @examples #dataBRAVO(channel)


dataBRAVO<-function(channel){
  dataBravoDay1And2 <- sqlQuery( channel , "SELECT BRAVODay1And2.*FROM BRAVODay1And2.")
}

#' dataImpClean
#' This extracts the symptoms
#' @param x dataframe usually the standard impedance data
#' @param y the dataframe usually main Impedance table
#' @keywords HRM CleanUp
#' @export
#' @examples #dataImpClean(Impedance2)

dataImpClean<-function(x){

  #Find and replace the common things
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("%", "", t)))
  x$HospNum_Id<-as.character(x$HospNum_Id)

  #Get the dates sorted
  x$MainPtDataPatientID<-as.character( x$MainPtDataPatientID)
  x$MainProcProcedureStart<-lubridate::ymd_hms(x$MainProcProcedureStart,tz=Sys.timezone())
  x$MainPtDataDateofAdmission<-as.character(x$MainPtDataDateofAdmission)
  x$MainPtDataDateofAdmission<-lubridate::ymd(x$MainPtDataDateofAdmission,tz=Sys.timezone())
  x$MainProcProcedureStart<-as.Date(as.character(x$MainProcProcedureStart),format="%Y-%m-%d",origin="30/12/1899")
  x$MainPtDataDateofAdmission<-as.Date(x$MainPtDataDateofAdmission,format="%Y-%m-%d",origin="30/12/1899")

  #Get visit date formatted correctly
  x$VisitDate<-as.Date(x$VisitDate,format="%d_%m_%Y",origin="30/12/1899")
  x$VisitDate<-as.Date(ifelse(is.na(x$VisitDate),as.character(x$MainProcProcedureStart),as.character(x$VisitDate)),format="%Y-%m-%d",origin="30/12/1899")
  x$VisitDate<-as.Date(ifelse(is.na(x$VisitDate),as.character(x$MainPtDataDateofAdmission),as.character(x$VisitDate)),format="%Y-%m-%d",origin="30/12/1899")

  #Get the file creation date properly formatted
  x$FileCreationDate<-stringr::str_extract(x$FileCreationDate,"^\\d{4}-\\d{2}-\\d{2}")
  x$FileCreationDate<-as.Date(as.character(x$FileCreationDate),format="%Y-%m-%d",origin="30/12/1899")

  x$MainPtDataPatientName<-as.character(x$MainPtDataPatientName)
  x$MainPtDataPatientID<-as.character(x$MainPtDataPatientID)
  x$MainPtDataPhysician<-as.character(x$MainPtDataPhysician)
  x$MainPtDataPatientSex<-as.character(x$MainPtDataPatientSex)

  #Get the date of birth properly formatted
  x$MainPtDataDateofBirth<-lubridate::ymd(x$MainPtDataDateofBirth)
  x$MainPtDataDateofBirth<-as.Date(as.character(x$MainPtDataDateofBirth),format="%Y-%m-%d",origin="30/12/1899")

  x<-as.data.frame(lapply(x, FUN = function(t) gsub("_", ":", t)),stringsAsFactors=FALSE)

  #Make sure the numbers are extracted as numeric from the duration columns
  i1 <- grepl("Duration", names(x))
  x[i1] <- lapply(x[i1], function(d) ifelse(grepl(":",d),(as.numeric(stringr::str_extract(d,"^\\d{2}"))*60)+(as.numeric(stringr::str_extract(d,"\\d{2}$"))),d))
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("%", "", t)),stringsAsFactors=FALSE)
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("pcent", "", t)),stringsAsFactors=FALSE)
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("min", "", t)),stringsAsFactors=FALSE)
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("sec", "", t)),stringsAsFactors=FALSE)

  #Make sure the numbers are extracted as numeric from the MainPt columns
  i2 <- !grepl("MainPt|HospNum_Id|FName|SName|DOB|Ethnicity|Gender", names(x))

  #Also exclude the first columns with hospital number etc.
  x[i2] <- lapply(x[i2], as.numeric)

  #Return as a dataframe instead of a tibble
  x<-data.frame(x)
  return(x)
}

#' dataImpSympClean
#' This cleans the data from the ImpSymp table and the main Impedance table
#' @param x dataframe usually the standard impedance data
#' @keywords ImpSymp CleanUp
#' @export
#' @examples #dataImpSympClean(x)

dataImpSympClean<-function(x){
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("%", "", t)))
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("pcent", "", t)))
  x<-as.data.frame(lapply(x, FUN = function(t) as.numeric(t)))
  x<-data.frame(x)
  return(x)
}

#' dataBRAVOClean
#' This cleans the BRAVO Data
#' @param x dataframe usually the standard impedance data
#' @keywords BRAVO CleanUp
#' @export
#' @examples #dataBRAVOClean(x)

dataBRAVOClean<-function(x){
  x<-as.data.frame(lapply(x, FUN = function(t) gsub("_", "-", t)),stringsAsFactors=FALSE)
  #Get visit date formatted correctly
  x$VisitDate<-as.Date(x$VisitDate,format="%d-%m-%Y",origin="30/12/1899")

  #Get the file creation date properly formatted
  x$FileCreationDate<-stringr::str_extract(x$FileCreationDate,"^\\d{4}-\\d{2}-\\d{2}")
  x$FileCreationDate<-as.Date(as.character(x$FileCreationDate),format="%Y-%m-%d",origin="30/12/1899")

  #This first filteres for all the Duration columns and then looks to see if a ":" is present indicating it is a time.
  #Then the first half is extracted and interpreted as an hour and multiplies by 60, then added to the second half
  i1 <- grepl("Duration", names(x))
  x[i1] <- lapply(x[i1], function(d) ifelse(grepl(":",d),(as.numeric(stringr::str_extract(d,"^\\d{2}"))*60)+(as.numeric(stringr::str_extract(d,"\\d{2}$"))),d))
  x[6:ncol(x)]<-lapply(x[6:ncol(x)], as.numeric)
  x<-data.frame(x)
  return(x)
}

#' dataBRAVODayLabeller
#' This organises the BRAVO data by day. Currently the files are extracted so that you get two days (usually)
#' per BRAVO file. This function will associate BRAVOs for one patient separated by max 4 days. The later file will have column names
#' changed to day3 and day4 and a flat dataframe produced with all the values for one BRAVO 'session' produced
#' to make downstream analyses easier.
#' @param x dataframe usually the standard impedance data
#' @keywords BRAVO CleanUp
#' @import tidyverse
#' @importFrom EndoMineR SurveilTimeByRow
#' @importFrom rlang sym
#' @export
#' @examples #dataBRAVODayLabeller(x)

dataBRAVODayLabeller<-function(x,HospNum_Id,VisitDate){

  # 1. Firstly order the dataset by patientID and then by date.
  x<-x[order(x["HospNum_Id"],x["VisitDate"]),]
  x<-EndoMineR::SurveilTimeByRow(x,"HospNum_Id","VisitDate")

  # create an id to flag consecutive rows within each HospNum

  HospNum_Ida <- sym(HospNum_Id)
  VisitDatea <- sym(VisitDate)

  x %>%
    group_by(!!HospNum_Ida) %>%
    mutate(id = ceiling(row_number() / 2)) %>%
    ungroup() -> df2


  # split to even and odd rows within each HospNum
  df_odd = df2 %>% group_by(!!HospNum_Ida) %>% filter(row_number() %in% seq(1, nrow(df2), 2)) %>% ungroup()
  df_even = df2 %>% group_by(!!HospNum_Ida) %>% filter(row_number() %in% seq(2, nrow(df2), 2)) %>% ungroup()

  # join on both ids and remove rows
  x<-inner_join(df_odd, df_even, by=c("id","HospNum_Id")) %>%
    filter(between(diffDate.x, -4, 4))


  #Now you need to select the columns that have the regular expression Day1\\.y and convert to Day1 to Day 3 and Day2 to Day4
    #Select the columns with names Day1\\.y

    x<- x %>% rename_all(~gsub('(.*)Day1(.*)\\.y', '\\1Day3\\2', .x))

    x<- x %>% rename_all(~gsub('(.*)Day2(.*)\\.y', '\\1Day4\\2', .x))
  #if name contains .y and Day 1 then rename Day 2 to Day 4
  x<-data.frame(x)

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
  x$AllImpSymptom<-gsub("NO,","",x$AllImpSymptom,ignore.case=T)
  x$AllImpSymptom<-gsub(",NO","",x$AllImpSymptom,ignore.case=T)
  x$AllImpSymptom<-gsub("NO","",x$AllImpSymptom,ignore.case=T)
  x$AllImpSymptom<-gsub("NO","",x$AllImpSymptom,ignore.case=T)
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




###### Categorise the BRAVO diagnoses ######

#' Diagnosis of GORD for BRAVO studies
#'
#' This extracts whether the patient had a formal GORD diagnosis
#' This is based on the Acid exposure table but the rules are different for BRAVO results based on the day
#'
#' The rules are that if a patient has a long acid exposure percentage (>4.2% total)
#' Or if there are a large number of reflux events (>73) - (not that the reflux episodes are acid only
#' Or if the the final report says pathological reflux or nocturnal (as the total may be normal) then the patient has a GORD diagnosis
#' @param x the impedance dataset for extraction
#' @keywords BRAVO acid GORD
#' @export
#' @import dplyr
#' @examples #GORD_AcidBRAVO(x)

GORD_AcidBRAVO<-function(dd){
 de<- dd %>%
    #Check that Fraction is % Time Spent in Reflux
    mutate(
      AcidReflux = case_when(
        ReflDay1FractionTimepHLessThan4Supine  > 4.2        ~ "SupineAcid",
        ReflDay1NumberofRefluxesSupine  > 73 ~ "SupineAcid",
        ReflDay1FractionTimepHLessThan4Upright> 4.2        ~ "UprightAcid",
        ReflDay1NumberofRefluxesUpright > 73 ~ "UprightAcid",
        ReflDay1FractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDay1NumberofRefluxesTotal > 73 ~ "UprightAcid",
        ReflDay2FractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDay2NumberofRefluxesTotal > 73 ~ "TotalAcid",
        ReflDay2FractionTimepHLessThan4Supine > 4.2        ~ "SupineAcid",
        ReflDay2NumberofRefluxesSupine > 73 ~ "SupineAcid",
        ReflDay2FractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDay2NumberofRefluxesUpright > 73 ~ "UprightAcid",
        ReflDay2FractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDay2NumberofRefluxesTotal > 73 ~ "TotalAcid",
        ReflDay3FractionTimepHLessThan4Supine > 4.2        ~ "SupineAcid",
        ReflDay3NumberofRefluxesSupine > 73 ~ "SupineAcid",
        ReflDay3FractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDay3NumberofRefluxesUpright > 73 ~ "UprightAcid",
        ReflDay3FractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDay4NumberofRefluxesTotal > 73 ~ "TotalAcid",
        ReflDay4FractionTimepHLessThan4Supine > 4.2        ~ "SupineAcid",
        ReflDay4NumberofRefluxesSupine > 73 ~ "SupineAcid",
        ReflDay4FractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDay4NumberofRefluxesUpright > 73 ~ "UprightAcid",
        ReflDay4FractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDay4NumberofRefluxesTotal > 73 ~ "TotalAcid",
        ReflDayTotalFractionTimepHLessThan4Supine > 4.2        ~ "SupineAcid",
        ReflDayTotalNumberofRefluxesSupine > 73 ~ "SupineAcid",
        ReflDayTotalFractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDayTotalNumberofRefluxesUpright > 73 ~ "UprightAcid",
        ReflDayTotalFractionTimepHLessThan4Total > 4.2        ~ "TotalAcid",
        ReflDayTotalNumberofRefluxesTotal > 73 ~ "TotalAcid"
        #TRUE ~ as.character(x)
      )
    )
  return(de)
}

###### Categorise the Impedance diagnoses ######

#' Diagnosis of GORD for Impedance studies
#'
#' This extracts whether the patient had a formal GORD diagnosis
#' This is based on the Acid exposure table. The rules are that if a patient has a long acid exposure percentage (>4.2% total)
#' Or if there are a large number of reflux events (>73) - field called
#' Or if the the final report says pathological reflux or nocturnal (as the total may be normal) then the patient has a GORD diagnosis
#' @param x the impedance dataset for extraction
#' @keywords Impedance acid GORD
#' @export
#' @importFrom dplyr select
#' @examples #GORD_AcidImp(x)

GORD_AcidImp<-function(x){

  x<-x %>%
    mutate(
      AcidReflux = case_when(
        MainAcidExpTotalClearanceChannelPercentTime > 4.2        ~ "TotalAcid",
        MainAcidExpRecumbentClearanceChannelPercentTime > 1.2        ~ "RecumbentAcid",
        MainAcidExpUprightClearanceChannelPercentTime > 6.3        ~ "UprightAcid"
      )
    )

return(x)
}


#' Acid subtype extractor
#' This creates the composite score and then subcategorises the reflux ie if acid reflux then it is recumbent vs upright vs postprandial etc. (postprandial to be done)
#' @param x the impedance dataset for extraction
#' @keywords Impedance acid
#' @export
#' @examples #AcidSubtypes(x)

AcidSubtypes<-function(x){
  #Calculate the composite score here:
  x$AcidRefluxScore<-x$MainAcidCompositeScorePatientScoreUprightTimeInReflux+
    x$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux+
    x$MainAcidCompositeScorePatientScoreTotalTimeInReflux+
    x$MainAcidCompositeScorePatientScoreEpisodesOver5min+
    x$MainAcidCompositeScorePatientScoreLongestEpisode+
    x$MainAcidCompositeScorePatientScoreTotalEpisodes


  #Need to classify whether the patient is predom acid vs non-acid reflux/recumbent vs upright reflux
  #Redo this one as it should be if any SAP >50% for Non-Acid reflux

  x$TypeOfAcid<-ifelse(x$AcidRefluxScore>14.7& rowSums(x[grepl("RSAPNonacid",names(x))]>=50,na.rm=T)>0,"Mixed",
                                  ifelse(x$AcidRefluxScore>14.7,"Acid",
                                         ifelse(rowSums(x[grepl("RSAPNonacid",names(x))]>=50,na.rm=T)>0,"NonAcid","Normal")))


  #Predom recumbent vs upright acid here

  x$PositionOfAcid<-ifelse(x$MainAcidCompositeScorePatientScoreUprightTimeInReflux>8.4&x$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux>3.5,"Upright&RecumbentAcid",
                                      ifelse(x$MainAcidCompositeScorePatientScoreUprightTimeInReflux>8.4&x$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux<3.5,"UprightAcid",
                                             ifelse(x$MainAcidCompositeScorePatientScoreRecumbentTimeInReflux>3.5,"RecumbentAcid","NoPosition")))
  #Predom recumbent vs upright NonAcid here
  x$PositionOfNonAcid<-ifelse(x$MainRflxEpisodeUprightNonacid/x$MainRflxEpisodeUprightAllReflux>0.5&x$MainRflxEpisodeRecumbentNonacid/x$MainRflxEpisodeRecumbentAllReflux>0.5,"MixedNonAcid",
                                         ifelse(x$MainRflxEpisodeUprightNonacid/x$MainRflxEpisodeUprightAllReflux>0.5,"UprightNonAcid",
                                                ifelse(x$MainRflxEpisodeRecumbentNonacid/x$MainRflxEpisodeRecumbentAllReflux>0.5,"RecumbentNonAcid","Normal_NoNonAcid")))


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


