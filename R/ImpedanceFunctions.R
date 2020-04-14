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
  x<-data.frame(lapply(x, FUN = function(t) as.character(gsub("%", "", t))), stringsAsFactors = FALSE)
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

  #Merge the composite score with the MainAcidExp score in cases where the latter is missing for whatever reason:

  x$MainAcidExpUprightClearanceChannelPercentTime<-ifelse(is.na(x$MainAcidExpUprightClearanceChannelPercentTime),x$MainAcidCompositeScorePatientValueUprightTimeInReflux,x$MainAcidExpUprightClearanceChannelPercentTime)
  x$MainAcidExpRecumbentClearanceChannelPercentTime<-ifelse(is.na(x$MainAcidExpRecumbentClearanceChannelPercentTime),x$MainAcidCompositeScorePatientValueRecumbentTimeInReflux,x$MainAcidExpRecumbentClearanceChannelPercentTime)
  x$MainAcidExpTotalClearanceChannelPercentTime<-ifelse(is.na(x$MainAcidExpTotalClearanceChannelPercentTime),x$MainAcidCompositeScorePatientValueTotalTimeInReflux,x$MainAcidExpTotalClearanceChannelPercentTime)

  #Get the date of birth properly formatted
  x$MainPtDataDateofBirth<-lubridate::ymd(x$MainPtDataDateofBirth)
  x$MainPtDataDateofBirth<-as.Date(as.character(x$MainPtDataDateofBirth),format="%Y-%m-%d",origin="30/12/1899")

  i1 <- grepl("_", x)
  x[i1]<-lapply(x[i1], FUN = function(t) gsub("_", ":", t))

  #Make sure the numbers are extracted as numeric from the duration columns
  i1 <- grepl("Duration", names(x))
  x[i1] <- lapply(x[i1], function(d) ifelse(grepl(":",d),(as.numeric(stringr::str_extract(d,"^\\d{2}"))*60)+(as.numeric(stringr::str_extract(d,"\\d{2}$"))),d))
  i1 <- grepl("%", x)
  x[i1]<-lapply(x[i1], FUN = function(t) gsub("%", ":", t))
  i1 <- grepl("pcent", x)
  x[i1]<-lapply(x[i1], FUN = function(t) gsub("pcent", "", t))
  i1 <- grepl("min", x)
  x[i1]<-lapply(x[i1], FUN = function(t) gsub("min", "", t))
  i1 <- grepl("sec", x)
  x[i1]<-lapply(x[i1], FUN = function(t) gsub("sec", "", t))


  x<-data.frame(x,stringsAsFactors = FALSE)

  #Make sure the numbers are extracted as numeric from the MainPt columns
  i2 <- !grepl("MainPt|HospNum_Id|FName|SName|DOB|Ethnicity|Gender|VisitDate", names(x))

  #Also exclude the first columns with hospital number etc.
  x[i2] <- lapply(x[i2], as.numeric)



  x <- x %>%
    mutate(
      SAPHeartburn = case_when(
        SxMainRSAPAcidHeartburn  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPChestPain = case_when(
        SxMainRSAPAcidChestPain  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPVomiting = case_when(
        SxMainRSAPAcidVomiting  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPNausea = case_when(
        SxMainRSAPAcidNausea  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPRegurgitation = case_when(
        SxMainRSAPAcidRegurgitation  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPBelch = case_when(
        SxMainRSAPAcidBelch  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPStomachPain = case_when(
        SxMainRSAPAcidStomachPain  >94 ~ 1,
        TRUE ~ 0))%>%




    mutate(
      SIHeartburn = case_when(
        MainSxRSIAcidHeartburn  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SIChestPain = case_when(
        MainSxRSIAcidChestPain  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SIVomiting = case_when(
        MainSxRSIAcidVomiting  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SINausea = case_when(
        MainSxRSIAcidNausea  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SIRegurgitation = case_when(
        MainSxRSIAcidRegurgitation  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SIBelch = case_when(
        MainSxRSIAcidBelch  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SIStomachPain = case_when(
        MainSxRSIAcidStomachPain   >94 ~ 1,
        TRUE ~ 0
      )) %>%

    mutate(
      SAPOesophageal = case_when(
        SxMainRSAPAcidHeartburn  >94 ~ 1,
        SxMainRSAPAcidChestPain  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SAPOther = case_when(
        SxMainRSAPAcidVomiting >94 ~ 1,
        SxMainRSAPAcidNausea  >94 ~ 1,
        SxMainRSAPAcidRegurgitation  >94 ~ 1,
        SxMainRSAPAcidBelch  >94 ~ 1,
        SxMainRSAPAcidStomachPain >94 ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      SIOesophageal = case_when(
        MainSxRSIAcidHeartburn  >49 ~ 1,
        MainSxRSIAcidChestPain  >49 ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      SIOther = case_when(
        MainSxRSIAcidVomiting >49 ~ 1,
        MainSxRSIAcidNausea  >49 ~ 1,
        MainSxRSIAcidRegurgitation  >49 ~ 1,
        MainSxRSIAcidBelch  >49 ~ 1,
        MainSxRSIAcidStomachPain >49 ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      SAPLPR = case_when(
        SxMainRSAPAcidThroat >94 ~ 1,
        SxMainRSAPAcidCough  >94 ~ 1,
        TRUE ~ 0
      )) %>%
    mutate(
      SILPR = case_when(
        MainSxRSIAcidThroat >49 ~ 1,
        MainSxRSIAcidCough  >49 ~ 1,
        TRUE ~ 0
      ))



  x$SAPOesophageal<-as.factor(x$SAPOesophageal)
  x$SIOesophageal<-as.factor(x$SIOesophageal)

  x$SAPLPR<-as.factor(x$SAPLPR)
  x$SILPR<-as.factor(x$SILPR)

  x$SAPOther <-as.factor(x$SAPOther)
  x$SIOther <-as.factor(x$SIOther)

  x$SAPHeartburn <-as.factor(x$SAPHeartburn)
  x$SAPChestPain <-as.factor(x$SAPChestPain )
  x$SAPVomiting <-as.factor(x$SAPVomiting)
  x$SAPNausea <-as.factor(x$SAPNausea)
  x$SAPRegurgitation <-as.factor(x$SAPRegurgitation)
  x$SAPBelch <-as.factor(x$SAPBelch)
  x$SAPStomachPain <-as.factor(x$SAPStomachPain)

  x$SIHeartburn <-as.factor(x$SIHeartburn)
  x$SIChestPain <-as.factor(x$SIChestPain)
  x$SIVomiting <-as.factor(x$SIVomiting)
  x$SINausea <-as.factor(x$SINausea)
  x$SIRegurgitation <-as.factor(x$SIRegurgitation)
  x$SIBelch <-as.factor(x$SIBelch)
  x$SIStomachPain <-as.factor(x$SIStomachPain)




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
  x<-full_join(df_odd, df_even, by=c("id","HospNum_Id"))
   # filter(between(diffDate.x, -4, 4))

  #Now you need to select the columns that have the regular expression Day1\\.y and convert to Day 3 and Day2 to Day4
    #Select the columns with names Day1\\.y
  #ReflDay1FractionTimepHLessThan4Total ReflDay2FractionTimepHLessThan4Total= Day 1 and Day 2
  #ReflDay1_2FractionTimepHLessThan4Total ReflDay2_2FractionTimepHLessThan4Total = Day 3 and 4

    x<- x %>% rename_all(~gsub('(.*)Day1(.*)\\.y', '\\1Day1_2\\2', .x))
    x<- x %>% rename_all(~gsub('(.*)Day2(.*)\\.y', '\\1Day2_2\\2', .x))
    x<- x %>% rename_all(~gsub('(.*)Day3(.*)\\.y', '\\1Day3_2\\2', .x))
    x<- x %>% rename_all(~gsub('(.*)Day4(.*)\\.y', '\\1Day4_2\\2', .x))
    x<- x %>% rename_all(~gsub('(.*)DayTotal(.*)\\.y', '\\1DayTotal_2\\2', .x))
    x<- x %>% rename_all(~gsub('(.*)\\.x', '\\1', .x))
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

  #Not sure what this is for:
  #x<-x[,colSums(is.na(x))<nrow(x)-5]

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


  x$AllSymps_Impgrouped<-gsub("Heartburn|Regurgitation","Typical",x$AllImpSymptom)
  x$AllSymps_Impgrouped<-gsub("ChestPain|Cough|Belch|Vomiting|StomachPain|Throat|Nausea","Atypical",x$AllSymps_Impgrouped)

  interim<-strsplit(x$AllSymps_Impgrouped,",")
  interim<-lapply(interim,function(x)unique(x,","))
  interim<-lapply(interim,function(x) sort(x))
  new<-unlist(lapply(interim,function(x) paste0(x,collapse=",")))
  x$AllSymps_Impgrouped<-gsub("Atypical,Typical","Mixed",new)


  return (x)
}


###################################### BRAVO Symptom Subset Prepare ###################################################

#Get the symptoms in each row then into own dataset so that each dataset contains the findings for those symptoms
#To extract the symptoms into their own box:

#' BRAVO symptom extractor function
#'
#' This extracts the relevant symptoms from the impedance dataset
#' @param x the BRAVO dataframe for extraction from SAPTotal as probably more accurate as patient entered (rather than from summary)
#' @keywords Impedance symptoms
#' @export
#' @examples
#' #dataBRAVOSymptoms(x)

dataBRAVOSymptoms<-function(x){


  myframe<-x[,grepl("ReflDayTotalTimepHLessThan4min|SITotal|SAPTotal",colnames(x))]

  #Add the colnames to each value in each row:
  mine<-data.frame(apply(myframe,1,function(y) unlist(paste0(colnames(myframe),y))))

  #Concatenate them
  AllSymps<-as.character(apply(mine, 2, paste, collapse=","))

  #Now extract the symptoms that have a value associated with them (ie should ignore the NA's)
  AllSymps2<-stringr::str_extract_all(AllSymps,"(ReflDayTotalTimepHLessThan4min[A-Za-z]*\\d+)|SITotal[A-Za-z]*\\d+|SAPTotal[A-Za-z]*\\d+")

  #Concatenate them:
  x$AllSymps_BRAVO<-unlist(lapply(AllSymps2,function(x) paste0(unlist(x),collapse=",")))

  #Now Clean it up
  x$AllSymps_BRAVO<-gsub("ReflDayTotalTimepHLessThan4min|SITotal|SAPTotal","",x$AllSymps_BRAVO)
  x$AllSymps_BRAVO<-gsub("\\d*","",x$AllSymps_BRAVO)
  x$AllSymps_BRAVO<-gsub("Meal,|Other,|PostPr,|Supine,|Total,|Upright,|Upright|PostPr","",x$AllSymps_BRAVO)

  #Now put symptoms into compartments (oesophageal/LPR/other)
  x$AllSymps_BRAVOcompartment<-gsub("Heartburn|Regurgitation","Oesophageal",x$AllSymps_BRAVO)
  x$AllSymps_BRAVOcompartment<-gsub("Cough|Throat","LPR",x$AllSymps_BRAVOcompartment)
  x$AllSymps_BRAVOcompartment<-gsub("Vomiting|ChestPain|StomachPain|Nausea|Epigastric|Belch","Other",x$AllSymps_BRAVOcompartment)

  interim<-strsplit(x$AllSymps_BRAVOcompartment,",")
  interim<-lapply(interim,function(x)unique(x,","))
  interim<-lapply(interim,function(x) sort(x))
  x$AllSymps_BRAVOcompartment<-unlist(lapply(interim,function(x) paste0(x,collapse=",")))


  #Now group them into typical and atypical
  x$AllSymps_BRAVOgrouped<-gsub("Heartburn|Regurgitation","Typical",x$AllSymps_BRAVO)
  x$AllSymps_BRAVOgrouped<-gsub("ChestPain|Cough|Belch|Vomiting|StomachPain|Throat|Nausea|Epigastric","Atypical",x$AllSymps_BRAVOgrouped)

  interim<-strsplit(x$AllSymps_BRAVOgrouped,",")
  interim<-lapply(interim,function(x)unique(x,","))
  interim<-lapply(interim,function(x) sort(x))
  new<-unlist(lapply(interim,function(x) paste0(x,collapse=",")))

  x$AllSymps_BRAVOgrouped<-gsub("Atypical,Typical","Mixed",new)

  return(x)

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
      AcidRefluxBRAVO = case_when(
        #ReflDay1FractionTimepHLessThan4Supine  > 4.9        ~ "SupineAcid",
        #ReflDay1FractionTimepHLessThan4Upright> 5.2        ~ "UprightAcid",
        ReflDay1FractionTimepHLessThan4Total > 5.3       ~ "TotalAcid",
        #ReflDay1NumberofRefluxesTotal > 36 ~ "TotalAcid",

        #ReflDay2FractionTimepHLessThan4Supine > 6.8        ~ "SupineAcid",
        #ReflDay2FractionTimepHLessThan4Upright > 8.8        ~ "UprightAcid",
        ReflDay2FractionTimepHLessThan4Total > 5.3        ~ "TotalAcid",
        #ReflDay2NumberofRefluxesTotal > 62 ~ "TotalAcid",

        #ReflDay1_2FractionTimepHLessThan4Supine > 6.8        ~ "SupineAcid",
        #ReflDay1_2FractionTimepHLessThan4Upright > 8.8        ~ "UprightAcid",
        ReflDay1_2FractionTimepHLessThan4Total > 5.3 ~ "TotalAcid",
       # ReflDay1_2NumberofRefluxesTotal > 62        ~ "TotalAcid",

        #ReflDay2_2FractionTimepHLessThan4Supine > 6.8        ~ "SupineAcid",
        #ReflDay2_2FractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDay2_2FractionTimepHLessThan4Total > 5.3       ~ "TotalAcid",
        #ReflDay2_2NumberofRefluxesTotal > 62 ~ "TotalAcid",

        #ReflDayTotalFractionTimepHLessThan4Supine > 6.8        ~ "SupineAcid",
       # ReflDayTotalFractionTimepHLessThan4Upright > 4.2        ~ "UprightAcid",
        ReflDayTotalFractionTimepHLessThan4Total > 5.3        ~ "TotalAcid",
        #ReflDayTotalNumberofRefluxesTotal > 62 ~ "TotalAcid",
        TRUE ~ "NoAcid"
      )
    ) %>%
   mutate(
     AcidRefluxBRAVOTotalOnly = case_when(
       ReflDay1FractionTimepHLessThan4Total > 4.9        ~ 1,
       ReflDay2FractionTimepHLessThan4Total > 4.9       ~ 1,
       ReflDay1_2FractionTimepHLessThan4Total > 4.9        ~ 1,
       ReflDay2_2FractionTimepHLessThan4Total > 4.9        ~ 1,
       ReflDayTotalFractionTimepHLessThan4Total > 4.9        ~ 1,

       TRUE ~ 0
     )
   )

 #Recoding the BRAVO rewflux columns:
 de$AcidRefluxBRAVO<-gsub("NoAcid",0,de$AcidRefluxBRAVO)
 de$AcidRefluxBRAVO<-gsub(".*Acid",1,de$AcidRefluxBRAVO)
 de$AcidRefluxBRAVO<-as.numeric(de$AcidRefluxBRAVO)


  return(de)
}


###### Categorise the Impedance diagnoses ######

#' Create Worst day and Average day analysis of BRAVO
#'
#' This extracts whether the patient had a formal GORD diagnosis
#' This is based on the Acid exposure table. The rules are that if a patient has a long acid exposure percentage (>4.2% total)
#' Or if there are a large number of reflux events (>73) - field called
#' Or if the the final report says pathological reflux or nocturnal (as the total may be normal) then the patient has a GORD diagnosis
#' @param x the impedance dataset for extraction
#' @keywords Impedance acid GORD
#' @export
#' @importFrom dplyr select
#' @examples #GORD_AcidImpImp(x)

GORD_BravoWDAAndAverage<-function(x){


  #Use Fraction <pH4 as the analysis:
  #x<-ForBRAVODescriptionLater[,grepl("FractionTimepHLessThan4Total|AcidRefluxBRAVO|SAP|SI",names(ForBRAVODescriptionLater))]

  #Only select the GORD patients by pH<4
  #x$SIDay1Heartburn<-as.numeric(x$SIDay1Heartburn)
  x$averageAll<-rowMeans(select_if(x, is.numeric)%>%select(matches("y[0-9]*_*[0-9]FractionTimepHLessThan4Total")), na.rm = TRUE)

  #This one just takes the first two days average so that on PPI measurements are not included.
  x$average<-rowMeans(select_if(x, is.numeric)%>%select(matches("y[1-2].*FractionTimepHLessThan4Total")), na.rm = TRUE)


  x<- x %>%
    #Check that Fraction is % Time Spent in Reflux
    mutate(
      AcidRefluxBRAVOAv = case_when(

        average > 5.3        ~ "Acid",
        #ReflDayTotalNumberofRefluxesTotal > 62 ~ "TotalAcid",
        TRUE ~ "NoAcid"
      )
    )
  #Recoding the BRAVO rewflux columns:
  x$AcidRefluxBRAVOAv<-gsub("NoAcid",0,x$AcidRefluxBRAVOAv)
  x$AcidRefluxBRAVOAv<-gsub(".*Acid",1,x$AcidRefluxBRAVOAv)
  x$AcidRefluxBRAVOAv<-as.numeric(x$AcidRefluxBRAVOAv)






  x$worst<-do.call(pmax, c(select_if(x, is.numeric)%>%select(matches("y[0-9]*_*[0-9]FractionTimepHLessThan4Total")), na.rm = TRUE))

  #Need to get which day was the worst:
  x$worstDaypH<-names(select_if(x, is.numeric)%>%select(matches("y[0-9]*_*[0-9]FractionTimepHLessThan4Total")))[max.col(replace(select_if(x, is.numeric)%>%select(matches("y[0-9]*_*[0-9]FractionTimepHLessThan4Total")), is.na(select_if(x, is.numeric)%>%select(matches("y[0-9]*_*[0-9]FractionTimepHLessThan4Total"))),0),ties.method = "first")]
  #Clean up the days:
  x$worstDaypH<-gsub("FractionTimepHLessThan4Total","",x$worstDaypH)
  x$worstDaypH<-gsub("ReflDay","",x$worstDaypH)
  x$worstDaypH<-gsub("1_2","3",x$worstDaypH)
  x$worstDaypH<-gsub("2_2","4",x$worstDaypH)
  x$worstDaypH<-gsub("3_2","5",x$worstDaypH)

  x$worstDaypH<-as.numeric(x$worstDaypH)

  x$Day2Pos<-ifelse(x$ReflDay2FractionTimepHLessThan4Total>5,"Day2Pos","Day2Neg")

  x<-x%>%
    mutate(
      Day1Pos=case_when(
        ReflDay1FractionTimepHLessThan4Total > 4.9        ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      Day2Pos=case_when(
        ReflDay2FractionTimepHLessThan4Total > 4.9         ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      Day3Pos=case_when(
        ReflDay1_2FractionTimepHLessThan4Total > 4.9         ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      Day4Pos=case_when(
        ReflDay2_2FractionTimepHLessThan4Total > 4.9         ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      Day5Pos=case_when(
        ReflDay3_2FractionTimepHLessThan4Total > 4.9         ~ 1,
        TRUE ~ 0
      ))%>%
    mutate(
      Day6Pos=case_when(
        ReflDay4_2FractionTimepHLessThan4Total > 4.9         ~ 1,
        TRUE ~ 0
      ))
  #Sum the rows to see how many days are positive
  x$NumDaysBravoPositive<-rowSums(x[grep("^Day", names(x))])

  return(x)


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
#' @examples #GORD_AcidImpImp(x)

GORD_AcidImp<-function(x){

  x<-x %>%
    mutate(
      AcidReflux_Imp = case_when(
        MainAcidExpTotalClearanceChannelPercentTime > 4.2        ~ "TotalAcid",
        MainAcidExpTotalClearanceChannelPercentTime < 4.1        ~ "NoAcid",
        #MainAcidExpRecumbentClearanceChannelPercentTime > 1.2        ~ "RecumbentAcid",
        #MainAcidExpUprightClearanceChannelPercentTime > 6.3        ~ "UprightAcid",

        #MainAcidCompositeScorePatientValueTotalTimeInReflux > 5.9        ~ "TotalAcid",
        #MainAcidCompositeScorePatientValueTotalTimeInReflux > 4.1        ~ "NoAcid",
        #MainAcidCompositeScorePatientValueRecumbentTimeInReflux > 1.2        ~ "RecumbentAcid",
        #MainAcidCompositeScorePatientValueUprightTimeInReflux > 6.3        ~ "UprightAcid",
        TRUE ~ "PossibleAcid"
        )
    )

  #Recoding the impedance reflux columns:

  x$AcidReflux_Imp<-gsub("NoAcid|PossibleAcid",0,x$AcidReflux_Imp)
  x$AcidReflux_Imp<-gsub("TotalAcid",1,x$AcidReflux_Imp)
  x$AcidReflux_Imp<-as.numeric(x$AcidReflux_Imp)


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
#' Extracts all the patients with supragastric belching- can only be done once the whole report
#' #has been merged with the impedance results
#' @param x the impedance merged with whole report dataset for extraction
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


