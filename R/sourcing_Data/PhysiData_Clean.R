#Data cleaning
#Use after PhysiData_Acq

AllImpedance<-merge(Impedance2,Imp_Symp,by="Imp_Id", all = TRUE)

#Date cleaning
#Clean up the HRM swallows:
HRMImportSwallows<-HRMImportSwallows[!is.na(HRMImportSwallows$panesophagealpressurizationMapSwallowsNum8),]


#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)
rm(HRMImportMain)
#Clean up the HRM HRMImportMain (to get rid of duplicates mainly)
#This looks for all the duplicates by hospital number and DistalLESfromnarescm (the latter so that duplicates means same test rather than same patient on two different days)

HRMImportMain2<-AllHRM %>%
  group_by(HospNum_Id,DistalLESfromnarescm) %>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))

#Clean Up the main HRM
HRMImportMain<-HRMCleanUp1(HRMImportMain2)

#Avoid cluttering things up
rm(AllHRM)


#Also need to get rid of the duplicates from the Diag table:
#Recognise duplicates by the same Hospital Number and HRM_Id (this will be the same even after HRM merging)).

Diag$HospNum_Id<-toupper(Diag$HospNum_Id)
Diag$FileCreationDate<-as.Date(gsub("(\\d{4}-\\d{2}-\\d{2}).*","\\1",Diag$FileCreationDate))
#Convert VisitDate into a Date format and extract the year (the month and day are unreliable here as sometimes American and sometimes British dates are used)

#Convert the dates to date format
Diag$VisitDate2<-as.Date(Diag$VisitDate,format="%d_%m_%Y")
#For NA dates make sure it is not because the dates are the wrong way around
Diag$VisitDate3<-as.Date(Diag$VisitDate,format="%m_%d_%Y")

#Merge the dates if there are separate HRM and Diag results which are reporting the same thing
Diag$VisitDate4<-as.Date(ifelse(is.na(Diag$VisitDate2),Diag$VisitDate3,Diag$VisitDate2),origin = "1970-01-01")
#If still NA then use the file creation date as the visit date
Diag$VisitDate4<-as.Date(ifelse(is.na(Diag$VisitDate4),Diag$FileCreationDate,Diag$VisitDate4),origin = "1970-01-01")

#Extract the Distal LES as is likely to be in both the final and the HRM report so can be merged on this
Diag$DistalLESnares<-gsub(".*Distal LES from nares.*?(\\d+).*","\\1",Diag$WholeReport)
#Get rid of whole reports that are copied over for some reason
Diag$DistalLESnares<-gsub(".*[A-Za-z].*","\\1",Diag$DistalLESnares)

#Extract some diagnoses from the report (using the histopath extractor from EndoMineR)
Diag$Dx<-str_extract_all(Diag$WholeReport, paste0("upragas|neffecti|ackham|utcrack|[Aa]peris|[Ff]requent [Ff]ail" , simplify = FALSE))
Diag$Dx <- sapply(Diag$Dx, toString)

#Merge the diag reports on the basis of being the same Hospital Number and date range here:
Diag2<-Diag %>%
  arrange(DistalLESnares,HospNum_Id) %>%
  group_by(DistalLESnares,HospNum_Id,lubridate::year(VisitDate4)) %>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))
rm(Diag)





# Get the whole impedance dataset
#You will need to re-clean the merged dataImpSympImpedance as cleaning function needed to be fixed
Impedance2_Clean<-dataImpClean(AllImpedance)
rm(AllImpedance)
#ImpSymp_Clean<-dataImpSympClean(ImpSymp)
#ImpAll<-merge(Impedance2_Clean,ImpSymp_Clean,by="Imp_Id")


#Get the whole BRAVO dataset:
# Need to get the BRAVO data merged from the two tables
AllBravo<-merge(BravoDay1And2,BravoDay3And4,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")

rm(BravoDay1And2)
rm(BravoDay3And4)

