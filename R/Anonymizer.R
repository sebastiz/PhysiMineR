#The Anonymizer

#Take the numerical columns only and sample them

# channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")
# BRAVODay1And2 <- sqlQuery( channel , "SELECT BravoDay1And2.*, PatientData.* FROM PatientData INNER JOIN BravoDay1And2 ON PatientData.HospNum_Id = BravoDay1And2.HospNum_Id")
# BRAVODay1And2<-dataBRAVOClean(BRAVODay1And2)
# #Generate fake numbers for the data
# BRAVODay1And2<-lapply(BRAVODay1And2, function(x) if(is.numeric(x)&!all(is.na(x))) {sample(min(as.numeric(x),na.rm=TRUE):max(as.numeric(x),na.rm=TRUE),length(x),replace=TRUE)})
#
# BRAVOTotal<-dataBRAVOClean(BRAVOTotal)
# #Generate fake numbers for the data
# BRAVOTotal<-lapply(BRAVOTotal, function(x) if(is.numeric(x)&!all(is.na(x))) {sample(min(as.numeric(x),na.rm=TRUE):max(as.numeric(x),na.rm=TRUE),length(x),replace=TRUE)})
#
# Impedance2<-dataImpClean(Impedance2)
# #Generate fake numbers for the data
# Impedance2<-lapply(Impedance2, function(x) if(is.numeric(x)&!all(is.na(x))) {sample(min(as.numeric(x),na.rm=TRUE):max(as.numeric(x),na.rm=TRUE),length(x),replace=TRUE)})
#
# ImpSymp<-dataImpSympClean(ImpSymp)
# #Generate fake numbers for the data
# ImpSymp<-lapply(ImpSymp, function(x) if(is.numeric(x)&!all(is.na(x))) {sample(min(as.numeric(x),na.rm=TRUE):max(as.numeric(x),na.rm=TRUE),length(x),replace=TRUE)})
#
# #HRMWithSwallows<-HRMCleanUp1(HRMWithSwallows)
# HRMWithSwallows<-lapply(HRMWithSwallows, function(x) if(is.numeric(x)&!all(is.na(x))) {sample(min(as.numeric(x),na.rm=TRUE):max(as.numeric(x),na.rm=TRUE),length(x),replace=TRUE)})



#Then get the minimum and maximum of the column
#Then sample at cut between the values to fill all the values.

#Also anonymize the Hospital Numbers if present
#Also anonymise the dates
#Get rid of the Diag free text column
