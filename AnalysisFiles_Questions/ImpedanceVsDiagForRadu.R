#Extract data from two related tables in the database:

.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology_Compacted6.mdb")
data <- sqlQuery( channel , "SELECT  Diag.* FROM Diag")

as.character(data$FileCreationDate)

#Create a date conversion function so that:





data$VisitDate<-as.character(data$VisitDate)
data$VisitDate<-as.Date(data$VisitDate,format="%m_%d_%Y")
data$VisitDate<-format(data$VisitDate,"%d_%m_%Y")


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
