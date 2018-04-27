library(RODBC)
library(dplyr)
library(ggplot2)

#############################  Get the HRM data #######################################################################################
.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()


channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")
data <- sqlQuery( channel , "SELECT  HRMImportMain.* FROM HRMImportMain")


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Manometry\\MotilityFunctions.R")
data<-HRMCleanUp1(data)
data<-HRMCleanUp(data)

data$dx[is.na(data$dx)]=0
data$HospNum_Id<-as.character(data$HospNum_Id)









#Separate into the patients with low LOS and then with hiatus hernia and then with both low LOS and hiatus hernia


#############################  Get the impedance data #######################################################################################
.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()


channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")
dataImp2 <- sqlQuery( channel , "SELECT Impedance2.*FROM Impedance2")
dataImp_Symp <- sqlQuery( channel , "SELECT Imp_Symp.* FROM Imp_Symp")
 
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\PhysiologyQuestions\\ImpedanceFunctions.R")
dataImpWhole<-dataImpClean(dataImp2,dataImp_Symp)

############################# Merge impedance data with HRM #######################################################################################



MyImpedanceDataWithHRM<-merge(dataImpWhole,data,by=c("HospNum_Id"),all=TRUE)
MyImpedanceDataWithHRM$DateDiff<-MyImpedanceDataWithHRM$VisitDate.x-MyImpedanceDataWithHRM$VisitDate.y

#Breakdown the contribution of acid and non ecide to the different manometry subtypes
table(MyImpedanceDataWithHRM$TypeOfAcid,MyImpedanceDataWithHRM$dx)
#Now determine the causes of absent peristalsis- get the data for non-acid,non-achalasia AbsentPeristalsis
AbsentPeristalsis_NonAcidNonAchalasia<-MyImpedanceDataWithHRM[MyImpedanceDataWithHRM$TypeOfAcid=="Normal"&MyImpedanceDataWithHRM$dx=="AbsentPeristalsis",]
AbsentPeristalsis_NonAcidNonAchalasia<-AbsentPeristalsis_NonAcidNonAchalasia[!is.na(AbsentPeristalsis_NonAcidNonAchalasia$dx),]
#Subdivide by LOS
AbsentPeristalsis_NonAcidNonAchalasia_NormalLOS<-AbsentPeristalsis_NonAcidNonAchalasia[AbsentPeristalsis_NonAcidNonAchalasia$LowerOesoph=="Normal",]
AbsentPeristalsis_NonAcidNonAchalasia_AbNormalLOS<-AbsentPeristalsis_NonAcidNonAchalasia[AbsentPeristalsis_NonAcidNonAchalasia$LowerOesoph!="Normal",]
#######################################################################################


# #How much impedance by year?
 dates<-dataImpWhole[order(dataImpWhole$VisitDate),]
 dates<-dates%>%select(VisitDate)%>% mutate(year = format(dates$VisitDate, "%Y"))%>%mutate(month = format(dates$VisitDate, "%m")) %>%
   group_by(year,month)
# 
 dates<-as.data.frame(dates)
table(dates$year,dates$month)
###################### Question: Which structural abnormalities are most likely to give you reflux ###################### 





######### Question: Is there a geriatric oesopahgus ie does the DCI decrease with age? ########################### 


#dateDiff to get the age of the patients:

data<-data[data$Age<100&data$Age>0,]
data<-data[data$dx=="Normal",]
data<-data[data$DCI<5000&data$DCI>0,]

ggplot(data, aes(x=Age))+geom_density()

#Now calculate how the DCI varies with age
qplot(data$Age,data$DCI)+geom_smooth()




