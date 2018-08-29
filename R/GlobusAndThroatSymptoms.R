#Globus and throat symptoms assessment:
library(dplyr)
library(ggplot2)
library(directlabels)
library(gridExtra)
library(splitstackshape)
library(googleVis)
library(tidyr)
library(stringr)
library(anytime)



.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")
dataImp2 <- sqlQuery( channel , "SELECT Impedance2.*FROM Impedance2")
dataImp_Symp <- sqlQuery( channel , "SELECT Imp_Symp.* FROM Imp_Symp")
#Clean up percentages and 'mins' etc.
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\PhysiologyQuestions\\ImpedanceFunctions.R")
dataImpWhole<-dataImpClean(dataImp2,dataImp_Symp)

#To get the NonAcid subset:
dfwNon<-subset(dataImpWhole,dataImpWhole$TypeOfAcid=="NonAcid")



#Question- how do the symptoms you get with non acid reflux differ from acid reflux:
s<-strsplit(as.character(dataImpWhole[["Symptom"]]),',')
TypeOfAcidVsSx<-data.frame(Symptom=unlist(s),TypeOfAcid=rep(dataImpWhole[["TypeOfAcid"]],sapply(s,FUN=length)))

TyepvsSymptom<-TypeOfAcidVsSx %>%
  group_by(TypeOfAcid,Symptom) %>%
  dplyr::summarise(n=n(Symptom,na.rm=T),count=n())





dfwNon<-Filter(is.numeric,dfwNon)
dfwNon[is.na(dfwNon)] <- 0
z = dfwNon[,-c(1,1)]
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)


##calculate distance matrix (default is Euclidean distance)
distance = dist(nor)


# Hierarchical agglomerative clustering using default complete linkage 
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=dfwNonAcid$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)


# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)


# Cluster membership
member = cutree(mydata.hclust,3)
table(member)


#Characterizing clusters 
aggregate(nor,list(member),mean)
mine<-as.data.frame(t(aggregate(dfwNon[,-c(1,1)],list(member),mean)))
names(mine)<-c("Gp1","Gp2","Gp3")
mine<-mine[-1,]
mine<-mine[order(mine$Gp1),]


#Here you need to select the data in rattle, then transform to rescale, then cluster via ewkm  
library(rattle)
rattle()
#Then do, to get the weights in order
myweights<-as.data.frame(t(crs$kmeans$weights))
names(myweights)<-c("Gp1","Gp2","Gp3")

myweights_Gp1<-myweights[order(myweights$Gp1),]
myweights_Gp2<-myweights[order(myweights$Gp2),]
myweights_Gp3<-myweights[order(myweights$Gp3),]
#To get the Recumbent acid refluxers vs diurnal vs Postprandial










########### So can identify symptoms individually########### ########### ########### ########### 

SxPlotter<-function(x,dataImpWholeSymptomsPlotter){
  this_is_a_name <- x; 
  print(this_is_a_name)
  
  #This is to make sure that I get the data from the dataframe as I am passing the names into the function
  x <- data.frame(dataImpWholeSymptomsPlotter[[x]],dataImpWholeSymptomsPlotter$Symptom)
  #print(nrow(x))
  #nameOfCol<-names(x[1,])
  #View(x)
  #print(nameOfCol)
  #nameOfCol<-x$MainBolusExpoRecumbentLongestEpisode

   try(ggplot(dataImpWholeSymptomsPlotter,aes(x=Symptom,y=this_is_a_name))+
     geom_boxplot(fill="red",outlier.shape = NA))
#     labs(title=this_is_a_name)+
#     theme(axis.text.x=element_text(angle=-45))+
#     coord_cartesian(ylim = c(0, 100))
}

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
mygraphs<-lapply(names(dataImpWholeSymptomsPlotter),SxPlotter,dataImpWholeSymptomsPlotter=dataImpWholeSymptomsPlotter)
try(multiplot(plotlist=mygraphs))

###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 



#Exploratory data analysis
#do this via rattle and then use the correlation matric (Pearson then just concentrate on the high correlation values)
crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")
crs$cor<-replace(crs$cor,crs$cor>-0.8&crs$cor<0.8,0)
crs$cor[crs$cor==0]<-NA
corrplot(crs$cor[1:100], mar=c(2,0,0,10),tl.cex = 0.4,tl.offset = 3)



###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
#####################################  Measurement Symptom Chopper########### ######################################
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 
###################################### ###################################### ###################################### 

boxplot(dataImp_Symp$MainSxRSIAcidRegurgitation)

#Hmmmm...What about splitting by Measurement

MainSxCorrSymptomAcidrel<-dataImpWhole[,grepl("MainSxCorrSymptomAcidrel",colnames(dataImpWhole))]
MainSxCorrSymptomNonAcidrel<-dataImpWhole[,grepl("MainSxCorrSymptomNonAcidrel",colnames(dataImpWhole))]
MainSxCorrSymptomAllRefluxrel<-dataImpWhole[,grepl("MainSxCorrSymptomAllRefluxrel",colnames(dataImpWhole))]
MainSxCorrSymptomSxIndex<-dataImpWhole[,grepl("MainSxCorrSymptomSxIndex",colnames(dataImpWhole))]
MainSxCorrSymptomUnrelated<-dataImpWhole[,grepl("MainSxCorrSymptomUnrelated",colnames(dataImpWhole))]

MainSxRSSISymptomAcid<-dataImpWhole[,grepl("MainSxRSSISymptomAcid",colnames(dataImpWhole))]
MainSxRSSISymptomNonacid<-dataImpWhole[,grepl("MainSxRSSISymptomNonacid",colnames(dataImpWhole))]
MainSxRSSISymptomAllReflux<-dataImpWhole[,grepl("MainSxRSSISymptomAllReflux",colnames(dataImpWhole))]


MainSxRSIAcid<-dataImpWhole[,grepl("MainSxRSIAcid",colnames(dataImpWhole))]
MainSxRSINonAcid<-dataImpWhole[,grepl("MainSxRSINonAcid",colnames(dataImpWhole))]
MainSxRSIAllReflux<-dataImpWhole[,grepl("MainSxRSIAllReflux",colnames(dataImpWhole))]

SxMainRSAPAcid<-dataImpWhole[,grepl("SxMainRSAPAcid",colnames(dataImpWhole))]
SxMainRSAPNonacid<-dataImpWhole[,grepl("SxMainRSAPNonacid",colnames(dataImpWhole))]
SxMainRSAPAllReflux<-dataImpWhole[,grepl("SxMainRSAPAllReflux",colnames(dataImpWhole))]


#Put into a list
mylist<-list(MainSxCorrSymptomAcidrel,
             MainSxCorrSymptomNonAcidrel,
             MainSxCorrSymptomAllRefluxrel,
             MainSxCorrSymptomSxIndex,
             MainSxCorrSymptomUnrelated,
             MainSxRSSISymptomAcid,
             MainSxRSSISymptomNonacid,
             MainSxRSSISymptomAllReflux,
             MainSxRSIAcid,
             MainSxRSINonAcid,
             MainSxRSIAllReflux,
             SxMainRSAPAcid,SxMainRSAPNonacid,SxMainRSAPAllReflux)
names(mylist)<-c("MainSxCorrSymptomAcidrel",
                 "MainSxCorrSymptomNonAcidrel",
                 "MainSxCorrSymptomAllRefluxrel",
                 "MainSxCorrSymptomSxIndex",
                 "MainSxCorrSymptomUnrelated",
                 "MainSxRSSISymptomAcid",
                 "MainSxRSSISymptomNonacid",
                 "MainSxRSSISymptomAllReflux",
                 "MainSxRSIAcid",
                 "MainSxRSINonAcid",
                 "MainSxRSIAllReflux",
                 "SxMainRSAPAcid","SxMainRSAPNonacid","SxMainRSAPAllReflux")

#iterate through each data frame and then gsub the name of the dataframe from the column names
#Then ggplot the column
SxPlotter<-function(x,mylist){
  
  
  this_is_a_name <- x; 
  print(this_is_a_name)
  
  #This is to make sure that I get the data from the dataframe as I am passing the names into the function
  x <- data.frame(mylist[[x]],stringsAsFactors=F)
  #class(x)
  colnames(x)<-gsub(this_is_a_name,"",colnames(x))


    #ggplot(meltdf,aes(x=variable,y=value),na.rm=T)+
      #geom_boxplot()
  try(meltdf<-melt(x))
 if(exists("meltdf")){
   if("variable" %in% colnames(meltdf)){
     

try(ggplot(meltdf,aes(x=variable,y=value))+
  geom_boxplot(fill="red",outlier.shape = NA)+
  labs(title=this_is_a_name)+
  theme(axis.text.x=element_text(angle=-45))+
    coord_cartesian(ylim = c(0, 100)))

}
}
}
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
mygraphs<-lapply(names(mylist),SxPlotter,mylist=mylist)
multiplot(plotlist=mygraphs)

#Then put all the plots on one so can see them all




#Now split-apply-combine into a series of bar charts grouped by symptoms
#Convert symptom variables to long format so they can be split

# 
# 
# 
# 
# PClst<-split(dataImp_SympLong,dataImp_SympLong$Symptom)
# #gsub the symptoms 
# 

# 
# 
# 
# #If the row has a number on it for a symptom then chalk up the symptom so you can then split-apply-combine it
# source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
# 
# source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
# #Some cleaning up
# #Need to apply this to one column only
# #data<-replace(data[1:ncol(data)], is.na(data[1:ncol(data)]), "Nought")
# data$VisitDate<-as.character(data$VisitDate)
# data$VisitDate<-as.Date(data$VisitDate,"%d_%m_%Y")
# 
# source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Manometry\\MotilityFunctions.R")
# data<-HRMCleanUp1(data)
# data<-HRMCleanUp(data)
# 
# data$dx[is.na(data$dx)]=0






######################################################################################################################################
######################################################################################################################################
################################### Subset spinning using dplyr ###################################################################### 
######################################################################################################################################
######################################################################################################################################

###############Timing subsets##########################################################################################
#To get the postprandial data only:

dataImpWholePP<-select(dataImpWhole,contains("PP"))

#To get the upright data only:
dataImpWholeUpright<-select(dataImpWhole,contains("Upright"))

#To get the recumbent data only:
dataImpWholeRecumb<-select(dataImpWhole,contains("Recum"))


###############Type subsets##########################################################################################
#To get the acid data only
dataImpWholeAcid<-select(dataImpWhole,contains("cid"))
dataImpWholeAcid<-select(dataImpWholeAcid,-contains("Non"))

#To get the Nonacid data only:
dataImpWholeNonAcid<-select(dataImpWhole,contains("Non"))

###############Symptom subsets##########################################################################################
dataImpWholeCough<-filter(dataImpWhole,Cough=="Cough")



############### Demonstration of how to spin all the symptoms together #################################################
########################################################################################################################

#Question - is there a difference between non-acid events depending on symptom:

#make data long according to the symptom:
dataImpWholeNonAcid<-select(dataImpWhole,contains("Non"),Cough,Vomiting,StomachPain,Nausea,Regurgitation,Belch,Chest) %>%
gather(variable, value, Cough:Chest)%>%
group_by(value)%>%
summarise_each(funs(mean = mean(., na.rm = TRUE))) 

#Then plot it:
#Get the columns that are not RSI etc symptom specific

dataImpWholeNonAcid<-dataImpWholeNonAcid[1:10]
dataImpWholeNonAcid_Long <- melt(dataImpWholeNonAcid ,  id.vars = 'value', variable.name = 'series')
names(dataImpWholeNonAcid_Long)<-c("Sx","Col","Value")
dataImpWholeNonAcid_Long$Col<-strwrap(dataImpWholeNonAcid_Long$Col,width=5)
ggplot(dataImpWholeNonAcid_Long, aes(Col,Value,group = Sx)) + geom_point()+ geom_line(colour="red")+facet_grid(Sx ~ .)+
  labs(title="Non acid reflux and relationship to symptoms")+
  theme(axis.text.x=element_text(angle=-45))


dataImpWholeNonAcid_Long$Col<-str_wrap(dataImpWholeNonAcid_Long$Col,5)
ggplot(dataImpWholeNonAcid_Long, aes(Sx,Value,group=Col)) + geom_point()+ geom_line(colour="red")+ facet_grid(Col ~ .,scales="free")+
  labs(title="Non acid reflux and relationship to symptoms")
  

#Question - is there a difference between Acid events depending on symptom:
#make data long according to the symptom:

dataImpWholeAcid<-select(dataImpWhole,contains("cid"),Cough,Vomiting,StomachPain,Nausea,Regurgitation,Belch,Chest) 
dataImpWholeAcid<-select(dataImpWholeAcid,-contains("Non"),Cough,Vomiting,StomachPain,Nausea,Regurgitation,Belch,Chest) %>%
  gather(variable, value, Cough:Chest)%>%
  group_by(value)%>%
  summarise_each(funs(mean = mean(., na.rm = TRUE))) 

#Then plot it:
#Get the columns that are not RSI etc symptom specific

dataImpWholeAcid<-dataImpWholeAcid[1:31]
dataImpWholeAcid_Long <- melt(dataImpWholeAcid ,  id.vars = 'value', variable.name = 'series')
names(dataImpWholeAcid_Long)<-c("Sx","Col","Value")
dataImpWholeAcid_Long$Col<-strwrap(dataImpWholeAcid_Long$Col,width=5)
ggplot(dataImpWholeAcid_Long, aes(Col,Value,group = Sx)) + geom_point()+ geom_line(colour="red")+facet_grid(Sx ~ .)+
  labs(title="Acid reflux and relationship to symptoms")+
  theme(axis.text.x=element_text(angle=-45,hjust=0))


dataImpWholeAcid_Long$Col<-str_wrap(dataImpWholeAcid_Long$Col,5)
ggplot(dataImpWholeAcid_Long, aes(Sx,Value,group=Col)) + geom_point()+ geom_line(colour="red")+ facet_grid(Col ~ .,scales="free")+
  labs(title="Acid reflux and relationship to symptoms")



#Question are you more likely to have acid or non acid events whilst recumbent

#To get the upright data only:
dataImpWholeUpright<-select(dataImpWhole,contains("Upright"))

#To get the recumbent data only:
dataImpWholeRecumb<-select(dataImpWhole,contains("Recum"))
#This compares the recumbent and upright column by column
boxplot(dataImpWholeUpright[,1],dataImpWholeRecumb[,1])
boxplot(dataImpWholeUpright[,2],dataImpWholeRecumb[,2])
boxplot(dataImpWholeUpright[,3],dataImpWholeRecumb[,3])
boxplot(dataImpWholeUpright[,4],dataImpWholeRecumb[,4])
boxplot(dataImpWholeUpright[,5],dataImpWholeRecumb[,5])
boxplot(dataImpWholeUpright[,6],dataImpWholeRecumb[,6])
boxplot(dataImpWholeUpright[,7],dataImpWholeRecumb[,7])
boxplot(dataImpWholeUpright[,8],dataImpWholeRecumb[,8])
boxplot(dataImpWholeUpright[,9],dataImpWholeRecumb[,9])
boxplot(dataImpWholeUpright[,10],dataImpWholeRecumb[,10])
boxplot(dataImpWholeUpright[,11],dataImpWholeRecumb[,11])
boxplot(dataImpWholeUpright[,12],dataImpWholeRecumb[,12])
boxplot(dataImpWholeUpright[,13],dataImpWholeRecumb[,13])
boxplot(dataImpWholeUpright[,14],dataImpWholeRecumb[,14])
boxplot(dataImpWholeUpright[,15],dataImpWholeRecumb[,15])
boxplot(dataImpWholeUpright[,16],dataImpWholeRecumb[,16])
boxplot(dataImpWholeUpright[,17],dataImpWholeRecumb[,17])
boxplot(dataImpWholeUpright[,18],dataImpWholeRecumb[,18])
boxplot(dataImpWholeUpright[,19],dataImpWholeRecumb[,19])
boxplot(dataImpWholeUpright[,20],dataImpWholeRecumb[,20])
boxplot(dataImpWholeUpright[,21],dataImpWholeRecumb[,21])
boxplot(dataImpWholeUpright[,22],dataImpWholeRecumb[,22])
boxplot(dataImpWholeUpright[,23],dataImpWholeRecumb[,23])
boxplot(dataImpWholeUpright[,24],dataImpWholeRecumb[,24])





###################################################################################################
###################################################################################################
########################Cross reference HRM with Impedance data ######################################
###################################################################################################

.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")
data <- sqlQuery( channel , "SELECT  HRMImportMain.* FROM HRMImportMain")
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
#Some cleaning up
#Need to apply this to one column only
#data<-replace(data[1:ncol(data)], is.na(data[1:ncol(data)]), "Nought")
data$VisitDate<-as.character(data$VisitDate)
data$VisitDate<-as.Date(data$VisitDate,"%d_%m_%Y")

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Manometry\\MotilityFunctions.R")
data<-HRMCleanUp1(data)
data<-HRMCleanUp(data)

data$dx[is.na(data$dx)]=0

data$HospNum_Id<-as.character(data$HospNum_Id)

###################################################################################################
###################################################################################################
########################Do the cross reference ######################################
###################################################################################################


#Rename Impedance HospNum so the columns can be merged


MyImpedanceDataWithHRM<-merge(dataImpWhole,data,by=c("HospNum_Id"),all=TRUE)

#MyImpedanceDataWithHRM<-dplyr::inner_join(dataImpWhole,data,by="HospNum_Id")
MyImpedanceDataWithHRM$DateDiff<-MyImpedanceDataWithHRM$VisitDate.x-MyImpedanceDataWithHRM$VisitDate.y

#TO prevent having duplicate barium swallows for a patient, pick the HRM closesnt to the barium
#THIS IS NOW ALL THE IMPEDANCE WITH ASSOCIATED HRM DATA AS A CENTRAL DATASET TO ASK QUESTIONS FROM:
MyImpedanceDataWithHRM<-subset(MyImpedanceDataWithHRM,MyImpedanceDataWithHRM$DateDiff<365)
MyImpedanceDataWithHRM<-subset(MyImpedanceDataWithHRM,MyImpedanceDataWithHRM$DateDiff>-365)

MyImpedanceDataWithHRMNoDups<-MyImpedanceDataWithHRM%>%
  group_by(HospNum_Id,HospNum_Id)%>%
  slice(which.min(DateDiff))

MyImpedanceDataWithHRMNoDups<-as.data.frame(MyImpedanceDataWithHRMNoDups)
MyImpedanceDataWithHRMNoDups2<-MyImpedanceDataWithHRMNoDups

MyImpedanceDataWithHRMNoDups$HHSize<-MyImpedanceDataWithHRMNoDups$DistalLESfromnarescm -MyImpedanceDataWithHRMNoDups$PIPfromnarescm
MyImpedanceDataWithHRMNoDups$HHSizeCategory<-ifelse(MyImpedanceDataWithHRMNoDups$HHSize<=0,"NoHH",
                                                    ifelse(MyImpedanceDataWithHRMNoDups$HHSize>0&MyImpedanceDataWithHRMNoDups$HHSize<=2,"Type1",
                                                           ifelse(MyImpedanceDataWithHRMNoDups$HHSize>2&MyImpedanceDataWithHRMNoDups$HHSize<=5,"Type2",
                                                                  ifelse(MyImpedanceDataWithHRMNoDups$HHSize>5,"Type3","Nil"))))

###############Question does the manometry reflect wheterh the patient has acid or non acid reflux at all?###################
#Just get the manometry and type of Acid:
MyImpedanceDataWithHRMNoDups<-MyImpedanceDataWithHRMNoDups[,c(208,219:248)]
MyImpedanceDataWithHRMNoDups$VisitDate.y<-NULL
#MyImpedanceDataWithHRMNoDups<-Filter(is.numeric,MyImpedanceDataWithHRMNoDups)

MyImpedanceDataWithHRMNoDups[is.na(MyImpedanceDataWithHRMNoDups)] <- 0
############### For acid reflux are there subtypes eg reduced clearance vs other?????############### 
MyImpedanceDataWithHRMNoDupsAcid<-subset(MyImpedanceDataWithHRMNoDups,MyImpedanceDataWithHRMNoDups$TypeOfAcid=="Acid")




##################### Question: For each manometry diagnosis determine the impedance ##################### 

# Need to separate the impedance columns into groups as there are too many of them
#Then group by diagnosis on manometry
#Do per group facet plots

#do as function

 

ImpedanceVsHRMDiagnoses<-function(x,y){
x<-select(x,contains(y),dx)%>%
  gather(variable, value, dx)%>%
  group_by(value)%>%
  summarise_each(funs(mean = mean(., na.rm = TRUE))) 
x <- melt(x ,  id.vars = 'value', variable.name = 'series')
names(x)<-c("Sx","Col","Value")
x$Col<-strwrap(x$Col,width=5)
ggplot(x, aes(Col,Value,group = Sx)) + geom_point()+ geom_line(colour="red")+facet_grid(Sx ~ .)+
  labs(title="Impedance and relationship to manometric findings")+
  theme(axis.text.x=element_text(angle=-45,hjust=0))
}

ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainAcidComp")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"PP")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainPPAcidExp")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainProxExtent")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainRflxEpisode")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainStats")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainBolusExpo")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainSxCorrSymptom")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainSxCorrSymptomS")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"MainSxRSI")
ImpedanceVsHRMDiagnoses(MyImpedanceDataWithHRMNoDups,"SxMainRSAP")



#Now compare types of reflux to manometry:
TheHRMBit<-MyImpedanceDataWithHRMNoDups[,207:247]
#Also remove the 
RefluxSubtypesVsManometry<-function(x,y){
x<-x%>%
gather(variable, value, match(y,names(.)))%>%
group_by(value)%>%
  summarise_each(funs(mean = mean(., na.rm = TRUE))) 

x<-x[,c(1,13:ncol(x)-3)]
x <- melt(x ,  id.vars = 'value', variable.name = 'series')
x<-as.data.frame(x)
names(x)<-c("Sx","Col","Value")

ggplot(x, aes(Col,Value,group = Sx)) + geom_point()+ geom_line(colour="red")+facet_grid(Sx ~ .)+
  labs(title="Impedance and relationship to manometric findings")+
  theme(axis.text.x=element_text(angle=-45,hjust=0))
}

RefluxSubtypesVsManometry(MyImpedanceDataWithHRMNoDups[,207:247],"TypeOfAcid")
RefluxSubtypesVsManometry(MyImpedanceDataWithHRMNoDups[,207:247],"PositionOfAcid")
RefluxSubtypesVsManometry(MyImpedanceDataWithHRMNoDups[,207:247],"PositionOfNonAcid")

#Do the same according to reflux type


#Do the same according to symptom 



#Also need to do multivariate analysis to see what is predictive of each condition from manometry findings




#Now group according to manometry diagnosis and do a facet plot



#Produce matrix where columns are the manometry diagnoses and rows are the impedance diagnoses



##################### Question: For each impedance subtype determine the manometric findings ##################### 

#Import breath test data into the mix as well


##################### Question: Oesophageal pump failure as DCI vs LOS #####################


#Restrict to normal values for LOS tension (as in not hyper) and DCI (to exclude spasm)

MyImpedanceDataWithHRM<-MyImpedanceDataWithHRM[MyImpedanceDataWithHRM$ResidualmeanmmHg<300&MyImpedanceDataWithHRM$ResidualmeanmmHg>0&MyImpedanceDataWithHRM$BasalrespiratoryminmmHg>0&MyImpedanceDataWithHRM$ResidualmeanmmHg<15,]
MyImpedanceDataWithHRM<-MyImpedanceDataWithHRM[MyImpedanceDataWithHRM$DCI>500&MyImpedanceDataWithHRM$DCI<5000&MyImpedanceDataWithHRM$DCI>0,]


ggplot(MyImpedanceDataWithHRM,aes(BasalrespiratoryminmmHg,DCI))+
  geom_point()+geom_smooth()

library(GGally)


lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = 'blue', alpha=0.3, size=4) +
    geom_smooth(color = 'black', method='lm', size=1,...)
  p
}
g <- ggpairs(data = MyImpedanceDataWithHRM,columns=219:243)

g <- g + theme(
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)


