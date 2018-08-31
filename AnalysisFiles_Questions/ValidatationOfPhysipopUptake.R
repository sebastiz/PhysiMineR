library(stringr)
library(data.table)
#This script does two things 1. It looks at the error logs and classifies them so making bug fixes easier 
#and secondly it tells you which files have not been entered into the database so you can be happy that there is no discrepancy
#This should be run every time the database is updated 
#Planned is to see if I can getr the error for the files that have not been uploaded so I can find out why easily.

#The idea is to get the data from the file attributes of the files in the Phjysiology folder and then
#see if the matching date attribute (Year only) is present for the patient in the database
#If the patient is missing then to look in the logs to see why
#Also allows examination of the logs to see patterns of bugs 




#####The way to work it is
#a)Get the bug list
#b)Move the files on the logError into their respective folders
#)Set the breakspoints on the Eclipse files 
#)Run whichever folder it is that you want to sort out and see what the breakpoint issues are:




##################################################################################################
##################################################################################################
##################################################################################################
##############     Preparing the data     ########################################################
##################################################################################################
##################################################################################################
##################################################################################################
#Get all the file names
myfileNames<-list.files(path="I:\\OesophagealLabReports\\1-All reports",recursive=T)
#Get all the file info
finf<-file.info(list.files(path="I:\\OesophagealLabReports\\1-All reports", full.names=TRUE,recursive=T))
finf$FileName<-row.names(finf)

#Get the filenames to look the saem without the whole path
finf$FileName<-gsub(".*?\\/","",finf$FileName)

#Convert list to a data frame
myfileNames_df<-data.frame(matrix(unlist(myfileNames), byrow=T),stringsAsFactors=FALSE)
#Rename the columns
names(myfileNames_df)<-c("FileName")
myfileNames_df$FileName<-gsub(".*?\\/","",myfileNames_df$FileName)


#Merge the two datasets together
myfileNames_df<-merge(myfileNames_df,finf,by=c("FileName"))

#Extract the Hospital Number
myfileNames_df$HospNum<-str_extract(myfileNames_df$FileName,"(?:\\d{6,9}(?:[A-Z]|[a-z]))|(?:(?:[A-Z]|[a-z])\\d{6,7})|\\d{10}")
#naftHosp<-myfileNames_df[is.na(myfileNames_df$HospNum),]

#Extract the file type
myfileNames_df$Filetype<-str_extract(myfileNames_df$FileName,"[Dd][Oo][Cc]|[Rr][Tt][Ff]|[Pp][Dd][Ff]|[Dd][Oo][Cc][Xx]")
#Quick check to make sure getting the files extracted:
#naft<-myfileNames_df[is.na(myfileNames_df$Filetype),]
myfileNames_df$Filetype<-gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(myfileNames_df$Filetype), perl=TRUE)
#Extract whether HRM/ impedance etc

myfileNames_df$StudyType<-str_extract(myfileNames_df$FileName,"[Mm][Ii][Ii]|[Hh][Rr][Mm]|[Ii][Mm][Pp]|[Pp][Hh]|[Bb][Rr][Aa][Vv][Oo]|[Ff][Ii][Nn][Aa][Ll]|[Pp][Aa][Ee]")
#naftSt<-myfileNames_df[is.na(myfileNames_df$StudyType),]
#Convert DtudyType to lower case
myfileNames_df$StudyType<-gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(myfileNames_df$StudyType), perl=TRUE)




#Exclude raw files etc:
myfileNames_df<-myfileNames_df[!grepl("\\.[Cc]|\\.[Ss]",myfileNames_df$FileName),]

#Sort the dates out
myfileNames_df$ctime<-as.Date(myfileNames_df$ctime)
myfileNames_df$mtime<-as.Date(myfileNames_df$mtime)
myfileNames_df$atime<-as.Date(myfileNames_df$atime)
myfileNames_df$Year<-format(myfileNames_df$ctime, "%Y") 
#Create the hospital number and Year merge
myfileNames_df$HospNumYear<-paste(myfileNames_df$HospNum,"_",myfileNames_df$Year)

####################### ####################### ####################### ####################### 
####################### ####################### ####################### ####################### 
####################### ####################### ####################### ####################### 
####################### Separate into the different test datasets ####################### 
####################### ####################### ####################### ####################### 
####################### ####################### ####################### ####################### 
####################### ####################### ####################### ####################### 
#Count the numbers of each:BRAVO
  myfileNames_dfBRAVO<-myfileNames_df[grepl("[Bb][Rr][Aa][Vv][Oo]",myfileNames_df$StudyType),]
#Count the numbers of each:HRM
myfileNames_dfHRM<-myfileNames_df[grepl("[Hh][Rr][Mm]",myfileNames_df$StudyType),]
#Count the numbers of each:Impedance
myfileNames_dfImpedance<-myfileNames_df[grepl("[Ii][Mm][Pp]",myfileNames_df$StudyType),]
#Count the numbers of each:Final
myfileNames_dfFinal<-myfileNames_df[grepl("[Ff][Ii][Nn][Aa][Ll]",myfileNames_df$StudyType),]
#Count the numbers of each:pH
myfileNames_dfpH<-myfileNames_df[grepl("[Pp][Hh]",myfileNames_df$StudyType),]

myfileNames_dfOthers<-myfileNames_df[!grepl("(?:[Pp][Aa][Ee])|(?:[Mm][Ii][Ii])|(?:[Pp][Hh])|(?:[Ff][Ii][Nn][Aa][Ll])|(?:[Bb][Rr][Aa][Vv][Oo])|(?:[Ii][Mm][Pp])|(?:[Hh][Rr][Mm])",myfileNames_df$StudyType),]

########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################           Import the logfile:                ########################################   ########################################
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
ErrorLogs <- read.delim("S:/Gastroenterology/Seb/JavaPortableLauncher/PhysiPopDONOTTOUCH/logMe.txt", header=FALSE)

#Extract the hospital numbers
ErrorLogs$HospNum<-str_extract(ErrorLogs$V2,"(?:\\d{6,9}(?:[A-Z]|[a-z]))|(?:(?:[A-Z]|[a-z])\\d{6,7})|\\d{10}")
#Extract the filetype
ErrorLogs$Filetype<-str_extract(ErrorLogs$V2,"[Dd][Oo][Cc]|[Rr][Tt][Ff]|[Pp][Dd][Ff]|[Dd][Oo][Cc][Xx]")
ErrorLogs$Filetype<-gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ErrorLogs$Filetype), perl=TRUE)
#Extract the StudyType
ErrorLogs$StudyType<-str_extract(ErrorLogs$V2,"[Mm][Ii][Ii]|[Hh][Rr][Mm]|[Ii][Mm][Pp]|[Pp][Hh]|[Bb][Rr][Aa][Vv][Oo]|[Ff][Ii][Nn][Aa][Ll]|[Pp][Aa][Ee]")
ErrorLogs$StudyType<-gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ErrorLogs$StudyType), perl=TRUE)

ErrorLogs$V2<-gsub("java.lang.NullPointerException","",ErrorLogs$V2)
#Remove patient names etc
ErrorLogs$V2<-gsub("[A-Z]{3}.*?:","",ErrorLogs$V2)
ErrorLogs$V2<-gsub("\\+.*","",ErrorLogs$V2)

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
########################      FIX THESE BUGS    ################################################################################################
################################################################################################################################################
################################################################################################################################################


#Get rid of blank space etc
ErrorLogs<-ErrorLogs[grepl("ERROR",ErrorLogs$V1),]
#########Overview of the Error Logs
barplot(table(ErrorLogs$StudyType))

#Just for the Final 
ErrorLogsFinal<-ErrorLogs[grepl("Final",ErrorLogs$StudyType),]

#Just for the Impedance 
ErrorLogsImp<-ErrorLogs[grepl("Imp",ErrorLogs$StudyType),]

#Just for the Bravo 
ErrorLogsBravo<-ErrorLogs[grepl("Bravo",ErrorLogs$StudyType),]

#Just for the HRM 
ErrorLogsHRM<-ErrorLogs[grepl("Hrm",ErrorLogs$StudyType),]


#Building a buglist
library(data.table)
dt = as.data.table(ErrorLogs)
Buglist<-dt[, .N ,by = ErrorLogs$V1]
Buglist<-as.data.frame(Buglist)
Buglist[order(Buglist$ErrorLogs),]


#Bug drilling:
library(dplyr)
ErrorLogs2<-ErrorLogs
View(ErrorLogs2%>%arrange(V1))
#Is this a filetype or studytype bug?
Mytab<-data.frame(table(ErrorLogs2$V1,ErrorLogs2$Filetype,ErrorLogs2$StudyType),stringsAsFactors=F)
Mytab2<-Mytab[grepl("ERROR:",Mytab$Var1),]
FinalTable<-Mytab2%>%filter(Freq>0)%>%arrange(Var1)
View(FinalTable)

#Now how to actually recreate the error to find the specific problem?

#Split the files according to File type and Study type
splitList<-split(ErrorLogs2,list(ErrorLogs$Filetype,ErrorLogs$StudyType))
#For each split, create a folder with studyname_filetype






MyfileCopyingFunction<-function(x,splitList,myList){
  this_is_a_name <- x; 
  #This is to make sure that I get the data from the dataframe as I am passing the names into the function
  x <- splitList[[x]] 
  #Get the hospital numbers from the list element
  TheNum<-tolower(as.character(unique(x$HospNum)))
  TheFileType<-unique(x$Filetype)
  TheStudyType<-unique(x$StudyType)

  
  #TheNum is a list so need to find each element of TheNum and match with any of finf
  #Then filter by Study type and File type from the report

  if((length(TheNum) != 0)){
  #Need to lapply for each value in TheNum
    dfNames2<-unlist(lapply(TheNum,function(x)myList[grepl(x,myList,ignore.case=TRUE)]))
    dfNames3<-unlist(lapply(TheFileType,function(x)dfNames2[grepl(x,dfNames2,ignore.case=TRUE)]))
    dfNames4<-unlist(lapply(TheStudyType,function(x)dfNames3[grepl(x,dfNames3,ignore.case=TRUE)]))
    #If can find the file, then dump it in the folder you create
    dir.create(paste0("S:/Gastroenterology/Seb/JavaPortableLauncher/PhysiPopDONOTTOUCH/Outputs/BugFolder/",this_is_a_name),showWarnings = FALSE)
    #Copy all the files for the dfNames4 into the folder
    lapply(dfNames4, function(x) file.copy(paste0("I:/OesophagealLabReports/1-All reports/", x ),  
                                           paste0("S:/Gastroenterology/Seb/JavaPortableLauncher/PhysiPopDONOTTOUCH/Outputs/BugFolder/",this_is_a_name,"/",gsub("/","",x,fixed=T)),  copy.mode = TRUE))
}
}

#Lower case all the files
#Then run the function to get the error files into the debugFolders
myList<-myfileNames
lapply(names(splitList),MyfileCopyingFunction,splitList=splitList,myList=myList)


########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################           Import the database to find which files are missing from the db ###########
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   
########################################   ########################################   ########################################   ########################################   



#Now need to compare what is stored in the database with the actual file
#Do this by comparing for each modality what is present in the files but not in the database
#So you need to get the hospital number and the year of the study from the file dataset and see if its in the database dataset for each modality

#SQL connection to the database:


library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")


#For HRM
#Validate the HRM dataset:
dataHRM <- sqlQuery( channel , "SELECT HRMImportMain.*FROM HRMImportMain",stringsAsFactors=F)
dataHRM$VisitDate<-as.Date(dataHRM$VisitDate,format="%m_%d_%Y")
#Extract the year
dataHRM$Year<-format(dataHRM$VisitDate, "%Y") 
dataHRM$HospNumYear<-paste(dataHRM$HospNum_Id,"_",dataHRM$Year)
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  
##########  These are the files that have not been uploaded ##########  ##########  ##########  ##########  ##########  
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########  

MyHRMsNotUploaded<-myfileNames_dfHRM[!myfileNames_dfHRM$HospNumYear %in% dataHRM$HospNumYear ,]
MyHRMsNotUploadedForMerge<-MyHRMsNotUploaded[!is.na(MyHRMsNotUploaded$HospNum),]
#Get the error logs for the ones not uploaded so yuo know why:
ErrorLogsForMerge<-ErrorLogs[!is.na(ErrorLogs$HospNum),]
MyHRMsNotUploaded<-merge(MyHRMsNotUploadedForMerge,ErrorLogsForMerge,by=c("HospNum"))
MyHRMsNotUploadedFile<-unique(data.frame(MyHRMsNotUploaded["HospNum"],MyHRMsNotUploaded["Filetype.x"]))



#For Impedance
dataImp <- sqlQuery( channel , "SELECT Impedance2.*FROM Impedance2",stringsAsFactors=F)
dataImp$VisitDate<-as.Date(dataImp$VisitDate,format="%m_%d_%Y")
#Extract the year
dataImp$Year<-format(dataImp$VisitDate, "%Y") 
dataImp$HospNumYear<-paste(dataImp$HospNum_Id,"_",dataImp$Year)
MyImpsNotUploaded<-myfileNames_dfImpedance[!myfileNames_dfImpedance$HospNumYear %in% dataImp$HospNumYear ,]
MyImpsNotUploadedForMerge<-MyImpsNotUploaded[!is.na(MyImpsNotUploaded$HospNum),]

#Get the error logs for the ones not uploaded so you know why:
ErrorLogsForMerge<-ErrorLogs[!is.na(ErrorLogs$HospNum),]
ImpErrorLofForNotUploaded<-merge(MyImpsNotUploadedForMerge,ErrorLogsForMerge,by=c("HospNum"))
ImpErrorLofForNotUploadedFile<-unique(data.frame(ImpErrorLofForNotUploaded["HospNum"],ImpErrorLofForNotUploaded["Filetype.x"]))





#For BRAVO
dataBravo <- sqlQuery( channel , "SELECT BRAVODay1And2.*FROM BRAVODay1And2",stringsAsFactors=F)
dataBravo$VisitDate<-as.Date(dataBravo$VisitDate,format="%m_%d_%Y")
#Extract the year
dataBravo$Year<-format(dataBravo$VisitDate, "%Y") 
dataBravo$HospNumYear<-paste(dataBravo$HospNum_Id,"_",dataBravo$Year)
MyBRAVONotUploaded<-myfileNames_dfBRAVO[!myfileNames_dfBRAVO$HospNumYear %in% dataBravo$HospNumYear ,]
MyBRAVONotUploadedForMerge<-MyBRAVONotUploaded[!is.na(MyBRAVONotUploaded$HospNum),]
#Get the error logs for the ones not uploaded so you know why:
ErrorLogsForMerge<-ErrorLogs[!is.na(ErrorLogs$HospNum),]
BRAVOErrorLofForNotUploaded<-merge(MyBRAVONotUploadedForMerge,ErrorLogsForMerge,by=c("HospNum"))
BRAVOErrorLofForNotUploadedFile<-unique(data.frame(BRAVOErrorLofForNotUploaded["HospNum"],BRAVOErrorLofForNotUploaded["Filetype.x"]))




#Validation for eac time PhysiPop is run:
#Basically I need to find out how many are uploaded and how many should have been uploaded.
#To find how many should have been uploaded I should count the number created in the previous 7 days from fileinfo
#To find out how many were actually uploaded to the database I need to find the difference between how many there were before upload
# and how many after the so record here:

#Number of new records in the past 7 days:
#Count the numbers of each:BRAVO
myfileNames_dfBRAVO<-myfileNames_df[grepl("[Bb][Rr][Aa][Vv][Oo]",myfileNames_df$StudyType),]
#Count the numbers of each:HRM
myfileNames_dfHRM<-myfileNames_df[grepl("[Hh][Rr][Mm]",myfileNames_df$StudyType),]
#Count the numbers of each:Impedance
myfileNames_dfImpedance<-myfileNames_df[grepl("[Ii][Mm][Pp]",myfileNames_df$StudyType),]
#Count the numbers of each:Final
myfileNames_dfFinal<-myfileNames_df[grepl("[Ff][Ii][Nn][Aa][Ll]",myfileNames_df$StudyType),]
#Count the numbers of each:pH
myfileNames_dfpH<-myfileNames_df[grepl("[Pp][Hh]",myfileNames_df$StudyType),]

myfileNames_dfOthers<-myfileNames_df[!grepl("(?:[Pp][Aa][Ee])|(?:[Mm][Ii][Ii])|(?:[Pp][Hh])|(?:[Ff][Ii][Nn][Aa][Ll])|(?:[Bb][Rr][Aa][Vv][Oo])|(?:[Ii][Mm][Pp])|(?:[Hh][Rr][Mm])",myfileNames_df$StudyType),]

Past7daysRecordsBRAVO<-myfileNames_dfBRAVO[myfileNames_dfBRAVO$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
Past7daysRecordsHRM<-myfileNames_dfHRM[myfileNames_dfHRM$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
Past7daysRecordsImpedance<-myfileNames_dfImpedance[myfileNames_dfImpedance$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
Past7daysRecordsFinal<-myfileNames_dfFinal[myfileNames_dfFinal$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
Past7daysRecordspH<-myfileNames_dfpH[myfileNames_dfpH$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
Past7daysRecordsOthers<-myfileNames_dfOthers[myfileNames_dfOthers$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]
#Number of records before upload:

