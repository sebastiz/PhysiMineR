library(stringr)
library(XLConnect)
library(dplyr)


Self = loadWorkbook("~\\Gastroenterology\\Seb\\R\\Data\\AdHoc\\BRAVO_RatesDonePerWeek.xlsx")
Self = XLConnect::readWorksheet(Self, sheet="Sheet1",header=TRUE)

Self$Weekday<-weekdays(as.Date(Self$DATEOFPROCEDURE,'%d/%m/%Y'))
Self$DATEOFPROCEDURE<-as.Date(Self$DATEOFPROCEDURE,'%d/%m/%Y')

SelfWed<-Self[Self$Weekday=='Wednesday',]

#Group by month

BRAVOsEachWed<-SelfWed%>%select(DATEOFPROCEDURE)%>%
  group_by(DATEOFPROCEDURE)%>%summarise (count=n())

library(ggplot2)
ggplot(BRAVOsEachWed,aes(DATEOFPROCEDURE, count, color = "red")) + 
 geom_bar(stat="identity", fill="lightblue")
