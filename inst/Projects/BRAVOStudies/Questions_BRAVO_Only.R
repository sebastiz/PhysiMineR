#Questions_BRAVO_Only

#Question: Yield- Number of patients with negative impedance with positive bravos.
#Question: Management- Bravos over time and number of failed bravos/ number of impedance etc over time.

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

#Question: Patient acceptability - BRAVO in patients with EoE tolerance and maintenance of the capsule as well as findings
#Question: Yield - Proportion of patients with negative bravos in the first 24 hours that have subsequent positives.Patient acceptability - BRAVO in patients with EoE tolerance and maintenance of the capsule as well as findingsYield - Proportion of patients with negative bravos in the first 24 hours that have subsequent positives.
#Question: BRAVO yield questions given different indications (eg heartburn  vs cough etc.)




#Statistical Questions
####Associations
####Correlations
####Predictions

#Clinical questions
####Yield
####Patient Acceptability
####
####

