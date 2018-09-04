#HRM Only Questions


#Normal parameters
# Question:Improvement in swallow over time.
# Question:Fluctuation over time in HRM- look at repeat HRM and plot each parameter as a timeline

#Aperistalsis
# Question:Full description of the aperistaltic oesophagus and it's potential causes. TICK


#Achalasia
# Question:Achalasia - HRM vs outcomes. HRM vs endoscopy findings.
# Question:Proportion of normal IRP achalasia by looking at relaxation OR relaxation cut off for achalasia as a separate metric.
# Question:does myotomy worsen peristalsis- have a look at any patients with pre and post myotomy HRM.

#EGOO
# Question:Swallow in patients with EGJ obstruction with intermittent normality.
# Question:HRM predictors of progression from EGF to achalasia. How to differentiate EGF subtypes.


#IOM
# Question:Age vs IOM...Is there are chance a thing as degenerative oesophagus. Age vs DCI?
# Question: Are there different phenotypes of IOM ..see....https://www.nature.com/articles/ctg20174....but what about redoing by clustering

#Import the test data
library(PhysiMineR)

data<-read_xlsx("/home/rstudio/PhysiMineR/data/HRMAll.xlsx")

data2<-HRMCleanUp1(data)
data2<-HRMDiagnoses(data2)



