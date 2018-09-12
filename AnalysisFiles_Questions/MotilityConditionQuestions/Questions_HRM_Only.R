#HRM Only Questions

#Generics:
# Time based questions swallows over time- to determine patterns over time esp with the swallows
# Subclassification/clustering functions


#Normal parameters
# Question:Improvement in swallow over time.
# Question:Fluctuation over time in HRM- look at repeat HRM and plot each parameter as a timeline


########## HRM DIAGNOSIS RELATED QUESTIONS #############
#Aperistalsis
# Question:Full description of the aperistaltic oesophagus and it's potential causes.
# Questions: Difference in parameters between different components of the Eckhardt score.
# Subclassification questions eg difference between GERD related achalasia and non-GERD related achalasia.

#Achalasia
# Question: Achalasia - HRM vs outcomes. HRM vs endoscopy findings.
# Question: Proportion of normal IRP achalasia by looking at relaxation OR relaxation cut off for achalasia as a separate metric.
# Question: Does myotomy worsen peristalsis- have a look at any patients with pre and post myotomy HRM.
# Question: Is there any difference in parameters between normal IRP achalasia and classic achalasia?
# Subclassification questions

#EGOO
# Question:Swallow in patients with EGJ obstruction with intermittent normality.
# Question:HRM predictors of progression from EGF to achalasia. How to differentiate EGF subtypes.
# Subclassification questions

#IOM
# Question:Age vs IOM...Is there are chance a thing as degenerative oesophagus. Age vs DCI?
# Question: Are there different phenotypes of IOM ..see....https://www.nature.com/articles/ctg20174.... and In ineffective esophageal motility, failed swallows are more functionally relevant than weak swallows....but what about redoing by clustering
# Subclassification questions

#Changes before and after a procedure
#Question: Before and after fundoplication
#Question: Before and after myotomy
#Question: Before and after any other procedure


############### SYMPTOM RELATED QUESTIONS ##########


#Which HRM findings are specific to which symptoms?
# see https://www.ncbi.nlm.nih.gov/pubmed/29969856


#Import the test data
library(PhysiMineR)

data<-read_xlsx("/home/rstudio/PhysiMineR/data/HRMAll.xlsx")

data2<-HRMCleanUp1(data)
data2<-HRMDiagnoses(data2)



