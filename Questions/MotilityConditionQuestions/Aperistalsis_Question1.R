#The aperistaltic oesophagus


#Import the test data
library(PhysiMineR)



######################################### Data acquisiton#########################################

data<-read_xlsx("/home/rstudio/PhysiMineR/data/HRMAll.xlsx")


######################################### Data merging#########################################

######################################### Data cleaning#########################################
data2<-HRMCleanUp1(data)

######################################### Data accordionisation#########################################
data2<-HRMDiagnoses(data2)

######################################### Data forking (filtering and subsetting)#########################################

######################################### Data analysis#########################################

######################################### Code overview (with CodeDepends)#########################################
#library(codeDepends)
#sc = readScript("S:\\Gastroenterology\\Seb\\R\\Scripts\\Eosinophilics\\Eosinophilix.R")
#g = makeVariableGraph( info =getInputs(sc))


#if(require(Rgraphviz))
#  plot(g)
