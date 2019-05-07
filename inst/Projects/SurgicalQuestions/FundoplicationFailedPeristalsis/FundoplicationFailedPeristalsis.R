#Script to extract the aperistaltic patients who underwent fundoplication


library(PhysiMineR)
library(DiagrammeR)
library(CodeDepends)
library(tidyverse)
library(readxl)
library(EndoMineR)
library(dplyr)
library(kableExtra)


######################################### Data acquisiton#########################################
source("/home/rstudio/PhysiMineR/R/sourcing_Data/PhysiData_Acq.R")

######################################### Data cleaning & Data merging#########################################
source("/home/rstudio/PhysiMineR/R/sourcing_Data/PhysiData_Clean.R")

######################################### Data accordionisation#########################################
source("/home/rstudio/PhysiMineR/R/sourcing_Data/PhysiData_Accordion.R")


HRMImportMain$LOS_relax<-as.factor(HRMImportMain$LOS_relax)
HRMImportMain$LowerOesoph<-as.factor(HRMImportMain$LowerOesoph)

#Get the fundoplication data

FundoplicationData <- read_excel("/home/rstudio/PhysiMineR/data/FundoplicationHospitalNums.xlsx")

names(FundoplicationData)<-c("HospNum_Id")


#Get the HRM for all the Fundoplication patients
HRM_In_Fundoplication<-filter(HRMImportMain, HospNum_Id %in% FundoplicationData$HospNum_Id)

Aperistaltic_Fundoplication<-HRM_In_Fundoplication[HRM_In_Fundoplication$failedChicagoClassification>100,]

View(HRM_In_Fundoplication%>%filter(failedChicagoClassification>99))


#Get the fundoplication
