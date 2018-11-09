#Suppress warnings

oldw <- getOption("warn")
options(warn = -1)


#library(RODBC)
#channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")
#dataHRM(channel)
# OR
# HRMAndSwallows(channel)
# OR
# HRMAndDiag(channel)
#
# dataImp2(x)
# OR
# dataImp_Symp(channel)
#
# dataBRAVOTotal(channel)
# OR
# dataBRAVO(channel)


#Get this from the data folder for PhysiMineR-  just click on the RData to load into
#the global environment

#Or if using the local file system

BravoDay1And2 <- read_excel("/home/rstudio/PhysiMineR/data/BravoDay1And2.xls")
BravoDay3And4 <- read_excel("/home/rstudio/PhysiMineR/data/BravoDay3And4.xls")
BRAVOTotal <- read_excel("/home/rstudio/PhysiMineR/data/BRAVOTotal.xls")
Diag <- read_excel("/home/rstudio/PhysiMineR/data/Diag.xlsx")
HRMImportMain <- read_excel("/home/rstudio/PhysiMineR/data/HRMImportMain.xls")
HRMImportSwallows <- read_excel("/home/rstudio/PhysiMineR/data/HRMImportSwallows.xlsx")
Imp_Symp <- read_excel("/home/rstudio/PhysiMineR/data/Imp_Symp.xls")
Impedance2 <- read_excel("/home/rstudio/PhysiMineR/data/Impedance2.xls")

options(warn = oldw)
